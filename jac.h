
#ifndef JAC_H
#define JAC_H

#ifdef JAC_IMPL

	#ifndef OSTD_IMPL
	#define OSTD_IMPL
	#else
	#undef OSTD_IMPL
	#endif
	
#endif // JAC_IMPL

#define OSTD_HEADLESS
#include "One-Std/one-headers/one_system.h"
#include "One-Std/one-headers/one_string.h"

#define JAC_EXPORT

#define MAX_PROCEDURE_PARAMETERS 128
// If this goes over U16_MAX, things will break (because u16)
#define MAX_FILES U16_MAX

typedef struct Jac_File {
	string path;
	u64 source_index;
	u64 source_length;
} Jac_File;

typedef struct Source_Code_Location {
	u64 pos;
	u64 line_start_pos;
	u64 line_length;
	u16 c0, c1;
	u64 line_num;
	Jac_File file;
} Source_Code_Location;

typedef enum Jac_Log_Tag {
	JAC_LOG_TRACE = 1 << 0,
	JAC_LOG_COMPILE_ERROR = 1 << 1,
} Jac_Log_Tag;

typedef enum Jac_Result_Code {
	JAC_RESULT_NONE,
	JAC_RESULT_UNEXPECTED_TOKEN,
	JAC_RESULT_FILE_NOT_FOUND,
	JAC_RESULT_UNTERMINATED_TOKEN,
	JAC_RESULT_UNRESOLVED_TYPE,
	JAC_RESULT_INVALID_CONVERSION,
	JAC_RESULT_UNDEFINED_IDENTIFIER,
	JAC_RESULT_BAD_ARGUMENT_COUNT,
	JAC_RESULT_BAD_ARGUMENT_TYPE,
	JAC_RESULT_LIMITATION,
	JAC_RESULT_BAD_IMPLICIT_CONVERSION,
	JAC_RESULT_NON_STORAGE_USED_AS_STORAGE,
} Jac_Result_Code;

typedef struct Jac_Result {
	Jac_Result_Code code;
	Source_Code_Location error_location;
	string message;
	// void *binary_blob;
	// u64 binary_blob_size;
} Jac_Result;

typedef struct Jac_Config {
	bool _;
} Jac_Config;

JAC_EXPORT Jac_Result jac_compile(string first_file_path, Jac_Config config);


#ifdef JAC_IMPL
////////////////////////////// Implementation

typedef enum Token_Kind {
	TOKEN_UNSET = 0,
	
	TOKEN_ASCII_START = 32,
	
	TOKEN_KIND_COLON = ':',
	TOKEN_KIND_SEMICOLON = ';',
	TOKEN_KIND_DOLLAR = '$',
	TOKEN_KIND_LPAREN = '(',
	TOKEN_KIND_RPAREN = ')',
	TOKEN_KIND_LBRACE = '{',
	TOKEN_KIND_RBRACE = '}',
	TOKEN_KIND_LBRACKET = '[',
	TOKEN_KIND_RBRACKET = ']',
	TOKEN_KIND_STAR = '*',
	TOKEN_KIND_DOT = '.',
	TOKEN_KIND_COMMA = ',',
	TOKEN_KIND_EXCLAMATION = '!',
	TOKEN_KIND_LT = '<',
	TOKEN_KIND_GT = '>',
	TOKEN_KIND_EQUALS = '=',
	TOKEN_KIND_PLUS = '+',
	TOKEN_KIND_MINUS = '-',
	TOKEN_KIND_FSLASH = '/',
	TOKEN_KIND_AMP = '&',
	TOKEN_KIND_VLINE = '|',
	TOKEN_KIND_DOUBLE_QUOTE = '"',
	
	TOKEN_ASCII_END = 128,
	
	TOKEN_KIND_IDENTIFIER,
	
	TOKEN_KIND_INT_LITERAL,
	TOKEN_KIND_FLOAT_LITERAL,
	TOKEN_KIND_STRING_LITERAL,
	
	TOKEN_KIND_EOF,
	
	TOKEN_KIND_RIGHT_ARROW,
	TOKEN_KIND_GTE,
	TOKEN_KIND_LTE,
	TOKEN_KIND_EQ,
	TOKEN_KIND_NEQ,
	TOKEN_KIND_LAND,
	TOKEN_KIND_LOR,
	TOKEN_KIND_DOUBLE_COLON,
	TOKEN_KIND_SHIFT_LEFT,
	TOKEN_KIND_SHIFT_RIGHT,
	
	TOKEN_KIND_KW_IF,
	TOKEN_KIND_KW_ELSE,
	TOKEN_KIND_KW_WHILE,
	TOKEN_KIND_KW_RETURN,
	TOKEN_KIND_KW_DEFER,
	TOKEN_KIND_KW_BREAK,
	TOKEN_KIND_KW_CONTINUE,
	
	TOKEN_KIND_DIRECTIVES_START,
	
	TOKEN_KIND_DIRECTIVE_IF, // #if
	TOKEN_KIND_DIRECTIVE_ELSE, // #else
	TOKEN_KIND_DIRECTIVE_COMPILER, // #compiler
	TOKEN_KIND_DIRECTIVE_INLINE, // #inline
	
	TOKEN_KIND_DIRECTIVES_END,
	
} Token_Kind;

typedef struct Token {
	string text; // slice into source
	Token_Kind kind;
	u64 line_num;
	u64 line_start_pos;
} Token;

typedef struct Code_Dependency {
	struct Code_Node *node;
	struct Code_Dependency *next;
} Code_Dependency;

typedef enum Value_Flags {
	VALUE_NONE = 0,
	VALUE_FLOAT = 1 << 0,
	VALUE_SLICE = 1 << 1,
	VALUE_LITERAL = 1 << 2,
	VALUE_STRING = 1 << 3,
	VALUE_CODE_RESULT = 1 << 4,
	VALUE_NOTHING = 1 << 5,
} Value_Flags;
typedef struct Value {
	u32 vnum;
	union {
		u64 literal;
		string str;
		struct Code_Node *code;
	} imp;
	u32 flags; // Value_Flags
	u32 width; // Number of 64-bit registers needed
} Value;

typedef enum Op_Kind {
	OP_ADD,
	OP_SUB,
	OP_DIV,
	OP_MUL,
} Op_Kind;

u64 get_op_precedence(Op_Kind op) {
	switch (op) {
		
		case OP_MUL: // fallthrough
		case OP_DIV: 
			return 600;
			
		case OP_ADD: // fallthrough
		case OP_SUB: 
			return 500;
		
		default: assert(false); break;
	}
	assert(false);
	return 0;
}

Op_Kind get_op_from_token(Token_Kind t) {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wswitch-enum"
	switch (t) { // incomplete
		case '+': return OP_ADD;
		case '-': return OP_SUB;
		case '/': return OP_DIV;
		case '*': return OP_MUL;
		default: assert(false); break;
	}
	assert(false);
	return 0;
#pragma clang diagnostic pop
}

typedef enum Code_Node_Kind {
	CODE_NODE_NONE,
	CODE_NODE_SET,
	CODE_NODE_CALL,
	CODE_NODE_OP_ADD,
	CODE_NODE_OP_SUB,
	CODE_NODE_OP_MUL,
	CODE_NODE_OP_DIV,
} Code_Node_Kind;

typedef struct Code_Node {
	// from depends on to
	Code_Dependency deps_to;
	Code_Dependency deps_from;
	Code_Node_Kind kind;
	bool visited; // remove
} Code_Node;

typedef struct Code_Node_Set {
	Code_Node base;
	Value dst;
	Value src;
} Code_Node_Set;

typedef struct Code_Node_Call {
	Code_Node base;
	u64 arg_count;
	string symbol;
	Value args[];
} Code_Node_Call;

typedef struct Code_Node_Op {
	Code_Node base;
	Value left;
	Value right;
	Value dst;
} Code_Node_Op;





typedef enum Type_Kind {
	TYPE_INT,
	TYPE_FLOAT,
	TYPE_LITERAL_INT,
	TYPE_LITERAL_FLOAT,
	TYPE_LITERAL_STRING,
	TYPE_SLICE,
	TYPE_ARRAY,
	TYPE_NOTHING,
} Type_Kind;

struct Type;

typedef struct Type_Int {
	bool is_signed;
} Type_Int;

typedef struct Type_Slice {
	struct Type *elem_type;
} Type_Slice;

typedef struct Type_Array {
	struct Type *elem_type;
	u64 count;
} Type_Array;

typedef struct Type {
	string name; // NOT necessarily a slice into source
	u64 size;
	Type_Kind kind;
	u32 register_width;
	union {
		Type_Int type_int;
		Type_Slice type_slice;
		Type_Array type_array;
	} val;
	
	struct Type *_sliceified;
} Type;

typedef enum Procedure_Traits {
	PROCEDURE_TRAIT_NONE = 0,
	PROCEDURE_TRAIT_COMPILER = 1 << 0,
	PROCEDURE_TRAIT_INLINE = 1 << 1,
} Procedure_Traits;

typedef struct Procedure_Reference {
	u64 source_header_start;
	u32 source_body_offset;
	u32 source_body_size_in_bytes;
	string name; // @speed This double the amount of cache lines we need to load when walking the procedure table. It's possible that it's worth it to just parse the name on each probe.
} Procedure_Reference;

typedef struct Procedure_Header {
	Procedure_Traits trait_flags;
	u32 param_count; // Params are in value_table, if do_push_to_value_table
	Type *return_type;
} Procedure_Header;

typedef enum Value_Name_Kind {
	VALUE_NAME_VARIABLE,
	VALUE_NAME_LITERAL,
	// note(charlie): if you add a value name kind which has storage, please update @value_storage_evaluation
} Value_Name_Kind;

typedef struct Value_Name {
	Value value;
	Type *type;
	string name;
	Code_Node_Set *first_set;
	u64 read_count;
	Value_Name_Kind kind;
} Value_Name;

typedef struct Expression {
	Value value;
	Value_Name *name; // Set if expression was just a value name
	Type *type;
	Token t0;
	Token t1;
	bool is_storage;
} Expression;



typedef struct Jac_Context {
	Arena source_arena;
	string source;
	Jac_Result *result;
		
	Procedure_Reference  *global_proc_table;
	Value_Name *global_value_table;
	Type       *global_type_table;
	
	Type *type_u8;
	Type *type_s8;
	Type *type_u16;
	Type *type_s16;
	Type *type_u32;
	Type *type_s32;
	Type *type_u64;
	Type *type_s64;
	Type *type_f32;
	Type *type_f64;
	
	// Not in the type table
	Type type_literal_float;
	Type type_literal_int;
	Type type_literal_string;
	Type type_nothing;
	
	u64 file_count;
	Jac_File files[MAX_FILES];
} Jac_Context;

typedef struct Token_Context {
	Jac_Context *ctx;
	u64 source_pos;
	u64 line_num;
	u64 last_line_start_pos;
} Token_Context;

typedef struct Compile_Proc_Context {
	Jac_Context *ctx;
	Token_Context *tokenizer;
	Procedure_Reference *proc_table;
	Value_Name *value_table;
	Type *type_table;
	Code_Node_Set **set_table;
	
	u32 next_vnum;
	
	Arena node_arena;
	Code_Node *nodes;
	
	Arena dependencies_arena;
	
} Compile_Proc_Context;

// from depends on to
void add_code_dependency(Compile_Proc_Context *compiler, Code_Node *from, Code_Node *to) {
	if (!from->deps_to.node) {
		from->deps_to.node = to;
	} else {
		Code_Dependency *next = (Code_Dependency*)arena_push(&compiler->dependencies_arena, sizeof(Code_Dependency));
		*next = (Code_Dependency){0};
		next->node = from->deps_to.node;
		next->next = from->deps_to.next;
		from->deps_to.next = next;
		from->deps_to.node = to;
	}
	
	if (!to->deps_from.node) {
		to->deps_from.node = from;
	} else {
		Code_Dependency *next = (Code_Dependency*)arena_push(&compiler->dependencies_arena, sizeof(Code_Dependency));
		*next = (Code_Dependency){0};
		next->node = to->deps_from.node;
		next->next = to->deps_from.next;
		to->deps_from.next = next;
		to->deps_from.node = from;
	}
}

void add_code_dependency_on_value_sets(Compile_Proc_Context *compiler, Code_Node_Set *from, Value_Name *name) {
	assert(name->kind == VALUE_NAME_VARIABLE); //@value_storage_evaluation
				
	// Add dependency on all previous set to value name
	// @optimization this will cause false dependencies, emitting code which shouldn't be emitted
	// Should probably just depend on the latest sets on each branch.
	for (s64 i = (s64)persistent_array_count(compiler->set_table)-1; i >= 0; i -= 1) {
		Code_Node_Set *prev_set = compiler->set_table[i];
		
		if (prev_set->dst.vnum == from->dst.vnum) {
			add_code_dependency(compiler, &from->base, &prev_set->base);
		}
		
		if (prev_set == name->first_set) {
			break;
		}
	}
}

bool push_source_file(Jac_Context *ctx, string path) {
	assert(ctx->file_count < MAX_FILES);
	
	u64 source_index = (u32)((u64)ctx->source_arena.position - (u64)ctx->source.data);
	
	string raw_source;
	bool ok = sys_read_entire_file(get_temp(), path, &raw_source);
	if (!ok) return false;
	
	string source = string_replace(get_temp(), raw_source, STR("\r\n"), STR("\n"));
	source = string_replace(get_temp(), source, STR("\t"), STR("    "));
	
	arena_push_copy(&ctx->source_arena, source.data, source.count);
	
	ctx->files[ctx->file_count].path = path;
	ctx->files[ctx->file_count].source_index = source_index;
	ctx->files[ctx->file_count].source_length = source.count;
	ctx->file_count += 1;
	
	ctx->source.count += source.count;
	
	return true;
}

string stringify_token_kind(Token_Kind t) {
	switch (t) {
	case TOKEN_KIND_COLON:              return STR("TOKEN_KIND_COLON");
	case TOKEN_KIND_SEMICOLON:          return STR("TOKEN_KIND_SEMICOLON");
	case TOKEN_KIND_DOLLAR:             return STR("TOKEN_KIND_DOLLAR");
	case TOKEN_KIND_LPAREN:             return STR("TOKEN_KIND_LPAREN");
	case TOKEN_KIND_RPAREN:             return STR("TOKEN_KIND_RPAREN");
	case TOKEN_KIND_LBRACE:             return STR("TOKEN_KIND_LBRACE");
	case TOKEN_KIND_RBRACE:             return STR("TOKEN_KIND_RBRACE");
	case TOKEN_KIND_LBRACKET:           return STR("TOKEN_KIND_LBRACKET");
	case TOKEN_KIND_RBRACKET:           return STR("TOKEN_KIND_RBRACKET");
	case TOKEN_KIND_STAR:               return STR("TOKEN_KIND_STAR");
	case TOKEN_KIND_DOT:                return STR("TOKEN_KIND_DOT");
	case TOKEN_KIND_COMMA:              return STR("TOKEN_KIND_COMMA");
	case TOKEN_KIND_EXCLAMATION:        return STR("TOKEN_KIND_EXCLAMATION");
	case TOKEN_KIND_LT:                 return STR("TOKEN_KIND_LT");
	case TOKEN_KIND_GT:                 return STR("TOKEN_KIND_GT");
	case TOKEN_KIND_EQUALS:             return STR("TOKEN_KIND_EQUALS");
	case TOKEN_KIND_PLUS:               return STR("TOKEN_KIND_PLUS");
	case TOKEN_KIND_MINUS:              return STR("TOKEN_KIND_MINUS");
	case TOKEN_KIND_FSLASH:             return STR("TOKEN_KIND_FSLASH");
	case TOKEN_KIND_AMP:                return STR("TOKEN_KIND_AMP");
	case TOKEN_KIND_VLINE:              return STR("TOKEN_KIND_VLINE");
	case TOKEN_KIND_DOUBLE_QUOTE:       return STR("TOKEN_KIND_DOUBLE_QUOTE");
	case TOKEN_KIND_IDENTIFIER:         return STR("TOKEN_KIND_IDENTIFIER");
	case TOKEN_KIND_INT_LITERAL:        return STR("TOKEN_KIND_INT_LITERAL");
	case TOKEN_KIND_FLOAT_LITERAL:      return STR("TOKEN_KIND_FLOAT_LITERAL");
	case TOKEN_KIND_STRING_LITERAL:     return STR("TOKEN_KIND_STRING_LITERAL");
	case TOKEN_KIND_EOF:                return STR("TOKEN_KIND_EOF");
	case TOKEN_KIND_RIGHT_ARROW:        return STR("TOKEN_KIND_RIGHT_ARROW");
	case TOKEN_KIND_GTE:                return STR("TOKEN_KIND_GTE");
	case TOKEN_KIND_LTE:                return STR("TOKEN_KIND_LTE");
	case TOKEN_KIND_EQ:                 return STR("TOKEN_KIND_EQ");
	case TOKEN_KIND_NEQ:                return STR("TOKEN_KIND_NEQ");
	case TOKEN_KIND_LAND:               return STR("TOKEN_KIND_LAND");
	case TOKEN_KIND_LOR:                return STR("TOKEN_KIND_LOR");
	case TOKEN_KIND_DOUBLE_COLON:       return STR("TOKEN_KIND_DOUBLE_COLON");
	case TOKEN_KIND_SHIFT_LEFT:         return STR("TOKEN_KIND_SHIFT_LEFT");
	case TOKEN_KIND_SHIFT_RIGHT:        return STR("TOKEN_KIND_SHIFT_RIGHT");
	case TOKEN_KIND_KW_IF:              return STR("TOKEN_KIND_KW_IF");
	case TOKEN_KIND_KW_ELSE:            return STR("TOKEN_KIND_KW_ELSE");
	case TOKEN_KIND_KW_WHILE:           return STR("TOKEN_KIND_KW_WHILE");
	case TOKEN_KIND_KW_RETURN:          return STR("TOKEN_KIND_KW_RETURN");
	case TOKEN_KIND_KW_DEFER:           return STR("TOKEN_KIND_KW_DEFER");
	case TOKEN_KIND_KW_BREAK:           return STR("TOKEN_KIND_KW_BREAK");
	case TOKEN_KIND_KW_CONTINUE:        return STR("TOKEN_KIND_KW_CONTINUE");
	case TOKEN_KIND_DIRECTIVES_START:   return STR("TOKEN_KIND_DIRECTIVES_START");
	case TOKEN_KIND_DIRECTIVE_IF:       return STR("TOKEN_KIND_DIRECTIVE_IF");
	case TOKEN_KIND_DIRECTIVE_ELSE:     return STR("TOKEN_KIND_DIRECTIVE_ELSE");
	case TOKEN_KIND_DIRECTIVE_COMPILER: return STR("TOKEN_KIND_DIRECTIVE_COMPILER");
	case TOKEN_KIND_DIRECTIVE_INLINE:   return STR("TOKEN_KIND_DIRECTIVE_INLINE");
	case TOKEN_KIND_DIRECTIVES_END:     return STR("TOKEN_KIND_DIRECTIVES_END");
	
	case TOKEN_ASCII_END:   // fallthrough
	case TOKEN_ASCII_START: // fallthrough
	case TOKEN_UNSET:       // fallthrough
	default:
		return STR("");
	}
	
	assert(false);
}

string stringify_result(Jac_Result_Code result) {
	switch (result) {
		case JAC_RESULT_NONE:                    return STR("JAC_RESULT_NONE");
		case JAC_RESULT_UNEXPECTED_TOKEN:        return STR("JAC_RESULT_UNEXPECTED_TOKEN");
		case JAC_RESULT_FILE_NOT_FOUND:          return STR("JAC_RESULT_FILE_NOT_FOUND");
		case JAC_RESULT_UNTERMINATED_TOKEN:      return STR("JAC_RESULT_UNTERMINATED_TOKEN");
		case JAC_RESULT_UNRESOLVED_TYPE:         return STR("JAC_RESULT_UNRESOLVED_TYPE");
		case JAC_RESULT_INVALID_CONVERSION:      return STR("JAC_RESULT_INVALID_CONVERSION");
		case JAC_RESULT_UNDEFINED_IDENTIFIER:    return STR("JAC_RESULT_UNDEFINED_IDENTIFIER");
		case JAC_RESULT_BAD_ARGUMENT_COUNT:      return STR("JAC_RESULT_BAD_ARGUMENT_COUNT");
		case JAC_RESULT_BAD_ARGUMENT_TYPE:       return STR("JAC_RESULT_BAD_ARGUMENT_TYPE");
		case JAC_RESULT_LIMITATION:              return STR("JAC_RESULT_LIMITATION");
		case JAC_RESULT_BAD_IMPLICIT_CONVERSION: return STR("JAC_RESULT_BAD_IMPLICIT_CONVERSION");
		case JAC_RESULT_NON_STORAGE_USED_AS_STORAGE: return STR("JAC_RESULT_NON_STORAGE_USED_AS_STORAGE");
		
		default: break;
	}
	
	assert(false);
	return STR("");
}

u64 tok_source_position(Token_Context *tokenizer, Token tok) {
	return (u64)tok.text.data - (u64)tokenizer->ctx->source.data;
}

Source_Code_Location tok_location(Jac_Context *ctx, Token tok) {

	u32 line_length = 0;
	
	for (u64 i = tok.line_start_pos; i < ctx->source.count; i += 1) {
		if (ctx->source.data[i] == '\n') break;
		line_length += 1;
	}

	Source_Code_Location loc = (Source_Code_Location){0};
	loc.pos = (u32)((u64)tok.text.data - (u64)ctx->source.data);
	loc.line_start_pos = tok.line_start_pos;
	assert(loc.pos >= loc.line_start_pos);
	loc.line_length = line_length;
	loc.c0 = (u16)(loc.pos - tok.line_start_pos);
	loc.c1 = (u16)(loc.c0 + tok.text.count);
	loc.line_num = tok.line_num;
	
	// Find file
	
	for (u64 i = 0; i < ctx->file_count; i += 1) {
		Jac_File f = ctx->files[i];
		if (f.source_index <= loc.pos && f.source_index + f.source_length > loc.pos) {
			loc.file = f;
			break;
		}
	}
	assert(loc.file.source_length != 0);
	
	return loc;
}

string tprint_token(Jac_Context *ctx, Token t, string message) {
	Source_Code_Location loc = tok_location(ctx, t);
	
	u64 l0 = loc.line_start_pos;
	u64 l1 = loc.line_start_pos + loc.line_length;
	
	string line = (string){l1-l0, ctx->source.data+l0};
	
	u64 pos_in_line = loc.pos - l0;
	
	string space = string_allocate(get_temp(), pos_in_line);
	memset(space.data, '-', pos_in_line);
	
	string arrows = string_allocate(get_temp(), t.text.count);
	memset(arrows.data, '^', (sys_uint)t.text.count);
	
	string kind_str = stringify_token_kind(t.kind);
	return tprint("Line %u, Token '%s' (%s): %s\n    %s\n    %s%s\n", loc.line_num, t.text, kind_str, message, line, space, arrows);
}

void terminate_error(Jac_Context *ctx, Jac_Result_Code result, string message, Token *t0, Token *t1) {
	(void)t1;
	assertmsg(result != JAC_RESULT_NONE, "Error: error ¯\\_(ツ)_/¯");
	if (t0) {
		message = tprint_token(ctx, *t0, message);
	}
	
	string err_log = tprint("Error: %s\n", message);
	logs(JAC_LOG_COMPILE_ERROR, err_log);
	
	ctx->result->code = result;
	ctx->result->message = err_log;
	
	if (t0) {
		ctx->result->error_location = tok_location(ctx, *t0);
	}
	
#if DEBUG
	sys_print_stack_trace(sys_get_stdout());
#endif // DEBUG
	
	sys_exit_current_thread((s64)result);
}

void set_source_position(Token_Context *t, u64 pos) {
    Jac_Context *ctx = t->ctx;
    if (pos < t->source_pos) {
        // Moving backwards
        u64 scan_from = pos;
        u64 scan_to   = t->source_pos;
        u64 nl_count  = 0;
        for (u64 i = scan_from; i < scan_to; i++) {
            if (ctx->source.data[i] == '\n')
                nl_count++;
        }
        t->line_num -= nl_count;

        // Find the last newline before pos
        s64 last_nl = -1;
        for (s64 i = (s64)pos - 1; i >= 0; i--) {
            if (ctx->source.data[i] == '\n') {
                last_nl = i;
                break;
            }
        }
        t->last_line_start_pos = (last_nl >= 0) ? (u64)(last_nl + 1) : 0;
        t->source_pos = pos;
    }
    else if (pos > t->source_pos) {
        // Moving forwards
        while (t->source_pos < pos) {
            if (ctx->source.data[t->source_pos] == '\n') {
                t->last_line_start_pos = t->source_pos + 1;
                t->line_num += 1;
            }
            t->source_pos += 1;
        }
    }
}

bool is_hex_digit(u8 c) {
	return is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}



Token tok_consume(Token_Context *tokenizer) {
	Jac_Context *ctx = tokenizer->ctx;
	u8 *p = ctx->source.data + tokenizer->source_pos;
	u8 *end = ctx->source.data + ctx->source.count;
	
	if (p >= end) {
		Token token = (Token){0};
		token.text.data = ctx->source.data-1;
		token.text.count = 1;
		token.kind = TOKEN_KIND_EOF;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	while (is_whitespace(*p)) { p += 1; }

	while (p[0] == '/' && (p[1] == '/' || p[1] == '*')) {
		while (p[0] == '/' && p[1] == '/') {
			while (p < end && *p != '\n') { p += 1; }
			if (*p == '\n') p += 1;
		}
		while (is_whitespace(*p)) { p += 1; }
		u32 depth = 0;
		while (p[0] == '/' && p[1] == '*') {
			while ((p+1) < end && !(p[0] == '*' && p[1] == '/' && depth == 1)) {  
				if (p[0] == '/' && p[1] == '*') depth += 1;
				if (p[0] == '*' && p[1] == '/') depth -= 1;
				p += 1;
			}
			if (p[0] == '*' && p[1] == '/') { p += 2; }
		}
		while (is_whitespace(*p)) { p += 1; }
	}
	while (is_whitespace(*p)) { p += 1; }
	
	Token token = (Token){0};
	
	token.text.data = p;
	
	u64 pp = (u64)token.text.data - (u64)tokenizer->ctx->source.data;
	assert(tokenizer->last_line_start_pos <= pp);
	
	if (*p == '"') {
		// todo(charlie) escape sequences
		
		p += 1;
		token.text.data += 1;
		
		while (p < end) {
			p += 1;
			if (*p == '"') {
				break;
			}
		}
		
		token.text.count = (u64)p - (u64)token.text.data;
		token.kind = TOKEN_KIND_STRING_LITERAL;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		
		p += 1;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		
		return token;
	}

	if (is_alpha(*p) || *p == '_') {
		
		while (is_alpha(*p) || *p == '_' || is_digit(*p)) {p += 1;}
		
		token.text.count = (u64)p - (u64)token.text.data;
		token.kind = TOKEN_KIND_IDENTIFIER;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	if (is_digit(*p)) {
		bool dot = false;
		bool is_hex = false;
		
		if (p+2 < ctx->source.data + ctx->source.count && p[0] == '0' && p[1] == 'x' && is_hex_digit(p[2])) {
			is_hex = true;
			p += 2;
		}
		
		while (is_digit(*p) || *p == '.' || (is_hex && (*p == 'x' || is_hex_digit(*p)))) {
			if (*p == '.') {
				if (!dot && !is_hex) dot = true;
				else {
					break;
				}
			}
			p += 1;
		}
		if (dot && *(p-1) == '.') {
			dot = false;
			p--;
		}
		token.text.count = (u64)p - (u64)token.text.data;
		token.kind = dot ? TOKEN_KIND_FLOAT_LITERAL : TOKEN_KIND_INT_LITERAL;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	if (p < end-2 && strings_match((string){2, p}, STR("->"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_RIGHT_ARROW;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR(">="))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_GTE;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("<="))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_LTE;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("=="))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_EQ;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("!="))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_NEQ;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("&&"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_LAND;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("||"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_LOR;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("::"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_DOUBLE_COLON;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("<<"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_SHIFT_LEFT;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR(">>"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_SHIFT_RIGHT;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-8 && strings_match((string){8, p}, STR("continue"))) {
		token.text.count = 8;
		token.kind = TOKEN_KIND_KW_CONTINUE;
		p += 8;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-5 && strings_match((string){5, p}, STR("break"))) {
		token.text.count = 5;
		token.kind = TOKEN_KIND_KW_BREAK;
		p += 5;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-5 && strings_match((string){5, p}, STR("defer"))) {
		token.text.count = 5;
		token.kind = TOKEN_KIND_KW_DEFER;
		p += 5;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-6 && strings_match((string){6, p}, STR("return"))) {
		token.text.count = 6;
		token.kind = TOKEN_KIND_KW_RETURN;
		p += 6;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-5 && strings_match((string){5, p}, STR("while"))) {
		token.text.count = 5;
		token.kind = TOKEN_KIND_KW_WHILE;
		p += 5;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-4 && strings_match((string){4, p}, STR("else"))) {
		token.text.count = 4;
		token.kind = TOKEN_KIND_KW_ELSE;
		p += 4;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-2 && strings_match((string){2, p}, STR("if"))) {
		token.text.count = 2;
		token.kind = TOKEN_KIND_KW_IF;
		p += 2;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-3 && strings_match((string){3, p}, STR("#if"))) {
		token.text.count = 3;
		token.kind = TOKEN_KIND_DIRECTIVE_IF;
		p += 3;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-5 && strings_match((string){5, p}, STR("#else"))) {
		token.text.count = 5;
		token.kind = TOKEN_KIND_DIRECTIVE_ELSE;
		p += 5;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-9 && strings_match((string){9, p}, STR("#compiler"))) {
		token.text.count = 9;
		token.kind = TOKEN_KIND_DIRECTIVE_COMPILER;
		p += 9;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	} else if (p < end-7 && strings_match((string){7, p}, STR("#inline"))) {
		token.text.count = 7;
		token.kind = TOKEN_KIND_DIRECTIVE_INLINE;
		p += 7;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	if (*p >= TOKEN_ASCII_START && *p < TOKEN_ASCII_END) {
		token.kind = (Token_Kind)*(p++);
		token.text.count = (u64)p - (u64)token.text.data;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	if (p == ctx->source.data+ctx->source.count) {
		token.text.data = ctx->source.data-1;
		token.text.count = 1;
		token.kind = TOKEN_KIND_EOF;
		set_source_position(tokenizer, (u64)p - (u64)ctx->source.data);
		token.line_num = tokenizer->line_num;
		token.line_start_pos = tokenizer->last_line_start_pos;
		return token;
	}
	
	assert(false);
	return (Token){0};
}
Token tok_peek(Token_Context *tokenizer) {
	u64 position = tokenizer->source_pos;
	Token t = tok_consume(tokenizer);
	set_source_position(tokenizer, position);
	return t;
}
Token tok_peekn(Token_Context *tokenizer, u64 lookahead) {
	u64 position = tokenizer->source_pos;
	Token t = tok_consume(tokenizer);
	for (u64 i = 0; i < lookahead; i += 1) {
		t = tok_consume(tokenizer);
	}
	set_source_position(tokenizer, position);
	return t;
}
Token tok_expect(Token_Context *tokenizer, Token_Kind kind) {
	Jac_Context *ctx = tokenizer->ctx;
	Token next = tok_consume(tokenizer);
	
	if (next.kind != kind) {
		string ts = stringify_token_kind(kind);
		terminate_error(ctx, JAC_RESULT_UNEXPECTED_TOKEN, tprint("Unexpected token. Expected %s, got this", ts), &next, 0);
	}
	
	return next;
}
bool tok_accept(Token_Context *tokenizer, Token_Kind kind, Token *tok) {
	*tok = tok_consume(tokenizer);
	
	if (tok->kind == kind) {
		return true;
	}
	
	return false;
}



Token skip_to_match(Token_Context *tokenizer, Token left_token) {
	Jac_Context *ctx = tokenizer->ctx;
	Token_Kind left = left_token.kind;
	Token_Kind right = 0;
	if (left == '(') {
		right = ')';
	} else if (left == '{') {
		right = '}';
	} else if (left == '[') {
		right = ']';
	} else if (left == '<') {
		right = '>';
	} else if (left == '"') {
		right = '"';
	} else {
		assert(false); // unimplemented
	}
	
	u32 depth = 1;
	
	Token next = tok_consume(tokenizer);
	if (next.kind == right) depth -= 1;
	while (next.kind != TOKEN_KIND_EOF && depth > 0) {
		next = tok_consume(tokenizer);
		if (next.kind == left) depth += 1;
		else if (next.kind == right) depth -= 1;
	}
	
	if (next.kind != right) {
		terminate_error(ctx, JAC_RESULT_UNTERMINATED_TOKEN, STR("Expected a match for this token, but hit eof before it occured"), &left_token, 0);
	}
	
	return next;
}

void prepass_some(Token_Context *tokenizer, Token first) {
	Jac_Context *ctx = tokenizer->ctx;
	Token t0 = first;
	Token t1 = tok_consume(tokenizer);
	Token t2 = tok_consume(tokenizer);
	
	if (t0.kind == TOKEN_KIND_IDENTIFIER && t1.kind == TOKEN_KIND_DOUBLE_COLON) {
		if (t2.kind == '(') { // procedure declaration
			//logs(JAC_LOG_TRACE, tprint_token(ctx, t0, STR("Prepass procedure")));
			
			Procedure_Reference *proc = (Procedure_Reference*)persistent_array_push_empty(ctx->global_proc_table);
			*proc = (Procedure_Reference){0};
			
			proc->name = t0.text;
			
			proc->source_header_start = tok_source_position(tokenizer, t0);
			skip_to_match(tokenizer, t2);
			
			Token next = tok_peek(tokenizer);
			
			if (next.kind == TOKEN_KIND_RIGHT_ARROW) {
				tok_consume(tokenizer); // ->
				
				next = tok_peek(tokenizer);
				
				while ((next.kind < TOKEN_KIND_DIRECTIVES_START || next.kind > TOKEN_KIND_DIRECTIVES_END) && next.kind != '{') {
					tok_consume(tokenizer);
					next = tok_peek(tokenizer);
				}
			}
			
			while (next.kind > TOKEN_KIND_DIRECTIVES_START && next.kind < TOKEN_KIND_DIRECTIVES_END) {
				//logs(JAC_LOG_TRACE, tprint_token(ctx, next, STR("Prepass procedure trait")));
				tok_consume(tokenizer);
				next = tok_peek(tokenizer);
			}
			
			//logs(JAC_LOG_TRACE, tprint_token(ctx, next, STR("Procedure_Reference header end")));
			
			proc->source_body_offset = 0;
			
			if (next.kind == '{') {
				proc->source_body_offset = (u32)(tok_source_position(tokenizer, next) - proc->source_header_start);
				
				//logs(JAC_LOG_TRACE, tprint_token(ctx, next, STR("Prepass procedure body start")));
				
				tok_consume(tokenizer);
				
				Token end = skip_to_match(tokenizer, next);
				
				//logs(JAC_LOG_TRACE, tprint_token(ctx, end, STR("Prepass procedure body end")));
				
				u64 source_body_start = tok_source_position(tokenizer, next);
				u64 source_body_end = tok_source_position(tokenizer, end);
				proc->source_body_size_in_bytes = (u32)(source_body_end - source_body_start);
			}
		}
	} else {
		terminate_error(ctx, JAC_RESULT_UNEXPECTED_TOKEN, STR("Unable to determine intent with this token."), &first, 0);
	}
}

typedef struct Compile_Proc_Args {
	Jac_Context *ctx;
	Procedure_Reference *proc;
	Compile_Proc_Context result;
} Compile_Proc_Args;

Type* resolve_type_name(Compile_Proc_Context *compiler, string name) {
	for (u64 i = 0; i < persistent_array_count(compiler->type_table); i += 1) {
		Type *t = compiler->type_table + i;
		
		if (strings_match(t->name, name)) {
			return t;
		}
	}
	return 0;
}

Type *parse_type(Compile_Proc_Context *compiler) {
	Token_Context *tokenizer = compiler->tokenizer;

	Token next = tok_consume(tokenizer);

	if (next.kind == TOKEN_KIND_IDENTIFIER) {
		Type *t = resolve_type_name(compiler, next.text);
		if (!t) {
			terminate_error(compiler->ctx, JAC_RESULT_UNRESOLVED_TYPE, STR("There is no type with this name"), &next, 0);
		}
		return t;
	} else if (next.kind == '[') {
		
		next = tok_consume(tokenizer);
		
		if (next.kind == ']') {
			
			Type *sub_type = parse_type(compiler);
			
			if (!sub_type->_sliceified) {
				sub_type->_sliceified = (Type*)persistent_array_push_empty(compiler->type_table);
				sub_type->_sliceified->kind = TYPE_SLICE;
				sub_type->_sliceified->size = 16;
				sub_type->_sliceified->name = tprint("[]%s", sub_type->name);
				sub_type->_sliceified->val.type_slice.elem_type = sub_type;
			}
			sub_type->_sliceified->register_width = 2; // Count and pointer
			return sub_type->_sliceified;
			
		} else {
			assert(false); // array not implemented, (const expr parse)
		}
	} else {
		terminate_error(compiler->ctx, JAC_RESULT_UNEXPECTED_TOKEN, STR("Unexpected token. Expected a Type."), &next, 0);
	}
	
	assert(false);
	return 0;
}

Value_Name* parse_value_declaration(Compile_Proc_Context *compiler, Token first) {
	Token_Context *tokenizer = compiler->tokenizer;
	Jac_Context *ctx = compiler->ctx;
	(void)ctx;
	
	assert(first.kind == TOKEN_KIND_IDENTIFIER);
	Value_Name *name = (Value_Name *)persistent_array_push_empty(compiler->value_table);
	assert(name);
	name->name = first.text;
		
	
	tok_expect(tokenizer, ':');
	
	Type *type = parse_type(compiler);
	
	//logs(JAC_LOG_TRACE, tprint_token(ctx, first, tprint("Value declaration of type %s", type->name)));
	
	name->type = type;
	name->value.vnum = compiler->next_vnum++;
	if (type->kind == TYPE_FLOAT) {
		name->value.flags |= VALUE_FLOAT;
	}
	name->value.width = type->register_width;
	
	
	return name;
}

Procedure_Header parse_procedure_header(Compile_Proc_Context *compiler, Token first) {
	
	Token_Context *tokenizer = compiler->tokenizer;
	Jac_Context *ctx = compiler->ctx;
	
	assert(first.kind == TOKEN_KIND_IDENTIFIER);
	assert(tok_consume(tokenizer).kind == TOKEN_KIND_DOUBLE_COLON);
	assert(tok_consume(tokenizer).kind == '(');
	
	Procedure_Header header = (Procedure_Header){0};
	
	Token next;
	while (!tok_accept(tokenizer, ')', &next)) {
		
		if (next.kind != TOKEN_KIND_IDENTIFIER) {
			terminate_error(ctx, JAC_RESULT_UNEXPECTED_TOKEN, STR("Unexpected token. Expected a parameter declaration, got this."), &next, 0);
		}
		
		parse_value_declaration(compiler, next);
		header.param_count += 1;
		
		tok_expect(tokenizer, ';');
	}
	
	next = tok_peek(tokenizer);
	
	if (next.kind == TOKEN_KIND_RIGHT_ARROW) {
		tok_consume(tokenizer); // ->
		
		header.return_type = parse_type(compiler);
		next = tok_peek(tokenizer);
	}
	
	while (next.kind > TOKEN_KIND_DIRECTIVES_START && next.kind < TOKEN_KIND_DIRECTIVES_END) {
		tok_consume(tokenizer);
		if (next.kind == TOKEN_KIND_DIRECTIVE_COMPILER) {
			header.trait_flags |= PROCEDURE_TRAIT_COMPILER;
		} else if (next.kind == TOKEN_KIND_DIRECTIVE_INLINE) {
			header.trait_flags |= PROCEDURE_TRAIT_INLINE;
		} else {
			terminate_error(ctx, JAC_RESULT_UNEXPECTED_TOKEN, STR("Unexpected token. Expected a procedure trait directive or '{'"), &next, 0);
		}
		next = tok_peek(tokenizer);
	}
	
	return header;
}

bool can_be_op_token(Token_Kind kind) {
	return kind == '+' || kind == '-' || kind == '*' || kind == '/';
}

bool attempt_implicit_cast(Compile_Proc_Context *compiler, Type *from, Type *to, Value val, Value *result) {
	(void)compiler;
	
	if (from == to) {
		*result = val;
		return true;
	}
	
	if (from->kind == TYPE_LITERAL_INT && to->kind == TYPE_INT) {
		*result = val;
		return true;
	}
	if (from->kind == TYPE_LITERAL_INT && to->kind == TYPE_FLOAT) {
		*result = val;
		result->flags = VALUE_FLOAT | VALUE_LITERAL;
		u64 int_val = val.imp.literal;
		result->imp.literal = 0;
		if (to->size == 4) {
			float32 f32_val = (float32)int_val;
			memcpy(&result->imp.literal, &f32_val, sizeof(float32));
			return true;
		} else if (to->size == 4) {
			float64 f64_val = (float64)int_val;
			memcpy(&result->imp.literal, &f64_val, sizeof(float64));
			return true;
		} else assert(false);
	}
	
	if (from->kind == TYPE_LITERAL_FLOAT && to->kind == TYPE_FLOAT) {
		*result = val;
		u64 int_val = val.imp.literal;
		result->imp.literal = 0;
		if (to->size == 4) {
			float32 f32_val = (float32)(*(float64*)&int_val);
			memcpy(&result->imp.literal, &f32_val, sizeof(float32));
			return true;
		} else if (to->size == 4) {
			float64 f64_val = *(float64*)&int_val;
			memcpy(&result->imp.literal, &f64_val, sizeof(float64));
			return true;
		} else assert(false);
	}
	
	if (from->kind == TYPE_LITERAL_STRING && to->kind == TYPE_SLICE && to->val.type_slice.elem_type->kind == TYPE_INT && to->val.type_slice.elem_type->size == 1 && !to->val.type_slice.elem_type->val.type_int.is_signed) {
		result->flags = VALUE_SLICE;
		result->vnum = compiler->next_vnum++;
		result->width = 2;
		return true;
	}
	
	return false;
}

Expression compile_expression(Compile_Proc_Context *compiler, Token first);
Expression compile_one_expression(Compile_Proc_Context *compiler, Token first) {
	/*
	
		Float literal:
		1.5
		
		Int literal:
		7
		
		String literal:
		"Text"
		
		Proc call:
		proc(...)
		
		Value name:
		var
		
		
	*/
	
	Token_Context *tokenizer = compiler->tokenizer;
	Jac_Context *ctx = compiler->ctx;
	
	Expression expr = (Expression){0};
	expr.t0 = first;
	
	if (first.kind == TOKEN_KIND_FLOAT_LITERAL) {
		expr.value.flags = VALUE_LITERAL | VALUE_FLOAT;
		expr.value.width = 1;
		bool succ = false;
		float64 lit = string_to_float(first.text, &succ);
		assert(succ); // string_to_float failed
		expr.value.imp.literal = *(u64*)&lit;
		expr.is_storage = false;
		expr.type = &ctx->type_literal_float;
		expr.t1 = first;
	} else if (first.kind == TOKEN_KIND_INT_LITERAL) {
		expr.value.flags = VALUE_LITERAL;
		expr.value.width = 1;
		bool succ = false;
		u64 lit = string_to_unsigned_int(first.text, 10, &succ);
		assert(succ); // string_to_unsigned_int failed
		expr.value.imp.literal = lit;
		expr.is_storage = false;
		expr.type = &ctx->type_literal_int;
		expr.t1 = first;
	} else if (first.kind == TOKEN_KIND_STRING_LITERAL) {
		expr.value.flags = VALUE_LITERAL | VALUE_STRING;
		expr.value.width = 1;
		expr.value.imp.str = first.text;
		expr.is_storage = false;
		expr.type = &ctx->type_literal_string;
		expr.t1 = first;
	} else if (first.kind == '(') {
		expr = compile_expression(compiler, tok_consume(tokenizer));
		tok_expect(tokenizer, ')');
	} else if (first.kind == TOKEN_KIND_IDENTIFIER) {
		
		Token next = tok_peek(tokenizer);
		
		if (next.kind == '(') {
			// Proc call
			
			tok_consume(tokenizer); // (
			
			Expression args[MAX_PROCEDURE_PARAMETERS] = {0};
			u64 arg_count = 0;
			
			while (!tok_accept(tokenizer, ')', &next)) {
				assertmsg(arg_count < MAX_PROCEDURE_PARAMETERS, "That's a lot of procedure parameters, buddy."); 
				
				args[arg_count++] = compile_expression(compiler, next);
				
				next = tok_peek(tokenizer);
				if (next.kind == ')') {
					tok_consume(tokenizer);
					break;
				}
				
				tok_expect(tokenizer, ',');
			}
			
			expr.t1 = next;
			
			u64 my_pos = tokenizer->source_pos;
			
			Procedure_Reference *proc_ref = 0;
			
			for (u64 i = 0; i < persistent_array_count(compiler->proc_table); i += 1) {
				Procedure_Reference *candidate = compiler->proc_table + i;
				if (strings_match(first.text, candidate->name)) {
					proc_ref = candidate;
					break;
				}
			}
			
			if (!proc_ref) {
				terminate_error(ctx, JAC_RESULT_UNDEFINED_IDENTIFIER, STR("There is no procedure with this name."), &first, 0);
			}
			
			u64 value_count_before = persistent_array_count(compiler->value_table);
			set_source_position(tokenizer, proc_ref->source_header_start);
			Procedure_Header header = parse_procedure_header(compiler, tok_consume(tokenizer));
			
			assert(persistent_array_count(compiler->value_table) == value_count_before + header.param_count);
			
			if (arg_count != header.param_count) {
				terminate_error(ctx, JAC_RESULT_BAD_ARGUMENT_COUNT, tprint("Expected %u arguments when calling procedure '%s', but we got %u...", header.param_count, proc_ref->name, arg_count), &first, 0);
			}
			
			
			for (u64 i = 0; i < arg_count; i += 1) {
				Type *arg_type = args[i].type;
				Type *param_type = compiler->value_table[value_count_before+i].type;
				
				// todo(charlie) literal conversions, implicit conversions
				if (arg_type != param_type) {
					terminate_error(ctx, JAC_RESULT_BAD_ARGUMENT_TYPE, tprint("Expected %u arguments when calling procedure '%s', but we got %u...", header.param_count, proc_ref->name, arg_count), &first, 0);
				}
			}
			
			// Jump back to where we were
			set_source_position(tokenizer, my_pos);
			// Reset value table (because we parsed paramters when parsing the procedure header)
			persistent_array_set_count(compiler->value_table, value_count_before);
			
			/*bool call_site_inline = tok_peek(tokenizer).kind == TOKEN_KIND_DIRECTIVE_INLINE;
			
			if (call_site_inline || (header.trait_flags & PROCEDURE_TRAIT_INLINE) || proc_ref->source_body_size_in_bytes < 2000) {
				if (call_site_inline) tok_consume(tokenizer);
				
				
			}*/
			
			Code_Node_Call *call = arena_push(&compiler->node_arena, sizeof(Code_Node_Call) + sizeof(Value)*arg_count);
			*call = (Code_Node_Call){0};
			call->base.kind = CODE_NODE_CALL;
			call->arg_count = arg_count;
			call->symbol = proc_ref->name;
			for (u64 i = 0; i < arg_count; i += 1) {
				call->args[i] = args[i].value;
				if (args[i].value.flags & VALUE_CODE_RESULT) {
					add_code_dependency(compiler, &call->base, args[i].value.imp.code);
				}
			}
			
		
			if (header.return_type) {
				expr.value.width = header.return_type->register_width;
				expr.value.flags = VALUE_CODE_RESULT;
				expr.value.imp.code = &call->base;
				expr.value.vnum = compiler->next_vnum++;
				
				expr.type = header.return_type;
				expr.is_storage = false;
			} else {
				expr.value.width = 0;
				expr.value.flags = VALUE_NOTHING;
				expr.is_storage = false;
				expr.type = &ctx->type_nothing;
			}
		} else {
			// Value name
			
			bool match = false;
			for (u64 i = 0; i < persistent_array_count(compiler->value_table); i += 1) {
				Value_Name *v = compiler->value_table + i;
				
				if (strings_match(v->name, first.text)) {
					expr.value = v->value;
					expr.type = v->type;
					expr.is_storage = v->kind == VALUE_NAME_VARIABLE; // @value_storage_evaluation
					match = true;
					break;
				}
			}
			
			if (!match)
				terminate_error(ctx, JAC_RESULT_UNDEFINED_IDENTIFIER, STR("This was used like a value name, but nothing with this name was declared here."), &first, 0);
				
			expr.t1 = first;
		}
		
	} else {
		terminate_error(ctx, JAC_RESULT_UNEXPECTED_TOKEN, STR("Unexpected token. Expected an expression, got this."), &first, 0);
	}
	
	assert(expr.type); // Forgot to set type
	return expr;
}

Expression compile_expression(Compile_Proc_Context *compiler, Token first) {

	Token_Context *tokenizer = compiler->tokenizer;
	Jac_Context *ctx = compiler->ctx;

	Op_Kind op_stack[256] = {0};
	u64 op_count = 0;
	Expression expr_stack[256] = {0};
	u64 expr_count = 0;
	
	expr_stack[expr_count++] = compile_one_expression(compiler, first);
	
	while (can_be_op_token(tok_peek(tokenizer).kind)) {
		Token rop_tok = tok_consume(tokenizer);
		Op_Kind rop = get_op_from_token(rop_tok.kind);
		u64 rprec = get_op_precedence(rop);
		
		while (op_count > 0) {
			Op_Kind lop = op_stack[op_count-1];
			u64 lprec = get_op_precedence(lop);
			
			if (lprec > rprec) {
				assert(expr_count >= 2);
				Expression rexpr = expr_stack[--expr_count];
				Expression lexpr = expr_stack[--expr_count];
				
				if (rexpr.type != lexpr.type) {
					terminate_error(ctx, JAC_RESULT_BAD_IMPLICIT_CONVERSION, tprint("Cannot perform this operation on lhs type %s and rhs type %s", lexpr.type->name, rexpr.type->name), &first, 0);
				}
				
				op_count -= 1; // pop lop
				
				Code_Node_Op *code_op = arena_push(&compiler->node_arena, sizeof(Code_Node_Op));
				*code_op = (Code_Node_Op){0};
				code_op->left = lexpr.value;
				code_op->right = rexpr.value;
				code_op->dst.flags = VALUE_CODE_RESULT;
				code_op->dst.vnum = compiler->next_vnum++;
				if (rexpr.type->kind == TYPE_FLOAT) {
					code_op->dst.flags |= VALUE_FLOAT;
				}
				code_op->dst.imp.code = &code_op->base;
				
				switch (lop) {
					case OP_ADD:
						code_op->base.kind = CODE_NODE_OP_ADD;
						break;
					case OP_SUB:
						code_op->base.kind = CODE_NODE_OP_SUB;
						break;
					case OP_MUL:
						code_op->base.kind = CODE_NODE_OP_MUL;
						break;
					case OP_DIV:
						code_op->base.kind = CODE_NODE_OP_DIV;
						break;
					
					default: assert(false); break;
				}
				
				// Op expr
				Expression op_expr = (Expression){0};
				op_expr.value = code_op->dst;
				op_expr.type = lexpr.type;
				op_expr.t0 = lexpr.t0;
				op_expr.t1 = rexpr.t1;
				op_expr.is_storage = false;
				
				expr_stack[expr_count++] = op_expr;
				
				if (lexpr.name) {
					
				}
				
				if (rexpr.name) {
					
				}
			} else {
				break;
			}
		}
		
		Expression rexpr = compile_one_expression(compiler, tok_consume(tokenizer));
		
		if (op_count >= 256) {
			terminate_error(ctx, JAC_RESULT_LIMITATION, STR("This expression has too many operations. Maximum is 256."), &rop_tok, 0);
		}
		if (expr_count >= 256) {
			terminate_error(ctx, JAC_RESULT_LIMITATION, STR("This expression has too many sub-expressions. Maximum is 256."), &rop_tok, 0);
		}
		op_stack[op_count++] = rop;
		expr_stack[expr_count++] = rexpr;
	}
	
	// Clear stack
	while (op_count > 0) {
		Op_Kind lop = op_stack[op_count-1];
		assert(expr_count >= 2);
		Expression rexpr = expr_stack[--expr_count];
		Expression lexpr = expr_stack[--expr_count];
		
		Value rvalue = rexpr.value;
		
		if (!attempt_implicit_cast(compiler, rexpr.type, lexpr.type, rvalue, &rvalue)) {
			terminate_error(ctx, JAC_RESULT_BAD_IMPLICIT_CONVERSION, tprint("Cannot perform this operation on lhs type '%s' and rhs type '%s'", lexpr.type->name, rexpr.type->name), &first, 0);
		}
		
		op_count -= 1; // pop lop
		
		Code_Node_Op *code_op = arena_push(&compiler->node_arena, sizeof(Code_Node_Op));
		*code_op = (Code_Node_Op){0};
		code_op->left = lexpr.value;
		code_op->right = rvalue;
		code_op->dst.flags = VALUE_CODE_RESULT;
		code_op->dst.vnum = compiler->next_vnum++;
		if (rexpr.type->kind == TYPE_FLOAT) {
			code_op->dst.flags |= VALUE_FLOAT;
		}
		code_op->dst.imp.code = &code_op->base;
		
		switch (lop) {
			case OP_ADD:
				code_op->base.kind = CODE_NODE_OP_ADD;
				break;
			case OP_SUB:
				code_op->base.kind = CODE_NODE_OP_SUB;
				break;
			case OP_MUL:
				code_op->base.kind = CODE_NODE_OP_MUL;
				break;
			case OP_DIV:
				code_op->base.kind = CODE_NODE_OP_DIV;
				break;
			
			default: assert(false); break;
		}
		
		if (lexpr.value.flags & VALUE_CODE_RESULT) {
			add_code_dependency(compiler, &code_op->base, lexpr.value.imp.code);
		}
		if (rvalue.flags & VALUE_CODE_RESULT) {
			add_code_dependency(compiler, &code_op->base, rvalue.imp.code);
		}
		
		// Op expr
		Expression op_expr = (Expression){0};
		op_expr.type = lexpr.type;
		op_expr.value = code_op->dst;
		op_expr.t0 = lexpr.t0;
		op_expr.t1 = rexpr.t1;
		op_expr.is_storage = false;
		
		expr_stack[expr_count++] = op_expr;
	}
	
	assert(expr_count == 1);
	return expr_stack[0];
}

void compile_one_statement(Compile_Proc_Context *compiler, Token first) {
	/*
	
		Set's:
		a = b;
		b = a = c + 5; == { b = c + 5; a = c + 5; }
		(p+5).* = d;
		^^^^^^^ ---------- expr
		(a, b, c) = multiple_returns(...);
		^^^^^^^^^ ---------- tuple
		
		Call's:
		proc(....);
		
		Local value decl's:
		a: u32;
		b: f32 = 1.0;
		
		if (expr) { statements...; }
		
		while (expr) { statements...; }
		
		return (expr);
		
		defer { statements...;}
		
		break;
		
		continue;
	*/
	
	Token_Context *tokenizer = compiler->tokenizer;
	Jac_Context *ctx = compiler->ctx;
	
	Token next = tok_peek(tokenizer);
	
	if (first.kind == TOKEN_KIND_KW_IF) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_KW_WHILE) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_KW_RETURN) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_KW_DEFER) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_KW_BREAK) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_KW_CONTINUE) {
		assert(false); // @unimplemented
	} else if (first.kind == TOKEN_KIND_IDENTIFIER && next.kind == ':') {
		Value_Name *val = parse_value_declaration(compiler, first);
		val->first_set = 0;
		
		next = tok_peek(tokenizer);
		if (next.kind == '=') {
			tok_consume(tokenizer);
			
			Token expr_start = tok_consume(tokenizer);
			Expression expr = compile_expression(compiler, expr_start);
			Value rvalue = expr.value;
			
			if (!attempt_implicit_cast(compiler, expr.type, val->type, rvalue, &rvalue)) {
				terminate_error(ctx, JAC_RESULT_INVALID_CONVERSION, tprint("This expression of type '%s' cannot be assigned to expression of type '%s'", expr.type->name, val->type->name), &expr_start, 0);
			}
			
			val->first_set = (Code_Node_Set*)arena_push(&compiler->node_arena, sizeof(Code_Node_Set));
			*val->first_set = (Code_Node_Set){0};
			val->first_set->base.kind = CODE_NODE_SET;
			val->first_set->dst = val->value;
			val->first_set->src = rvalue;
			if (rvalue.flags & VALUE_CODE_RESULT) {
				add_code_dependency(compiler, &val->first_set->base, rvalue.imp.code);
			}
			
			add_code_dependency_on_value_sets(compiler, val->first_set, val);
			
			persistent_array_push_copy(compiler->set_table, &val->first_set);
		}
		
		tok_expect(tokenizer, ';');
		
	} else {
		Expression expr = compile_expression(compiler, first);
		
		if (tok_peek(tokenizer).kind == '=') {
			Token eq = tok_consume(tokenizer); // =
			
			if (!expr.is_storage) {
				terminate_error(ctx, JAC_RESULT_NON_STORAGE_USED_AS_STORAGE, STR("Trying to do a set on a non-storage expression."), &eq, 0);
			}
			
			Token rexpr_start = tok_consume(tokenizer);
			Expression rexpr = compile_expression(compiler, rexpr_start);
			Value rvalue = rexpr.value;
			
			if (!attempt_implicit_cast(compiler, rexpr.type, expr.type, rvalue, &rvalue)) {
				terminate_error(ctx, JAC_RESULT_INVALID_CONVERSION, tprint("This expression of type '%s' cannot be assigned to expression of type '%s'", rexpr.type->name, expr.type->name), &rexpr_start, 0);
			}
			
			Code_Node_Set *set = (Code_Node_Set*)arena_push(&compiler->node_arena, sizeof(Code_Node_Set));
			*set = (Code_Node_Set){0};
			set->base.kind = CODE_NODE_SET;
			set->dst = expr.value;
			set->src = rvalue;
			
			if (expr.value.flags & VALUE_CODE_RESULT) {
				add_code_dependency(compiler, &set->base, expr.value.imp.code);
			}
			
			if (rvalue.flags & VALUE_CODE_RESULT) {
				add_code_dependency(compiler, &set->base, rvalue.imp.code);
			}
			
			if (expr.name) {
				add_code_dependency_on_value_sets(compiler, set, expr.name);
			}
			
			persistent_array_push_copy(compiler->set_table, &set);
		}
		tok_expect(tokenizer, ';');
	}
}



void compile_scope(Compile_Proc_Context *compiler, Token first) {
	
	Token_Context *tokenizer = compiler->tokenizer;
	//Jac_Context *ctx = compiler->ctx;
	
	
	if (first.kind == '{') {
		u64 proc_count = persistent_array_count(compiler->proc_table);
		u64 value_count = persistent_array_count(compiler->value_table);
		u64 type_count = persistent_array_count(compiler->type_table);
		
		Token next;
		while (!tok_accept(tokenizer, '}', &next)) {
			compile_scope(compiler, next);
		}
		
		// Reset stacks to what they were
		persistent_array_set_count(compiler->proc_table, proc_count);
		persistent_array_set_count(compiler->value_table, value_count);
		persistent_array_set_count(compiler->type_table, type_count);
	} else {
		compile_one_statement(compiler, first);
	}
	
}


void print_value(Value v) {
	if (v.flags & VALUE_LITERAL) {
		if (v.flags & VALUE_STRING) {
			print("\"%s\"", v.imp.str);
		} else {
			print("(U: %u, I: %i, f: %f)", v.imp.literal, v.imp.literal, v.imp.literal);
		}
	} else if (v.flags & VALUE_NOTHING) {
		print("NOTHING");
	} else {
		print("$%i", v.vnum);
	}
}

void visit_rec(Code_Node *node) {
    if (!node || node->visited) return;
    node->visited = true;

	Code_Dependency *dep_to = &node->deps_to;
	
	while (1) {
		if (dep_to->node) {
			visit_rec(dep_to->node);
		}
		if (!dep_to->next) break;
		dep_to = dep_to->next;
	}

	switch (node->kind) {
		
		case CODE_NODE_SET:    
			Code_Node_Set *set = (Code_Node_Set*)node;
			print_value(set->dst);
			print(" = ");
			print_value(set->src);
			print("\n");
			break;
		case CODE_NODE_CALL:   
			Code_Node_Call *call = (Code_Node_Call*)node;
			print("call %s\n", call->symbol);
			break;
		case CODE_NODE_OP_ADD:
			string sym = STR("+"); goto OPS;
		case CODE_NODE_OP_SUB: 
			sym = STR("-"); goto OPS;
		case CODE_NODE_OP_MUL: 
			sym = STR("*"); goto OPS;
		case CODE_NODE_OP_DIV: 
			sym = STR("/");
			OPS:
			Code_Node_Op *op_add = (Code_Node_Op*)node;
			print_value(op_add->dst);
			print(" = ");
			print_value(op_add->left);
			print(" %s ", sym);
			print_value(op_add->right);
			print("\n");
			break;
		
		case CODE_NODE_NONE: // fallthrough
		default: assert(false); break;
	}

	Code_Dependency *dep_from = &node->deps_from;
	
	while (1) {
		if (dep_from->node) {
			visit_rec(dep_from->node);
		}
		if (!dep_from->next) break;
		dep_from = dep_from->next;
	}
}
Code_Node *next_code(Code_Node *n) {
	switch (n->kind) {
		case CODE_NODE_NONE: // fallthrough
		case CODE_NODE_SET:    return (Code_Node*)((u8*)n + sizeof(Code_Node_Set));
		case CODE_NODE_CALL:   return (Code_Node*)((u8*)n + sizeof(Code_Node_Call) + sizeof(Value)*((Code_Node_Call*)n)->arg_count);
		case CODE_NODE_OP_ADD: return (Code_Node*)((u8*)n + sizeof(Code_Node_Op));
		case CODE_NODE_OP_SUB: return (Code_Node*)((u8*)n + sizeof(Code_Node_Op));
		case CODE_NODE_OP_MUL: return (Code_Node*)((u8*)n + sizeof(Code_Node_Op));
		case CODE_NODE_OP_DIV: return (Code_Node*)((u8*)n + sizeof(Code_Node_Op));
		default: assert(false); return 0;
	}
	return 0;
}

s64 compile_proc_thread(Thread *t) {
	Compile_Proc_Args *args = (Compile_Proc_Args*)t->userdata;
	
	Jac_Context *ctx = args->ctx;
	Procedure_Reference *proc = args->proc;
	
	Token_Context _tokenizer = (Token_Context){0};
	
	Compile_Proc_Context _compiler = (Compile_Proc_Context){0};
	Compile_Proc_Context *compiler = &_compiler;
	compiler->ctx = ctx;
	compiler->tokenizer = &_tokenizer;
	compiler->tokenizer->ctx = ctx;
	compiler->next_vnum = 1;
	
	Token_Context *tokenizer = compiler->tokenizer;
	
	// Copy global tables into this proc's tables
	
	persistent_array_init((void**)&compiler->proc_table, sizeof(*compiler->proc_table));
	persistent_array_set_count(compiler->proc_table, persistent_array_count(ctx->global_proc_table));
	u64 sz = persistent_array_count(ctx->global_proc_table)*sizeof(*ctx->global_proc_table);
	memcpy(compiler->proc_table, ctx->global_proc_table, sz);
	
	persistent_array_init((void**)&compiler->value_table, sizeof(*compiler->value_table));
	persistent_array_set_count(compiler->value_table, persistent_array_count(ctx->global_value_table));
	sz = persistent_array_count(ctx->global_value_table)*sizeof(*ctx->global_value_table);
	memcpy(compiler->value_table, ctx->global_value_table, sz);
	
	persistent_array_init((void**)&compiler->type_table, sizeof(*compiler->type_table));
	persistent_array_set_count(compiler->type_table, persistent_array_count(ctx->global_type_table));
	sz = persistent_array_count(ctx->global_type_table)*sizeof(*ctx->global_type_table);
	memcpy(compiler->type_table, ctx->global_type_table, sz);
	
	persistent_array_init((void**)&compiler->set_table, sizeof(*compiler->set_table));
	
	compiler->node_arena = make_arena(1024ULL*1024ULL*1024ULL*8ULL, 8*1024);
	compiler->nodes = (Code_Node*)compiler->node_arena.start;
	
	compiler->dependencies_arena = make_arena(1024ULL*1024ULL*1024ULL*8ULL, 4*1024);
	
	set_source_position(tokenizer, proc->source_header_start);
	
	Token ident = tok_consume(tokenizer);
	assert(ident.kind == TOKEN_KIND_IDENTIFIER);
	
	Procedure_Header header = parse_procedure_header(compiler, ident);
	
	if (!(header.trait_flags & PROCEDURE_TRAIT_COMPILER)) {
		compile_scope(compiler, tok_consume(tokenizer));
	}
	
	
	persistent_array_uninit(compiler->proc_table);
	persistent_array_uninit(compiler->value_table);
	persistent_array_uninit(compiler->type_table);
	persistent_array_uninit(compiler->set_table);
	
	args->result = _compiler;
	
	return 0;
}

typedef struct Jac_Compile_Args {
	Jac_Result result;
	string first_file_path;
} Jac_Compile_Args; 

void jac_context_cleanup(Jac_Context *ctx) {
	persistent_array_uninit(ctx->global_proc_table);
	persistent_array_uninit(ctx->global_value_table);
	persistent_array_uninit(ctx->global_type_table);
}

s64 jac_compile_thread(Thread *t) {
	Jac_Compile_Args *args = (Jac_Compile_Args*)t->userdata;
	
	u64 ps = sys_get_info().page_size;
	Jac_Context *ctx = (Jac_Context*)sys_map_pages(SYS_MEMORY_RESERVE | SYS_MEMORY_ALLOCATE, 0, ((sizeof(Jac_Context)+ps)/ps), false);
	// *ctx = (Jac_Context){0}; // causes stack overflow
	memset(ctx, 0, sizeof(Jac_Context));
	
	// @leak
	// todo(charlie) Allocate this before this thread, so it can be consistently deallocated after even if terminate_error()
	
	ctx->source_arena = make_arena(1024ULL*1024ULL*1024ULL*64ULL, 1024*32);
	ctx->source.data = (u8*)ctx->source_arena.start;
	ctx->result = &args->result;
	
	Token_Context _tokenizer = (Token_Context){0};
	Token_Context *tokenizer = &_tokenizer;
	tokenizer->ctx = ctx;
	
	tokenizer->line_num = 1;
	
	persistent_array_init((void**)&ctx->global_proc_table, sizeof(*ctx->global_proc_table));
	persistent_array_init((void**)&ctx->global_value_table, sizeof(*ctx->global_value_table));
	persistent_array_init((void**)&ctx->global_type_table, sizeof(*ctx->global_type_table));
	
	persistent_array_reserve(ctx->global_type_table, 10);
	
	Type *type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("u8");
	type->kind = TYPE_INT;
	type->size = sizeof(u8);
	type->register_width = 1;
	ctx->type_u8 = type;
	type->val.type_int.is_signed = false;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("s8");
	type->kind = TYPE_INT;
	type->size = sizeof(s8);
	type->register_width = 1;
	ctx->type_s8 = type;
	type->val.type_int.is_signed = true;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("u16");
	type->kind = TYPE_INT;
	type->size = sizeof(u16);
	type->register_width = 1;
	ctx->type_u16 = type;
	type->val.type_int.is_signed = false;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("s16");
	type->kind = TYPE_INT;
	type->size = sizeof(s16);
	type->register_width = 1;
	ctx->type_s16 = type;
	type->val.type_int.is_signed = true;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("u32");
	type->kind = TYPE_INT;
	type->size = sizeof(u32);
	type->register_width = 1;
	ctx->type_u32 = type;
	type->val.type_int.is_signed = false;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("s32");
	type->kind = TYPE_INT;
	type->size = sizeof(s32);
	type->register_width = 1;
	ctx->type_s32 = type;
	type->val.type_int.is_signed = true;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("u64");
	type->kind = TYPE_INT;
	type->size = sizeof(u64);
	type->register_width = 1;
	ctx->type_u64 = type;
	type->val.type_int.is_signed = false;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("s64");
	type->kind = TYPE_INT;
	type->size = sizeof(s32);
	type->register_width = 1;
	ctx->type_s32 = type;
	type->val.type_int.is_signed = true;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("f32");
	type->kind = TYPE_FLOAT;
	type->size = sizeof(f32);
	type->register_width = 1;
	ctx->type_f32 = type;
	
	type = (Type*)persistent_array_push_empty(ctx->global_type_table);
	type->name = STR("f64");
	type->kind = TYPE_FLOAT;
	type->size = sizeof(f64);
	type->register_width = 1;
	ctx->type_f64 = type;
	
	ctx->type_literal_float.name = STR("Float Literal");
	ctx->type_literal_float.kind = TYPE_LITERAL_FLOAT;
	ctx->type_literal_float.size = 0;
	
	ctx->type_literal_int.name = STR("Int Literal");
	ctx->type_literal_int.kind = TYPE_LITERAL_INT;
	ctx->type_literal_int.size = 0;
	
	ctx->type_literal_string.name = STR("String Literal");
	ctx->type_literal_string.kind = TYPE_LITERAL_STRING;
	ctx->type_literal_string.size = 0;
	
	ctx->type_nothing.name = STR("Nothing Type");
	ctx->type_nothing.kind = TYPE_NOTHING;
	ctx->type_nothing.size = 0;
	
	
	if (!push_source_file(ctx, args->first_file_path)) {
		terminate_error(ctx, JAC_RESULT_FILE_NOT_FOUND, tprint("File '%s' not found", args->first_file_path), 0, 0);
	} else {
		
		///
		// Prepass to probe for top-level declarations ahead of time
		
		Token next = tok_consume(tokenizer);
		while (tok_peek(tokenizer).kind != TOKEN_KIND_EOF) {
		
			prepass_some(tokenizer, next);
			
			next = tok_consume(tokenizer);
		}
		
		///
		// Parse types
		
		///
		// Parse globals
		
		///
		// Compile procedures in parallell
		
		u64 thread_count = persistent_array_count(ctx->global_proc_table);
		
		Thread *threads = PushTempBuffer(Thread, thread_count);
		Compile_Proc_Args *proc_args = PushTempBuffer(Compile_Proc_Args, thread_count);
		memset(threads, 0, sizeof(Thread)*thread_count);
		memset(proc_args, 0, sizeof(Compile_Proc_Args)*thread_count);
		
		for (u64 i = 0; i < thread_count; i += 1) {
			Thread *thread = threads + i;
			Compile_Proc_Args *a = proc_args + i;
			a->proc = ctx->global_proc_table + i;
			a->ctx = ctx;
			sys_thread_init(thread, compile_proc_thread, a);
		}
		for (u64 i = 0; i < thread_count; i += 1) {
			Thread *thread = threads + i;
			sys_thread_start(thread);
		}
		
		s64 exit_code = 0;
		for (u64 i = 0; i < thread_count; i += 1) {
			Thread *thread = threads + i;
			
			s64 result = sys_thread_join(thread);
			
			if (result != 0) {
				if (exit_code == 0) exit_code = result;
			}
			
			Compile_Proc_Args *a = proc_args + i;
			
			if (exit_code == 0) {
				print("Proc %s:\n", a->proc->name);
				Code_Node *n = a->result.nodes;
				while ((u8*)n < (u8*)a->result.node_arena.position) {
					visit_rec(n);
					n = next_code(n);
				}
				print("\n");
			}
		}
		if (exit_code != 0) {
			log(JAC_LOG_COMPILE_ERROR, "A procedure failed compiling. Aborted.");
			sys_exit_current_thread(exit_code);
		}
		
		/*for (u64 i = 0; i < persistent_array_count(ctx->global_proc_table); i += 1) {
			
			Procedure_Reference *proc = ctx->global_proc_table + i;
			
			if (!proc->compiled) {
			
				if (!header_parsed) {
					set_source_position(tokenizer, proc->source_header_start);
				} else {
					set_source_position(tokenizer, proc->last_compile_pos);
				}
				
				if (try_compile_proc(ctx
			}
		}*/
	}
	
	
	
	return 0;
}

JAC_EXPORT Jac_Result jac_compile(string first_file_path, Jac_Config config) {
	(void)config;
	Jac_Compile_Args args = (Jac_Compile_Args){0};
	args.first_file_path = first_file_path;
	
	// Run on separate thread so we can just exit current thread on error and don't need to return correctly
	// in each possible error path.
	
	Thread t;
	
	bool thread_ok = sys_thread_init(&t, jac_compile_thread, &args);
	assert(thread_ok);
	
	sys_thread_start(&t);
	sys_thread_join(&t);
	sys_thread_close(&t);
	
	return args.result;
}

#endif // JAC_IMPL

#endif // JAC_H

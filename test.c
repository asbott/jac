
#define JAC_IMPL
#include "jac.h"

int main(void) {
	
	string file = STR("test.jac");
	
	Jac_Config conf = (Jac_Config){0};
	conf.inline_mode = JAC_INLINE_EXPLICIT;
	conf.out_file_path = STR("test_jac.c");
	
	Jac_Result result = jac_compile(file, conf);
	
	return (int)result.code;
}






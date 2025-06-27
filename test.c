
#define JAC_IMPL
#include "jac.h"

int main(void) {
	
	string file = STR("test.jac");
	
	Jac_Config conf = (Jac_Config){0};
	conf.out_file_path = STR("test.exe");
	
	Jac_Result result = jac_compile(file, conf);
	
	return (int)result.code;
}


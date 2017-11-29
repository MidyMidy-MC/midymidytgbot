#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <curl/curl.h>
#include "uselibcurl.h"

int main()
{
	void* dl_handle0 = dlopen("/lib/libcurl.so", RTLD_NOW);
	void* dl_handle = dlopen("./libuselibcurl.so", RTLD_LAZY);
	if(!dl_handle){
		printf("unable to load libuselibcurl.so\n");
		return 1;
	}

	void (* init_curl_fp)(void) = dlsym(dl_handle, "global_init_curl");
	struct mem_block* (* http_get_fp)(char*) = dlsym(dl_handle, "http_get");

	(*init_curl_fp)(); // init
	struct mem_block* mem = (*http_get_fp)("http://127.0.0.1:8000/");
	printf("size: %ld\n", mem->size);

	mem->mem = (char*)realloc(mem->mem, mem->size + 1);
	mem->mem[mem->size] = 0;
	printf("%s\n", mem->mem);

	free(mem);
	dlclose(dl_handle);

	return 0;
}

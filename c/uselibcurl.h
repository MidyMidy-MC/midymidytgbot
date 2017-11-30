#ifndef USELIBCURL
#define USELIBCURL
struct mem_block {
	char* mem;
	unsigned long size;
};

void global_init_curl();
struct mem_block* c_http_get(char* url);
struct mem_block* c_http_post(char* url, char* content);
#endif

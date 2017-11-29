#ifndef USELIBCURL
#define USELIBCURL
struct mem_block {
	char* mem;
	unsigned long size;
};

void global_init_curl();
struct mem_block* http_get(char* url);
#endif

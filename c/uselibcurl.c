#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include "uselibcurl.h"

void global_init_curl()
{
	curl_global_init(CURL_GLOBAL_DEFAULT);
}

struct mem_block* make_mem_block()
{
	struct mem_block* mem = (struct mem_block*)malloc(sizeof(struct mem_block));
	mem->size = 0;
	mem->mem = (char*)malloc(0);
	return mem;
}

size_t write_callback
(const void* ptr, size_t size, size_t nmemb, void* target)
{
	size_t write_size = size * nmemb;
	struct mem_block* mem = (struct mem_block*)target;

	mem->mem = realloc(mem->mem, mem->size + write_size);
	if(mem->mem == NULL){
		printf("libuselibcurl: write_callback: Memory error, realloc returned NULL\n");
		return 0;
	}
	memcpy(&(mem->mem[mem->size]), ptr, write_size);
	mem->size += write_size;

	return write_size;
}

struct mem_block* c_http_get(char* url)
	// char* url: end with \0
{
	CURL* curl = curl_easy_init();
	if(!curl)
		return NULL;

	struct mem_block* mem = make_mem_block();

	CURLcode res;
	struct curl_slist *list = curl_slist_append(list, "Content-Type: application/json");

	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10L); // complete within 10 sec
	curl_easy_setopt(curl, CURLOPT_USERAGENT, "midymidytgbot");
	curl_easy_setopt(curl, CURLOPT_HTTPHEADER, list);
	curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void*)mem);

	res = curl_easy_perform(curl);

	curl_slist_free_all(list);
	curl_easy_cleanup(curl);

	if(res != CURLE_OK){
		free(mem->mem);
		free(mem);
		return NULL;
	}else{
		return mem;
	}
}


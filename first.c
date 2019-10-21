#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>


typedef struct blocks{	
	unsigned long long int tagBits;
	int validBit;
	int lruCount;
} blocks;

typedef struct set{
	blocks **blocks;
} set;

typedef struct cache{
	int cacheSize;
	int blockSize;
	int reads;
	int writes;
	int numSets;
	int numBlocks;
	int hits;
	int misses;
	set **set;
} cache;


void createCache(cache* cache, int cacheSize, int blockSize, char associativity, int assocNum);
void cacheSim(cache *cache, FILE *traceFile, int wprefetch);
void updateLRU(cache *cache, int indexBits,unsigned long long int tagBits, int numBlocks);
void insertInCache(cache *cache, int indexBits, unsigned long long int tagBits, int numBlocks, int typeMiss);
int isInCache(cache *cache, int indexBits, unsigned long long int tagBits, int numBlocks);
void preFetch(cache *cache, unsigned long long int binaryNum, int offsetBlock, int indexSet, int tagbits, int getSet);
void printCache(cache *cache, int prefetch);
void freeCache(cache *cache);


void createCache(cache* cache, int cacheSize, int blockSize, char associativity, int assocNum){
	int i = 0;	
	int j = 0;

	cache->reads = 0;
	cache->writes = 0;
	cache->hits = 0;
	cache->misses =0;
	cache->blockSize = blockSize; 
	cache->cacheSize = cacheSize;
	if(associativity == 'a'){
		cache->numSets = cacheSize/blockSize;
		cache->numBlocks = 1;
		cache->set = (set**)malloc(cache->numSets*sizeof(set*));
		for(i = 0; i < cache->numSets; i++){
			cache->set[i] = (set*)malloc(sizeof(set));
			cache->set[i]->blocks = malloc(sizeof(blocks) * cache->numBlocks);
			for(j = 0; j < cache->numBlocks; j++){
				cache->set[i]->blocks[j] = malloc(sizeof(blocks));
				cache->set[i]->blocks[j]->tagBits = 0;
				cache->set[i]->blocks[j]->validBit = 0;
				cache->set[i]->blocks[j]->lruCount = 0;
			}
		}
		return;
		
	}else if(associativity == 'b'){
		cache->numSets = 1;
		cache->numBlocks = cacheSize/blockSize;
		cache->set = (set**)malloc(cache->numSets*sizeof(set*));
		for(i = 0; i < cache->numSets; i++){
			cache->set[i] = (set*)malloc(sizeof(set));
			cache->set[i]->blocks = malloc(sizeof(blocks) * cache->numBlocks);
			for(j = 0; j < cache->numBlocks; j++){
				cache->set[i]->blocks[j] = malloc(sizeof(blocks));
				cache->set[i]->blocks[j]->tagBits = 0;
				cache->set[i]->blocks[j]->validBit = 0;
				cache->set[i]->blocks[j]->lruCount = 0;
			}
		}
		return;
		
	}else{
		cache->numSets = cacheSize/(blockSize*assocNum);
		cache->numBlocks = assocNum;
		cache->set = (set**)malloc(cache->numSets*sizeof(set*));
		for(i = 0; i < cache->numSets; i++){
			cache->set[i] = (set*)malloc(sizeof(set));
			cache->set[i]->blocks = malloc(sizeof(blocks) * cache->numBlocks);
			for(j = 0; j < cache->numBlocks; j++){
				cache->set[i]->blocks[j] = malloc(sizeof(blocks));
				cache->set[i]->blocks[j]->tagBits = 0;
				cache->set[i]->blocks[j]->validBit = 0;
				cache->set[i]->blocks[j]->lruCount = 0;
			}
		}
		return;
	}
	
}

void cacheSim(cache *cache, FILE *traceFile, int wprefetch){
//	char *irrelevant;
	char readOrWrite;
	char memAdd[100];
	int i = 0;
	int k = 0;
	int numBlocks = cache->numBlocks;
	int getSet = 0;
	char tempLet;
	unsigned long long int tempNum = 0;
	unsigned long long int binaryNum = 0;
	int eof = 0;

	int offsetBlock = (int)(log(cache->blockSize)/log(2));
	int indexSet = (int)(log(cache->numSets)/log(2));
	int tagbits = 48 - indexSet - offsetBlock;

	getSet = ((1<<indexSet)-1);

	while(eof == 0){
		binaryNum = 0;
		fscanf(traceFile, "%s ", memAdd);
		if(strcmp(memAdd, "#eof") == 0){
			break;
		}
		fscanf(traceFile, "%c %s\n", &readOrWrite, memAdd);
		for(i = strlen(memAdd)-1; i>1; i--){
			if(memAdd[i] == 'a'){
				tempNum = 10;
			}else if(memAdd[i] == 'b'){
				tempNum = 11;
			}else if(memAdd[i] == 'c'){
				tempNum = 12;
			}else if(memAdd[i] == 'd'){
				tempNum = 13;
			}else if(memAdd[i] == 'e'){
				tempNum = 14;
			}else if(memAdd[i] == 'f'){
				tempNum = 15;
			}else{
				tempLet = memAdd[i];
				tempNum = tempLet - '0';
			}
			binaryNum += tempNum*(int)pow(16,k);
			k++;
			tempNum = 0;
		}
		k = 0;


		unsigned long long int tag = binaryNum>>(indexSet+offsetBlock);
		int index = (binaryNum>>offsetBlock)&getSet;

		if(readOrWrite == 'R'){
			if(isInCache(cache, index, tag, numBlocks) == 1){ //reg miss or cold miss
				cache->misses++;
				cache->reads++;
				if(wprefetch == 1){
					preFetch(cache, binaryNum, offsetBlock, indexSet, tagbits, getSet); //call prefetch function
				}
			}else {//hit
				cache->hits++;
				updateLRU(cache, index, tag, numBlocks);
				
			}
		}else if(readOrWrite == 'W'){
			if(isInCache(cache, index, tag, numBlocks) == 1){// reg miss or cold miss
				cache->reads++;
				cache->writes++;
				cache->misses++;
				if(wprefetch == 1){
					preFetch(cache, binaryNum, offsetBlock, indexSet, tagbits, getSet);//call prefetch function
				}
			}else{//hit
				cache->hits++;
				cache->writes++;
				updateLRU(cache, index, tag, numBlocks);
			}
		}
		
	}
	return; 
}	
	
	
void updateLRU(cache *cache, int indexBits,unsigned long long int tagBits, int numBlocks){
	int j =0;
	int i = 0;
	int prevLRU = 0;
	for(i = 0; i<numBlocks; i++){
		if(cache->set[indexBits]->blocks[i]->tagBits == tagBits){
			prevLRU = cache->set[indexBits]->blocks[i]->lruCount;
			cache->set[indexBits]->blocks[i]->lruCount = 1;
			for(j =0; j<numBlocks; j++){
				if(cache->set[indexBits]->blocks[j]->lruCount < prevLRU && j != i){
					cache->set[indexBits]->blocks[j]->lruCount++;
				}
			}
			return;
		}
	}
		
	return;
}


void insertInCache(cache *cache, int indexBits, unsigned long long int tagBits, int numBlocks, int typeMiss){	//Remember to change all writeInCache to insertInCache
	int j = 0;
	int i = 0;	
	int prevLRU = 0;
	if(typeMiss == 2){//write in memAdd for cold miss
		for(j=0; j < numBlocks; j++){
			if(cache->set[indexBits]->blocks[j]->validBit == 1){
				cache->set[indexBits]->blocks[j]->lruCount++;
			}else{
				cache->set[indexBits]->blocks[j]->tagBits = tagBits;
				cache->set[indexBits]->blocks[j]->validBit = 1;
				cache->set[indexBits]->blocks[j]->lruCount = 1;
				return;
			}
		}
	}else if(typeMiss == 1){
			
		for(i=0; i<numBlocks; i++){	
			if(cache->set[indexBits]->blocks[i]->lruCount == numBlocks){
				cache->set[indexBits]->blocks[i]->tagBits = tagBits;
				prevLRU = cache->set[indexBits]->blocks[i]->lruCount;
				cache->set[indexBits]->blocks[i]->lruCount = 1;
				for(j =0; j<numBlocks; j++){
					if(cache->set[indexBits]->blocks[j]->lruCount < prevLRU && j != i){
						cache->set[indexBits]->blocks[j]->lruCount++;
					}
				}
				return;
			}
		}	
	}
	return;
}

int isInCache(cache *cache, int indexBits, unsigned long long int tagBits, int numBlocks){
	int i = 0;
	for(i = 0; i<numBlocks; i++){
			if(cache->set[indexBits]->blocks[i]->tagBits == tagBits){
				return 0; //is in cache
			}else if(cache->set[indexBits]->blocks[i]->validBit == 0){
				insertInCache(cache, indexBits, tagBits, numBlocks, 2);
				return 1; //cold miss
			}
						
	}
	insertInCache(cache, indexBits, tagBits, numBlocks, 1);
	return 1; //is not in cache reg miss
}


void preFetch(cache *cache, unsigned long long int binaryNum, int offsetBlock, int indexSet, int tagbits, int getSet){
	//int getSet = ((1<<indexSet)-1);
	binaryNum = binaryNum + cache->blockSize;
	int numBlocks = cache->numBlocks;
	unsigned long long int tag = binaryNum>>(indexSet+offsetBlock); 
	int index = (binaryNum>>offsetBlock)&getSet;
		
	if(isInCache(cache, index, tag, numBlocks) == 1){	//miss
		cache->reads++;
	}
	return;
}

void printCache(cache *cache, int wprefetch){
	int reads = cache->reads;
	int writes = cache->writes;
	int hits = cache->hits;
	int misses = cache->misses;
	if(wprefetch == 0){ // no prefetch
		printf("no-prefetch\n");
		printf("Memory reads: %d\n", reads);
		printf("Memory writes: %d\n", writes);
		printf("Cache hits: %d\n", hits);
		printf("Cache misses: %d\n", misses);
	} else{
		printf("with-prefetch\n");
		printf("Memory reads: %d\n", reads);
		printf("Memory writes: %d\n", writes);
		printf("Cache hits: %d\n", hits);
		printf("Cache misses: %d\n", misses);
	}
	return;
}

void freeCache(cache *cache){
	int i = 0;
	int j = 0;
	int nSets = cache->numSets;
	int nBlocks = cache->numBlocks;
	for(i = 0; i<nSets; i++){
		for(j = 0; j<nBlocks; j++){
			free(cache->set[i]->blocks[j]);
		}
		free(cache->set[i]);
	}
	free(cache);
	return;
}


int main(int argc, char**argv){
	int cacheSize = atoi(argv[1]);
	char *assoc = argv[2];
 	char *cachePolicy = argv[3];
	int blockSize = atoi(argv[4]);
  	FILE *traceFile = fopen(argv[5], "r");
 	char newArr[10];
  	int i = 0;
 	int j = 0;
  	char tempAssocNum[20];
	int n = 0;
	char associativity;
	cache *cache1 = (cache*)malloc(sizeof(cache));
	cache *cache2 = (cache*)malloc(sizeof(cache));
	
	if(cacheSize%2 != 0){
		printf("error");
		return 0;
	}
  	if(strcmp(cachePolicy,"lru") == 1){
		printf("error");
		return 0;
	}
	if(strcmp(assoc,"direct") == 0){
		associativity = 'a';
		createCache(cache1, cacheSize, blockSize, associativity, 0);
		createCache(cache2, cacheSize, blockSize, associativity, 0);
 	}else if(strcmp(assoc,"assoc")== 0){
		associativity = 'b';
		createCache(cache1, cacheSize, blockSize, associativity, 0);
		createCache(cache2, cacheSize, blockSize, associativity, 0);
	} else {
		strncpy(newArr,assoc,6);
		newArr[6] = '\0';
		if(strcmp(newArr,"assoc:") == 0){
			j=0;
			for(i = 6; i<strlen(assoc); i++){
				if(isdigit(assoc[i])){
					tempAssocNum[j] = assoc[i];
					j++;
				}else{
					printf("error\n");
				}
			}
			tempAssocNum[j] = '\0';
		}
		n = atoi(tempAssocNum);
		if(n%2 == 0){
			associativity = 'c';
			createCache(cache1, cacheSize, blockSize, associativity, n);
			createCache(cache2, cacheSize, blockSize, associativity, n);
		}else{
			printf("error");
			return 0;
		}
      }
	cacheSim(cache1, traceFile, 0); // no prefetch
	printCache(cache1, 0);
	rewind(traceFile);
	cacheSim(cache2, traceFile, 1); // prefetch 
	printCache(cache2, 1);
	fclose(traceFile);
	freeCache(cache1);
	freeCache(cache2);
	return 0;
}

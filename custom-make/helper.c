#include <stdio.h>
#include <stdlib.h>
#include "helper.h"

char* getenv_ec(const char* name) {
  char* tmp = getenv(name);
  
  if(tmp == NULL) {
    perror("getenv");
    exit(EXIT_FAILURE);
  }

  return tmp;
}

FILE* fopen_ec(const char* name, const char* mode) {
  FILE *tmp = fopen(name, mode);
  
  if(tmp == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  return tmp;
}

int fclose_ec(FILE *stream) {
  if(fclose(stream) == EOF) {
    perror("fclose");
    exit(EXIT_FAILURE);
  }
  return 0;
}

int fflush_ec(FILE *stream) {
  if(fflush(stream) == EOF) {
    perror("fflush");
    exit(EXIT_FAILURE);
  }
  return 0;
}

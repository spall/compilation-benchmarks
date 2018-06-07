#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "helper.h"
#include "custom-time.h"

int main(int argc, char **argv) {

  if (argc < 2) {
    fprintf(stderr, "Error: not enough arguments to rusage\n");
    exit(EXIT_FAILURE);
  }

  // environment variables we use
  const char* scnum = getenv_ec("SCNUM");
  const char* outputfile = getenv_ec("OUTPUTFILE");
  
  FILE *tmp = fopen_ec(scnum, "r");
  
  char* line = NULL;
  size_t len = 0;
  ssize_t read = getline(&line, &len, tmp);
  if (read == -1) {
    perror("getline");
    exit(EXIT_FAILURE);
  }

  fclose_ec(tmp);
  fopen_ec(scnum, "w");
  
  setenv("CURSCNUM", line, 1); // overwrite curretn value

  // turn line into a number.
  int old = atoi(line);
  fprintf(tmp, "%d\n", old + 1);
  fflush(tmp);

  fclose_ec(tmp);
  
  FILE *out = fopen_ec(outputfile, "a");
  
  fprintf(out, "executing shell-command: %d ", old);
  
  int a;
  for(a = 1; a < argc; a ++) {
    fprintf(out, "%s ", argv[a]);
  }
  fprintf(out, "\n");

  fflush_ec(out);

  // estimate timing overhead
  struct timespec *overhead = malloc(sizeof(struct timespec));
  if (overhead == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  estimate_timing_overhead(overhead);  
  
  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));
  
  if (start == NULL || end == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  
  int r1 = clock_gettime(CLOCK_REALTIME, start);

  int mpid = fork();
  if (mpid == 0) {
    char** args = malloc(sizeof(char*)*argc);
    if (args == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    int j = 0;
    int i;
    for(i = 1; i < argc; i ++) {
      args[j] = argv[i];
      j = j + 1;
    }
    args[j] = 0;

    execv("/bin/bash", args);

    perror("execv");
    exit(EXIT_FAILURE);
  } else if (mpid == -1) {
    perror("fork");
    exit(EXIT_FAILURE);
  } else {
    int status;
    pid_t pid = wait(&status);

    // todo check status
    int r2 = clock_gettime(CLOCK_REALTIME, end);
    
    if (pid == -1) {
      perror("wait");
      exit(EXIT_FAILURE);
    }
    
    /*
    if (WIFEXITED(status)) {
      printf("exited, status=%d\n", WEXITSTATUS(status));
    } else if (WIFSIGNALED(status)) {
      printf("killed by signal %d\n", WTERMSIG(status));
    } else if (WIFSTOPPED(status)) {
      printf("stopped by signal %d\n", WSTOPSIG(status));
    } else if (WIFCONTINUED(status)) {
      printf("continued\n");
      } */
    
    if (-1 == r1 || -1 == r2) {
      exit(EXIT_FAILURE);
    }
    
    struct timespec *tt = malloc(sizeof(struct timespec));
    
    if (tt == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }
    
    timespec_subtract_3(tt, end, start, overhead);
    
    tmp = fopen_ec(outputfile, "a");
    
    fprintf(tmp, "argv= ");
    
    for(a = 0; a < argc; a ++) {
      fprintf(tmp, "%s ", argv[a]);
    }
    
    fprintf(tmp, "\nelapsed= %lld.%.9ld\n finished shell-command: %d\n", (long long)tt->tv_sec, tt->tv_nsec, old);
    
    fflush(tmp);
    
    fclose_ec(tmp);

    free(start);
    free(end);
    free(tt);
    free(overhead);
  }
  exit(EXIT_SUCCESS);
}

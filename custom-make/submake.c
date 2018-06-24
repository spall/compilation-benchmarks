#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include "helper.h"
#include "custom-time.h"

int main(int argc, char **argv) {

  // environment variables we use
  const char* cdir = getenv_ec("PWD");
  const char* outputfile = getenv_ec("OUTPUTFILE");
  const char* curscnum = getenv_ec("CURSCNUM");
  unsetenv("CURSCNUM");

  /*
  // remove curscnum from envp
  int i;
  int j = 0;
  for(i = 0; envp[i] != 0; ++i) {
    if (0 != strncmp(envp[i], "CURSCNUM", 8)) {
      envp[j] = envp[i];
      j = j +1;
    }
  }
  envp[j] = 0;
  */
 
  int old = atoi(curscnum);
  
  // estimate timing overhead
  struct timespec *overhead = malloc(sizeof(struct timespec));

  if (overhead == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  estimate_timing_overhead(overhead);

  // write first line to file

  FILE *tmp = fopen_ec(outputfile, "a");
  
  fprintf(tmp, "executing sub-make: %d : ", old);
  int a;
  for(a = 1; a < argc; a ++) {
    fprintf(tmp, "%s ", argv[a]);
  }
  
  fprintf(tmp, "; in directory %s\n", cdir);

  fflush_ec(tmp);
  fclose_ec(tmp);

  // time make
  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));
  
  if (start == NULL || end == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  int r1 = clock_gettime(CLOCK_REALTIME, start);

  // run real make
  int mpid = fork();
  if (mpid == 0) {
    int argnum = argc + 3;
    const char** args = malloc(sizeof(char*)*argnum);
    if (args == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    args[0] = argv[0];
    args[1] = "--debug=v";
    args[2] = "MAKE=submake";
    int j = 3;
    int i;
    for(i = 1; i < argc; i ++) {
      args[j] = argv[i];
      j = j + 1;
    }
    args[j] = 0;
 
    execvp("make-4.2", args);
    perror("execvp");
    exit(EXIT_FAILURE);
  } else if (mpid == -1) {
    perror("fork");
    exit(EXIT_FAILURE);
  } else {
    int status;
    pid_t pid = wait(&status);
    
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
      perror("clock_gettime");
      exit(EXIT_FAILURE);
    }    
    
    struct timespec *tt = malloc(sizeof(struct timespec));
    
    if (tt == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    timespec_subtract_3(tt, end, start, overhead);

    tmp = fopen_ec(outputfile, "a");

    fprintf(tmp, "submake-argv= ");

    int a;
    for(a = 0; a < argc; a ++) {
      fprintf(tmp, "%s ", argv[a]);
    }
    fprintf(tmp, "\n");
    
    fprintf(tmp, "elapsed= %lld.%.9ld\n finishing sub-make: %d : ", (long long)tt->tv_sec, tt->tv_nsec, old);

    for(a = 1; a < argc; a ++) {
      fprintf(tmp, "%s ", argv[a]);
    }

    fprintf(tmp, "; in directory %s\n", cdir);
    fflush(tmp);
    fclose_ec(tmp);

    free(start);
    free(end);
    free(tt);
    free(overhead);
  }
  exit(EXIT_SUCCESS);
}

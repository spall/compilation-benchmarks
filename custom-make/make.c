#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include "helper.h"

int main(int argc, char** argv) {

  // environment variables we use
  char* cdir = getenv_ec("PWD");
  char* outputfile = getenv_ec("OUTPUTFILE");
  char* makej = getenv_ec("MAKEJ");
  
  // estimate timing overhead
  struct timespec *overhead = malloc(sizeof(struct timespec));
  
  if (overhead == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  estimate_timing_overhead(overhead);

  // write first line to file  
  FILE *out = fopen_ec(outputfile, "a");
  
  fprintf(out, "executing top-make: ");

  int a;
  for(a = 0; a < argc; a ++) {
    fprintf(out, "%s ", argv[a]);
  }
  
  fprintf(out, "; in directory %s\n", cdir);

  fflush_ec(out);
  fclose_ec(out);

  // time make
  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));
  
  if (start == NULL || end == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  int r1 = clock_gettime(CLOCK_REALTIME, start);

  // set up output redirection
  int fd = open(outputfile, O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);
  if (-1 == fd) {
    perror("open");
    exit(EXIT_FAILURE);
  }
  
  if (-1 == dup2(fd, STDOUT_FILENO)) {
    perror("dup2");
    exit(EXIT_FAILURE);
  }
  if (-1 == dup2(fd, STDERR_FILENO)) {
    perror("dup2");
    exit(EXIT_FAILURE);
  }
  if (-1 == close(fd)) {
    perror("close");
    exit(EXIT_FAILURE);
  }

  // run real make
  int mpid = fork();
  if (mpid == 0) {
    char** args = malloc(sizeof(char*)*(argc+5));
    if (args == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    args[0] = argv[0];
    args[1] = "--debug=v";
    args[2] = "MAKE=submake";
    args[3] = "-j";
    args[4] = makej;

    int j = 5;
    int i;
    for(i = 1; i < argc; i ++) {
      args[j] = argv[i];
      j = j + 1;
    }
    args[j] = 0;

    execvp("actual-make", args);

    perror("execvp");
  } else if (mpid == -1) {
    perror("fork");
    exit(EXIT_FAILURE);
  } else {
    int status;
    pid_t pid = wait(&status);

    int r2 = clock_gettime(CLOCK_REALTIME, end);

    if(pid == -1) {
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

    // todo check status
    
    if (-1 == r1 || -1 == r2) {
      exit(EXIT_FAILURE);
    }

    struct timespec *tt = malloc(sizeof(struct timespec));
    
    if (tt == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    timespec_subtract_3(tt, end, start, overhead);

    FILE *out = fopen_ec(outputfile, "a");

    fprintf(out, "topmake-argv= ");
    int a;
    for(a = 0; a < argc; a ++) {
      fprintf(out, "%s ", argv[a]);
    }
    
    fprintf(out, "\nelapsed= %lld.%.9ld\n finishing top-make: ", (long long)tt->tv_sec, tt->tv_nsec);

    for(a = 0; a < argc; a ++) {
      fprintf(out, "%s ", argv[a]);
    }

    fprintf(out, "; in directory %s\n", cdir);
    fflush_ec(out);    
    fclose_ec(out);
        
    free(start);
    free(end);
    free(tt);
    free(overhead);
  }
  exit(EXIT_SUCCESS);
}

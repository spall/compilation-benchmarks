#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

int main(int argc, char** argv) {

  // environment variables we use
  const char* cdir = getenv("PWD");
  const char* outputfile = getenv("OUTPUTFILE");
  char* makej = getenv("MAKEJ");
  
  if(cdir == NULL) {
    printf("Error: CDIR environment variable not set\n");
    exit(EXIT_FAILURE);
  }

  if (outputfile == NULL) {
    printf("Error: OUTPUTFILE environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  if (makej == NULL) {
    printf("Error: MAKEJ environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  
  // estimate timing overhead
  struct timespec *ov1 = malloc(sizeof(struct timespec));
  struct timespec *ov2 = malloc(sizeof(struct timespec));
  struct timespec *overhead = malloc(sizeof(struct timespec));
  int r1 = clock_gettime(CLOCK_REALTIME, ov1);

  int r2 = clock_gettime(CLOCK_REALTIME, ov2);
  if (-1 == r1 || -1 == r2) {
    exit(EXIT_FAILURE);
  }

  if (1 == timespec_subtract(overhead, ov2, ov1)) { // ov2 - ov1
    printf("Negative overhead\n");
    exit(EXIT_FAILURE);
  }

  free(ov1);
  free(ov2);

  // write first line to file
  FILE *tmp = fopen(outputfile, "a");
  
  if (tmp == NULL) {
    exit(EXIT_FAILURE);
  }
  
  fprintf(tmp, "executing top-make: ");
  int a;
  for(a = 0; a < argc; a ++) {
    fprintf(tmp, "%s ", argv[a]);
  }
  
  fprintf(tmp, "; in directory %s\n", cdir);

  if (fclose(tmp) == EOF) {
    exit(EXIT_FAILURE);
  }

  // time make
  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));
  r1 = clock_gettime(CLOCK_REALTIME, start);

  // set up output redirection
  int fd = open(outputfile, O_APPEND || O_CREAT);
  if (-1 == fd) {
    perror("open");
    exit(EXIT_FAILURE);
  }
  
  dup2(fd, STDOUT_FILENO);
  dup2(fd, STDERR_FILENO);
  close(fd);

  // run real make
  int mpid = fork();
  if (mpid == 0) {
    int argnum = argc + 6;
    char** args = malloc(sizeof(char*)*argnum);
    args[0] = argv[0];
    args[1] = "--debug=v";
    args[2] = "MAKE=submake";
    args[3] = "SHELL=rusage /bin/bash";
    args[4] = "-j";
    args[5] = makej;

    int j = 6;
    int i;
    for(i = 1; i < argc; i ++) {
      args[j] = argv[i];
      j = j + 1;
    }
    args[j] = 0;

    execv("/usr/bin/make", args);

    perror("Execv failed");
  } else if (mpid == -1) {
    printf("fork failed\n");
    exit(EXIT_FAILURE);
  } else {
    int status;
    pid_t pid = wait(&status);

    // todo check status
    r2 = clock_gettime(CLOCK_REALTIME, end);
    
    if (-1 == r1 || -1 == r2) {
      exit(EXIT_FAILURE);
    }

    struct timespec *tmptt = malloc(sizeof(struct timespec));
    struct timespec *tt = malloc(sizeof(struct timespec));
    
    if (1 == timespec_subtract(tmptt, end, start)) {
      printf("Negative time\n");
      exit(EXIT_FAILURE);
    }
    
    timespec_subtract(tt, tmptt, overhead);

    FILE *tmp = fopen(outputfile, "a");
    if (tmp == NULL) {
      exit(EXIT_FAILURE);
    }

    fprintf(tmp, "topmake-argv=");
    int a;
    for(a = 0; a < argc; a ++) {
      fprintf(tmp, " %s ", argv[a]);
    }
    
    fprintf(tmp, "\nelapsed= %lld.%ld\n finishing top-make: ", (long long)tt->tv_sec, tt->tv_nsec);
    for(a = 0; a < argc; a ++) {
      fprintf(tmp, "%s ", argv[a]);
    }

    fprintf(tmp, "; in directory %s\n", cdir);

    free(start);
    free(end);
    free(tmptt);
    free(tt);
    free(overhead);

    if (fclose(tmp) == EOF) {
      exit(EXIT_FAILURE);
    }
  }
}

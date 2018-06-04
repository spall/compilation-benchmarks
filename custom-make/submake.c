#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

  // environment variables we use
  const char* cdir = getenv("PWD");
  const char* outputfile = getenv("OUTPUTFILE");
  const char* curscnum = getenv("CURSCNUM");

  if(cdir == NULL) {
    fprintf(stderr, "Error: CDIR environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  
  if (outputfile == NULL) {
    fprintf(stderr, "Error: OUTPUTFILE environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  if (curscnum == NULL) {
    fprintf(stderr, "Error: CURSCNUM environment variable not set\n");
    exit(EXIT_FAILURE);
  }

  // estimate timing overhead
  struct timespec *ov1 = malloc(sizeof(struct timespec));
  struct timespec *ov2 = malloc(sizeof(struct timespec));
  struct timespec *overhead = malloc(sizeof(struct timespec));

  
  if (ov1 == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  if (ov2 == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  if (overhead == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  int r1 = clock_gettime(CLOCK_REALTIME, ov1);
  int r2 = clock_gettime(CLOCK_REALTIME, ov2);
  if (-1 == r1 || -1 == r2) {
    perror("clock-gettime");
    exit(EXIT_FAILURE);
  }

  if (1 == timespec_subtract(overhead, ov2, ov1)) { // ov2 - ov1
    fprintf(stderr, "Negative overhead\n");
    exit(EXIT_FAILURE);
  }

  free(ov1);
  free(ov2);

  // write first line to file

  /*
  FILE *tmp = fopen(outputfile, "a");
  if (tmp == NULL) {
    exit(EXIT_FAILURE);
    } */
  
  fprintf(stderr, "executing sub-make: ");
  int a;
  for(a = 0; a < argc; a ++) {
    fprintf(stderr, "%s ", argv[a]);
  }
  
  fprintf(stderr, "; in directory %s\n", cdir);

  if(fflush(stderr) == EOF) {
    perror("fflush");
    exit(EXIT_FAILURE);
  }

  /*
  if (fclose(tmp) == EOF) {
    perror("fclose");
    exit(EXIT_FAILURE);
    } */
  

  // time make
  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));

  
  if (start == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }
  if (end == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  r1 = clock_gettime(CLOCK_REALTIME, start);

  // run real make
  int mpid = fork();
  if (mpid == 0) {
    int argnum = argc + 4;
    const char** args = malloc(sizeof(char*)*argnum);
    if (args == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }

    args[0] = argv[0];
    args[1] = "--debug=v";
    args[2] = "MAKE=submake";
    args[3] = "SHELL=rusage /bin/bash";
    int j = 4;
    int i;
    for(i = 1; i < argc; i ++) {
      args[j] = argv[i];
      j = j + 1;
    }
    args[j] = 0;
 
    execv("/usr/bin/make", args);
    perror("execv");
    exit(EXIT_FAILURE);
  } else if (mpid == -1) {
    perror("fork");
    exit(EXIT_FAILURE);
  } else {
    int status;
    pid_t pid = wait(&status);

    if (pid == -1) {
      perror("wait");
      exit(EXIT_FAILURE);
    }

        // check exit status of child
    if (WIFSIGNALED(status)) {
      printf("submake: child terminated by signal\n");
      exit(EXIT_FAILURE);
    }


    // todo check status
    r2 = clock_gettime(CLOCK_REALTIME, end);
    
    if (-1 == r1 || -1 == r2) {
      perror("clock_gettime");
      exit(EXIT_FAILURE);
    }
    
    
    struct timespec *tmptt = malloc(sizeof(struct timespec));
    struct timespec *tt = malloc(sizeof(struct timespec));

    
    if (tmptt == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }
    if (tt == NULL) {
      perror("malloc");
      exit(EXIT_FAILURE);
    }


    if (1 == timespec_subtract(tmptt, end, start)) {
      fprintf(stderr, "Negative time\n");
      exit(EXIT_FAILURE);
    }

    timespec_subtract(tt, tmptt, overhead);
    /*
    FILE *tmp = fopen(outputfile, "a");
    if (tmp == NULL) {
      perror("fopen");
      exit(EXIT_FAILURE);
      } */

    fprintf(stderr, "submake-argv=");
    int a;
    for(a = 0; a < argc; a ++) {
      fprintf(stderr, " %s ", argv[a]);
    }
    
    fprintf(stderr, "\nelapsed= %lld.%ld\n finishing sub-make: %s :", (long long)tt->tv_sec, tt->tv_nsec, curscnum);
    for(a = 0; a < argc; a ++) {
      fprintf(stderr, "%s ", argv[a]);
    }

    fprintf(stderr, "; in directory %s\n", cdir);
    /*
    if (fflush(tmp) == EOF) {
      perror("fflush");
      exit(EXIT_FAILURE);
      } */

    free(start);
    free(end);
    free(tmptt);
    free(tt);
    free(overhead);
    
    /*
    if (fclose(tmp) == EOF) {
      exit(EXIT_FAILURE);
      } */
  }
}

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char **argv) {

  if (argc < 2) {
    fprintf(stderr, "Error: not enough arguments to rusage\n");
    exit(EXIT_FAILURE);
  }

  // environment variables we use
  const char* scnum = getenv("SCNUM");
  const char* outputfile = getenv("OUTPUTFILE");
  
  
  if(scnum == NULL) {
    fprintf(stderr, "Error: SCNUM environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  
  if (outputfile == NULL) {
    fprintf(stderr, "Error: OUTPUTFILE environment variable not set\n");
    exit(EXIT_FAILURE);
  }

  FILE *tmp = fopen(scnum, "r");
  if(tmp == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  char* line = NULL;
  size_t len = 0;
  ssize_t read = getline(&line, &len, tmp);
  if (read == -1) {
    perror("getline");
    exit(EXIT_FAILURE);
  }

  if (fclose(tmp) == EOF) {
    perror("fclose");
    exit(EXIT_FAILURE);
  }

  tmp = fopen(scnum, "w");
  if(tmp == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  
  setenv("CURSCNUM", line, 1); // overwrite curretn value

  // turn line into a number.
  int old = atoi(line);
  fprintf(tmp, "%d\n", old + 1);
  fflush(tmp);

  if (fclose(tmp) == EOF) {
    perror("fclose");
    exit(EXIT_FAILURE);
  }

  fflush(stderr);
  fflush(stdout);
  
  FILE *out = fopen(outputfile, "a");
  if (out == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  } 
  
  fprintf(out, "executing shell-command: %d ", old);
  fflush(out);
  int a;
  for(a = 1; a < argc; a ++) {
    fprintf(out, "%s ", argv[a]);
    fflush(out);
  }
  fprintf(out, "\n");

  if (fflush(out) == EOF) {
    perror("fflush");
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
    exit(EXIT_FAILURE);
  }
  
  if (1 == timespec_subtract(overhead, ov2, ov1)) { // ov2 - ov1
    fprintf(stderr, "Negative overhead\n");
    exit(EXIT_FAILURE);
  }

  free(ov1);
  free(ov2);

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
    r2 = clock_gettime(CLOCK_REALTIME, end);
    

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

          fprintf(stderr, "end is: %lld.%ld\n", (long long)end->tv_sec, end->tv_nsec);
      fprintf(stderr, "start is: %lld.%ld\n", (long long)start->tv_sec, start->tv_nsec);


    if (1 == timespec_subtract(tmptt, end, start)) {
      fprintf(stderr, "end is: %lld.%ld\n", (long long)end->tv_sec, end->tv_nsec);
      fprintf(stderr, "start is: %lld.%ld\n", (long long)start->tv_sec, start->tv_nsec);
      fprintf(stderr, "3 Negative time\n");
      exit(EXIT_FAILURE);
    }
    
    timespec_subtract(tt, tmptt, overhead);
    
    tmp = fopen(outputfile, "a");
    if (tmp == NULL) {
      exit(EXIT_FAILURE);
    }
    
    fprintf(tmp, "argv=");
    fflush(tmp);

    for(a = 0; a < argc; a ++) {
      fprintf(tmp, " %s ", argv[a]);
      fflush(tmp);
    }
    
    fprintf(tmp, "\nelapsed= %lld.%ld\n finishing shell-command: %d\n", (long long)tt->tv_sec, tt->tv_nsec, old);
    
    fflush(tmp);
    
    if (fclose(tmp) == EOF) {
      exit(EXIT_FAILURE);
    }

    free(start);
    free(end);
    free(tmptt);
    free(tt);
    free(overhead);
  }
  exit(EXIT_SUCCESS);
}

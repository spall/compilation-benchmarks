#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

int main(int argc, char **argv) {

  if (argc < 2) {
    printf("Error: not enough arguments to rusage\n");
    exit(EXIT_FAILURE);
  }

  // environment variables we use
  const char* scnum = getenv("SCNUM");
  const char* outputfile = getenv("OUTPUTFILE");
  
  
  if(scnum == NULL) {
    printf("Error: SCNUM environment variable not set\n");
    exit(EXIT_FAILURE);
  }
  
  if (outputfile == NULL) {
    printf("Error: OUTPUTFILE environment variable not set\n");
    exit(EXIT_FAILURE);
  }

  FILE *tmp = fopen(scnum, "r");
  if(tmp == NULL) {
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
    exit(EXIT_FAILURE);
  }

  tmp = fopen(scnum, "w");
  if(tmp == NULL) {
    exit(EXIT_FAILURE);
  }
  
  setenv("CURSCNUM", line, 1); // overwrite curretn value

  // turn line into a number.
  int old = atoi(line);
  fprintf(tmp, "%d\n", old + 1);
  
  if (fclose(tmp) == EOF) {
    exit(EXIT_FAILURE);
  }
  
  FILE *out = fopen(outputfile, "a");
  if (out == NULL) {
    exit(EXIT_FAILURE);
  }
  
  fprintf(out, "executing shell-command: %d ", old);
  int a;
  for(a = 1; a < argc; a ++) {
    fprintf(out, "%s ", argv[a]);
  }
  fprintf(out, "\n");

  

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

  struct timespec *start = malloc(sizeof(struct timespec));
  struct timespec *end = malloc(sizeof(struct timespec));
  
  r1 = clock_gettime(CLOCK_REALTIME, start);

  

  int mpid = fork();
  if (mpid == 0) {
    int argnum = (argc - 2) + 1;
    char** args = malloc(sizeof(char*)*argnum);
    args[0] = "-c";
    int j = 1;
    int i;
    for(i = 3; i < argc; i ++) {
      args[j] = malloc(sizeof(char)*(3 + strlen(argv[i])));
      sprintf(args[j], "\"%s\"", argv[i]);
      fprintf(out, "arg is %s\n", args[j]);
      j = j + 1;
    }
    args[j] = 0;

    if (fclose(out) == EOF) {
      exit(EXIT_FAILURE);
    }

    execvp(argv[1], args);

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
    
    tmp = fopen(outputfile, "a");
    if (tmp == NULL) {
      exit(EXIT_FAILURE);
    }
    
    fprintf(tmp, "argv=");
    
    for(a = 0; a < argc; a ++) {
      fprintf(tmp, " %s ", argv[a]);
    }
    
    fprintf(tmp, "\nelapsed= %lld.%ld\n finishing shell-command: %d\n", (long long)tt->tv_sec, tt->tv_nsec, old);
    
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

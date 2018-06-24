#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int estimate_timing_overhead(struct timespec *overhead) {
  struct timespec *ov1 = malloc(sizeof(struct timespec));
  struct timespec *ov2 = malloc(sizeof(struct timespec));
  
  if (ov1 == NULL || ov2 == NULL) {
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
  return EXIT_SUCCESS;
}

int timespec_subtract(struct timespec *result, struct timespec *x, struct timespec *y)
{

  time_t sec_add;
  unsigned long long nsec_add;
  if (x->tv_nsec < y->tv_nsec) {
    sec_add = x->tv_sec - 1; /* steal 1 second */
    nsec_add = 1000000000 + x->tv_nsec;
    
    result->tv_nsec = nsec_add - y->tv_nsec;
    result->tv_sec = sec_add - y->tv_sec;
  } else {
    result->tv_nsec = x->tv_nsec - y->tv_nsec;
    result->tv_sec = x->tv_sec - y->tv_sec;
  }

  if (x->tv_sec == y->tv_sec) {
    return x->tv_nsec < y->tv_nsec;
  }
  
  return x->tv_sec < y->tv_sec;
}

// result = x - y - z
int timespec_subtract_3(struct timespec *result, struct timespec *x, struct timespec *y, struct timespec *z) {

  struct timespec *tmp = malloc(sizeof(struct timespec));
  
  if(tmp == NULL) {
    perror("malloc");
    exit(EXIT_FAILURE);
  }

  if (1 == timespec_subtract(tmp, x, y)) {
    fprintf(stderr, "Negative time: %lld.%ld - %lld.%ld\n", (long long)x->tv_sec, x->tv_nsec, (long long)y->tv_sec, y->tv_nsec);
    exit(EXIT_FAILURE);
  }

  if (1 == timespec_subtract(result, tmp, z)) {
    fprintf(stderr, "Negative time: %lld.%ld - %lld.%ld\n", (long long)tmp->tv_sec, tmp->tv_nsec, (long long)z->tv_sec, z->tv_nsec);
    exit(EXIT_FAILURE);
  }

  free(tmp);

  return EXIT_SUCCESS;
}

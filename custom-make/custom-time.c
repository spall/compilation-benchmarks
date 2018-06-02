#include <time.h>


/* Subtract the ‘struct timespec’ values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0. */
int timespec_subtract (struct timespec *result, struct timespec *x, struct timespec *y) {
  
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_nsec < y->tv_nsec) {
    int nsec = (y->tv_nsec - x->tv_nsec) / 100000000 + 1;
    y->tv_nsec -= 100000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_nsec - y->tv_nsec > 100000000) {
    int nsec = (x->tv_nsec - y->tv_nsec) / 100000000;
    y->tv_nsec += 100000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait.
     tv_nsec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_nsec = x->tv_nsec - y->tv_nsec;

  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

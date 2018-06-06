#include <time.h>


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

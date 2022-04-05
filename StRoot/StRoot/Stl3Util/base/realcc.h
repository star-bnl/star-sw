
// real clock count - low level timing information
static inline unsigned int realcc (void) 
{
#ifdef __ALPHA
#define CYCLES_PER_SEC 466000000 
  unsigned long cc;
  asm volatile("rpcc %0" : "=r"(cc) : : "memory");
  return cc;                  
#elif defined (__I386)
#define CYCLES_PER_SEC 800000000
  unsigned long eax, edx;
  asm volatile("rdtsc":"=a" (eax), "=d" (edx));
  return eax; 

#else 
  return 0;

#endif
}

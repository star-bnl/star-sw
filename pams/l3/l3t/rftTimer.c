#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static clock_t last_time ;
float rftTimer( void )

{
   clock_t now ;
   double  duration;

   now = clock();
   duration = (double)(now - last_time) / CLOCKS_PER_SEC;
   last_time = now ;

   return (float)duration ;
}

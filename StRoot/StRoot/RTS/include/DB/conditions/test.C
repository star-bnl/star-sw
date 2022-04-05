#include <stdio.h>
#include "rtsConditions.h"
#include <stdlib.h>

int main()
{
  rtsConditions *c;

  
  printf("sizeof(rtsConditions) = %d\n",sizeof(rtsConditions));

  c = (rtsConditions *)malloc(sizeof(rtsConditions));
  if(!c) printf("Can't alloc\n");
}

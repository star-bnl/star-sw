#include <stdio.h>
#include "asuAlloc.h"

int main()
{
   char *a,*b,*c,*d;

   printf("\n");asuAllocStats();
   a = (char*)asuAlloc(123);

   printf("sizeof a  = %d\n",sizeof a  );
   printf("sizeof(a) = %d\n",sizeof(a) );
   printf("sizeof *a = %d\n",sizeof *a );
   printf("sizeof(*a)= %d\n",sizeof(*a));

   printf("\n");asuAllocStats();
   b = (char*)asuAlloc(234);
   printf("\n");asuAllocStats();
   a = (char*)asuRealloc(a,444);
   printf("\n");asuAllocStats();
   asuFree(b);
   printf("\n");asuAllocStats();
   c = (char*)asuAlloc(10101);
   printf("\n");asuAllocStats();
   asuFree(c);
   printf("\n");asuAllocStats();
   asuFree(a);
   printf("\n");asuAllocStats();
}


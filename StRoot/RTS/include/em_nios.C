#include <stdio.h>
#include <string.h>

#include "em_nios.h" 

int main(int argc, char *argv[])
{
  // Setup a test device in linux memory
  UINT32 nios_mem[1000];
  for(int i=0;i<1000;i++) nios_mem[i] = i;

  
#ifdef LOCAL_NIOS
  printf("Using LOCAL_NIOS settings\n\n");
#else
  printf("Using REMOTE_NIOS setting\n\n");
#endif

  // replace with nios_device<type> nios((UINT32)nios_mem, 1000)
  // for any type...
  nios_device<UINT32> nios((UINT32)nios_mem, 1000);


  for(int i=5;i<15;i++) {
    printf("nios[%d] = %d\n",i,(int)nios[i]);
  }

  nios[10] = 150;

  printf("after nios[10] = 150;  nios[10] = %d\n",(int)nios[10]);

  printf("nios[5] * nios[6] : %d\n", nios[5] * nios[6]);

  printf("nios[5] = nios[5] * nios[6] : %d\n", nios[5] = nios[5] * nios[6]);

  printf("nios[5] = nios[5] * nios[5] : %d\n", nios[5] = nios[5] * nios[5]);

  printf("\n\nBe careful to cast in any varargs function: (local_nios, fine, remote fails)\n(int)nios[6] = %d           nios[6] = %d\n", (int)nios[6], nios[6]);


  UINT32 other[1000];
  memset(other, 0, sizeof(other));

  nios.copyto(7*4, other, 12);


  printf("\n\n1.\n\n");
  for(int i=5;i<15;i++) {
    printf("nios[%d] = %d\n",i,(int)nios[i]);
  }
  

  memset(other, 0xff, sizeof(other));
  nios.copyto(&nios[8], other, 4);

  printf("\n\n2.\n\n");
  for(int i=5;i<15;i++) {
    printf("nios[%d] = %d\n",i,(int)nios[i]);
  }

  nios.copyfrom(other, &nios[0], 20*4);

  printf("\n\n3.\n\n");
  for(int i=0;i<20;i++) {
    printf("other[%d] = %d\n",i,other[i]);
  }
  
}

#include <stdio.h>
#include <stdlib.h>

int makearray_(int *base, int* srowpp, int *nrowpp) {

  int i,*a,*user,Size,size,nrow,srow;

  srow = *srowpp; nrow = *nrowpp;
  if (srow<=0) srow = 1;
  if (nrow<=0) nrow = 1;
  if (nrow==1) { i = srow ; srow = nrow; nrow = i;} 

  size = srow * nrow; Size = size + srow;
  a = (int*)malloc((Size+10)*sizeof(int));
  if (!a) return 0;

  memcpy(base+1,&a,sizeof(int*)); 		/* Save pointer	*/
  base[0] = (((a+4) - base) + srow -1)/srow;	/* Save shift   */
  user = base + base[0]*srow;
  a[0] = size;					/* save size    */
   memcpy(a+1,&user,sizeof(int*)); 		/* Save upointer*/
   a[3] = 0xABCD1998;				/* Set start magic	*/
  user[-1]=0xABCD1998; user[size] = 0xDCBA1998; /* Set end   magic	*/
  return base[0];
}
int killarray_( int *base ){
  int *a,*u,size;
  memcpy(&a,base+1,sizeof(int*));		/* Get pointer	*/
  if ( base[0]==0xDEAD1998 ) { 
    printf(" KillArray: ***Error***,Array is already deleted\n");
    return 1;}
  if ( !a ) { 
    printf(" KillArray: ***Error*** 2nd word of Array iz corrupted\n");
    return 1;}
  base[0]=0xDEAD1998;

  if (a[3] != 0xABCD1998) { 
    printf("KillArray: ***Error*** Start of Array iz corrupted, CAN NOT delete\n");
    return 2;}
  memcpy(&u,a+1,sizeof(int*));

  if (u[-1] != 0xABCD1998) { 
    printf("KillArray: ***Error*** Start of Array iz corrupted, CAN NOT delete\n");
    return 2;}

  a[1] = 0; size = a[0];
  if (u[size] != 0xDCBA1998) { 
    printf("KillArray: ***Error*** End of Array iz corrupted, CAN NOT delete\n");
    return 3;}
  u[size]=0; free(a); 
  return 0;
}

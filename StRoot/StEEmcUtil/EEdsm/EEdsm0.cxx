/**************************************************************
 * $Id: EEdsm0.cxx,v 1.1 2004/11/29 20:12:59 balewski Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>


#include "EEdsm0.h"


//--------------------------------------------------
//
//--------------------------------------------------
EEdsm0 ::  EEdsm0() {
  clear();
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm0 ::  ~EEdsm0() { }

//--------------------------------------------------
//--------------------------------------------------
void EEdsm0 :: clear() {
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsm0 :: setBite(int b, uchar val){
  assert(b>=0 && b<nw);
  data[b]=val;
}

//--------------------------------------------------
//--------------------------------------------------
uint EEdsm0 :: getChan(int ch) {
  //printf("ch=%d\n",ch);
  assert(ch>=0 && ch<nc);

  uint val=99999;
  switch( ch%2 ) {
  case 0: // even: take 4 lower bytes form higher bite + lower bite   
    {// hi daddy! how are you? no! it's not red! uh!?
      int k=ch/2*3; // position of low bite
      val = ((data[k+1] & 0xf) <<8) + data[k];
      // printf("even k=%d d1=x%2.2x d0=x%2.2x val=x%4.4x\n",k,data[k+1],data[k],val);
    }  break;
  case 1: // odd: take higher bite + higher 4 bytes from lower bite   
    {
      int k=(ch-1)/2*3+1; // position of low bite
      val =( data[k+1] <<4) + (data[k]>>4);
      // printf("odd  k=%d d1=x%2.2x d0=x%2.2x val=x%4.4x\n",k,data[k+1],data[k],val);
    }  break;
  }

  return val;

}


//--------------------------------------------------
//--------------------------------------------------
void EEdsm0 :: print( int k) {
  printf("EEdsm0:: print()  \n");

  int i;
  printf("byte = ");
  for(i=0;i<nw;i++) printf(" %2d ",nw-i-1); 
  printf("\nraw  = ");
  for(i=0;i<nw;i++) printf("x%2.2x ",data[nw-i-1]); 
  //  printf("\n");

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("   %3d ",i); 
  printf("\nTP+HT  = ");
   for(i=nc-1;i>=0;i--) printf(" x%3.3x  ", getChan(i) );
 printf("\n[dec]  = ");
 for(i=nc-1;i>=0;i--) printf("%2d+%2d  ", getTP(i), getHT(i) );

  printf("\n");
  int k2=maxHT();
  printf(" maxHT[ch=%d]=%2d\n",k2,getHT(k2));

  int k1=maxTP();
  printf(" maxTP[ch=%d]=%2d\n",k1,getTP(k1));


}
 


//--------------------------------------------------
//--------------------------------------------------
int EEdsm0 :: maxHT() {
  int i;
  int k=0;
  uint max=getHT(k); 
  for(i=1;i<nc;i++) {
    uint val=getHT(i); 
    if(val<=max) continue;
    max=val;
    k=i;
  }
  //  printf(" maxHT[ch=%d]=%2d\n",k,max);
  return k;
}


//--------------------------------------------------
//--------------------------------------------------
int EEdsm0 :: maxTP() {
  int i;
  int k=0;
  uint max=getTP(k); 
  for(i=1;i<nc;i++) {
    uint val=getTP(i);
    if(val<=max) continue;
    max=val;
    k=i;
  }
  // printf(" maxTP[ch=%d]=%2d\n",k,max);
  return k;
}



/*
 * $Log: EEdsm0.cxx,v $
 * Revision 1.1  2004/11/29 20:12:59  balewski
 * first
 *
 * Revision 1.1  2004/02/17 03:09:17  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2003/12/29 02:18:38  balewski
 * star
 *
 * Revision 1.1  2003/05/22 19:39:00  balewski
 */


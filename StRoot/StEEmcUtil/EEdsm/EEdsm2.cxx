#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "EEdsm2.h"

//--------------------------------------------------
//
//--------------------------------------------------
EEdsm2 ::  EEdsm2() {
  clear();
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm2 ::  ~EEdsm2() { }

//--------------------------------------------------
//--------------------------------------------------
void EEdsm2 :: clear() {
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsm2::setWord(int ch, ushort val){
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort EEdsm2::getJPthr(int jp) { 
  assert(jp>=0 && jp<6);
  int ch= jp/3;
  int i=jp%3;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch]>>8;
  val=val>>(2*i);
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort EEdsm2::get3JPHTthr(int i3p) { 
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch]>>14;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort EEdsm2::get3JPsum(int i3p) { 

  int ch= i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch]&0xff;
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsm2 :: print( int k) {
  printf("EEdsm2:: print()  \n");
  int i;

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nword  = ");
  for(i=nc-1;i>=0;i--) printf(" x%4.4x ", data[i] );

  printf("\nJP_Falk =");
  for(i=njp-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nJP_Steve=");
  for(i=njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);

  printf("\nJPthr   =");
  for(i=njp-1;i>=0;i--) printf("  %4d ", getJPthr(i));
  printf("\n");

  for(i=0;i<2;i++){
    printf("3x.9 JP_Falk(%d+%d+%d) energy/dec:  8bit=%d  HTthr=%d\n",3*i,3*i+1,3*i+2,get3JPsum(i), get3JPHTthr(i));
  }
}
 
// $Log: EEdsm2.cxx,v $
// Revision 1.2  2005/02/01 22:13:40  perev
// Compatibility to redhat
//
// Revision 1.1  2004/11/29 20:12:59  balewski
// first
//
// Revision 1.1  2004/02/17 03:09:17  balewski
// *** empty log message ***
//


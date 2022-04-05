#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
>Jan:
>in 2005 pairs of bits 8+9, 10+11, 12+13 carried
>3 JP thresholds results. This is gone for 2006+, right?
>So I need to disable this part of the code for years>=2006.

Steve:
Yes, these bits formerly used for adjacent jet patch use are now gone, so we do not have output bits that handle JP bits for individual jet patches.

>Also bits 0-7 used to carry Etot - now are gone, right?
>
>
Etot out of each layer 1 DSM is now reduced to 5 bits, 0-4, in order to allow Eleanor to form the sum of 8 layer 1 DSM's in the layer 2 DSM.  Bits 5-6 are now empty, and bit 7 has been reassigned to register-selected HT x TP threshold.

*/

#include "EEdsm2.h"

//--------------------------------------------------
//
//--------------------------------------------------
EEdsm2::EEdsm2(){
  mYear=999; // to force initialization
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm2::~EEdsm2() { }

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm2::clear() {
  assert(mYear>2000);// to assure it was initialized
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm2::setWord(int ch, ushort val){
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort  
EEdsm2::getJPthr(int jp) const { //used for adjacent jet patch
  assert(jp>=0 && jp<6);
  assert(mYear<2006);
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
ushort 
EEdsm2::get3JPHTthr(int i3p) const { // ??2006
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch]>>14;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm2::getHTTPthr(int i3p) const { //selected HT x TP threshold passed?Y/N
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >>7;
  return val&1;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm2::getTPthr(int i3p) const { //selected TP threshold passed?Y/N
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >>9;
  return val&1;
}

//--------------------------------------------------
//--------------------------------------------------
ushort 
EEdsm2::get3JPsum(int i3p) const {// Etot out of each layer 1 DSM 

  int ch= i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch]&0xff;
  if(mYear>=2006) val=val &0x1f; //is now reduced to 5 bits, 0-4,
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm2::print( int k) const {
  printf("EEdsm2:: INPUTS,  year=%d  \n",mYear); 
  int i;

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nword  = ");
  for(i=nc-1;i>=0;i--) printf(" x%4.4x ", data[i] );
  if(mYear>=2006) {
    printf("\nTPthr = ");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getTPthr(i) );
    printf("\nHTTPthr = ");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getHTTPthr(i) );
  }
  printf("\nJP_Falk =");
  for(i=njp-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nJP_Steve=");
  for(i=njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);

  if(mYear<2006) {
    printf("\nJPthr   =");
    for(i=njp-1;i>=0;i--) printf("  %4d ", getJPthr(i));
  } 
  printf("\n");
 
  const char *txt="5bit";
  if(mYear<2006)txt="8bit";

  for(i=0;i<2;i++){
    printf("3x.9 JP_Falk(%d+%d+%d) energy/dec:  %s=%d  HTthr=%d\n",3*i,3*i+1,3*i+2,txt,get3JPsum(i), get3JPHTthr(i));
  }
}
 
// $Log: EEdsm2.cxx,v $
// Revision 1.2  2010/04/18 06:05:25  pibero
// Address compiler warnings.
//
// Revision 1.1  2009/10/12 18:04:26  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.4  2009/02/24 03:56:19  ogrebeny
// Corrected const-ness
//
// Revision 1.3  2006/04/05 18:34:10  balewski
// new DSM bit assignment in 2006,
// possibly lost backward compatibility
// use tagged 2005 version if needed
//
// Revision 1.2  2005/02/01 22:13:40  perev
// Compatibility to redhat
//
// Revision 1.1  2004/11/29 20:12:59  balewski
// first
//
// Revision 1.1  2004/02/17 03:09:17  balewski
// *** empty log message ***
//


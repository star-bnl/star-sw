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

#include "BEdsm2.h"

//--------------------------------------------------
//
//--------------------------------------------------
BEdsm2::BEdsm2(){
  mYear=999; // to force initialization
}

//--------------------------------------------------
//--------------------------------------------------
BEdsm2::~BEdsm2() { }

//--------------------------------------------------
//--------------------------------------------------
void 
BEdsm2::clear() {
  memset(data,0,sizeof(data));
}

//--------------------------------------------------
//--------------------------------------------------
void 
BEdsm2::setWord(int ch, ushort val){
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}


//--------------------------------------------------
//--------------------------------------------------
int 
BEdsm2::getInpHT2bit(int i3p) const { // year 2006
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch] >> 12;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
int 
BEdsm2::getInpHT2bit_2(int i3p) const { // year 2006
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch] >> 14;
  val=val & 3;
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
int 
BEdsm2::getInpJP2bit(int i3p) const { // year 2006
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch] >> 10;
  val=val & 3;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
int 
BEdsm2::getInpHTTP1bit(int i3p) const { //selected HT x TP threshold passed?Y/N
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >> 7;
  val=val & 1;
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
int
BEdsm2::getInpTP1bit(int i3p) const { //selected TP threshold passed?Y/N
  int ch=i3p;
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >> 9;
  val=val & 1;
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
ushort  
BEdsm2::getJPthr(int jp) const { //used for adjacent jet patch
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
int
BEdsm2::getInpEsum5bit(int i3p) const {// Etot out of each layer 1 DSM 

  int ch= i3p;
  assert(ch>=0 && ch<nc);
  ushort val=data[ch] & 0xff;
  if(mYear>=2006) val=val & 0x1f; //is now reduced to 5 bits, 0-4,
  return val;
}


//--------------------------------------------------
//--------------------------------------------------
void 
BEdsm2::print( int k) const {
  printf("BEdsm2:: INPUTS,  year=%d  \n",mYear); 
  int i;

  printf("ch    = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nword  = ");
  for(i=nc-1;i>=0;i--) printf(" x%4.4x ", data[i] );
  if(mYear>=2006) {
    printf("\ninpTP 1bit = ");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getInpTP1bit(i) );
    printf("\nHTTPthr = ");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getInpHTTP1bit(i) );
  }

#if 0
  printf("\nJP_Falk =");
  for(i=njp-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nJP_Steve=");
  for(i=njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);
#endif

  if(mYear<2006) {
    printf("\nJPthr   =");
    for(i=njp-1;i>=0;i--) printf("  %4d ", getJPthr(i));
  } 
  printf("\n");
 
  const char *txt="5bit";
  if(mYear<2006)txt="8bit";

  for(i=0;i<2;i++){
    printf("3x.9 JP_Falk(%d+%d+%d) energy/dec:  %s=%d  HT2bit=%d (only for EEMC)\n",3*i,3*i+1,3*i+2,txt,getInpEsum5bit(i), getInpHT2bit(i));
  }
  printf("\n");

}
 
// $Log: BEdsm2.cxx,v $
// Revision 1.2  2010/04/18 06:05:25  pibero
// Address compiler warnings.
//
// Revision 1.1  2009/10/12 18:04:24  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.2  2009/02/24 03:56:18  ogrebeny
// Corrected const-ness
//
// Revision 1.1  2007/08/17 01:15:34  balewski
// full blown Endcap trigger simu, by Xin
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


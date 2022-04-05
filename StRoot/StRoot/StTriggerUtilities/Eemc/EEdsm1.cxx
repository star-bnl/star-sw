#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include "EEdsm1.h"

//--------------------------------------------------
//
//--------------------------------------------------
EEdsm1 ::  EEdsm1() {
  type=0;
  clear();
  mYear=2005;
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm1::~EEdsm1() { }

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1::clear() {
  memset(data,0,sizeof(data));
  memset(intJP11bit,0,sizeof(intJP11bit));
  intJPsum13bit=0;
  outJPsum5bit=0;
  outHTTP1bit=0;
  outTP1bit=0;
  outJP2bit=0;
  outHT2bit=0;
}
//--------------------------------------------------
//--------------------------------------------------
void EEdsm1::setYear(int y, int *JPth, int TPthrSelc, int HTTPthrSelc){
  mYear=y;
  int i;
  for( i=0; i<mxTh; i++){
    JPthr[i]=JPth[i];
  }
  TPthrSelect=TPthrSelc;
  HTTPthrSelect=HTTPthrSelc;
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsm1::setWord(int ch, ushort val){
  
  assert(ch>=0 && ch<nc);
  data[ch]=val;
}


//--------------------------------------------------
//--------------------------------------------------
int 
EEdsm1::getInp16bit(int ch) const { 
  assert(ch>=0 && ch<nc);
  ushort val=data[ch];
  return val;
}

//--------------------------------------------------
//--------------------------------------------------
int 
EEdsm1::getInpHT2bit(int ch) const { 
  assert(ch>=0 && ch<nc);
  ushort val=(data[ch]& 0xfff) >>10;
  return val;
}

/* 2006 change in tier1 file:
a) selected TP threshold passed?:  extract bits 12-13; .
b) HTxTP bits for highest threshold passed:  repeat (a), but now for bits 14-15 
*/

//--------------------------------------------------
//--------------------------------------------------
int 
EEdsm1::getInpTP2bit(int ch) const { 
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >>12;
  return val&3;
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1::getInpHTTP2bit(int ch) const { 
  assert(ch>=0 && ch<nc);
  assert(mYear>=2006);
  ushort val=data[ch] >>14;
  return val&3;
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1::getInpTPsum(int ch) const { 
  assert(ch>=0 && ch<nc);

  /* The first board
      has patches  .6 .3  .3 .6  .6 .3
      Steve's JP#  \_3_/  \_4_/  \_5_/

     The second board
     has patches  .3 .6  .6 .3  .3 .6
     Steve's JP#  \_6_/  \_1_/  \_2_/
  */

  ushort val=0xfff;

  if (type == 1) {
    switch (ch) {
    case 0: // use last 10 bits for 0.6x1 patches
    case 3:
    case 4:
      val=data[ch]& 0x3ff;
      break;
    case 1: // use last 9 bits for 0.3x1 patches
    case 2:
    case 5:
      val=data[ch]& 0x1ff;
      break;
    }
  }
  else if (type == 2) {
    switch (ch) {
    case 1: // use last 10 bits for 0.6x1 patches
    case 2:
    case 5:
      val=data[ch]& 0x3ff;
      break;
    case 0: // use last 9 bits for 0.3x1 patches
    case 3:
    case 4:
      val=data[ch]& 0x1ff;
      break;
    }
  }
  else assert (1==2);

  return val;

}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1::compute() {
  /* At layer 1 DSM's, the "HTTP threshold select register"
     is used to determine which of the three thresholds of
     HTTP output from layer 0 to pass on.  We did not
     change this register value throughout run 6, but I
     thought we set the value = 1, meaning we used
     HTth1 combined with TPth1.  I assume that would
     mean that if any HTTP output from layer 0 DSM's
     is set to 10 or 11, then the HTTP output bit from
     layer 1 will be set to 1. 
  */
  assert(mYear>=2006);
  int combHT2bit=0, combTP2bit=0, combHTTP2bit=0;
  int i;
  for ( i=0; i<nc; i++ ){
    int iJP=i/2;
    intJP11bit[iJP]+=getInpTPsum(i);
    intJPsum13bit +=getInpTPsum(i);
    if ( combHT2bit <= getInpHT2bit(i) ) combHT2bit = getInpHT2bit(i);
    if ( combTP2bit <= getInpTP2bit(i) ) combTP2bit = getInpTP2bit(i);
    if ( combHTTP2bit <= getInpHTTP2bit(i) ) combHTTP2bit = getInpHTTP2bit(i);
  }
  
  outHT2bit = combHT2bit;
  
  //printf("!!!TPthrSelect=%d\n",TPthrSelect);
  //printf("!!!HTTPthrSelect=%d\n",HTTPthrSelect);

  if ( combTP2bit >= TPthrSelect )  outTP1bit=1;
  else  outTP1bit=0;
  
  if ( combHTTP2bit >= HTTPthrSelect )  outHTTP1bit=1;
  else  outHTTP1bit=0;
  
  int mxJP=0;
  int j;
  for ( j=0; j<nJP; j++ ){
    if ( mxJP <= intJP11bit[j] ) mxJP = intJP11bit[j];
  }
  
  if ( mxJP <= JPthr[0]) outJP2bit=0;
  if ( mxJP >  JPthr[0] && mxJP <= JPthr[1] ) outJP2bit=1;
  if ( mxJP >  JPthr[1] && mxJP <= JPthr[2] ) outJP2bit=2;
  if ( mxJP >  JPthr[2] ) outJP2bit=3;

  int flag;
  flag =( intJPsum13bit >>7 ) & 0x3f;
  if ( flag > 0 ) outJPsum5bit = 31;
  else outJPsum5bit = ( intJPsum13bit >>2 ) & 0x1f;

  out16bit= outJPsum5bit + ( outHTTP1bit << 7 ) + ( outTP1bit << 9 ) + ( outJP2bit << 10 ) + ( outHT2bit << 12 );
  //printf("done processing jet patch sums \n");


}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1::print( int k ) const {
  printf("EEdsm1:: print() INPUTS,  year=%d  \n",mYear);
  int i;

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ",i); 
  printf("\nword  = ");
  for(i=nc-1;i>=0;i--) printf(" x%4.4x ", data[i] );

  printf("\nTPsum = ");
  for(i=nc-1;i>=0;i--) printf("  %4d ", getInpTPsum(i));

  printf("\nJPsum = ");
  for(i=2;i>=0;i--) printf("     %8d ", intJP11bit[i]);
  
  printf("\nHT2bit= ");
  for(i=nc-1;i>=0;i--) printf("  %4d ", getInpHT2bit(i));
  
  if(mYear>=2006) {
    printf("\nTP2bit= ");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getInpTP2bit(i));
    
    printf("\nHTTP2bit=");
    for(i=nc-1;i>=0;i--) printf("  %4d ", getInpHTTP2bit(i));
  }

  printf("\n output:  HT2bit=%d, JP2bit=%d, TP1bit=%d, HTTP1bit=%d, JPsum5bit=%d\n", outHT2bit ,outJP2bit, outTP1bit ,outHTTP1bit,outJPsum5bit);

}
 
// $Log: EEdsm1.cxx,v $
// Revision 1.1  2009/10/12 18:04:25  pibero
// Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
//
// Revision 1.5  2009/02/24 03:56:18  ogrebeny
// Corrected const-ness
//
// Revision 1.4  2007/08/17 01:15:36  balewski
// full blown Endcap trigger simu, by Xin
//
// Revision 1.3  2006/04/05 18:34:10  balewski
// new DSM bit assignment in 2006,
// possibly lost backward compatibility
// use tagged 2005 version if needed
//
// Revision 1.2  2005/02/01 22:13:39  perev
// Compatibility to redhat
//
// Revision 1.1  2004/11/29 20:12:59  balewski
// first
//
// Revision 1.2  2004/04/23 20:16:56  balewski
// trig patch fix
//
// Revision 1.1  2004/02/17 03:09:17  balewski
// *** empty log message ***
//


/**************************************************************
 * $Id: EEdsm0.cxx,v 1.1 2009/10/12 18:04:25 pibero Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>

#include "EEdsm0.h"

//--------------------------------------------------
//--------------------------------------------------
EEdsm0::EEdsm0() {
  clear();
  type=1; // 0.6 JP type is the default
  mYear=-1; 
  memset( HTthr,0,sizeof(HTthr)); // year 2006+
  memset( TPthr,0,sizeof(TPthr));
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm0::~EEdsm0() { }

//--------------------------------------------------
//--------------------------------------------------
void
EEdsm0::setYear(int y, int*HTth, int*TPth ) {
 mYear=y;
 assert(type==1 || type==2);
 int i;
 for(i=0;i<mxTh;i++) {
   HTthr[i]= HTth[i];
   TPthr[i]= TPth[i];
 }
}
//--------------------------------------------------
//--------------------------------------------------
void
EEdsm0::clear() {
  memset(data,0,sizeof(data));
  memset(value,0,sizeof(value));
  memset(outHT2bit,0,sizeof(outHT2bit));
  memset(outTP2bit,0,sizeof(outTP2bit));
  memset(outHTTP2bit,0,sizeof(outHTTP2bit));
  memset(outTPsum,0,sizeof(outTPsum));
  memset(out16bit,0,sizeof(out16bit));
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0::setBite(int b, uchar val){
  assert(b>=0 && b<nw);
  data[b]=val;
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0::setInp12bit(int ch, short val){
  assert(ch>=0 && ch<nc);
  value[ch]=val;
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm0::getInp12bit(int ch) const {
  assert(ch>=0 && ch<nc);
  return  value[ch];
}


//--------------------------------------------------
//--------------------------------------------------
void  
EEdsm0::unpack() {
  int ch;
  for(ch=0;ch<nc;ch++) {

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

  value[ch]= val;
  }
}
 

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm0::print( int k) const {
  printf("EEdsm0 type=%d  year=%d \n",type,mYear);

  int i;
  printf("thresholds  HT=");  for(i=0;i<mxTh;i++) printf(" %2d ", HTthr[i]);
  printf("  TP=");  for(i=0;i<mxTh;i++) printf(" %2d ", TPthr[i]);printf("\n");

  printf("byte = ");
  for(i=0;i<nw;i++) printf(" %2d ",nw-i-1); 
  printf("\nraw  = ");
  for(i=0;i<nw;i++) printf("x%2.2x ",data[nw-i-1]); 
  //  printf("\n");

  printf("\nch    = ");
  for(i=nc-1;i>=0;i--) printf("   %3d ",i); 
  printf("\nTP+HT  = ");
   for(i=nc-1;i>=0;i--) printf(" x%3.3x  ", getInp12bit(i) );
 printf("\n[dec]  = ");                   
 for(i=nc-1;i>=0;i--) printf("%2d+%2d  ", getInpTP6bit(i), getInpHT6bit(i) );

 printf("\n output(s) TPsum=%d  HT2b=%d  HTTP2b=%d",outTPsum[0],outHT2bit[0],outHTTP2bit[0]);
 if(type==1) printf("\n");
 else  printf("   AND  TPsum=%d  HT2b=%d  HTTP2b=%d\n",outTPsum[1],outHT2bit[1],outHTTP2bit[1]);

}
 


//--------------------------------------------------
//--------------------------------------------------
void
EEdsm0::compute() {
  /*Hi Jan,
    
  In layer 0 DSM's, the HTTP bits are set as follows:
  
  1) for each input, check whether HT > HTth0 .AND.
  TPsum > TPth0.  Perform similar checks for th1 and
  th2.
  
  2) If any of the 10 input trigger patches pass th2 in
  test (1), then set HTTP output bits = 11;
  if none of the 10 input trigger patches pass th2, but
  at least one passes th1, set HTTP output bits = 10;
  if none of the 10 input trigger patches pass th2 or
  th1, but at least one passes th0, set HTTP output = 01;
  if none of the 10 input trigger patches pass th0, set
  HTTP output = 00.
  
  At layer 1 DSM's, the "HTTP threshold select register"
  is used to determine which of the three thresholds of
  HTTP output from layer 0 to pass on.  We did not
  change this register value throughout run 6, but I
  thought we set the value = 1, meaning we used
  HTth1 combined with TPth1.  I assume that would
  mean that if any HTTP output from layer 0 DSM's
  is set to 10 or 11, then the HTTP output bit from
  layer 1 will be set to 1.
  
  The only other register values you will need (eventually)
  to use are ones for layer 2 DSM's to indicate whether
  both E and W barrel, and whether both barrel and endcap,
  contribute to E_tot.

  
  In her written documentation, Eleanor doesn't actually specify the
  values of the HTTP register.  It is possible that value = 2 selects
  threshold 1 -- i.e., that possible register values are 1,2,3 corresponding
  to thresholds 0,1,2. 

  Steve 
  */
  
  assert(mYear=2006);

  int iOu=0;
  int i;
  for(i=0;i<nc;i++) {
    if(type==2 && i>=5) iOu=1;
    outTPsum[iOu]+=getInpTP6bit(i);
    int k;
    for(k=0;k<mxTh; k++) {
      int lev=k+1;
      if( getInpHT6bit(i)>HTthr[k] && outHT2bit[iOu]<lev) outHT2bit[iOu]=lev; 
      if( getInpTP6bit(i)>TPthr[k] && outTP2bit[iOu]<lev) outTP2bit[iOu]=lev; 
      if( getInpHT6bit(i)>HTthr[k] && getInpTP6bit(i)>TPthr[k] && outHTTP2bit[iOu]<lev) outHTTP2bit[iOu]=lev; 
    }

    out16bit[iOu] = outTPsum[iOu] + ( outHT2bit[iOu] << 10 ) + ( outTP2bit[iOu] << 12 ) + ( outHTTP2bit[iOu] << 14 );
   
    //  printf("i=%d iOu=%d  HT=%d TP=%d  TPsum=%d HT2b=%d HTTP2b=%d\n",i,iOu, getHT(i), getTP(i),outTPsum[iOu],outHT2bit[iOu],outHTTP2bit[iOu]);
  }
  
}


#if 0
probably wrong, do not account for type 1 & 2
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
#endif


/*
 * $Log: EEdsm0.cxx,v $
 * Revision 1.1  2009/10/12 18:04:25  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.3  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.2  2007/08/17 01:15:35  balewski
 * full blown Endcap trigger simu, by Xin
 *
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


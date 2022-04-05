/**************************************************************
 * $Id: EEdsm1Tree.cxx,v 1.3 2009/11/18 15:50:59 pibero Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>
 

#include "EEdsm1Tree.h"
#include "EEdsm1.h"


//--------------------------------------------------
//--------------------------------------------------
EEdsm1Tree::EEdsm1Tree(const char *nameX) {
  mYear=-999;
  int thr[3]={1000, 2000, 3000};
  int TPthr=100, HTTPthr=100;
  ee1=new EEdsm1[Nee1];
  int i=0;
  for(i=0; i<Nee1; i++) ee1[i].setYear(-999, thr, TPthr, HTTPthr); // must be defined before use
  ee1[0].setType(1); //see below  
  ee1[1].setType(2); //see below

  /* The first board
      has patches  .6 .3  .3 .6  .6 .3
      Steve's JP#  \_3_/  \_4_/  \_5_/

     The second board
     has patches  .3 .6  .6 .3  .3 .6
     Steve's JP#  \_6_/  \_1_/  \_2_/
  */

  clear();
  strncpy(name,nameX,mxTxt);
}

//--------------------------------------------------
//--------------------------------------------------
EEdsm1Tree::~EEdsm1Tree() {
  //fix it: delete 
 }

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1Tree::setYear(int x, int *JPth, int TPthrSelc, int HTTPthrSelc) {
  mYear=x;
  int i;
  for (i=0;i<Nee1;i++) ee1[i].setYear(mYear, JPth, TPthrSelc, HTTPthrSelc);
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsm1Tree :: clear() {
  int i;

  for (i=0;i<Nee1;i++) ee1[i].clear();  
#if 0
  memset(ee1out3JPadc,0,sizeof(ee1out3JPadc));
  memset(ee1outJPadc,0,sizeof(ee1outJPadc));
  memset(ee1outHT,0,sizeof(ee1outHT));
  memset(ee1outTPthrMax, 0, sizeof(ee1outTPthrMax));
  memset(ee1outHTTPthrMax, 0, sizeof(ee1outHTTPthrMax));
#endif
  // memset(,0,sizeof());
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1Tree::setInp16bit(int brd, int ch, ushort val){
  assert(brd>0 && brd<=Nee1);
  ee1[brd-1].setWord(ch,val);
  //  printf("addX %d %d %d\n",brd,ch,val);
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1Tree::compute() {
  int i;
  for (i=0;i<Nee1;i++) {
    ee1[i].compute();

  }
/*Note jet patches are numbered
                                            in DSM input order.  So ijp=0
                                            is FEE crate 3.  */

#if 0
  //Add some further processing to check jet patch sums for passing thresholds
  //and form adjacent patch sums.
  for (i=0;i<EEnJetPatch;i++) {
    AdjJPsum[i]=ee1outJPadc[i]+ee1outJPadc[(i+1)%EEnJetPatch]; //sum adj patches
    //printf("patch=%d  adc1=%d adc2=%d sum=%d  \n",i,ee1outJPadc[i],ee1outJPadc[(i+1)%EEnJetPatch],AdjJPsum[i]);
  }
#endif
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree:: getInpTPsum(int ch) const {
  // ch=halfPatches 0...11, for both DSM1 boards
  return ee1[ch/6].getInpTPsum(ch%6);
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getInpHT2bit(int ch) const {
  // ch=halfPatches 0...11, for both DSM1 boards
 return  ee1[ch/6].getInpHT2bit(ch%6);
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getInpTP2bit(int ch) const {
  // ch=halfPatches 0...11, for both DSM1 boards
 return  ee1[ch/6].getInpTP2bit(ch%6);
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getInpHTTP2bit(int ch) const {
  // ch=halfPatches 0...11, for both DSM1 boards
 return  ee1[ch/6].getInpHTTP2bit(ch%6);
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getInp16bit(int ch) const {
  // ch=halfPatches 0...11, for both DSM1 boards
 return  ee1[ch/6].getInp16bit(ch%6);
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOutHT2bit(int ibr) const { 
  return  ee1[ibr].getOutHT2bit();
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOutJP2bit(int ibr) const { 
  return  ee1[ibr].getOutJP2bit();
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOutTP1bit(int ibr) const { 
 return  ee1[ibr].getOutTP1bit();
}


//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOutHTTP1bit(int ibr) const { 
 return  ee1[ibr].getOutHTTP1bit();
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOutEsum5bit(int ibr) const { 
 return  ee1[ibr].getOutEsum5bit();
}

//--------------------------------------------------
//--------------------------------------------------
int
EEdsm1Tree::getOut16bit(int ibr) const { 
 return  ee1[ibr].getOut16bit();
}


//--------------------------------------------------
//--------------------------------------------------
void 
EEdsm1Tree::print( int k) const {
  printf("EEdsm1Tree(%s) , year=%d  \n",name,mYear);
  
  printf("\n\n----------- level-1 -----------------\n ");
  int i;
  for(i=0;i<Nee1;i++) {
    printf("\n----------- level-1 Board %2d , ",i+1);
    ee1[i].print();
    //    printf("emul out 3x.9 JP_Falk(%d+%d+%d) energy/dec: 13bit=%d  8bit=%d  mxTPthr=%d mxHTTPthr=%d\n",3*i,3*i+1,3*i+2,ee1out3JPadc[i],ee1out3JPadc[i]>>5, ee1outTPthrMax[i], ee1outHTTPthrMax[i]);
  }
#if 0
  printf("\n----------- level-1 emulated output \n JP_Falk =");
  int Njp=EEnJetPatch;
  for(i=Njp-1;i>=0; i--) printf("  %4d ",i);
  printf("\n JP_Steve=");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);
  printf("\n JPsum   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outJPadc[i]);
  printf("\n HTthr   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outHT[i]);
  if(mYear<2006) {
    printf("\n AdjJPsum=");
    for(i=Njp-1;i>=0; i--) printf("  %4d ",AdjJPsum[i]);
  }
  printf("\n");
  printf("emul: orTPbit=%d  orHTTPbit=%d \n",ee1outTPthrMax[0]||ee1outTPthrMax[1], ee1outHTTPthrMax[0] ||ee1outHTTPthrMax[1]);
 
#endif 
}




/*
 * $Log: EEdsm1Tree.cxx,v $
 * Revision 1.3  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.2  2009/10/13 17:00:49  pibero
 * Changed location of #include file
 *
 * Revision 1.1  2009/10/12 18:04:26  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.2  2009/02/24 03:56:18  ogrebeny
 * Corrected const-ness
 *
 * Revision 1.1  2007/08/17 01:15:36  balewski
 * full blown Endcap trigger simu, by Xin
 *
 */


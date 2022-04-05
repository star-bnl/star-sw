/**************************************************************
 * $Id: EEfeeTPTree.cxx,v 1.6 2020/01/13 20:45:50 zchang Exp $
 **************************************************************/

#include <iostream>
#include <assert.h>


#include "EEfeeTPTree.h"
#include "EEfeeTP.h"

#define EEmapFEE_USE // trick instattiates data only in the cxx
#include "EEmapTP.h"
#undef EEmapFEE_USE


//--------------------------------------------------
//
//--------------------------------------------------
EEfeeTPTree::EEfeeTPTree(const char *nameX, int nc) {

  memset(feeTP,0,sizeof(feeTP)); // clear all pointers
  memset(TPmap,0,sizeof(TPmap));

  //.... map all TP to Hank's channels (input to DSM 0)

  int i;
  for(i=0;i<90;i++)  {
    EEfeeTPmap *x=eeTPmap+i;
    feeTP[i]=new EEfeeTP( x->JP,x->name,x->lenCh,x->cha0L,x->cha0H);
  }


  mxChan=nc;
  clear();
  strncpy(name,nameX,mxTxt);
}

//--------------------------------------------------
//--------------------------------------------------
EEfeeTPTree::~EEfeeTPTree() {
  for (int i = 0; i < 90; ++i) {
    delete feeTP[i];
    feeTP[i] = 0;
 }
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTPTree::clear() {
  int i;
  for(i=0;i<mxTP;i++) {
    if( feeTP[i]==0) continue; // skip uninitialized TP
    feeTP[i]->clear();
  }
  //  memset(ee0outHTTP2bit,0,sizeof(ee0outHTTP2bit));

}


//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTPTree::compute(int *rawAdc, int *feePed, int *feeMask, int* highTowerMask, int* patchSumMask) {
  int i;

  for(i=0;i<mxTP;i++) {
    if( feeTP[i]==0) continue; // skip uninitialized TP
    int icr=feeTP[i]->getCrateID()-1;
    int off=icr*mxChan;
    feeTP[i]->compute(rawAdc+off, feePed+off, feeMask+off, highTowerMask[i], patchSumMask[i]);
  }  
}
void
EEfeeTPTree::test(int *tp, int *ht)
{
  int i;
  for(i=0;i<mxTP;i++) {
    feeTP[i]->test(tp[i], ht[i]);
  }  
}
#if 0
//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTPTree::setInput12bit(int HankCh, short val){
  int ibr=HankCh/10; // board #
  assert(ibr>=0 && ibr<Nee0);
  int ch=HankCh%10;  
  ee0[ibr].setInput12bit(ch,val);
  // printf("add %d %d %d\n",ibr,ch,val);
}



//--------------------------------------------------
//--------------------------------------------------
short
EEfeeTPTree::getInput12bit(int HankCh){
  int ibr=HankCh/10; // board #
  assert(ibr>=0 && ibr<Nee0);
  int ch=HankCh%10;  
  return ee0[ibr].getInput12bit(ch);
}

//--------------------------------------------------
//--------------------------------------------------
void
EEfeeTPTree::setYear(int y, int*HTth, int*TPth ) {
  int i;
  for (i=0;i<Nee0;i++) ee0[i].setYear(y,HTth,TPth );
}

//--------------------------------------------------
//--------------------------------------------------
void 
EEfeeTPTree::print( int k) {
  printf("EEfeeTPTree(%s) \n",name);

  int i;
  for(i=0;i<Nee0;i++) {
    printf("\n----------- level-0 Board %2d ",i+1);
    ee0[i].print();
  }

  printf("\n----------- level-0 emulated output \n   ch =");
  for(i=Nee0out-1;i>=0; i--) printf("  %2d ",i);
  printf("\n TPsum=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outTPsum[i]);
  printf("\n  HT2b=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outHT2bit[i]);

  printf("\nHTTP2b=");
  for(i=Nee0out-1;i>=0; i--) printf(" %3d ",ee0outHTTP2bit[i]);

  printf("\n");

}
#endif 
/*
 * $Log: EEfeeTPTree.cxx,v $
 * Revision 1.6  2020/01/13 20:45:50  zchang
 * removing old run13 dsm algo files
 *
 * Revision 1.5  2011/10/16 17:41:59  pibero
 * Implement EEMC FEE HT & TP masks
 *
 * Revision 1.4  2009/11/18 21:27:15  pibero
 * Fix memory leak.
 *
 * Revision 1.3  2009/11/18 15:50:59  pibero
 * Address several compiler warnings of the type:
 *
 * warning: deprecated conversion from string constant 'char*'
 *
 * Revision 1.2  2009/10/13 17:02:49  pibero
 * Changed location of #include file
 *
 * Revision 1.1  2009/10/12 18:04:27  pibero
 * Moved StEEmcUtil/EEdsm to StTriggerUtilities/Eemc
 *
 * Revision 1.1  2007/08/17 01:15:37  balewski
 * full blown Endcap trigger simu, by Xin
 *
 */


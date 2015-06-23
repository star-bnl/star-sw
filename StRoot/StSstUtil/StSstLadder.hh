//$Id: StSstLadder.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstLadder.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein
#ifndef STSSTLADDER_HH
#define STSSTLADDER_HH

#include "StSstUtil/StSstWafer.hh"
class St_sstWafersPosition;
class St_sstWaferConfiguration; 

class StSstLadder
{
 public:
  StSstLadder(Int_t rLadderNumb,Int_t rSstLayer, Int_t rNWaferPerLadder, Int_t rNStripPerSide);
 ~StSstLadder();
  void         initWafers(St_sstWafersPosition *wafpos);
  Int_t        getLadderNumb()     { return mLadderNumb; }
  Int_t        getWaferPerLadder() { return mNWaferPerLadder; } 
  StSstWafer*  getWafer(Int_t i)   { return mWafers[i];}
  void         debugUnPeu(Int_t monwafer);
  Int_t        idWaferToWafer(Int_t idWafer) {return (idWafer-7000)/100-1;}
  Int_t        idWaferToWaferNumb(Int_t idWafer);
  Int_t        waferNumbToIdWafer(Int_t waferNumb);
  void         Reset();
  void         SetDebug(Int_t k = 0) {mDebug = k;}
  Int_t        Debug() {return mDebug;}
 private:
  Char_t   first[1];
 public:
  StSstWafer** mWafers;
 private:
  Int_t    mLadderNumb;
  Int_t    mSstLayer;
  Int_t    mNWaferPerLadder;
  Int_t    mNStripPerSide;
  Int_t    mDebug;
  Char_t   last[1];
};

#endif

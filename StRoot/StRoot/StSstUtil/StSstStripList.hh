//$Id: StSstStripList.hh,v 1.2 2016/06/10 19:27:47 bouchet Exp $
//
//$Log: StSstStripList.hh,v $
//Revision 1.2  2016/06/10 19:27:47  bouchet
//coverity : FORWARD_NULL
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTSTRIPLIST_HH
#define STSSTSTRIPLIST_HH

#include "StSstDynamicControl.h"
#include "StSstStrip.hh"
class StSpaListNoise;

class StSstStripList {
 public:
  StSstStripList();
  ~StSstStripList();
  StSstStripList(const StSstStripList & originalStripList);
  StSstStripList& operator=(const StSstStripList originalStripList);

  StSstStrip* next(StSstStrip *ptr);
  StSstStrip* prev(StSstStrip *ptr);
  StSstStrip* first();
  StSstStrip* last();
  StSstStrip* getStrip(Int_t idStrip);
  Int_t       addNewStrip(StSstStrip *ptr);
  Int_t       removeStrip(StSstStrip *ptr); //
  void        exchangeTwoStrips(StSstStrip *ptr1, StSstStrip *ptr2);
  void        updateStrip(StSstStrip *ptr); //
  void        updateStripList(StSpaListNoise *ptr);
  void        sortStrip();
  void        setPedestalSigma(Int_t iStrip, Int_t iPedestal, Int_t iSigma, StSstDynamicControl *dynamicControl);
  Int_t       getSize();
  int*        getListAdc(Int_t idStrip, Int_t SizeCluster);
  Int_t       isSorted();

 private:
  Int_t       mListLength;
  StSstStrip *mFirstStrip;
  StSstStrip *mLastStrip;
};
#endif

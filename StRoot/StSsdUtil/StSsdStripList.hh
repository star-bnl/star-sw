// $Id: StSsdStripList.hh,v 1.1 2006/10/16 16:43:30 bouchet Exp $
//
// $Log: StSsdStripList.hh,v $
// Revision 1.1  2006/10/16 16:43:30  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.3  2005/03/18 15:02:37  lmartin
// setPedestalSigma method added, setSigma removed
//
// Revision 1.2  2005/03/18 14:17:39  lmartin
// missing CVS header added
//

#ifndef STSSDSTRIPLIST_HH
#define STSSDSTRIPLIST_HH

#include "StSsdDynamicControl.h"
#include "StSsdStrip.hh"
class StSpaListNoise;

class StSsdStripList {
 public:
  StSsdStripList();
  ~StSsdStripList();
  StSsdStripList(const StSsdStripList & originalStripList);
  StSsdStripList& operator=(const StSsdStripList originalStripList);

  StSsdStrip* next(StSsdStrip *ptr);
  StSsdStrip* prev(StSsdStrip *ptr);
  StSsdStrip* first();
  StSsdStrip* last();
  StSsdStrip* getStrip(Int_t idStrip);
  Int_t       addNewStrip(StSsdStrip *ptr);
  Int_t       removeStrip(StSsdStrip *ptr); //
  void        exchangeTwoStrips(StSsdStrip *ptr1, StSsdStrip *ptr2);
  void        updateStrip(StSsdStrip *ptr); //
  void        updateStripList(StSpaListNoise *ptr);
  void        sortStrip();
  StSsdStripList* addStripList(StSsdStripList *list); //
  void        setPedestalSigma(Int_t iStrip, Int_t iPedestal, Int_t iSigma, StSsdDynamicControl *dynamicControl);
  Int_t       getSize();
  int*        getListAdc(Int_t idStrip, Int_t SizeCluster);
  Int_t       isSorted();

 private:
  Int_t       mListLength;
  StSsdStrip *mFirstStrip;
  StSsdStrip *mLastStrip;
};
#endif

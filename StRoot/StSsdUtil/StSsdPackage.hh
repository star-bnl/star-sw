// $Id: StSsdPackage.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPackage.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:21:44  lmartin
// missing CVS header added
//

#ifndef STSSDPACKAGE_HH
#define STSSDPACKAGE_HH
#include "Rtypes.h"
class StSsdCluster;
class StSsdClusterControl;

class StSsdPackage
{
 public:
  StSsdPackage(Int_t rNPackage, StSsdClusterControl *control);
  StSsdPackage(Int_t rNPackage, Int_t rNMatched);
  StSsdPackage(const StSsdPackage & originalPackage);
  ~StSsdPackage();

  StSsdPackage& operator=(const StSsdPackage originalPackage);

  StSsdCluster* next(StSsdCluster *ptr);
  StSsdCluster* prev(StSsdCluster *ptr);
  StSsdCluster* first();
  StSsdCluster* last();

  Int_t           getNPackage();
  StSsdCluster* getMatched(Int_t numMatched);
  char*         getKind();
  StSsdPackage* getPrevPackage();
  StSsdPackage* getNextPackage();

  void          purgePackage();
  void          takeMatcheds(StSsdPackage *ptr);

  void          setNPackage(Int_t rNPackage);
  void          setKind(char *rKind);
  void          setPrevPackage(StSsdPackage *rPrevPackage);
  void          setNextPackage(StSsdPackage *rNextPackage);
  
  Int_t           addNewMatched(StSsdCluster *ptr, Int_t maxMatcheds);
  Int_t           addKindPackage(Int_t numMatched, Int_t rSide, Int_t maxMatcheds);
  Int_t           removeMatched(StSsdCluster *ptr);
  void          exchangeTwoMatcheds(StSsdCluster *ptr1, StSsdCluster *ptr2);
  void          sortMatched();
  void          renumMatched();
  Int_t           getSize();
  Int_t           isSorted();

 private:
  Int_t             mListLengthM;
  StSsdCluster   *mFirstMatched;
  StSsdCluster   *mLastMatched;

  Int_t             mNPackage;
  StSsdCluster*  *mMatcheds;
  char*           mKind;
  StSsdPackage   *mPrevPackage;
  StSsdPackage   *mNextPackage;
};
#endif

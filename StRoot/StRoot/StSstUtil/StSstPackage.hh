//$Id: StSstPackage.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstPackage.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTPACKAGE_HH
#define STSSTPACKAGE_HH
#include "Rtypes.h"
class StSstCluster;
class StSstClusterControl;

class StSstPackage
{
 public:
  StSstPackage(Int_t rNPackage, StSstClusterControl *control);
  StSstPackage(Int_t rNPackage, Int_t rNMatched);
  StSstPackage(const StSstPackage & originalPackage);
  ~StSstPackage();

  StSstPackage& operator=(const StSstPackage originalPackage);

  StSstCluster* next(StSstCluster *ptr);
  StSstCluster* prev(StSstCluster *ptr);
  StSstCluster* first();
  StSstCluster* last();

  Int_t         getNPackage();
  StSstCluster* getMatched(Int_t numMatched);
  char*         getKind();
  StSstPackage* getPrevPackage();
  StSstPackage* getNextPackage();

  void          purgePackage();
  void          takeMatcheds(StSstPackage *ptr);

  void          setNPackage(Int_t rNPackage);
  void          setKind(char *rKind);
  void          setPrevPackage(StSstPackage *rPrevPackage);
  void          setNextPackage(StSstPackage *rNextPackage);
  
  Int_t         addNewMatched(StSstCluster *ptr, Int_t maxMatcheds);
  Int_t         addKindPackage(Int_t numMatched, Int_t rSide, Int_t maxMatcheds);
  Int_t         removeMatched(StSstCluster *ptr);
  void          exchangeTwoMatcheds(StSstCluster *ptr1, StSstCluster *ptr2);
  void          sortMatched();
  void          renumMatched();
  Int_t         getSize();
  Int_t         isSorted();

 private:
  Int_t           mListLengthM;
  StSstCluster   *mFirstMatched;
  StSstCluster   *mLastMatched;

  Int_t           mNPackage;
  StSstCluster*  *mMatcheds;
  char*           mKind;
  StSstPackage   *mPrevPackage;
  StSstPackage   *mNextPackage;
};
#endif

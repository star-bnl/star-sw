#ifndef STSCMPACKAGE_HH
#define STSCMPACKAGE_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <string.h>
# include <math.h>
# include "StScmCluster.hh"
# include "StScmPoint.hh"
# include "scm_am.h"

class StScmPackage
{
 public:
  StScmPackage(int rNPackage, scm_ctrl_st *scm_ctrl);
  StScmPackage(int rNPackage, int rNMatched);
  StScmPackage(const StScmPackage & originalPackage);
  ~StScmPackage();

  StScmPackage& operator=(const StScmPackage originalPackage);

  StScmCluster* next(StScmCluster *ptr);
  StScmCluster* prev(StScmCluster *ptr);
  StScmCluster* first();
  StScmCluster* last();

  int           getNPackage();
  StScmCluster* getMatched(int numMatched);
  char*         getKind();
  StScmPackage* getPrevPackage();
  StScmPackage* getNextPackage();

  void          purgePackage();
  void          takeMatcheds(StScmPackage *ptr);

  void          setNPackage(int rNPackage);
  void          setKind(char *rKind);
  void          setPrevPackage(StScmPackage *rPrevPackage);
  void          setNextPackage(StScmPackage *rNextPackage);
  
  int           addNewMatched(StScmCluster *ptr, int maxMatcheds);
  int           addKindPackage(int numMatched, int rSide, int maxMatcheds);
  int           removeMatched(StScmCluster *ptr);
  void          exchangeTwoMatcheds(StScmCluster *ptr1, StScmCluster *ptr2);
  void          sortMatched();
  void          renumMatched();
  int           getSize();
  int           isSorted();

 private:
  int             mListLengthM;
  StScmCluster   *mFirstM;
  StScmCluster   *mLastM;

  int             mNPackage;
  StScmCluster*  *mMatcheds;
  char*           mKind;
  StScmPackage   *mPrevPackage;
  StScmPackage   *mNextPackage;
};
#endif

// $Id: StSsdPackage.hh,v 1.3 2006/05/06 00:56:26 fisyak Exp $
//
// $Log: StSsdPackage.hh,v $
// Revision 1.3  2006/05/06 00:56:26  fisyak
// Add local coordinate
//
// Revision 1.2  2005/05/17 14:16:39  lmartin
// CVS tags added
//
#ifndef STSSDPACKAGE_HH
#define STSSDPACKAGE_HH
# include <stdlib.h>
# include <string.h>
# include <math.h>
# include "StSsdCluster.hh"
# include "StSsdPointMaker/StSsdPoint.hh"
#include "tables/St_scm_ctrl_Table.h"

class StSsdPackage
{
 public:
  StSsdPackage(int rNPackage, scm_ctrl_st *scm_ctrl);
  StSsdPackage(int rNPackage, int rNMatched);
  StSsdPackage(const StSsdPackage & originalPackage);
  ~StSsdPackage();

  StSsdPackage& operator=(const StSsdPackage originalPackage);

  StSsdCluster* next(StSsdCluster *ptr);
  StSsdCluster* prev(StSsdCluster *ptr);
  StSsdCluster* first();
  StSsdCluster* last();

  int           getNPackage();
  StSsdCluster* getMatched(int numMatched);
  char*         getKind();
  StSsdPackage* getPrevPackage();
  StSsdPackage* getNextPackage();

  void          purgePackage();
  void          takeMatcheds(StSsdPackage *ptr);

  void          setNPackage(int rNPackage);
  void          setKind(char *rKind);
  void          setPrevPackage(StSsdPackage *rPrevPackage);
  void          setNextPackage(StSsdPackage *rNextPackage);
  
  int           addNewMatched(StSsdCluster *ptr, int maxMatcheds);
  int           addKindPackage(int numMatched, int rSide, int maxMatcheds);
  int           removeMatched(StSsdCluster *ptr);
  void          exchangeTwoMatcheds(StSsdCluster *ptr1, StSsdCluster *ptr2);
  void          sortMatched();
  void          renumMatched();
  int           getSize();
  int           isSorted();

 private:
  int             mListLengthM;
  StSsdCluster   *mFirstMatched;
  StSsdCluster   *mLastMatched;

  int             mNPackage;
  StSsdCluster*  *mMatcheds;
  char*           mKind;
  StSsdPackage   *mPrevPackage;
  StSsdPackage   *mNextPackage;
};
#endif

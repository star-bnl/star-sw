#ifndef STSCFLISTCLUSTER_HH
#define STSCFLISTCLUSTER_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StScfCluster.hh"
# include "scf_am.h"
# include "StScfListStrip.hh"

class StScfListCluster
{
 public:
  StScfListCluster();
  ~StScfListCluster();

  StScfCluster* next(StScfCluster *ptr);
  StScfCluster* prev(StScfCluster *ptr);
  StScfCluster* first();
  StScfCluster* last();  
  int        addNewCluster(StScfCluster *ptr);
  void       exchangeTwoClusters(StScfCluster *ptr1, StScfCluster *ptr2);
  void       sortCluster();
  void       renumCluster();
  int        removeCluster(StScfCluster *ptr);
  int        getSize();
  int        splitCluster(scf_ctrl_st *scf_ctrl, StScfCluster *CurrentCluster, int *ListAdc, StScfListStrip *currentListStrip);
  int        isSorted();
  

private:
  int        mListLength;
  StScfCluster *mFirstS;
  StScfCluster *mLastS;
};
#endif

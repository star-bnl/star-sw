// $Id: StSsdClusterList.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdClusterList.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:23:46  lmartin
// missing CVS header added
//

#ifndef STSSDCLUSTERLIST_HH
#define STSSDCLUSTERLIST_HH
#include "Rtypes.h"
class StSsdClusterControl;
class StSsdCluster;
class StSsdStripList;

class StSsdClusterList
{
 public:
  StSsdClusterList();
  ~StSsdClusterList();

  StSsdCluster* next(StSsdCluster *ptr);
  StSsdCluster* prev(StSsdCluster *ptr);
  StSsdCluster* first();
  StSsdCluster* last();  
  Int_t        addNewCluster(StSsdCluster *ptr);
  void       exchangeTwoClusters(StSsdCluster *ptr1, StSsdCluster *ptr2);
  void       sortCluster();
  void       renumCluster();
  Int_t        removeCluster(StSsdCluster *ptr);
  Int_t        getSize();
  Int_t        splitCluster(StSsdClusterControl *clusterControl, StSsdCluster *CurrentCluster, Int_t *ListAdc, StSsdStripList *currentStripList);
  Int_t        isSorted();

 private:
  StSsdClusterList(const StSsdClusterList & originalClusterList);
  StSsdClusterList& operator=(const StSsdClusterList originalClusterList);

  Int_t        mListLength;
  StSsdCluster *mFirstCluster;
  StSsdCluster *mLastCluster;
};
#endif

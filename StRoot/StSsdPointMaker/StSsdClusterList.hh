// $Id: StSsdClusterList.hh,v 1.2 2005/03/18 14:23:46 lmartin Exp $
//
// $Log: StSsdClusterList.hh,v $
// Revision 1.2  2005/03/18 14:23:46  lmartin
// missing CVS header added
//

#ifndef STSSDCLUSTERLIST_HH
#define STSSDCLUSTERLIST_HH

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
  int        addNewCluster(StSsdCluster *ptr);
  void       exchangeTwoClusters(StSsdCluster *ptr1, StSsdCluster *ptr2);
  void       sortCluster();
  void       renumCluster();
  int        removeCluster(StSsdCluster *ptr);
  int        getSize();
  int        splitCluster(StSsdClusterControl *clusterControl, StSsdCluster *CurrentCluster, int *ListAdc, StSsdStripList *currentStripList);
  int        isSorted();

 private:
  StSsdClusterList(const StSsdClusterList & originalClusterList);
  StSsdClusterList& operator=(const StSsdClusterList originalClusterList);

  int        mListLength;
  StSsdCluster *mFirstCluster;
  StSsdCluster *mLastCluster;
};
#endif

//$Id: StSstClusterList.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstClusterList.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTCLUSTERLIST_HH
#define STSSTCLUSTERLIST_HH
#include "Rtypes.h"
class StSstClusterControl;
class StSstCluster;
class StSstStripList;

class StSstClusterList
{
 public:
  StSstClusterList();
  ~StSstClusterList();

  StSstCluster* next(StSstCluster *ptr);
  StSstCluster* prev(StSstCluster *ptr);
  StSstCluster* first();
  StSstCluster* last();  
  Int_t         addNewCluster(StSstCluster *ptr);
  void         exchangeTwoClusters(StSstCluster *ptr1, StSstCluster *ptr2);
  void         sortCluster();
  void         renumCluster();
  Int_t        removeCluster(StSstCluster *ptr);
  Int_t        getSize();
  Int_t        splitCluster(StSstClusterControl *clusterControl, StSstCluster *CurrentCluster, Int_t *ListAdc, StSstStripList *currentStripList);
  Int_t        isSorted();

 private:
  StSstClusterList(const StSstClusterList & originalClusterList);
  StSstClusterList& operator=(const StSstClusterList originalClusterList);

  Int_t        mListLength;
  StSstCluster *mFirstCluster;
  StSstCluster *mLastCluster;
};
#endif

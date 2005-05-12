// $Id: StSceListCluster.hh,v 1.2 2005/05/12 08:22:10 lmartin Exp $
//
// $Log: StSceListCluster.hh,v $
// Revision 1.2  2005/05/12 08:22:10  lmartin
// cvs tags added and histograms in the .hist branch
//

# ifndef STSCELISTCLUSTER_HH
# define STSCELISTCLUSTER_HH
# include "StSceCluster.hh"

class StSceListCluster
{
 public:
  StSceListCluster();
  ~StSceListCluster();

  StSceCluster* next(StSceCluster *ptr);
  StSceCluster* prev(StSceCluster *ptr);
  StSceCluster* first();
  StSceCluster* last();
  int           getSize();
  int           addNewCluster(StSceCluster *ptr);
  int           removeCluster(StSceCluster *ptr);
  void          exchangeTwoClusters(StSceCluster *ptr1, StSceCluster *ptr2);
  void          sortCluster();
  void          renumCluster();
  int           isSorted();

 private:
  StSceListCluster(const StSceListCluster & originalListCluster);
  StSceListCluster& operator=(const StSceListCluster originalListCluster);

  int           mListLength;
  StSceCluster *mFirstC;
  StSceCluster *mLastC;
};
# endif

# ifndef STSCMLISTCLUSTER_HH
# define STSCMLISTCLUSTER_HH
# include "StScmCluster.hh"

class StScmListCluster
{
 public:
  StScmListCluster();
  ~StScmListCluster();

  StScmCluster* next(StScmCluster *ptr);
  StScmCluster* prev(StScmCluster *ptr);
  StScmCluster* first();
  StScmCluster* last();
  int           getSize();
  int           addNewCluster(StScmCluster *ptr);
  int           removeCluster(StScmCluster *ptr);
  void          exchangeTwoClusters(StScmCluster *ptr1, StScmCluster *ptr2);
  void          sortCluster();
  void          renumCluster();
  int           isSorted();

 private:
  StScmListCluster(const StScmListCluster & originalListCluster);
  StScmListCluster& operator=(const StScmListCluster originalListCluster);

  int           mListLength;
  StScmCluster *mFirstC;
  StScmCluster *mLastC;
};
# endif

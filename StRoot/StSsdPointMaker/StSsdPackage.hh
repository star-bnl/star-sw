#ifndef STSSDPACKAGE_HH
#define STSSDPACKAGE_HH

class StSsdCluster;
class StSsdClusterControl;

class StSsdPackage
{
 public:
  StSsdPackage(int rNPackage, StSsdClusterControl *control);
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

#ifndef STSSDSTRIPLIST_HH
#define STSSDSTRIPLIST_HH

class StSsdDynamicControl;
class StSsdStrip;

class StSsdStripList
{
 public:
  StSsdStripList();
  ~StSsdStripList();
  StSsdStripList(const StSsdStripList & originalStripList);
  StSsdStripList& operator=(const StSsdStripList originalStripList);

  StSsdStrip* next(StSsdStrip *ptr);
  StSsdStrip* prev(StSsdStrip *ptr);
  StSsdStrip* first();
  StSsdStrip* last();
  StSsdStrip* getStrip(int idStrip);
  int        addNewStrip(StSsdStrip *ptr);
  void       exchangeTwoStrips(StSsdStrip *ptr1, StSsdStrip *ptr2);
  void       sortStrip();
  void       setSigma(int iStrip, int iSigma, StSsdDynamicControl *dynamicControl);
  int        removeStrip(StSsdStrip *ptr);
  int        getSize();
  int*       getListAdc(int idStrip, int SizeCluster);
  int        isSorted();

 private:
  int        mListLength;
  StSsdStrip *mFirstStrip;
  StSsdStrip *mLastStrip;
};
#endif

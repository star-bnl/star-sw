#ifndef STSSDPACKAGELIST_HH
#define STSSDPACKAGELIST_HH

class StSsdPackage;

class StSsdPackageList
{
 public:
  StSsdPackageList();
  ~StSsdPackageList();
  StSsdPackageList(const StSsdPackageList & originalPackageList);
  StSsdPackageList& operator=(const StSsdPackageList originalPackageList);

  StSsdPackage* next(StSsdPackage *ptr);
  StSsdPackage* prev(StSsdPackage *ptr);
  StSsdPackage* first();
  StSsdPackage* last();
  
  int           addNewPackage(StSsdPackage *ptr);
  int           getSize();

 private:
  int           mListLengthP;
  StSsdPackage *mFirstPackage;
  StSsdPackage *mLastPackage;
};
#endif

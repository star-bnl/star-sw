// $Id: StSsdPackageList.hh,v 1.2 2005/03/18 14:20:21 lmartin Exp $
//
// $Log: StSsdPackageList.hh,v $
// Revision 1.2  2005/03/18 14:20:21  lmartin
// missing CVS header added
//

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

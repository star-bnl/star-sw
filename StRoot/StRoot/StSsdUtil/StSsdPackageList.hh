// $Id: StSsdPackageList.hh,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPackageList.hh,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:20:21  lmartin
// missing CVS header added
//

#ifndef STSSDPACKAGELIST_HH
#define STSSDPACKAGELIST_HH

#include "StSsdPackage.hh"

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
  
  Int_t           addNewPackage(StSsdPackage *ptr);
  Int_t           getSize();

 private:
  Int_t           mListLengthP;
  StSsdPackage *mFirstPackage;
  StSsdPackage *mLastPackage;
};
#endif

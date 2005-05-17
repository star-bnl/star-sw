// $Id: StSsdPackageList.hh,v 1.2 2005/05/17 14:16:39 lmartin Exp $
//
// $Log: StSsdPackageList.hh,v $
// Revision 1.2  2005/05/17 14:16:39  lmartin
// CVS tags added
//
#ifndef STSSDPACKAGELIST_HH
#define STSSDPACKAGELIST_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdPackage.hh"
//#include "scm_am.h"

class StSsdPackageList
{
 public:
  StSsdPackageList();
  ~StSsdPackageList();

  StSsdPackage* next(StSsdPackage *ptr);
  StSsdPackage* prev(StSsdPackage *ptr);
  StSsdPackage* first();
  StSsdPackage* last();
  
  int           addNewPackage(StSsdPackage *ptr);
  int           getSize();

 private:
  StSsdPackageList(const StSsdPackageList & originalPackageList);
  StSsdPackageList& operator=(const StSsdPackageList originalPackageList);

  int           mListLengthP;
  StSsdPackage *mFirstPackage;
  StSsdPackage *mLastPackage;
};
#endif

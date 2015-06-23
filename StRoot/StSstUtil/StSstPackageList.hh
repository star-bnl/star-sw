//$Id: StSstPackageList.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
//$Log: StSstPackageList.hh,v $
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTPACKAGELIST_HH
#define STSSTPACKAGELIST_HH

#include "StSstPackage.hh"

class StSstPackageList
{
 public:
  StSstPackageList();
  ~StSstPackageList();
  StSstPackageList(const StSstPackageList & originalPackageList);
  StSstPackageList& operator=(const StSstPackageList originalPackageList);

  StSstPackage* next(StSstPackage *ptr);
  StSstPackage* prev(StSstPackage *ptr);
  StSstPackage* first();
  StSstPackage* last();
  
  Int_t         addNewPackage(StSstPackage *ptr);
  Int_t         getSize();

 private:
  Int_t         mListLengthP;
  StSstPackage *mFirstPackage;
  StSstPackage *mLastPackage;
};
#endif

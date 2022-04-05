//$Id: StSstPackageList.cc,v 1.1 2015/06/23 16:26:20 jeromel Exp $
//
//$Log: StSstPackageList.cc,v $
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstPackageList.hh"

StSstPackageList::StSstPackageList()
{
  mListLengthP  = 0;
  mFirstPackage = 0;
  mLastPackage  = 0;
}

StSstPackageList::~StSstPackageList()
{
  if (mListLengthP)
    {
      StSstPackage *ptr = mLastPackage;
      StSstPackage *toDele;
      while (mListLengthP)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLengthP--;
	}
    }
}

StSstPackage* StSstPackageList::next(StSstPackage *ptr)
{  return ptr->getNextPackage(); }

StSstPackage* StSstPackageList::prev(StSstPackage *ptr)
{  return ptr->getPrevPackage(); }

StSstPackage* StSstPackageList::first()
{  return mFirstPackage; }

StSstPackage* StSstPackageList::last()
{  return mLastPackage; }

Int_t StSstPackageList::getSize()
{  return mListLengthP; }


Int_t StSstPackageList::addNewPackage(StSstPackage *ptr)
{
  if (!ptr) return 0;
  if (mListLengthP == 0)
    {
      ptr->setPrevPackage(0);
      ptr->setNextPackage(0);
      mFirstPackage = ptr;
      mLastPackage  = ptr;
    }
  else
    {
      ptr->setPrevPackage(mLastPackage);
      ptr->setNextPackage(0);
      mLastPackage->setNextPackage(ptr);
      mLastPackage  = ptr;
    }
  mListLengthP++;
  return 1;
}

StSstPackageList::StSstPackageList(const StSstPackageList & originalPackageList)
{
  mListLengthP = originalPackageList.mListLengthP;
  mFirstPackage      = originalPackageList.mFirstPackage;
  mLastPackage       = originalPackageList.mLastPackage;
}

StSstPackageList& StSstPackageList::operator=(const StSstPackageList originalPackageList)
{
  mListLengthP = originalPackageList.mListLengthP;
  mFirstPackage      = originalPackageList.mFirstPackage;
  mLastPackage       = originalPackageList.mLastPackage;

  return *this;
}

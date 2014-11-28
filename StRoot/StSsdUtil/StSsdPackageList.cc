// $Id: StSsdPackageList.cc,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPackageList.cc,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:20:21  lmartin
// missing CVS header added
//

#include "StSsdPackageList.hh"

StSsdPackageList::StSsdPackageList()
{
  mListLengthP  = 0;
  mFirstPackage = 0;
  mLastPackage  = 0;
}

StSsdPackageList::~StSsdPackageList()
{
  if (mListLengthP)
    {
      StSsdPackage *ptr = mLastPackage;
      StSsdPackage *toDele;
      while (mListLengthP)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLengthP--;
	}
    }
}


StSsdPackage* StSsdPackageList::next(StSsdPackage *ptr)
{  return ptr->getNextPackage(); }

StSsdPackage* StSsdPackageList::prev(StSsdPackage *ptr)
{  return ptr->getPrevPackage(); }

StSsdPackage* StSsdPackageList::first()
{  return mFirstPackage; }

StSsdPackage* StSsdPackageList::last()
{  return mLastPackage; }

Int_t StSsdPackageList::getSize()
{  return mListLengthP; }


Int_t StSsdPackageList::addNewPackage(StSsdPackage *ptr)
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

StSsdPackageList::StSsdPackageList(const StSsdPackageList & originalPackageList)
{
  mListLengthP = originalPackageList.mListLengthP;
  mFirstPackage      = originalPackageList.mFirstPackage;
  mLastPackage       = originalPackageList.mLastPackage;
}

StSsdPackageList& StSsdPackageList::operator=(const StSsdPackageList originalPackageList)
{
  mListLengthP = originalPackageList.mListLengthP;
  mFirstPackage      = originalPackageList.mFirstPackage;
  mLastPackage       = originalPackageList.mLastPackage;

  return *this;
}

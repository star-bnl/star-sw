#include "StScmListPackage.hh"

StScmListPackage::StScmListPackage()
{
  mListLengthP = 0;
  mFirstP      = 0;
  mLastP       = 0;
}

StScmListPackage::~StScmListPackage()
{
  if (mListLengthP)
    {
      StScmPackage *ptr = mLastP;
      StScmPackage *toDele;
      while (mListLengthP)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLengthP--;
	}
    }
}


StScmPackage* StScmListPackage::next(StScmPackage *ptr)
{  return ptr->getNextPackage(); }

StScmPackage* StScmListPackage::prev(StScmPackage *ptr)
{  return ptr->getPrevPackage(); }

StScmPackage* StScmListPackage::first()
{  return mFirstP; }

StScmPackage* StScmListPackage::last()
{  return mLastP; }

int StScmListPackage::getSize()
{  return mListLengthP; }


int StScmListPackage::addNewPackage(StScmPackage *ptr)
{
  if (!ptr) return 0;
  if (mListLengthP == 0)
    {
      ptr->setPrevPackage(0);
      ptr->setNextPackage(0);
      mFirstP = ptr;
      mLastP  = ptr;
    }
  else
    {
      ptr->setPrevPackage(mLastP);
      ptr->setNextPackage(0);
      mLastP->setNextPackage(ptr);
      mLastP  = ptr;
    }
  mListLengthP++;
  return 1;
}

StScmListPackage::StScmListPackage(const StScmListPackage & originalListPackage)
{
  mListLengthP = originalListPackage.mListLengthP;
  mFirstP      = originalListPackage.mFirstP;
  mLastP       = originalListPackage.mLastP;
}

StScmListPackage& StScmListPackage::operator=(const StScmListPackage originalListPackage)
{
  mListLengthP = originalListPackage.mListLengthP;
  mFirstP      = originalListPackage.mFirstP;
  mLastP       = originalListPackage.mLastP;

  return *this;
}

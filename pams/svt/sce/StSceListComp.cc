#include "StSceListComp.hh"

StSceListComp::StSceListComp()
{
  mListLength = 0;
  mFirstC     = 0;
  mLastC      = 0;
}

StSceListComp::~StSceListComp()
{
  if (mListLength)
    {
      StSceComp *ptr = mLastC;
      StSceComp *ToDelete;
      while (mListLength)
	{
	  ToDelete = ptr;
	  ptr     = prev(ptr);
	  delete ToDelete;
	  mListLength--;
	}
    }
}


StSceComp* StSceListComp::next(StSceComp *ptr)
{  return ptr->getNextComp(); }

StSceComp* StSceListComp::prev(StSceComp *ptr)
{  return ptr->getPrevComp(); }

StSceComp* StSceListComp::first()
{  return mFirstC; }

int StSceListComp::getSize()
{  return mListLength; }

StSceComp* StSceListComp::last()
{  return mLastC; }


int StSceListComp::addNewComp(StSceComp *ptr)
{
  if (!ptr) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevComp(0);
      ptr->setNextComp(0);
      this->mFirstC = ptr;
      this->mLastC  = ptr;
    }
  else
    {
      ptr->setPrevComp(mLastC);
      ptr->setNextComp(0);
      mLastC->setNextComp(ptr);
      mLastC = ptr;
    }
  mListLength++;
  return 1;
}


void StSceListComp::exchangeTwoComps(StSceComp *ptr1, StSceComp *ptr2)
{
  StSceComp *ptr_tmp = ptr1->giveCopy();
  int e = 0; 

  ptr1->setNComp(ptr2->getNComp());
  ptr1->setProb(ptr2->getProb());
  ptr1->setGhostOrTrue(ptr2->getGhostOrTrue());
  ptr1->setKindPackage(ptr2->getKindPackage());
  ptr1->setIdMatch(ptr2->getIdMatch());
  ptr1->setIdWaf(ptr2->getIdWaf());
  for (e = 0; e < 3; e++)
    {
      ptr1->setDxg(ptr2->getDxg(e),e);
      ptr1->setDxl(ptr2->getDxl(e),e);
    }
  for (e = 0; e < 2; e++)
      ptr1->setD2e(ptr2->getD2e(e),e);

  ptr2->setNComp(ptr_tmp->getNComp());
  ptr2->setProb(ptr_tmp->getProb());
  ptr2->setGhostOrTrue(ptr_tmp->getGhostOrTrue());
  ptr2->setKindPackage(ptr_tmp->getKindPackage());
  ptr2->setIdMatch(ptr_tmp->getIdMatch());
  ptr2->setIdWaf(ptr_tmp->getIdWaf());
  for (e = 0; e < 3; e++)
    {
      ptr2->setDxg(ptr_tmp->getDxg(e),e);
      ptr2->setDxl(ptr_tmp->getDxl(e),e);
    }
  for (e = 0; e < 2; e++)
    ptr2->setD2e(ptr_tmp->getD2e(e),e);
}

StSceListComp* StSceListComp::sortComp()
{
  int localSize = this->getSize();
  if (localSize<2) return this;
  
  StSceComp *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSceComp *ptB1 = ptCurr;
      StSceComp *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNComp() > ptB1->getNComp())
	    {
	      this->exchangeTwoComps(ptB1,ptB2);
	      ptB1 = ptB2;
	    }
	  else
	    {
	      isCont = 0;
	    }
	}
      ptCurr = this->next(ptCurr);
      
    }
  return this;
}

#include "StScmListPoint.hh"

StScmListPoint::StScmListPoint()
{
  mListLength = 0;
  mFirstS     = 0;
  mLastS      = 0;
}

StScmListPoint::~StScmListPoint()
{
  if (mListLength)
    {
      StScmPoint *ptr = mLastS;
      StScmPoint *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StScmPoint* StScmListPoint::next(StScmPoint *ptr)
{  return ptr->getNextPoint(); }

StScmPoint* StScmListPoint::prev(StScmPoint *ptr)
{  return ptr->getPrevPoint(); }

StScmPoint* StScmListPoint::first()
{  return mFirstS; }

StScmPoint* StScmListPoint::last()
{  return mLastS; }

int StScmListPoint::getSize()
{  return mListLength; }

int StScmListPoint::addNewPoint(StScmPoint *ptr)
{
  if (!ptr) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevPoint(0);
      ptr->setNextPoint(0);
      mFirstS = ptr;
      mLastS  = ptr;
    }
  else
    {
      ptr->setPrevPoint(mLastS);
      ptr->setNextPoint(0);
      mLastS->setNextPoint(ptr);
      mLastS = ptr;
    }
  mListLength++;
  return 1;
}

int StScmListPoint::removePoint(StScmPoint *ptr)
{
  if (!this->getSize()) return 0;
  StScmPoint *ptBefore = ptr->getPrevPoint();
  StScmPoint *ptAfter  = ptr->getNextPoint();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstS =     0;
	  this->mLastS  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstS = ptAfter;
	  ptAfter->setPrevPoint(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter== 0)
	{
	  this->mLastS = ptBefore;
	  ptBefore->setNextPoint(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextPoint(ptAfter);
	  ptAfter->setPrevPoint(ptBefore);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }

}

void StScmListPoint::exchangeTwoPoints(StScmPoint *ptr1,StScmPoint *ptr2)
{
  int i = 0;

  ptr1->setFlag(ptr2->getFlag()) ;
  ptr1->setNPoint(ptr2->getNPoint()) ;
  ptr1->setNCluster(ptr2->getNCluster()) ;
  ptr1->setNMatched(ptr2->getNMatched());
  for (i = 0; i < 5; i++)
  ptr1->setNMchit(ptr2->getNMchit(i),i);
  ptr1->setNWafer(ptr2->getNWafer());
  for (i = 0; i < 3; i++)
   { 
  ptr1->setXg(ptr2->getXg(i),i);
  ptr1->setXl(ptr2->getXl(i),i);
   }
  for (i = 0; i < 2; i++)
   { 
  ptr1->setDe(ptr2->getDe(i),i);
   }

  ptr2->setFlag(ptr1->getFlag()) ;
  ptr2->setNPoint(ptr1->getNPoint()) ;
  ptr2->setNCluster(ptr1->getNCluster()) ;
  ptr2->setNMatched(ptr1->getNMatched());
  for (i = 0; i < 5; i++)
  ptr2->setNMchit(ptr1->getNMchit(i),i);
  ptr2->setNWafer(ptr1->getNWafer());
  for (i = 0; i < 3; i++)
   { 
  ptr2->setXg(ptr1->getXg(i),i);
  ptr2->setXl(ptr1->getXl(i),i);
   }
  for (i = 0; i < 2; i++)
   {
  ptr1->setDe(ptr2->getDe(i),i);
   }
}

StScmListPoint* StScmListPoint::addListPoint(StScmListPoint *list)
{
  int size2 = list->getSize();
  
  if (!size2) return this;

  StScmPoint *pt1 ;
  StScmPoint *pt2 = list->first();
  
  for (int i = 0; i < size2; i++)
    {
      pt1 = pt2->giveCopy();
      this->addNewPoint(pt1);
      pt2 = list->next(pt2);
    }
  return this;  
}

StScmListPoint* StScmListPoint::substractListPoint(StScmListPoint *list)
{

  int localSizeToDelete = list->getSize();
  int localSizeToKeep = this->getSize();

  if((!localSizeToDelete)||(!localSizeToKeep)) return this;
  StScmPoint *currDele = list->first();

  for (int iDele = 0; iDele < localSizeToDelete; iDele++)
    {
      StScmPoint *currKeep = this->first();
      int iKeep = 0;
      for (iKeep =0 ; ((iKeep < this->getSize())&&((currDele->getNPoint())!=(currKeep->getNPoint()))); iKeep++)
	{
	  currKeep = list->next(currKeep);
	}
      if (currDele->getNPoint()==currKeep->getNPoint()) 
	{
	  this->removePoint(currKeep);
	}
      currDele = list->next(currDele);
    }
  return this;
}

StScmListPoint* StScmListPoint::removeMultipleCount()
{
  int localSize = this->getSize();
  if (localSize < 2) return this;
  StScmPoint *ptBigLoop = this->first();
  
  while ((ptBigLoop != this->last())&&(ptBigLoop != 0))
    {
      StScmPoint *ptSmallLoop = this->next(ptBigLoop);

      while (ptSmallLoop!=0)
	{
	  if (ptSmallLoop->getNPoint() == ptBigLoop->getNPoint()) 
	    {
	      StScmPoint *temp = ptSmallLoop;
	      ptSmallLoop = this->next(ptSmallLoop);
	      this->removePoint(temp);
	    }
	  else
	    {
	      ptSmallLoop = this->next(ptSmallLoop);
	    }
	}
      ptBigLoop = this->next(ptBigLoop);
    }
  return this;
}

StScmListPoint* StScmListPoint::sortPoint()
{
  int localSize=this->getSize();
  if (localSize<2) return this;
  
  StScmPoint *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StScmPoint *ptB1 = ptCurr;
      StScmPoint *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNPoint() > ptB1->getNPoint())
	    {
	      this->exchangeTwoPoints(ptB1,ptB2);
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

int StScmListPoint::renumHits(int last)
{
  int localSize = this->getSize();
  if (!localSize) return last;
  StScmPoint *ptr = this->first();
  for (int iPt = 0; iPt < localSize; iPt++)
    {
      ptr->setNPoint(last + iPt + 1);
      ptr = this->next(ptr);
    }
  return (last + localSize);
}

StScmListPoint::StScmListPoint(const StScmListPoint & originalListPoint)
{
  mListLength = originalListPoint.mListLength;
  mFirstS     = originalListPoint.mFirstS;
  mLastS      = originalListPoint.mLastS;
}

StScmListPoint& StScmListPoint::operator=(const StScmListPoint originalListPoint)
{
  mListLength = originalListPoint.mListLength;
  mFirstS     = originalListPoint.mFirstS;
  mLastS      = originalListPoint.mLastS;

  return *this;
}

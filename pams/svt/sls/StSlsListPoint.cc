#include "StSlsListPoint.hh"

StSlsListPoint::StSlsListPoint()
{
  mListLength = 0;
  mFirstP     = 0;
  mLastP      = 0;
}

StSlsListPoint::~StSlsListPoint()
{
  if (mListLength)
    {
      StSlsPoint *ptr = mLastP;
      StSlsPoint *ToDelete;
      while (mListLength)
	{
	  ToDelete = ptr;
	  ptr     = prev(ptr);
	  delete ToDelete;
	  mListLength--;
	}
    }
}

StSlsPoint* StSlsListPoint::next(StSlsPoint *ptr)
{  return ptr->getNextPoint(); }

StSlsPoint* StSlsListPoint::prev(StSlsPoint *ptr)
{  return ptr->getPrevPoint(); }

StSlsPoint* StSlsListPoint::first()
{  return mFirstP; }

StSlsPoint* StSlsListPoint::last()
{  return mLastP; }

int StSlsListPoint::getSize()
{  return mListLength; }

int StSlsListPoint::addNewPoint(StSlsPoint *ptr)
{
  if (ptr->getNId()== 0) return 0;
  if (!this->mListLength)
    {
      ptr->setPrevPoint(0);
      ptr->setNextPoint(0);
      this->mFirstP = ptr;
      this->mLastP  = ptr;
    }
  else
    {
      ptr->setPrevPoint(this->mLastP);
      ptr->setNextPoint(0);
      (this->mLastP)->setNextPoint(ptr);
      this->mLastP = ptr;
    }
  this->mListLength++;
  return 1;
}

int StSlsListPoint::removePoint(StSlsPoint *ptr)
{
  if (!this->getSize()) return 0;
  StSlsPoint *PtBefore = ptr->getPrevPoint();
  StSlsPoint *PtAfter  = ptr->getNextPoint();
  
  if (PtBefore == 0)
    {
      if (PtAfter == 0)
	{
	  this->mFirstP =     0;
	  this->mLastP  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstP = PtAfter;
	  PtAfter->setPrevPoint(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (PtAfter== 0)
	{
	  this->mLastP = PtBefore;
	  PtBefore->setNextPoint(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  PtBefore->setNextPoint(PtAfter);
	  PtAfter->setPrevPoint(PtBefore);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
}

void StSlsListPoint::exchangeTwoPoints(StSlsPoint *ptr1, StSlsPoint *ptr2)
{
  StSlsPoint *ptr_tmp = ptr1->giveCopy();
  int i = 0;

  ptr1->setNId(ptr2->getNId());
  ptr1->setMcHit(ptr2->getMcHit());
  ptr1->setMcTrack(ptr2->getMcTrack());
  ptr1->setDe(ptr2->getDe());
  for(i = 0; i < 3; i++)
    {
      ptr1->setXg(ptr2->getXg(i),i);
      ptr1->setXl(ptr2->getXl(i),i);
    }
  for(i = 0; i < 2; i++)
    {
      ptr1->setUpos(ptr2->getUpos(i),i);
      ptr1->setAngle(ptr2->getAngle(i),i);
    }

  ptr2->setNId(ptr_tmp->getNId()) ;
  ptr2->setMcHit(ptr_tmp->getMcHit());
  ptr2->setMcTrack(ptr_tmp->getMcTrack());
  ptr2->setDe(ptr_tmp->getDe());
  for(i = 0; i < 3; i++)
    {
      ptr2->setXg(ptr_tmp->getXg(i),i);
      ptr2->setXl(ptr_tmp->getXl(i),i);
    }
  for(i = 0; i < 2; i++)
    {
      ptr2->setUpos(ptr_tmp->getUpos(i),i);
      ptr2->setAngle(ptr_tmp->getAngle(i),i);
    }
}

StSlsListPoint* StSlsListPoint::addListPoint(StSlsListPoint *list)
{
  int i = 0;
  int size2 = list->getSize();
  
  if (!size2) return this;

  StSlsPoint *pt1 ;
  StSlsPoint *pt2 = list->first();
  
  for (i = 0; i < size2; i++)
    {
      pt1 = pt2->giveCopy();
      this->addNewPoint(pt1);
      pt2 = list->next(pt2);
    }
  return this;  
}

StSlsListPoint* StSlsListPoint::substractListPoint(StSlsListPoint *list)
{
  int localSizeToDelete = list->getSize();
  int localSizeToKeep   = this->getSize();

  if((!localSizeToDelete)||(!localSizeToKeep)) return this;
  StSlsPoint *currDele = list->first();

  for (int iDele = 0; iDele < localSizeToDelete; iDele++)
    {
      StSlsPoint *currKeep = this->first();
      for (int iKeep =0 ; ((iKeep < this->getSize())&&((currDele->getNId())!=(currKeep->getNId()))); iKeep++)
	{
	  currKeep = list->next(currKeep);
	}
      if (currDele->getNId()==currKeep->getNId()) 
	{
	  this->removePoint(currKeep);
	}
      currDele = list->next(currDele);
    }
  return this;
}

StSlsListPoint* StSlsListPoint::removeMultipleCount()
{
  int localSize = this->getSize();
  if (localSize < 2) return this;
  StSlsPoint *ptBigLoop = this->first();
  
  while ((ptBigLoop != this->last())&&(ptBigLoop != 0))
    {
      StSlsPoint *ptSmallLoop = this->next(ptBigLoop);
      while (ptSmallLoop!=0)
	{
	  if (ptSmallLoop->getNId() == ptBigLoop->getNId()) 
	    {
	      StSlsPoint *temp = ptSmallLoop;
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

StSlsListPoint* StSlsListPoint::sortPoint()
{
  int localSize=this->getSize();
  if (localSize<2) return this;
  
  StSlsPoint *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSlsPoint *ptB1 = ptCurr;
      StSlsPoint *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNId() > ptB1->getNId())
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

int StSlsListPoint::renumHits(int last)
{
  int localSize = this->getSize();
  if (!localSize) return last;
  StSlsPoint *ptr = this->first();
  for (int iPt = 0; iPt < localSize; iPt++)
    {
      ptr->setNId(last + iPt + 1);
      ptr = this->next(ptr);
    }
  return (last + localSize);
}

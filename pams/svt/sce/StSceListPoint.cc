#include "StSceListPoint.hh"

StSceListPoint::StSceListPoint()
{
  mListLength = 0;
  mFirstP     = 0;
  mLastP      = 0;
}

StSceListPoint::~StSceListPoint()
{
  if (mListLength)
    {
      StScePoint *ptr = mLastP;
      StScePoint *ToDelete;
      while (mListLength)
	{
	  ToDelete = ptr;
	  ptr     = prev(ptr);
	  delete ToDelete;
	  mListLength--;
	}
    }
}

StScePoint* StSceListPoint::next(StScePoint *ptr)
{  return ptr->getNextPoint(); }

StScePoint* StSceListPoint::prev(StScePoint *ptr)
{  return ptr->getPrevPoint(); }

StScePoint* StSceListPoint::first()
{  return mFirstP; }

int StSceListPoint::getSize()
{  return mListLength; }

StScePoint* StSceListPoint::last()
{  return mLastP; }


int StSceListPoint::addNewPoint(StScePoint *ptr)
{
  if (ptr->getNPoint()== 0) return 0;
  if (!mListLength)
    {
      ptr->setPrevPoint(0);
      ptr->setNextPoint(0);
      mFirstP = ptr;
      mLastP  = ptr;
    }
  else
    {
      ptr->setPrevPoint(mLastP);
      ptr->setNextPoint(0);
      (mLastP)->setNextPoint(ptr);
      mLastP = ptr;
    }
  this->mListLength++;
  return 1;
}

int StSceListPoint::removePoint(StScePoint *ptr)
{
  if (!this->getSize()) return 0;
  StScePoint *PtBefore = ptr->getPrevPoint();
  StScePoint *PtAfter  = ptr->getNextPoint();
  
  if (PtBefore == 0)
    {
      if (PtAfter == 0)
	{
	  // taille = 1
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

void StSceListPoint::exchangeTwoPoints(StScePoint *ptr1, StScePoint *ptr2)
{
  
  StScePoint *ptr_tmp = ptr1->giveCopy();
  int e = 0; 

  ptr1->setNPoint(ptr2->getNPoint());
  ptr1->setIdCluster(ptr2->getIdCluster());
  ptr1->setIdGlobTrk(ptr2->getIdGlobTrk());
  ptr1->setIdMatch(ptr2->getIdMatch());
  for(e=0;e<5;e++)
    {
      ptr1->setIdMcHit(ptr2->getIdMcHit(e),e);
      ptr1->setIdMcTrack(ptr2->getIdMcTrack(e),e);
      ptr1->setIdTrack(ptr2->getIdTrack(e),e);
    }
  ptr1->setIdWaf(ptr2->getIdWaf());
  for(e=0;e<3;e++)
    {
      ptr1->setCov(ptr2->getCov(e),e);
      ptr1->setRes(ptr2->getRes(e),e);
      ptr1->setXg(ptr2->getXg(e),e);
      ptr1->setXl(ptr2->getXl(e),e);
    }
  for(e=0;e<2;e++)
    {
      ptr1->setMom2(ptr2->getMom2(e),e);
      ptr1->setDe(ptr2->getDe(e),e);
    }

  ptr2->setNPoint(ptr_tmp->getNPoint());
  ptr2->setIdCluster(ptr_tmp->getIdCluster());
  ptr2->setIdGlobTrk(ptr_tmp->getIdGlobTrk());
  ptr2->setIdMatch(ptr_tmp->getIdMatch());
  for(e=0;e<5;e++)
    {
      ptr2->setIdMcHit(ptr_tmp->getIdMcHit(e),e);
      ptr2->setIdMcTrack(ptr_tmp->getIdMcTrack(e),e);
      ptr2->setIdTrack(ptr_tmp->getIdTrack(e),e);
    }
  ptr2->setIdWaf(ptr_tmp->getIdWaf());
  for(e=0;e<3;e++)
    {
      ptr2->setCov(ptr_tmp->getCov(e),e);
      ptr2->setRes(ptr_tmp->getRes(e),e);
      ptr2->setXg(ptr_tmp->getXg(e),e);
      ptr2->setXl(ptr_tmp->getXl(e),e);
    }
  for(e=0;e<2;e++)
    {
      ptr2->setMom2(ptr_tmp->getMom2(e),e);
      ptr2->setDe(ptr_tmp->getDe(e),e);
    }
}

StSceListPoint* StSceListPoint::addListPoint(StSceListPoint *list)
{
  int i = 0;
  int size2 = list->getSize();
  
  if (!size2) return this;

  StScePoint *pt1 ;
  StScePoint *pt2 = list->first();
  
  for (i = 0; i < size2; i++)
    {
      pt1 = pt2->giveCopy();
      this->addNewPoint(pt1);
      pt2 = list->next(pt2);
    }
  return this;  
}

StSceListPoint* StSceListPoint::substractListPoint(StSceListPoint *list)
{
  int localSizeToDelete = list->getSize();
  int localSizeToKeep   = this->getSize();

  if((!localSizeToDelete)||(!localSizeToKeep)) return this;
  StScePoint *currDele = list->first();

  for (int iDele = 0; iDele < localSizeToDelete; iDele++)
    {
      StScePoint *currKeep = this->first();
      for (int iKeep =0 ; ((iKeep < this->getSize())&&((currDele->getNPoint())!=(currKeep->getNPoint()))); iKeep++)
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

StSceListPoint* StSceListPoint::removeMultipleCount()
{
  int localSize = this->getSize();
  if (localSize < 2) return this;
  StScePoint *ptBigLoop = this->first();
  
  while ((ptBigLoop != this->last())&&(ptBigLoop != 0))
    {
      StScePoint *ptSmallLoop = this->next(ptBigLoop);
      while (ptSmallLoop!=0)
	{
	  if (ptSmallLoop->getNPoint() == ptBigLoop->getNPoint()) 
	    {
	      StScePoint *temp = ptSmallLoop;
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

StSceListPoint* StSceListPoint::sortPoint()
{
  int localSize = this->getSize();
  if (localSize<2) return this;
  
  StScePoint *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StScePoint *ptB1 = ptCurr;
      StScePoint *ptB2;
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

int StSceListPoint::renumHits(int last)
{
  int localSize = this->getSize();
  if (!localSize) return last;
  StScePoint *ptr = this->first();
  for (int iPt = 0; iPt < localSize; iPt++)
    {
      ptr->setNPoint(last + iPt + 1);
      ptr = this->next(ptr);
    }
  return (last + localSize);
}

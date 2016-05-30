//$Id: StSstPointList.cc,v 1.2 2016/05/30 21:44:30 bouchet Exp $
//
//$Log: StSstPointList.cc,v $
//Revision 1.2  2016/05/30 21:44:30  bouchet
//coverity : RESOURCE_LEAK fixed in exchangePoints method
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstPointList.hh"

StSstPointList::StSstPointList()
{
  mListLength = 0;
  mFirstPoint = 0;
  mLastPoint  = 0;
}

StSstPointList::~StSstPointList()
{
  if (mListLength)
    {
      StSstPoint *ptr = mLastPoint;
      StSstPoint *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSstPoint* StSstPointList::next(StSstPoint *ptr)
{  return ptr->getNextPoint(); }

StSstPoint* StSstPointList::prev(StSstPoint *ptr)
{  return ptr->getPrevPoint(); }

StSstPoint* StSstPointList::first()
{  return mFirstPoint; }

StSstPoint* StSstPointList::last()
{  return mLastPoint; }

Int_t StSstPointList::getSize()
{  return mListLength; }

Int_t StSstPointList::addNewPoint(StSstPoint *ptr)
{
  if (!ptr) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevPoint(0);
      ptr->setNextPoint(0);
      mFirstPoint = ptr;
      mLastPoint  = ptr;
    }
  else
    {
      ptr->setPrevPoint(mLastPoint);
      ptr->setNextPoint(0);
      mLastPoint->setNextPoint(ptr);
      mLastPoint = ptr;
    }
  mListLength++;
  return 1;
}

Int_t StSstPointList::removePoint(StSstPoint *ptr)
{
  if (!this->getSize()) return 0;
  StSstPoint *ptBefore = ptr->getPrevPoint();
  StSstPoint *ptAfter  = ptr->getNextPoint();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstPoint =     0;
	  this->mLastPoint  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstPoint = ptAfter;
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
	  this->mLastPoint = ptBefore;
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

void StSstPointList::exchangeTwoPoints(StSstPoint *ptr1,StSstPoint *ptr2)
{
  StSstPoint *ptrTemp = ptr1;
  ptr1 = ptr2;
  ptr2 = ptrTemp;
  delete ptrTemp;
}

StSstPointList* StSstPointList::addPointList(StSstPointList *list)
{
  Int_t size2 = list->getSize();
  
  if (!size2) return this;
  StSstPoint *pt1 ;
  StSstPoint *pt2 = list->first();
  
  for (Int_t i = 0; i < size2; i++)
    {
      pt1 = pt2->giveCopy();
      this->addNewPoint(pt1);
      pt2 = list->next(pt2);
    }
  return this;  
}

StSstPointList* StSstPointList::substractPointList(StSstPointList *list)
{

  Int_t localSizeToDelete = list->getSize();
  Int_t localSizeToKeep = this->getSize();

  if((!localSizeToDelete)||(!localSizeToKeep)) return this;
  StSstPoint *currDele = list->first();

  for (Int_t iDele = 0; iDele < localSizeToDelete; iDele++)
    {
      StSstPoint *currKeep = this->first();
      Int_t iKeep = 0;
      for (iKeep =0 ; ((iKeep < this->getSize())&&((currDele->getNId())!=(currKeep->getNId()))); iKeep++)
	{
	  currKeep = list->next(currKeep);
	}
      //if (currDele->getNPoint()==currKeep->getNPoint())
      //StSstPoint->NId is used for the simulation, not NPoint 
      //this method is used only for simulation
	if (currDele->getNId()==currKeep->getNId()) 
	{
	  this->removePoint(currKeep);
	}
      currDele = list->next(currDele);
    }
  return this;
}

StSstPointList* StSstPointList::removeMultipleCount()
{
  Int_t localSize = this->getSize();
  if (localSize < 2) return this;
  StSstPoint *ptBigLoop = this->first();
  
  while ((ptBigLoop != this->last())&&(ptBigLoop != 0))
    {
      StSstPoint *ptSmallLoop = this->next(ptBigLoop);

      while (ptSmallLoop!=0)
	{
	  //if (ptSmallLoop->getNPoint() == ptBigLoop->getNPoint()) 
	  //StSstPoint->NId is used for the simulation, not NPoint 
	  //this method is used only for simulation
	    if (ptSmallLoop->getNId() == ptBigLoop->getNId()) 
	    {
	      StSstPoint *temp = ptSmallLoop;
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

StSstPointList* StSstPointList::sortPoint()
{
  Int_t localSize=this->getSize();
  if (localSize<2) return this;
  
  StSstPoint *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSstPoint *ptB1 = ptCurr;
      StSstPoint *ptB2;
      Int_t isCont = 1;
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

Int_t StSstPointList::renumHits(Int_t last)
{
  Int_t localSize = this->getSize();
  if (!localSize) return last;
  StSstPoint *ptr = this->first();
  for (Int_t iPt = 0; iPt < localSize; iPt++)
    {
      ptr->setNPoint(last + iPt + 1);
      ptr = this->next(ptr);
    }
  return (last + localSize);
}

StSstPointList::StSstPointList(const StSstPointList & originalPointList)
{
  mListLength = originalPointList.mListLength;
  mFirstPoint     = originalPointList.mFirstPoint;
  mLastPoint      = originalPointList.mLastPoint;
}

StSstPointList& StSstPointList::operator=(const StSstPointList originalPointList)
{
  mListLength = originalPointList.mListLength;
  mFirstPoint     = originalPointList.mFirstPoint;
  mLastPoint      = originalPointList.mLastPoint;

  return *this;
}



#include "StSceListCluster.hh"

StSceListCluster::StSceListCluster()
{
  mListLength = 0;
  mFirstC     = 0;
  mLastC      = 0;
}

StSceListCluster::~StSceListCluster()
{
  if (mListLength)
    {
      StSceCluster *ptr = mLastC;
      StSceCluster *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSceCluster* StSceListCluster::next(StSceCluster *ptr)
{  return ptr->getNextCluster(); }

StSceCluster* StSceListCluster::prev(StSceCluster *ptr)
{  return ptr->getPrevCluster(); }

StSceCluster* StSceListCluster::first()
{  return mFirstC; }

StSceCluster* StSceListCluster::last()
{  return mLastC; }

int StSceListCluster::getSize()
{  return mListLength; }

int StSceListCluster::addNewCluster(StSceCluster *ptr)
{
  if (!ptr) return 0;
  if (!this->mListLength)
    {
      ptr->setPrevCluster(0);
      ptr->setNextCluster(0);
      mFirstC = ptr;
      mLastC  = ptr;
    }
  else
    {
      ptr->setPrevCluster(mLastC);
      ptr->setNextCluster(0);
      mLastC->setNextCluster(ptr);
      mLastC = ptr;
    }
  mListLength++;
  return 1;
}

int StSceListCluster::removeCluster(StSceCluster *ptr)
{
  if (!this->getSize()) return 0;
  StSceCluster *ptBefore = ptr->getPrevCluster();
  StSceCluster *ptAfter  = ptr->getNextCluster();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstC =     0;
	  this->mLastC  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstC = ptAfter;
	  ptAfter->setPrevCluster(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter== 0)
	{
	  this->mLastC = ptBefore;
	  ptBefore->setNextCluster(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextCluster(ptAfter);
	  ptAfter->setPrevCluster(ptBefore);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }

}

void StSceListCluster::exchangeTwoClusters(StSceCluster *ptr1, StSceCluster *ptr2)
{
  int i = 0;
  StSceCluster *ptrTmp = ptr1->giveCopy();

  ptr1->setNCluster(ptr2->getNCluster()) ;
  ptr1->setFirstStrip(ptr2->getFirstStrip()) ;
  ptr1->setClusterSize(ptr2->getClusterSize());
  ptr1->setTotAdc(ptr2->getTotAdc());
  ptr1->setFirstAdc(ptr2->getFirstAdc());
  ptr1->setLastAdc(ptr2->getLastAdc());
  ptr1->setTotNoise(ptr2->getTotNoise());
  ptr1->setStripMean(ptr2->getStripMean());
  ptr1->setFlag(ptr2->getFlag());
  for(i=0;i<5;i++)
    ptr1->setIdMcHit(ptr2->getIdMcHit(i),i);


  ptr2->setNCluster(ptrTmp->getNCluster()) ;
  ptr2->setFirstStrip(ptrTmp->getFirstStrip()) ;
  ptr2->setClusterSize(ptrTmp->getClusterSize());
  ptr2->setTotAdc(ptrTmp->getTotAdc());
  ptr2->setFirstAdc(ptrTmp->getFirstAdc());
  ptr2->setLastAdc(ptrTmp->getLastAdc());
  ptr2->setTotNoise(ptrTmp->getTotNoise());
  ptr2->setStripMean(ptr1->getStripMean());
  ptr2->setFlag(ptrTmp->getFlag());
  for(i=0;i<5;i++)
    ptr2->setIdMcHit(ptrTmp->getIdMcHit(i),i);
}

void StSceListCluster::sortCluster()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StSceCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSceCluster *ptB1 = ptCurr;
      StSceCluster *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getFirstStrip() > ptB1->getFirstStrip())
	    {
	      this->exchangeTwoClusters(ptB1,ptB2);
	      ptB1 = ptB2;
	    }
	  else
	    {
	      isCont = 0;
	    }
	}
      ptCurr = this->next(ptCurr);
      
    }
  return;
}

void StSceListCluster::renumCluster()
{
  int  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StSceCluster *CurrCluster = this->first();
  for (int i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}

int StSceListCluster::isSorted()
{
  StSceCluster *ptr1 = this->first();
  StSceCluster *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getFirstStrip()>ptr2->getFirstStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

StSceListCluster::StSceListCluster(const StSceListCluster & originalListCluster)
{
  mListLength = originalListCluster.mListLength;
  mFirstC     = originalListCluster.mFirstC;
  mLastC      = originalListCluster.mLastC;
}

StSceListCluster& StSceListCluster::operator=(const StSceListCluster originalListCluster)
{
  mListLength = originalListCluster.mListLength;
  mFirstC     = originalListCluster.mFirstC;
  mLastC      = originalListCluster.mLastC;

  return *this;
}

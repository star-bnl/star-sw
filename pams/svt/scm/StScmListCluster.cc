#include "StScmListCluster.hh"

StScmListCluster::StScmListCluster()
{
  mListLength = 0;
  mFirstC     = 0;
  mLastC      = 0;
}

StScmListCluster::~StScmListCluster()
{
  if (mListLength)
    {
      StScmCluster *ptr = mLastC;
      StScmCluster *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StScmCluster* StScmListCluster::next(StScmCluster *ptr)
{  return ptr->getNextCluster(); }

StScmCluster* StScmListCluster::prev(StScmCluster *ptr)
{  return ptr->getPrevCluster(); }

StScmCluster* StScmListCluster::first()
{  return mFirstC; }

StScmCluster* StScmListCluster::last()
{  return mLastC; }

int StScmListCluster::getSize()
{  return mListLength; }

int StScmListCluster::addNewCluster(StScmCluster *ptr)
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

int StScmListCluster::removeCluster(StScmCluster *ptr)
{
  if (!this->getSize()) return 0;
  StScmCluster *ptBefore = ptr->getPrevCluster();
  StScmCluster *ptAfter  = ptr->getNextCluster();
  
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

void StScmListCluster::exchangeTwoClusters(StScmCluster *ptr1, StScmCluster *ptr2)
{
  int i = 0;
  StScmCluster *ptrTmp = ptr1->giveCopy();

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

void StScmListCluster::sortCluster()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StScmCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StScmCluster *ptB1 = ptCurr;
      StScmCluster *ptB2;
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

void StScmListCluster::renumCluster()
{
  int  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StScmCluster *CurrCluster = this->first();
  for (int i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}

int StScmListCluster::isSorted()
{
  StScmCluster *ptr1 = this->first();
  StScmCluster *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getFirstStrip()>ptr2->getFirstStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

StScmListCluster::StScmListCluster(const StScmListCluster & originalListCluster)
{
  mListLength = originalListCluster.mListLength;
  mFirstC     = originalListCluster.mFirstC;
  mLastC      = originalListCluster.mLastC;
}

StScmListCluster& StScmListCluster::operator=(const StScmListCluster originalListCluster)
{
  mListLength = originalListCluster.mListLength;
  mFirstC     = originalListCluster.mFirstC;
  mLastC      = originalListCluster.mLastC;

  return *this;
}

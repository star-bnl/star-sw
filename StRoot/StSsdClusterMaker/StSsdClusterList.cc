#include "StSsdClusterList.hh"

StSsdClusterList::StSsdClusterList()
{
  mListLength   = 0;
  mFirstCluster = 0;
  mLastCluster  = 0;
}

StSsdClusterList::~StSsdClusterList()
{
  if (mListLength)
    {
      StSsdCluster *ptr = mLastCluster;
      StSsdCluster *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr    = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSsdCluster* StSsdClusterList::next(StSsdCluster *ptr)
{  return ptr->getNextCluster(); }

StSsdCluster* StSsdClusterList::prev(StSsdCluster *ptr)
{  return ptr->getPrevCluster(); }

StSsdCluster* StSsdClusterList::first()
{  return mFirstCluster; }

StSsdCluster* StSsdClusterList::last()
{  return mLastCluster; }

int StSsdClusterList::addNewCluster(StSsdCluster *ptr)
{
  if (!ptr) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevCluster(0);
      ptr->setNextCluster(0);
      mFirstCluster = ptr;
      mLastCluster  = ptr;
    }
  else
    {
      ptr->setPrevCluster(mLastCluster);
      ptr->setNextCluster(0);
      mLastCluster->setNextCluster(ptr);
      mLastCluster = ptr;
    }
  mListLength++;
  return 1;
}

void StSsdClusterList::exchangeTwoClusters(StSsdCluster *ptr1,StSsdCluster *ptr2)
{
  StSsdCluster *ptrTemp = new StSsdCluster(ptr1->getNCluster());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StSsdClusterList::sortCluster()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StSsdCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSsdCluster *ptB1 = ptCurr;
      StSsdCluster *ptB2;
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

void StSsdClusterList::renumCluster()
{
  int  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StSsdCluster *CurrCluster = this->first();
  for (int i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}

int StSsdClusterList::removeCluster(StSsdCluster *ptr)
{
  if (!this->getSize()) return 0;
  StSsdCluster *ptBefore = ptr->getPrevCluster();
  StSsdCluster *ptAfter  = ptr->getNextCluster();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstCluster =     0;
	  this->mLastCluster  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstCluster = ptAfter;
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
	  this->mLastCluster = ptBefore;
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

int StSsdClusterList::getSize()
{  return mListLength; }

int StSsdClusterList::splitCluster(scf_ctrl_st *scf_ctrl, StSsdCluster *CurrentCluster, int *ListAdc, StSsdStripList *currentStripList)
{
  int  CurrentClusterSize = CurrentCluster->getClusterSize();
  if (CurrentClusterSize<3) return 0;
  int nSubCluster = 0;
  int isClimbOrFall = 0;
  
  float testTolerance = scf_ctrl[0].testTolerance;//20%
  int *minima = new int[CurrentClusterSize];
  int *maxima = new int[CurrentClusterSize];
  int *keyToIdStrip = new int[CurrentClusterSize];
  int nMinima = 0;
  int nMaxima = 0; 
  int iStrip = 0;
 
  keyToIdStrip[0] = CurrentCluster->getFirstStrip();
  
  for(iStrip = 1; iStrip<CurrentClusterSize; iStrip++)
    {
      keyToIdStrip[iStrip] = keyToIdStrip[iStrip-1]+1;
    }

  for(iStrip = 0; iStrip<CurrentClusterSize; iStrip++)
    {
      minima[iStrip] = 0;
      maxima[iStrip] = 0;
    }
  iStrip = 0;

  for (iStrip = 0; iStrip<CurrentClusterSize-1; iStrip++) // we check both iStrip and iStrip+1
    {
      if (ListAdc[iStrip]<ListAdc[iStrip+1])
	{
	  if ((isClimbOrFall == -1) || (isClimbOrFall == 0))
	    {
	      isClimbOrFall = 1;
	      minima[nMinima] = iStrip;
	      nMinima++;
	    }
	  if ((isClimbOrFall == 1) && (iStrip+2 == CurrentClusterSize))
	    {
	      maxima[nMaxima] = iStrip+1;
	      nMaxima++;
	    }
	}
      if (ListAdc[iStrip]>ListAdc[iStrip+1])
	{
	  if ((isClimbOrFall == 1) || (isClimbOrFall == 0))
	    {
	      isClimbOrFall = -1;
	      maxima[nMaxima] = iStrip;
	      nMaxima++;
	    }
	  if ((isClimbOrFall == -1) && (iStrip+2 == CurrentClusterSize))
	    {
	      minima[nMinima] = iStrip+1;
	      nMinima++;
	    }
	}
    }
  
  if (nMaxima<2) 
    {
      delete [] maxima;
      delete [] minima;
      delete [] keyToIdStrip;
      return 0;
    }
  int iMaxima = 0;
  int maxLeft = 0;
  int maxRight =0;
  int localFirstStrip = CurrentCluster->getFirstStrip();
  float weight = 1.;

  for (iMaxima = 0; iMaxima<nMaxima-1 ; iMaxima++)
    {
      maxLeft  = maxima[iMaxima];
      maxRight = maxima[iMaxima+1];
      int iMinima = 0;
      while (!(minima[iMinima]>maxLeft && minima[iMinima]<maxRight) && iMinima<nMinima) iMinima++;
      
      if (iMinima == nMinima)
	{
	  cout<<"Big Bug in StSsdClusterList -> I do not find any minima between the two maxima !"<<endl;
	  return 0;
	}
      
      // process strip in the Left direction
      int theEnd = 0;
      int iStripLeft  = minima[iMinima]-1;
      int iStripRight = minima[iMinima]+1; 
      int iSuccess = 1;
      int addLeft = 0;
      int addRight = 0;
      int nStripInTheGroup = 1;
      float meanAdc = (float)ListAdc[minima[iMinima]];
      localFirstStrip = CurrentCluster->getFirstStrip();
      weight = 1.;

      while(!theEnd)
	{
	  if (fabs(((float)ListAdc[iStripLeft]-meanAdc)/meanAdc)<testTolerance)
	    {
	      if (iStripLeft > maxLeft)
		{
		  addLeft = 1;
		}
	      else 
		{
		  theEnd = 1;
		  iSuccess = 0;
		}
	    }
	  if (fabs(((float)ListAdc[iStripRight]-meanAdc)/meanAdc)<testTolerance)
	    {
	      if (iStripRight < maxRight)
		{
		  addRight = 1;
		}
	      else 
		{
		  theEnd = 1;
		  iSuccess = 0; 
		}
	    }
	  
	  if ((!addLeft)&&(!addRight)) theEnd =1;
	  if (addLeft)
	    {
	      meanAdc = (((float)nStripInTheGroup)*meanAdc + (float)ListAdc[iStripLeft])/((float)(nStripInTheGroup+1));
	      nStripInTheGroup++;
	      iStripLeft--;
	      addLeft = 0;
	    }
	  if (addRight)
	    {
	      meanAdc = (((float)nStripInTheGroup)*meanAdc + (float)ListAdc[iStripRight])/((float)(nStripInTheGroup+1));
	      nStripInTheGroup++;
	      iStripRight++;
	      addRight = 0;
	    }
	  
	}

      if (iSuccess)
	{

	  if (nStripInTheGroup%2==0)
	    {
	      StSsdCluster *newCluster = new StSsdCluster(this->getSize());
	      newCluster->setFlag(1);
	      int currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(nStripInTheGroup/2)); currentStrip++)
		{		  
		  newCluster->update(currentStripList->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;	     
		}
	      this->addNewCluster(newCluster);
	    }
	  else
	    {
	      StSsdCluster *newCluster = new StSsdCluster(this->getSize());
	      newCluster->setFlag(1);
	      int currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(int)(nStripInTheGroup/2)); currentStrip++)
		{
		  newCluster->update(currentStripList->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;
		}
	      weight=0.5;
	      newCluster->update(currentStripList->getStrip(localFirstStrip),weight);//last strip
	      this->addNewCluster(newCluster);
	    }
	  nSubCluster++;
	}
    }

    if(nSubCluster)
      {
	StSsdCluster *newCluster = new StSsdCluster(this->getSize());
	newCluster->setFlag(1);
	int currentStrip = localFirstStrip;
	for (currentStrip = localFirstStrip; currentStrip<(CurrentCluster->getFirstStrip()+CurrentCluster->getClusterSize()); currentStrip++)
	  {		  
	    newCluster->update(currentStripList->getStrip(currentStrip),weight);
	    weight=1.;
	  }
	this->addNewCluster(newCluster);
	nSubCluster++;
      }
    delete [] maxima;
    delete [] minima;
    delete [] keyToIdStrip;
    return nSubCluster;
}

int StSsdClusterList::isSorted()
{
  StSsdCluster *ptr1 = this->first();
  StSsdCluster *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getFirstStrip()>ptr2->getFirstStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

StSsdClusterList::StSsdClusterList(const StSsdClusterList & originalClusterList)
{
  mListLength   = originalClusterList.mListLength;
  mFirstCluster = originalClusterList.mFirstCluster;
  mLastCluster  = originalClusterList.mLastCluster;
}

StSsdClusterList& StSsdClusterList::operator=(const StSsdClusterList originalClusterList)
{
  mListLength   = originalClusterList.mListLength;
  mFirstCluster = originalClusterList.mFirstCluster;
  mLastCluster  = originalClusterList.mLastCluster;

  return *this;
}

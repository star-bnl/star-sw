#include "StScfListCluster.hh"
StScfListCluster::StScfListCluster()
{
  mListLength = 0;
  mFirstS=0;
  mLastS=0;
}

StScfListCluster::~StScfListCluster()
{
  if (mListLength)
    {
      StScfCluster *ptr = mLastS;
      StScfCluster *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}


StScfCluster* StScfListCluster::next(StScfCluster *ptr)
{  return ptr->getNextCluster(); }

StScfCluster* StScfListCluster::prev(StScfCluster *ptr)
{  return ptr->getPrevCluster(); }

StScfCluster* StScfListCluster::first()
{  return mFirstS; }

StScfCluster* StScfListCluster::last()
{  return mLastS; }

int StScfListCluster::getSize()
{  return mListLength; }

int StScfListCluster::addNewCluster(StScfCluster *ptr)
{
  if (!ptr) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevCluster(0);
      ptr->setNextCluster(0);
      mFirstS = ptr;
      mLastS  = ptr;
    }
  else
    {
      ptr->setPrevCluster(mLastS);
      ptr->setNextCluster(0);
      mLastS->setNextCluster(ptr);
      mLastS = ptr;
    }
  mListLength++;
  return 1;
}

int StScfListCluster::removeCluster(StScfCluster *ptr)
{
  if (!this->getSize()) return 0;
  StScfCluster *ptBefore = ptr->getPrevCluster();
  StScfCluster *ptAfter  = ptr->getNextCluster();
  
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
	  this->mLastS = ptBefore;
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


void StScfListCluster::exchangeTwoClusters(StScfCluster *ptr1,StScfCluster *ptr2)
{
  StScfCluster *ptrTemp = new StScfCluster(ptr1->getNCluster());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StScfListCluster::sortCluster()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StScfCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StScfCluster *ptB1 = ptCurr;
      StScfCluster *ptB2;
      int is_cont = 1;
      while ((ptB1 != this->first())&&(is_cont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getFirstStrip() > ptB1->getFirstStrip())
	    {
	      this->exchangeTwoClusters(ptB1,ptB2);
	      ptB1 = ptB2;
	    }
	  else
	    {
	      is_cont = 0;
	    }
	}
      ptCurr = this->next(ptCurr);
      
    }
  return;
}

void StScfListCluster::renumCluster()
{
  int  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StScfCluster *CurrCluster = this->first();
  for (int i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}


int StScfListCluster::splitCluster(scf_ctrl_st *scf_ctrl, StScfCluster *CurrentCluster, int *ListAdc, StScfListStrip *currentListStrip)
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
	  cout<<"big bug -> je ne trouve pas de minima entre mes deux maximas"<<endl;
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
	      StScfCluster *newCluster = new StScfCluster(this->getSize());
	      newCluster->setFlag(1);
	      int currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(nStripInTheGroup/2)); currentStrip++)
		{		  
		  newCluster->update(currentListStrip->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;	     
		}
	      this->addNewCluster(newCluster);
	    }
	  else
	    {
	      StScfCluster *newCluster = new StScfCluster(this->getSize());
	      newCluster->setFlag(1);
	      int currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(int)(nStripInTheGroup/2)); currentStrip++)
		{
		  newCluster->update(currentListStrip->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;
		}
	      weight=0.5;
	      newCluster->update(currentListStrip->getStrip(localFirstStrip),weight);//last strip
	      this->addNewCluster(newCluster);
	    }
	  nSubCluster++;
	}
    }

    if(nSubCluster)
      {
	StScfCluster *newCluster = new StScfCluster(this->getSize());
	newCluster->setFlag(1);
	int currentStrip = localFirstStrip;
	for (currentStrip = localFirstStrip; currentStrip<(CurrentCluster->getFirstStrip()+CurrentCluster->getClusterSize()); currentStrip++)
	  {		  
	    newCluster->update(currentListStrip->getStrip(currentStrip),weight);
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

int StScfListCluster::isSorted()
{
  StScfCluster *ptr1 = this->first();
  StScfCluster *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getFirstStrip()>ptr2->getFirstStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

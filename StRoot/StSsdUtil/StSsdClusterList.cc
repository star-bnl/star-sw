// $Id: StSsdClusterList.cc,v 1.2 2007/01/08 18:14:06 bouchet Exp $
//
// $Log: StSsdClusterList.cc,v $
// Revision 1.2  2007/01/08 18:14:06  bouchet
// correction in the clusters splitting : the size (number of strips) of the clusters found after splitting the original cluster in several are not successively
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:23:46  lmartin
// missing CVS header added
//

#include "StSsdUtil/StSsdClusterList.hh"
#include "StSsdUtil/StSsdClusterControl.h"
#include "StSsdUtil/StSsdCluster.hh"
#include "StSsdStripList.hh"
#include <Stiostream.h>
#include <math.h>

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

Int_t StSsdClusterList::addNewCluster(StSsdCluster *ptr)
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
  Int_t localSize=this->getSize();
  if (localSize<2) return;
  
  StSsdCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSsdCluster *ptB1 = ptCurr;
      StSsdCluster *ptB2;
      Int_t isCont = 1;
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
  Int_t  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StSsdCluster *CurrCluster = this->first();
  for (Int_t i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}

Int_t StSsdClusterList::removeCluster(StSsdCluster *ptr)
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

Int_t StSsdClusterList::getSize()
{  return mListLength; }

Int_t StSsdClusterList::splitCluster(StSsdClusterControl *clusterControl, StSsdCluster *CurrentCluster, Int_t *ListAdc, StSsdStripList *currentStripList)
{
  Int_t  CurrentClusterSize = CurrentCluster->getClusterSize();
  if (CurrentClusterSize<3) return 0;
  Int_t nSubCluster = 0;
  Int_t isClimbOrFall = 0;
  
  Float_t testTolerance = clusterControl->getTestTolerance();//20%
  Int_t *minima = new int[CurrentClusterSize];
  Int_t *maxima = new int[CurrentClusterSize];
  Int_t *keyToIdStrip = new int[CurrentClusterSize];
  Int_t nMinima = 0;
  Int_t nMaxima = 0; 
  Int_t iStrip = 0;
 
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
  Int_t iMaxima = 0;
  Int_t maxLeft = 0;
  Int_t maxRight =0;
  Int_t localFirstStrip = CurrentCluster->getFirstStrip();
  Float_t weight = 1.;
  Int_t strip_offset = 0;//JB : 01/08/2007 : update the current strip when the splitting of clusters is done
  Int_t strip_diff   = 0;
  for (iMaxima = 0; iMaxima<nMaxima-1 ; iMaxima++)
    {
      maxLeft  = maxima[iMaxima];
      maxRight = maxima[iMaxima+1];
      Int_t iMinima = 0;
      while (!(minima[iMinima]>maxLeft && minima[iMinima]<maxRight) && iMinima<nMinima) iMinima++;
      
      if (iMinima == nMinima)
	{
	  cout<<"Big Bug in StSsdClusterList -> I do not find any minima between the two maxima !"<<endl;
	  return 0;
	}
      
      // process strip in the Left direction
      Int_t theEnd = 0;
      Int_t iStripLeft  = minima[iMinima]-1;
      Int_t iStripRight = minima[iMinima]+1; 
      Int_t iSuccess = 1;
      Int_t addLeft = 0;
      Int_t addRight = 0;
      Int_t nStripInTheGroup = 1;
      Float_t meanAdc = (float)ListAdc[minima[iMinima]];
      localFirstStrip = CurrentCluster->getFirstStrip()+strip_offset;
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
	      Int_t currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(nStripInTheGroup/2)); currentStrip++)
		{		  
		  newCluster->update(currentStripList->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;
		  strip_diff++;	     
		}
	      this->addNewCluster(newCluster);
	    }
	  else
	    {
	      StSsdCluster *newCluster = new StSsdCluster(this->getSize());
	      newCluster->setFlag(1);
	      Int_t currentStrip = localFirstStrip;
	      for (currentStrip = localFirstStrip; currentStrip<=(keyToIdStrip[iStripLeft]+(int)(nStripInTheGroup/2)); currentStrip++)
		{
		  newCluster->update(currentStripList->getStrip(currentStrip),weight);
		  weight=1.;
		  localFirstStrip=currentStrip+1;
		  strip_diff++;
		}
	      weight=0.5;
	      newCluster->update(currentStripList->getStrip(localFirstStrip),weight);//last strip
	      this->addNewCluster(newCluster);
	    }
	  nSubCluster++;
	}
      strip_offset = strip_diff;
    }

    if(nSubCluster)
      {
	StSsdCluster *newCluster = new StSsdCluster(this->getSize());
	newCluster->setFlag(1);
	Int_t currentStrip = localFirstStrip;
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

Int_t StSsdClusterList::isSorted()
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

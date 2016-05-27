//$Id: StSstClusterList.cc,v 1.3 2016/05/26 14:10:34 bouchet Exp $
//
//$Log: StSstClusterList.cc,v $
//Revision 1.3  2016/05/26 14:10:34  bouchet
//cpp-check for leak in splitCluster()
//
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:31  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstUtil/StSstClusterList.hh"
#include "StSstUtil/StSstClusterControl.h"
#include "StSstUtil/StSstCluster.hh"
#include "StSstStripList.hh"
#include "St_base/Stiostream.h"
#include <math.h>

StSstClusterList::StSstClusterList()
{
  mListLength   = 0;
  mFirstCluster = 0;
  mLastCluster  = 0;
}

StSstClusterList::~StSstClusterList()
{
  if (mListLength)
    {
      StSstCluster *ptr = mLastCluster;
      StSstCluster *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr    = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSstCluster* StSstClusterList::next(StSstCluster *ptr)
{  return ptr->getNextCluster(); }

StSstCluster* StSstClusterList::prev(StSstCluster *ptr)
{  return ptr->getPrevCluster(); }

StSstCluster* StSstClusterList::first()
{  return mFirstCluster; }

StSstCluster* StSstClusterList::last()
{  return mLastCluster; }

Int_t StSstClusterList::addNewCluster(StSstCluster *ptr)
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

void StSstClusterList::exchangeTwoClusters(StSstCluster *ptr1,StSstCluster *ptr2)
{
  StSstCluster *ptrTemp = new StSstCluster(ptr1->getNCluster());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StSstClusterList::sortCluster()
{
  Int_t localSize=this->getSize();
  if (localSize<2) return;
  
  StSstCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSstCluster *ptB1 = ptCurr;
      StSstCluster *ptB2;
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

void StSstClusterList::renumCluster()
{
  Int_t  CurrentListSize = this->getSize();
  if (!CurrentListSize) return;
  StSstCluster *CurrCluster = this->first();
  for (Int_t i = 0; i < CurrentListSize; i++)
    {
      CurrCluster->setNCluster(i);
      CurrCluster = this->next(CurrCluster);
    }
  return;
}

Int_t StSstClusterList::removeCluster(StSstCluster *ptr)
{
  if (!this->getSize()) return 0;
  StSstCluster *ptBefore = ptr->getPrevCluster();
  StSstCluster *ptAfter  = ptr->getNextCluster();
  
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

Int_t StSstClusterList::getSize()
{  return mListLength; }

Int_t StSstClusterList::splitCluster(StSstClusterControl *clusterControl, StSstCluster *CurrentCluster, Int_t *ListAdc, StSstStripList *currentStripList)
{
  Int_t  CurrentClusterSize = CurrentCluster->getClusterSize();
  if (CurrentClusterSize<3) return 0;
  Int_t nSubCluster = 0;
  Int_t isClimbOrFall = 0;
  
  Float_t testTolerance = clusterControl->getTestTolerance();//20%
  vector<int> minima(CurrentClusterSize,0);
  vector<int> maxima(CurrentClusterSize,0);
  vector<int> keyToIdStrip(CurrentClusterSize,0);
  Int_t nMinima = 0;
  Int_t nMaxima = 0; 
  Int_t iStrip = 0;
 
  keyToIdStrip[0] = CurrentCluster->getFirstStrip();
  
  for(iStrip = 1; iStrip<CurrentClusterSize; iStrip++)
    {
      keyToIdStrip[iStrip] = keyToIdStrip[iStrip-1]+1;
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
  
  if (nMaxima<2){
    if(maxima.size()>0) maxima.clear();
    if(minima.size()>0) minima.clear();
    if(keyToIdStrip.size()>0) keyToIdStrip.clear();
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
	  cout<<"Big Bug in StSstClusterList -> I do not find any minima between the two maxima !"<<endl;
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
	      StSstCluster *newCluster = new StSstCluster(this->getSize());
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
	      StSstCluster *newCluster = new StSstCluster(this->getSize());
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
      StSstCluster *newCluster = new StSstCluster(this->getSize());
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
  if(maxima.size()>0) maxima.clear();
  if(minima.size()>0) minima.clear();
  if(keyToIdStrip.size()>0) keyToIdStrip.clear();
  
  return nSubCluster;
}

Int_t StSstClusterList::isSorted()
{
  StSstCluster *ptr1 = this->first();
  StSstCluster *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getFirstStrip()>ptr2->getFirstStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

StSstClusterList::StSstClusterList(const StSstClusterList & originalClusterList)
{
  mListLength   = originalClusterList.mListLength;
  mFirstCluster = originalClusterList.mFirstCluster;
  mLastCluster  = originalClusterList.mLastCluster;
}

StSstClusterList& StSstClusterList::operator=(const StSstClusterList originalClusterList)
{
  mListLength   = originalClusterList.mListLength;
  mFirstCluster = originalClusterList.mFirstCluster;
  mLastCluster  = originalClusterList.mLastCluster;

  return *this;
}

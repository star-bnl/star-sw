#include "StSsdStripList.hh"
#include <Stiostream.h>
#include "StSsdStrip.hh"
#include "StSsdDynamicControl.h"


StSsdStripList::StSsdStripList()
{
  mListLength = 0;
  mFirstStrip=0;
  mLastStrip=0;
}

StSsdStripList::~StSsdStripList()
{
  if (mListLength)
    {
      StSsdStrip *ptr = mLastStrip;
      StSsdStrip *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}


StSsdStrip* StSsdStripList::next(StSsdStrip *ptr)
{  return ptr->getNextStrip(); }

StSsdStrip* StSsdStripList::prev(StSsdStrip *ptr)
{  return ptr->getPrevStrip(); }

StSsdStrip* StSsdStripList::first()
{  return mFirstStrip; }

StSsdStrip* StSsdStripList::last()
{  return mLastStrip; }

StSsdStrip* StSsdStripList::getStrip(int idStrip)
{
  StSsdStrip *currentStrip = this->first();
  while(currentStrip)
    {
      if(currentStrip->getNStrip()==idStrip) return currentStrip;
      currentStrip=this->next(currentStrip);       
    }
  cout<<"No match in getStrip"<<endl;
  return 0;
}

int StSsdStripList::getSize()
{  return mListLength; }

int StSsdStripList::addNewStrip(StSsdStrip *ptr)
{
  if (ptr->getNStrip() == 0) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevStrip(0);
      ptr->setNextStrip(0);
      mFirstStrip = ptr;
      mLastStrip  = ptr;
    }
  else
    {
      ptr->setPrevStrip(mLastStrip);
      ptr->setNextStrip(0);
      mLastStrip->setNextStrip(ptr);
      mLastStrip = ptr;
    }
  mListLength++;
  return 1;
}

int StSsdStripList::removeStrip(StSsdStrip *ptr)
{
  if (!this->getSize()) return 0;
  StSsdStrip *ptBefore = ptr->getPrevStrip();
  StSsdStrip *ptAfter  = ptr->getNextStrip();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstStrip =     0;
	  this->mLastStrip  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstStrip = ptAfter;
	  ptAfter->setPrevStrip(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter== 0)
	{
	  this->mLastStrip = ptBefore;
	  ptBefore->setNextStrip(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextStrip(ptAfter);
	  ptAfter->setPrevStrip(ptBefore);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }

}

void StSsdStripList::exchangeTwoStrips(StSsdStrip *ptr1, StSsdStrip *ptr2)
{
  StSsdStrip *ptrTemp = new StSsdStrip(ptr1->getNStrip(), ptr1->getDigitSig(), ptr1->getSigma());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StSsdStripList::sortStrip()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StSsdStrip *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSsdStrip *ptB1 = ptCurr;
      StSsdStrip *ptB2;
      int is_cont = 1;
      while ((ptB1 != this->first())&&(is_cont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNStrip() > ptB1->getNStrip())
	    {
	      this->exchangeTwoStrips(ptB1,ptB2);
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

int* StSsdStripList::getListAdc(int idStrip, int sizeCluster)
{
  StSsdStrip *CurrentStrip = this->first();
  int* localListAdc = new int[sizeCluster];
  while((CurrentStrip->getNStrip()!=idStrip)&&(CurrentStrip)) CurrentStrip = this->next(CurrentStrip);
  if (!CurrentStrip) return localListAdc;
  int iStrip = 0;
  for (iStrip=0; iStrip<sizeCluster;iStrip++) 
    {
      localListAdc[iStrip]=CurrentStrip->getDigitSig();
      CurrentStrip = this->next(CurrentStrip);
    }
  return localListAdc;
}


void StSsdStripList::setSigma(int iStrip, int iSigma, StSsdDynamicControl *dynamicControl)
{
  const int     NAdcChannel             = 1 << (dynamicControl->getNBitEncoding()); // 1 << x == 2^x 
  const float   conversionFactor = (float)(NAdcChannel)/(dynamicControl->getADCDynamic()*dynamicControl->getNElectronInAMip());
  
  StSsdStrip *currentStrip=this->first(); 
  while((currentStrip) && (currentStrip->getNStrip()!=iStrip))
    currentStrip=this->next(currentStrip);
  if(currentStrip) 
    {
      int sigmaAdc = (int)(iSigma*conversionFactor);
      if (sigmaAdc<NAdcChannel)	currentStrip->setSigma(sigmaAdc);
      else                      currentStrip->setSigma(NAdcChannel-1);
    }
  return;
}


int StSsdStripList::isSorted()
{
  StSsdStrip *ptr1 = this->first();
  StSsdStrip *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getNStrip()>ptr2->getNStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

StSsdStripList::StSsdStripList(const StSsdStripList & originalStripList)
{
  mListLength      = originalStripList.mListLength;
  mFirstStrip      = originalStripList.mFirstStrip;
  mLastStrip       = originalStripList.mLastStrip;
}

StSsdStripList& StSsdStripList::operator=(const StSsdStripList originalStripList)
{
  mListLength      = originalStripList.mListLength;
  mFirstStrip      = originalStripList.mFirstStrip;
  mLastStrip       = originalStripList.mLastStrip;

  return *this;
}

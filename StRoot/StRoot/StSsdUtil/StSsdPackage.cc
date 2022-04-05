// $Id: StSsdPackage.cc,v 1.1 2006/10/16 16:43:29 bouchet Exp $
//
// $Log: StSsdPackage.cc,v $
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.2  2005/03/18 14:21:44  lmartin
// missing CVS header added
//

#include "StSsdUtil/StSsdPackage.hh"
#include <Stiostream.h>
#include "StSsdUtil/StSsdCluster.hh"
#include "StSsdUtil/StSsdClusterControl.h"
#include <string.h>

StSsdPackage::StSsdPackage(Int_t rNPackage, StSsdClusterControl *control)
{
  Int_t maxMatcheds = control->getClusterTreat();
  mNPackage    = rNPackage;
  mListLengthM = 0;
  mFirstMatched      = 0;
  mLastMatched       = 0;
  mMatcheds    = new StSsdCluster*[maxMatcheds];
  for(Int_t i=0;i<maxMatcheds;i++) mMatcheds[i]=0;
  mKind        = new char[2*maxMatcheds+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StSsdPackage::StSsdPackage(Int_t rNPackage, Int_t rNMatched)
{
  mNPackage    = rNPackage;
  mListLengthM = rNMatched;
  mFirstMatched      = 0;
  mLastMatched       = 0;
  mMatcheds    = new StSsdCluster*[rNMatched];
  for(Int_t i=0;i<rNMatched;i++) mMatcheds[i]=0;
  mKind        = new char[2*rNMatched+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StSsdPackage::StSsdPackage(const StSsdPackage & originalPackage)
{
  Int_t i        = 0;
  mNPackage    = originalPackage.mNPackage;
  mListLengthM = originalPackage.mListLengthM;
  mFirstMatched      = originalPackage.mFirstMatched;
  mLastMatched       = originalPackage.mLastMatched;
  mMatcheds    = new StSsdCluster*[mListLengthM];
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  mKind        = new char[2*mListLengthM+1];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;
}

StSsdPackage::~StSsdPackage()
{
  delete [ ] mMatcheds;
  delete [ ] mKind;
}

StSsdPackage& StSsdPackage::operator=(const StSsdPackage originalPackage)
{
  Int_t i        = 0;
  mNPackage    = originalPackage.mNPackage;
  mListLengthM = originalPackage.mListLengthM;
  mFirstMatched      = originalPackage.mFirstMatched;
  mLastMatched       = originalPackage.mLastMatched;
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;

  return *this;
}

StSsdCluster* StSsdPackage::next(StSsdCluster *ptr)
{  return ptr->getNextCluster(); }

StSsdCluster* StSsdPackage::prev(StSsdCluster *ptr)
{  return ptr->getPrevCluster(); }

StSsdCluster* StSsdPackage::first()
{  return mFirstMatched; }

StSsdCluster* StSsdPackage::last()
{  return mLastMatched; }

void          StSsdPackage::purgePackage()
{ 
  mKind[0] = '\0';
  mListLengthM = 0;
  mPrevPackage=0;
  mNextPackage=0;
}

void          StSsdPackage::takeMatcheds(StSsdPackage *ptr)
{
  if  (this->mListLengthM == ptr->mListLengthM)
    {
      this->mFirstMatched      = ptr->mFirstMatched;
      this->mLastMatched       = ptr->mLastMatched;
      strcpy(this->mKind, ptr->mKind);
      for(Int_t i = 0;i < this->mListLengthM;i++)
	{  this->mMatcheds[i] = ptr->mMatcheds[i]; }
    }
  else
    cout<<"Problem to transfer matched clusters (not enough room) !!\n";
}

void          StSsdPackage::setNPackage(Int_t rNPackage)
{  this->mNPackage = rNPackage; }

void          StSsdPackage::setKind(char *rKind)
{  this->mKind = rKind ; }

void          StSsdPackage::setPrevPackage(StSsdPackage *rPrevPackage)
{  this->mPrevPackage = rPrevPackage ; }

void          StSsdPackage::setNextPackage(StSsdPackage *rNextPackage)
{  this->mNextPackage = rNextPackage ; }

Int_t StSsdPackage::getNPackage()
{  return this->mNPackage; }

StSsdCluster* StSsdPackage::getMatched(Int_t numMatched)
{
  if (numMatched>mListLengthM) return 0;
  return this->mMatcheds[numMatched]; 
}

char* StSsdPackage::getKind()
{  return this->mKind; }

StSsdPackage* StSsdPackage::getPrevPackage()
{  return this->mPrevPackage; }

StSsdPackage* StSsdPackage::getNextPackage()
{  return this->mNextPackage; }


Int_t StSsdPackage::addNewMatched(StSsdCluster *ptr, Int_t maxMatcheds)
{
  if ( (!ptr) || (mListLengthM == maxMatcheds) ) return 0;
   if (mListLengthM == 0)
     {
       mFirstMatched = ptr;
       mLastMatched  = ptr;
     }
  else
    {
      mLastMatched = ptr;
    }
  mMatcheds[mListLengthM] = ptr;  
  mListLengthM++;
  return 1;
}

Int_t StSsdPackage::addKindPackage(Int_t numMatched, Int_t rSide, Int_t maxMatcheds)
{
  if ( (!this->getSize()) || (mListLengthM == maxMatcheds)) return 0;
  mKind[2*(this->mListLengthM-1)]=char(48+numMatched);
  if (rSide==0)
    {
      mKind[2*(this->mListLengthM)-1]='p';
      mKind[2*(this->mListLengthM)]='\0';      
    }
  else
    {
      mKind[2*(this->mListLengthM)-1]='n';
      mKind[2*(this->mListLengthM)]='\0';
    }
  return 1; 
}

Int_t StSsdPackage::removeMatched(StSsdCluster *ptr)
{
  if (!this->getSize()) return 0;
  StSsdCluster *ptBefore = ptr->getPrevCluster();
  StSsdCluster *ptAfter  = ptr->getNextCluster();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstMatched =     0;
	  this->mLastMatched  =     0;
	  this->mListLengthM = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstMatched = ptAfter;
	  ptAfter->setPrevCluster(0);
	  this->mListLengthM--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter== 0)
	{
	  this->mLastMatched = ptBefore;
	  ptBefore->setNextCluster(0);
	  this->mListLengthM--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextCluster(ptAfter);
	  ptAfter->setPrevCluster(ptBefore);
	  this->mListLengthM--;
	  delete ptr;
	  return 1;
	}
    }

}


void StSsdPackage::exchangeTwoMatcheds(StSsdCluster *ptr1, StSsdCluster *ptr2)
{
  StSsdCluster *ptrTmp = ptr1->giveCopy();

  ptr1->setNCluster(ptr2->getNCluster()) ;
  ptr1->setFirstStrip(ptr2->getFirstStrip()) ;
  ptr1->setClusterSize(ptr2->getClusterSize());
  ptr1->setTotAdc(ptr2->getTotAdc());
  ptr1->setFirstAdc(ptr2->getFirstAdc());
  ptr1->setLastAdc(ptr2->getLastAdc());
  ptr1->setTotNoise(ptr2->getTotNoise());
  ptr1->setFlag(ptr2->getFlag());

  ptr2->setNCluster(ptrTmp->getNCluster()) ;
  ptr2->setFirstStrip(ptrTmp->getFirstStrip()) ;
  ptr2->setClusterSize(ptrTmp->getClusterSize());
  ptr2->setTotAdc(ptrTmp->getTotAdc());
  ptr2->setFirstAdc(ptrTmp->getFirstAdc());
  ptr2->setLastAdc(ptrTmp->getLastAdc());
  ptr2->setTotNoise(ptrTmp->getTotNoise());
  ptr2->setFlag(ptrTmp->getFlag());

}

void StSsdPackage::sortMatched()
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
	      this->exchangeTwoMatcheds(ptB1,ptB2);
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

void StSsdPackage::renumMatched()
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

Int_t StSsdPackage::getSize()
{  return mListLengthM; }

Int_t StSsdPackage::isSorted()
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

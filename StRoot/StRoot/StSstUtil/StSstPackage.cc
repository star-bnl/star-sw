//$Id: StSstPackage.cc,v 1.3 2016/05/30 21:43:07 bouchet Exp $
//
//$Log: StSstPackage.cc,v $
//Revision 1.3  2016/05/30 21:43:07  bouchet
//coverity : RESOURCE_LEAK fixed in exchange method
//
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#include "StSstUtil/StSstPackage.hh"
#include "St_base/Stiostream.h"
#include "StSstUtil/StSstCluster.hh"
#include "StSstUtil/StSstClusterControl.h"
#include <string.h>
#include "StMessMgr.h"

StSstPackage::StSstPackage(Int_t rNPackage, StSstClusterControl *control)
{
  Int_t maxMatcheds = control->getClusterTreat();
  mNPackage         = rNPackage;
  mListLengthM      = 0;
  mFirstMatched     = 0;
  mLastMatched      = 0;
  mMatcheds         = new StSstCluster*[maxMatcheds];
  for(Int_t i=0;i<maxMatcheds;i++) mMatcheds[i]=0;
  mKind        = new char[2*maxMatcheds+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StSstPackage::StSstPackage(Int_t rNPackage, Int_t rNMatched)
{
  mNPackage     = rNPackage;
  mListLengthM  = rNMatched;
  mFirstMatched = 0;
  mLastMatched  = 0;
  mMatcheds     = new StSstCluster*[rNMatched];
  for(Int_t i=0;i<rNMatched;i++) mMatcheds[i]=0;
  mKind        = new char[2*rNMatched+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StSstPackage::StSstPackage(const StSstPackage & originalPackage)
{
  Int_t i       = 0;
  mNPackage     = originalPackage.mNPackage;
  mListLengthM  = originalPackage.mListLengthM;
  mFirstMatched = originalPackage.mFirstMatched;
  mLastMatched  = originalPackage.mLastMatched;
  mMatcheds     = new StSstCluster*[mListLengthM];
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  mKind        = new char[2*mListLengthM+1];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;
}

StSstPackage::~StSstPackage()
{
  delete [ ] mMatcheds;
  delete [ ] mKind;
}

StSstPackage& StSstPackage::operator=(const StSstPackage originalPackage)
{
  Int_t i       = 0;
  mNPackage     = originalPackage.mNPackage;
  mListLengthM  = originalPackage.mListLengthM;
  mFirstMatched = originalPackage.mFirstMatched;
  mLastMatched  = originalPackage.mLastMatched;
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;

  return *this;
}

StSstCluster* StSstPackage::next(StSstCluster *ptr)
{  return ptr->getNextCluster(); }

StSstCluster* StSstPackage::prev(StSstCluster *ptr)
{  return ptr->getPrevCluster(); }

StSstCluster* StSstPackage::first()
{  return mFirstMatched; }

StSstCluster* StSstPackage::last()
{  return mLastMatched; }

void          StSstPackage::purgePackage()
{ 
  mKind[0] = '\0';
  mListLengthM = 0;
  mPrevPackage=0;
  mNextPackage=0;
}

void          StSstPackage::takeMatcheds(StSstPackage *ptr)
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
    LOG_INFO<<"Problem to transfer matched clusters (not enough room) !! "<< endm;
}

void          StSstPackage::setNPackage(Int_t rNPackage)
{  this->mNPackage = rNPackage; }

void          StSstPackage::setKind(char *rKind)
{  this->mKind = rKind ; }

void          StSstPackage::setPrevPackage(StSstPackage *rPrevPackage)
{  this->mPrevPackage = rPrevPackage ; }

void          StSstPackage::setNextPackage(StSstPackage *rNextPackage)
{  this->mNextPackage = rNextPackage ; }

Int_t StSstPackage::getNPackage()
{  return this->mNPackage; }

StSstCluster* StSstPackage::getMatched(Int_t numMatched)
{
  if (numMatched>mListLengthM) return 0;
  return this->mMatcheds[numMatched]; 
}

char* StSstPackage::getKind()
{  return this->mKind; }

StSstPackage* StSstPackage::getPrevPackage()
{  return this->mPrevPackage; }

StSstPackage* StSstPackage::getNextPackage()
{  return this->mNextPackage; }


Int_t StSstPackage::addNewMatched(StSstCluster *ptr, Int_t maxMatcheds)
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

Int_t StSstPackage::addKindPackage(Int_t numMatched, Int_t rSide, Int_t maxMatcheds)
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

Int_t StSstPackage::removeMatched(StSstCluster *ptr)
{
  if (!this->getSize()) return 0;
  StSstCluster *ptBefore = ptr->getPrevCluster();
  StSstCluster *ptAfter  = ptr->getNextCluster();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstMatched = 0;
	  this->mLastMatched  = 0;
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

void StSstPackage::exchangeTwoMatcheds(StSstCluster *ptr1, StSstCluster *ptr2)
{
  StSstCluster *ptrTemp = new StSstCluster(ptr1->getNCluster());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StSstPackage::sortMatched()
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

void StSstPackage::renumMatched()
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

Int_t StSstPackage::getSize()
{  return mListLengthM; }

Int_t StSstPackage::isSorted()
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

#include "StScmPackage.hh"

StScmPackage::StScmPackage(int rNPackage, scm_ctrl_st *scm_ctrl)
{
  int maxMatcheds = scm_ctrl[0].clusterTreat;
  mNPackage    = rNPackage;
  mListLengthM = 0;
  mFirstM      = 0;
  mLastM       = 0;
  mMatcheds    = new StScmCluster*[maxMatcheds];
  for(int i=0;i<maxMatcheds;i++) mMatcheds[i]=0;
  mKind        = new char[2*maxMatcheds+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StScmPackage::StScmPackage(int rNPackage, int rNMatched)
{
  mNPackage    = rNPackage;
  mListLengthM = rNMatched;
  mFirstM      = 0;
  mLastM       = 0;
  mMatcheds    = new StScmCluster*[rNMatched];
  for(int i=0;i<rNMatched;i++) mMatcheds[i]=0;
  mKind        = new char[2*rNMatched+1];
  mKind[0]     = '\0';
  mPrevPackage = 0;
  mNextPackage = 0;
}

StScmPackage::StScmPackage(const StScmPackage & originalPackage)
{
  int i        = 0;
  mNPackage    = originalPackage.mNPackage;
  mListLengthM = originalPackage.mListLengthM;
  mFirstM      = originalPackage.mFirstM;
  mLastM       = originalPackage.mLastM;
  mMatcheds    = new StScmCluster*[mListLengthM];
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  mKind        = new char[2*mListLengthM+1];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;
}

StScmPackage::~StScmPackage()
{
  delete [ ] mMatcheds;
  delete [ ] mKind;
}

StScmPackage& StScmPackage::operator=(const StScmPackage originalPackage)
{
  int i        = 0;
  mNPackage    = originalPackage.mNPackage;
  mListLengthM = originalPackage.mListLengthM;
  mFirstM      = originalPackage.mFirstM;
  mLastM       = originalPackage.mLastM;
  for(i=0;i<mListLengthM;i++) mMatcheds[i] = originalPackage.mMatcheds[i];
  for(i=0;i<2*mListLengthM+1;i++) mKind[i] = originalPackage.mKind[i];
  mPrevPackage = originalPackage.mPrevPackage;
  mNextPackage = originalPackage.mNextPackage;

  return *this;
}

StScmCluster* StScmPackage::next(StScmCluster *ptr)
{  return ptr->getNextCluster(); }

StScmCluster* StScmPackage::prev(StScmCluster *ptr)
{  return ptr->getPrevCluster(); }

StScmCluster* StScmPackage::first()
{  return mFirstM; }

StScmCluster* StScmPackage::last()
{  return mLastM; }

void          StScmPackage::purgePackage()
{ 
  mKind[0] = '\0';
  mListLengthM = 0;
  mPrevPackage=0;
  mNextPackage=0;
}

void          StScmPackage::takeMatcheds(StScmPackage *ptr)
{
  if  (this->mListLengthM == ptr->mListLengthM)
    {
      this->mFirstM      = ptr->mFirstM;
      this->mLastM       = ptr->mLastM;
      strcpy(this->mKind, ptr->mKind);
      for(int i = 0;i < this->mListLengthM;i++)
	{  this->mMatcheds[i] = ptr->mMatcheds[i]; }
    }
  else
    cout<<"Not able to take matcheds!!\n";
}

void          StScmPackage::setNPackage(int rNPackage)
{  this->mNPackage = rNPackage; }

void          StScmPackage::setKind(char *rKind)
{  this->mKind = rKind ; }

void          StScmPackage::setPrevPackage(StScmPackage *rPrevPackage)
{  this->mPrevPackage = rPrevPackage ; }

void          StScmPackage::setNextPackage(StScmPackage *rNextPackage)
{  this->mNextPackage = rNextPackage ; }

int StScmPackage::getNPackage()
{  return this->mNPackage; }

StScmCluster* StScmPackage::getMatched(int numMatched)
{
  if (numMatched>mListLengthM) return 0;
  return this->mMatcheds[numMatched]; 
}

char* StScmPackage::getKind()
{  return this->mKind; }

StScmPackage* StScmPackage::getPrevPackage()
{  return this->mPrevPackage; }

StScmPackage* StScmPackage::getNextPackage()
{  return this->mNextPackage; }


int StScmPackage::addNewMatched(StScmCluster *ptr, int maxMatcheds)
{
  if ( (!ptr) || (mListLengthM == maxMatcheds) ) return 0;
   if (mListLengthM == 0)
     {
       mFirstM = ptr;
       mLastM  = ptr;
     }
  else
    {
      mLastM = ptr;
    }
  mMatcheds[mListLengthM] = ptr;  
  mListLengthM++;
  return 1;
}

int StScmPackage::addKindPackage(int numMatched, int rSide, int maxMatcheds)
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

int StScmPackage::removeMatched(StScmCluster *ptr)
{
  if (!this->getSize()) return 0;
  StScmCluster *ptBefore = ptr->getPrevCluster();
  StScmCluster *ptAfter  = ptr->getNextCluster();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  this->mFirstM =     0;
	  this->mLastM  =     0;
	  this->mListLengthM = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstM = ptAfter;
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
	  this->mLastM = ptBefore;
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


void StScmPackage::exchangeTwoMatcheds(StScmCluster *ptr1, StScmCluster *ptr2)
{
  StScmCluster *ptrTmp = ptr1->giveCopy();

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

void StScmPackage::sortMatched()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StScmCluster *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StScmCluster *ptB1 = ptCurr;
      StScmCluster *ptB2;
      int is_cont = 1;
      while ((ptB1 != this->first())&&(is_cont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getFirstStrip() > ptB1->getFirstStrip())
	    {
	      this->exchangeTwoMatcheds(ptB1,ptB2);
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

void StScmPackage::renumMatched()
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

int StScmPackage::getSize()
{  return mListLengthM; }

int StScmPackage::isSorted()
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

#include "StSlsListStrip.hh"

StSlsListStrip::StSlsListStrip()
{
  mListLength = 0;
  mFirstS     = 0;
  mLastS      = 0;
}

StSlsListStrip::~StSlsListStrip()
{
  if (mListLength)
    {
      StSlsStrip *ptr = mLastS;
      StSlsStrip *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSlsStrip* StSlsListStrip::next(StSlsStrip *ptr)
{  return ptr->getNextStrip(); }

StSlsStrip* StSlsListStrip::prev(StSlsStrip *ptr)
{  return ptr->getPrevStrip(); }

StSlsStrip* StSlsListStrip::first()
{  return mFirstS; }

StSlsStrip* StSlsListStrip::last()
{  return mLastS; }

int StSlsListStrip::getSize()
{  return mListLength; }

int StSlsListStrip::addNewStrip(StSlsStrip *ptr)
{
  if (ptr->getNStrip() == 0) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevStrip(0);
      ptr->setNextStrip(0);
      mFirstS = ptr;
      mLastS  = ptr;
    }
  else
    {
      ptr->setPrevStrip(mLastS);
      ptr->setNextStrip(0);
      mLastS->setNextStrip(ptr);
      mLastS = ptr;
    }
  mListLength++;
  return 1;
}

int StSlsListStrip::removeStrip(StSlsStrip *ptr)
{
  if (!this->getSize()) return 0;
  StSlsStrip *ptBefore = ptr->getPrevStrip();
  StSlsStrip *ptAfter  = ptr->getNextStrip();
  
  if (ptBefore == 0)
    {
      if (ptAfter == 0)
	{
	  this->mFirstS =     0;
	  this->mLastS  =     0;
	  this->mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  this->mFirstS = ptAfter;
	  ptAfter->setPrevStrip(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter == 0)
	{
	  this->mLastS = ptBefore;
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

void StSlsListStrip::exchangeTwoStrips(StSlsStrip *ptr1, StSlsStrip *ptr2)
{
  StSlsStrip *ptrTemp = new StSlsStrip(ptr1->getNStrip(), ptr1->getIdHit(0), ptr1->getIdMcHit(0), ptr1->getIdMcTrack(0), ptr1->getAnalogSig());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StSlsListStrip::updateStrip(StSlsStrip *ptr)
{
  int localSize  = this->getSize();
  if (!localSize)
    {
     this->addNewStrip(ptr);
     return;
    }
  StSlsStrip *stripScan = this->first();
  while ((stripScan != this->last())&&(stripScan->getNStrip()!=ptr->getNStrip())) stripScan = this->next(stripScan);
  if (stripScan->getNStrip()!=ptr->getNStrip()) 
    {
      this->addNewStrip(ptr);
      return;
    }
  else
    {
      int   dum    = stripScan->getNHits();
      float tmpSig = stripScan->getAnalogSig();
      stripScan->setNHits(dum+1);
      stripScan->setAnalogSig(ptr->getAnalogSig()+tmpSig) ;
      
      if (dum<5)
	{
	  stripScan->setIdHit(ptr->getIdHit(0), dum);
	  stripScan->setIdMcHit(ptr->getIdMcHit(0), dum);  
	  stripScan->setIdMcTrack(ptr->getIdMcTrack(0), dum);	
	}
      delete ptr;
      return;
    }
}

StSlsListStrip* StSlsListStrip::addListStrip(StSlsListStrip *list)
{
  int size2 = list->getSize();
  if (!size2) return this;
  
  StSlsStrip *st1;
  StSlsStrip *st2 = list->first();
  int i = 0;
  for (i=0 ; i < size2 ; i++)
    {
      st2->copyTo(st1);
      this->addNewStrip(st1);
      st2 = list->next(st2);
    }
  return this;  
}

StSlsListStrip* StSlsListStrip::sortStrip()
{
  int localSize=this->getSize();
  if (localSize<2) return this;
  
  StSlsStrip *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSlsStrip *ptB1 = ptCurr;
      StSlsStrip *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNStrip() > ptB1->getNStrip())
	    {
	      this->exchangeTwoStrips(ptB1,ptB2);
	      ptB1 = ptB2;
	    }
	  else
	    {
	      isCont = 0;
	    }
	}
      ptCurr = this->next(ptCurr);
    }
  return this;
}

#include "StSpaListStrip.hh"
StSpaListStrip::StSpaListStrip()
{
  mListLength = 0;
  mFirstS     = 0;
  mLastS      = 0;
}

StSpaListStrip::~StSpaListStrip()
{
  if (mListLength)
    {
      StSpaStrip *ptr = mLastS;
      StSpaStrip *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}


StSpaStrip* StSpaListStrip::next(StSpaStrip *ptr)
{ return ptr->getNextStrip(); }

StSpaStrip* StSpaListStrip::prev(StSpaStrip *ptr)
{ return ptr->getPrevStrip(); }

StSpaStrip* StSpaListStrip::first()
{ return mFirstS; }

StSpaStrip* StSpaListStrip::last()
{ return mLastS; }

int StSpaListStrip::getSize()
{  return mListLength; }

int StSpaListStrip::addNewStrip(StSpaStrip *ptr)
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

int StSpaListStrip::removeStrip(StSpaStrip *ptr)
{
  if (!this->getSize()) return 0;
  StSpaStrip *ptBefore = ptr->getPrevStrip();
  StSpaStrip *ptAfter  = ptr->getNextStrip();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
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
      if (ptAfter== 0)
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

StSpaListStrip* StSpaListStrip::addListStrip(StSpaListStrip *list)
{
  int size2 = list->getSize();
  if (!size2) return this;
  
  StSpaStrip *st1;
  StSpaStrip *st2 = list->first();
  while (st2)
    {
      st1 = st2->giveCopy();
      this->addNewStrip(st1);
      st2 = list->next(st2);
    }
  return this;  
}

void StSpaListStrip::exchangeTwoStrips(StSpaStrip *ptr1,StSpaStrip *ptr2)
{
  int i = 0;
  StSpaStrip *ptrTmp = ptr1->giveCopy();

  ptr1->setNStrip(ptr2->getNStrip()) ;
  ptr1->setMcStrip(ptr2->getMcStrip()) ;
  ptr1->setAnalogSig(ptr2->getAnalogSig());
  ptr1->setDigitSig(ptr2->getDigitSig());
  for(i=0;i<5;i++)
    ptr1->setIdMcHit(ptr2->getIdMcHit(i),i);

  ptr2->setNStrip(ptrTmp->getNStrip());
  ptr2->setMcStrip(ptrTmp->getMcStrip());
  ptr2->setAnalogSig(ptrTmp->getAnalogSig());
  ptr2->setDigitSig(ptrTmp->getDigitSig());
  for(i=0;i<5;i++)
    ptr2->setIdMcHit(ptrTmp->getIdMcHit(i),i);
}

void StSpaListStrip::sortStrip()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StSpaStrip *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSpaStrip *ptB1 = ptCurr;
      StSpaStrip *ptB2;
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

void StSpaListStrip::updateListStrip(StSpaListNoise *ptr)
{
  StSpaStrip *pt1      = this->first();
  StSpaNoise *pt2      = ptr->first();
  int adcCount   = 0; 

  while (pt1)
    {
      pt2 = ptr->first();
      while (pt2&&((pt2->getNStrip())!=(pt1->getNStrip())))
	{
	  pt2 = ptr->next(pt2);
	}
      if (!pt2) 
	{
	  StSpaStrip *tmp = pt1;
	  pt1 = this->next(pt1);
	  this->removeStrip(tmp);  
	}
      else
	{
	  adcCount = pt2->getNoiseValue();
	  pt1->setDigitSig(adcCount);
	  pt2->setNStrip(-1);
	  pt1 = this->next(pt1);  
	}
    }
  pt2      = ptr->first();
  while (pt2)
    {
      if (pt2->getNStrip() < 0)
	{
	  StSpaNoise *tmpNoise = pt2;
	  pt2              = ptr->next(pt2);
	  ptr->removeNoise(tmpNoise);
	}
      else
 	{
 	  StSpaStrip *tmpStrip = new StSpaStrip(pt2->getNStrip(), pt2->getNoiseValue());
	  this->addNewStrip(tmpStrip);
 	  pt2              = ptr->next(pt2);  
 	}
    }
}

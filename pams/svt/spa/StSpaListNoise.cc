#include "StSpaListNoise.hh"
StSpaListNoise::StSpaListNoise()
{
  mListLength = 0;
  mFirstS=0;
  mLastS=0;
}

StSpaListNoise::~StSpaListNoise()
{
  if (mListLength)
    {
      StSpaNoise *ptr = mLastS;
      StSpaNoise *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}

StSpaNoise* StSpaListNoise::next(StSpaNoise *ptr)
{ return ptr->getNextNoise(); }

StSpaNoise* StSpaListNoise::prev(StSpaNoise *ptr)
{ return ptr->getPrevNoise(); }

StSpaNoise* StSpaListNoise::first()
{ return mFirstS; }

int StSpaListNoise::getSize()
{ return mListLength; }

StSpaNoise* StSpaListNoise::last()
{ return mLastS; }

int StSpaListNoise::addNewNoise(StSpaNoise *ptr)
{
  if (ptr->getNStrip() == 0) return 0;
  if (mListLength == 0)
    {
      ptr->setPrevNoise(0);
      ptr->setNextNoise(0);
      mFirstS = ptr;
      mLastS  = ptr;
    }
  else
    {
      ptr->setPrevNoise(mLastS);
      ptr->setNextNoise(0);
      mLastS->setNextNoise(ptr);
      mLastS = ptr;
    }
  mListLength++;
  return 1;
}

void StSpaListNoise::setIsActive(int rIsActive, int rNStrip)
{
  if (!(this->getSize())) return;
  StSpaNoise *ptr = this->first();
  while ((ptr)&&(ptr->getNStrip() != rNStrip))
    {
      ptr = this->next(ptr);
    }
  if (ptr) ptr->setIsActive(rIsActive);
  
}

int StSpaListNoise::removeNoise(StSpaNoise *ptr)
{
  if (!this->getSize()) return 0;
  StSpaNoise *ptBefore = ptr->getPrevNoise();
  StSpaNoise *ptAfter  = ptr->getNextNoise();
  
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
	  ptAfter->setPrevNoise(0);
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
	  ptBefore->setNextNoise(0);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextNoise(ptAfter);
	  ptAfter->setPrevNoise(ptBefore);
	  this->mListLength--;
	  delete ptr;
	  return 1;
	}
    }
}

StSpaListNoise* StSpaListNoise::addListNoise(StSpaListNoise *list)
{
  int size2 = list->getSize();
  if (!size2) return this;
  
  StSpaNoise *st1;
  StSpaNoise *st2 = list->first();
  while (st2)
    {
      st1 = st2->giveCopy();
      this->addNewNoise(st1);
      st2 = list->next(st2);
    }
  return this;  
}

void StSpaListNoise::exchangeTwoNoise(StSpaNoise *ptr1,StSpaNoise *ptr2)
{
  StSpaNoise *ptrTmp = ptr1->giveCopy();

  ptr1->setNStrip(ptr2->getNStrip());
  ptr1->setPedestal(ptr2->getPedestal());
  ptr1->setSigma(ptr2->getSigma());
  ptr1->setNoiseValue(ptr2->getNoiseValue());
  ptr1->setIsActive(ptr2->getIsActive());

  ptr2->setNStrip(ptrTmp->getNStrip());
  ptr2->setPedestal(ptrTmp->getPedestal());
  ptr2->setSigma(ptrTmp->getSigma());
  ptr2->setNoiseValue(ptrTmp->getNoiseValue());
  ptr2->setIsActive(ptrTmp->getIsActive());
}

void StSpaListNoise::sortStrip()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StSpaNoise *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSpaNoise *ptB1 = ptCurr;
      StSpaNoise *ptB2;
      int isCont = 1;
      while ((ptB1 != this->first())&&(isCont))
	{
	  ptB2 = this->prev(ptB1);
	  if (ptB2->getNStrip() > ptB1->getNStrip())
	    {
	      this->exchangeTwoNoise(ptB1,ptB2);
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

void StSpaListNoise::addSignal(StSpaListStrip *ptr,
			       long NElectronInAMip,long A128Dynamic)
{
  const int NSaturationSignal = (int)A128Dynamic*NElectronInAMip;
  int size1                     = this->getSize();
  
  if (!size1) return;
  StSpaNoise *ptr1 = this->first();
  StSpaStrip *ptr2 = ptr->first();
  int tmpNoiseValue = 0;
  while (ptr2)
    {
      while((ptr1)&&(ptr2->getNStrip() != ptr1->getNStrip()))
	{
	  ptr1 = this->next(ptr1);
	}
      if(ptr1) 
	{
	  tmpNoiseValue = ptr1->getNoiseValue();
	  ptr1->setNoiseValue(ptr2->getDigitSig() + tmpNoiseValue);
	  ptr2 = ptr->next(ptr2);  
	}
      else
	{
	  cout<<"signal and noise not matched !"<<endl;
	}
    }
  
  ptr1 = this->first();
  while (ptr1)
    {
      if(ptr1->getIsActive())
	{
	  if (ptr1->getNoiseValue() > NSaturationSignal) 
	    ptr1->setNoiseValue(NSaturationSignal);
	}
      else
	{
	  ptr1->setNoiseValue(0);
	}
      tmpNoiseValue = ptr1->getNoiseValue();
      ptr1->setNoiseValue(ptr1->getPedestal() + tmpNoiseValue);
      ptr1 = this->next(ptr1);
    }
}

void StSpaListNoise::substractPedestal()
{
  if ((this->getSize())==0) return;
  StSpaNoise *ptr = this->first();
  int tmpNoiseValue = 0;
  while (ptr)
    {
      tmpNoiseValue = ptr->getNoiseValue();
      ptr->setNoiseValue(tmpNoiseValue - ptr->getPedestal()) ;
      ptr = this->next(ptr);
    }
}

void StSpaListNoise::convertAnalogToDigit(long NElectronInAMip,long ADCDynamic,
					  long NBitEncoding, float DAQCutValue)
{
  const int     NAdcChannel             = (int)pow(2,NBitEncoding);
  const float   conversionFactor = (float)(NAdcChannel)/(ADCDynamic*NElectronInAMip);

  int localSize  = this->getSize();
  if (!localSize) return;
  StSpaNoise *curr = this->first();
  while(curr)
    {
      curr->setNoiseValue((int)(curr->getNoiseValue()*conversionFactor));
      if (curr->getNoiseValue() > (NAdcChannel-1)) curr->setNoiseValue(NAdcChannel-1);
      curr->setPedestal((int)((curr->getPedestal()*conversionFactor)+0.5));
      if (curr->getPedestal()    > (NAdcChannel-1)) curr->setPedestal(NAdcChannel-1);
      curr->setSigma((int)(((curr->getSigma()*conversionFactor)*DAQCutValue)+0.5));
      if (curr->getSigma()       > (NAdcChannel-1)) curr->setSigma(NAdcChannel-1); //Now sigma is the DAQ cut...
      
      curr = this->next(curr);
    }
}

void StSpaListNoise::zeroSubstraction()
{
  int localSize = this->getSize();
  if (!localSize) return;
  StSpaNoise *ptr = this->first();
  StSpaNoise *tmp = 0;

  while(ptr)
    {
      tmp = ptr;
      ptr = this->next(ptr);
      if (tmp->getNoiseValue()<= tmp->getSigma())
	{ this->removeNoise(tmp); }
    }
}

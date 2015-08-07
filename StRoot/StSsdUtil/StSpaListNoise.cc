// $Id: StSpaListNoise.cc,v 1.4 2015/08/06 17:46:53 smirnovd Exp $
//
// $Log: StSpaListNoise.cc,v $
// Revision 1.4  2015/08/06 17:46:53  smirnovd
// Removed unused local variables
//
// Revision 1.3  2009/02/23 21:10:40  bouchet
// increase NSaturationSignal to reflect the energy increase of the GEANT hit
//
// Revision 1.2  2006/12/01 22:04:12  bouchet
// get back to previous daqCutValue
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.3  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#include "StSpaListNoise.hh"
#include "StSsdStrip.hh"
#include "StSsdStripList.hh"
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

Int_t StSpaListNoise::getSize()
{ return mListLength; }

StSpaNoise* StSpaListNoise::last()
{ return mLastS; }

Int_t StSpaListNoise::addNewNoise(StSpaNoise *ptr)
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

void StSpaListNoise::setIsActive(Int_t rIsActive, Int_t rNStrip)
{
  if (!(this->getSize())) return;
  StSpaNoise *ptr = this->first();
  while ((ptr)&&(ptr->getNStrip() != rNStrip))
    {
      ptr = this->next(ptr);
    }
  if (ptr) ptr->setIsActive(rIsActive);
  
}

Int_t StSpaListNoise::removeNoise(StSpaNoise *ptr)
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
  Int_t size2 = list->getSize();
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

  delete ptrTmp;
}

void StSpaListNoise::sortStrip()
{
  Int_t localSize=this->getSize();
  Int_t temp = 0;
  if (localSize<2) return;
  StSpaNoise *ptCurr = this->first();
  temp++;
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    {
      StSpaNoise *ptB1 = ptCurr;
      StSpaNoise *ptB2;
      Int_t isCont = 1;
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
      temp++;
    }
  return;
}

void StSpaListNoise::addSignal(StSsdStripList *ptr,
			       long nElectronInAMip,long adcDynamic)
{
  const Int_t NSaturationSignal = (int)adcDynamic*nElectronInAMip;
  Int_t size1                     = this->getSize();
  
  if (!size1) return;
  StSpaNoise *ptr1 = this->first();  
  StSsdStrip *ptr2 = ptr->first();
  //printf("SpaNoise first Id=%d  SpaStrip first Id=%d\n",ptr1->getNStrip(),ptr2->getNStrip());
  Int_t tmpNoiseValue = 0;
  while (ptr2)
    {
      if(!ptr1)return;
      while((ptr1)&&(ptr2->getNStrip() != ptr1->getNStrip()))
      	{
	  ptr1 = this->next(ptr1);
	}
      if(ptr1) 
	{
	  tmpNoiseValue = ptr1->getNoiseValue();
	  ptr1->setNoiseValue(ptr2->getDigitSig() + tmpNoiseValue);
	  ptr2 = ptr->next(ptr2);
	  ptr1=this->first(); 
	}
      else
	{
	  cout<<"signal and noise not matched !"<<endl;
	  ptr1=this->first();
	  ptr2 = ptr->next(ptr2); 
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
  Int_t tmpNoiseValue = 0;
  while (ptr)
    {
      tmpNoiseValue = ptr->getNoiseValue();
      ptr->setNoiseValue(tmpNoiseValue - ptr->getPedestal()) ;
      ptr = this->next(ptr);
    }
}

void StSpaListNoise::convertAnalogToDigit(long nElectronInAMip,long adcDynamic,
					  long nbitEncoding, Float_t daqCutValue)
{
  const Int_t     NAdcChannel             = (int)pow(2.0,nbitEncoding*1.0);
  const Float_t   conversionFactor = (float)(NAdcChannel)/(adcDynamic*nElectronInAMip);

  Int_t localSize  = this->getSize();
  if (!localSize) return;
  StSpaNoise *curr = this->first();
  while(curr)
    {
      curr->setNoiseValue((int)(curr->getNoiseValue()*conversionFactor));
      if (curr->getNoiseValue() > (NAdcChannel-1)) curr->setNoiseValue(NAdcChannel-1);
      curr->setPedestal((int)((curr->getPedestal()*conversionFactor)+0.5));
      if (curr->getPedestal()    > (NAdcChannel-1)) curr->setPedestal(NAdcChannel-1);
      curr->setSigma((int)(((curr->getSigma()*conversionFactor)*daqCutValue)+0.5));
      if (curr->getSigma()       > (NAdcChannel-1)) curr->setSigma(NAdcChannel-1); //Now sigma is the DAQ cut...
      
      curr = this->next(curr);
    }
}

void StSpaListNoise::zeroSubstraction()
{
  Int_t localSize = this->getSize();
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

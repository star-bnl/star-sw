#include "StScfListStrip.hh"
StScfListStrip::StScfListStrip()
{
  mListLength = 0;
  mFirstS=0;
  mLastS=0;
}

StScfListStrip::~StScfListStrip()
{
  if (mListLength)
    {
      StScfStrip *ptr = mLastS;
      StScfStrip *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}


StScfStrip* StScfListStrip::next(StScfStrip *ptr)
{  return ptr->getNextStrip(); }

StScfStrip* StScfListStrip::prev(StScfStrip *ptr)
{  return ptr->getPrevStrip(); }

StScfStrip* StScfListStrip::first()
{  return mFirstS; }

StScfStrip* StScfListStrip::last()
{  return mLastS; }

StScfStrip* StScfListStrip::getStrip(int idStrip)
{
  StScfStrip *currentStrip = this->first();
  while(currentStrip)
    {
      if(currentStrip->getNStrip()==idStrip) return currentStrip;
      currentStrip=this->next(currentStrip);       
    }
  cout<<"No match in getStrip"<<endl;
  return 0;
}

int StScfListStrip::getSize()
{  return mListLength; }

int StScfListStrip::addNewStrip(StScfStrip *ptr)
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

int StScfListStrip::removeStrip(StScfStrip *ptr)
{
  if (!this->getSize()) return 0;
  StScfStrip *ptBefore = ptr->getPrevStrip();
  StScfStrip *ptAfter  = ptr->getNextStrip();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
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

void StScfListStrip::exchangeTwoStrips(StScfStrip *ptr1, StScfStrip *ptr2)
{
  StScfStrip *ptrTemp = new StScfStrip(ptr1->getNStrip(), ptr1->getDigitSig(), ptr1->getSigma());
  ptr1->copyTo(ptrTemp);
  ptr2->copyTo(ptr1);
  ptrTemp->copyTo(ptr2);
  delete ptrTemp;
}

void StScfListStrip::sortStrip()
{
  int localSize=this->getSize();
  if (localSize<2) return;
  
  StScfStrip *ptCurr = this->first();
  ptCurr = this->next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StScfStrip *ptB1 = ptCurr;
      StScfStrip *ptB2;
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

int* StScfListStrip::getListAdc(int idStrip, int sizeCluster)
{
  StScfStrip *CurrentStrip = this->first();
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


void StScfListStrip::setSigma(int iStrip, int iSigma, sls_ctrl_st *sls_ctrl)
{
  const int     NAdcChannel             = (int)pow(2,sls_ctrl[0].NBitEncoding);
  const float   conversionFactor = (float)(NAdcChannel)/(sls_ctrl[0].ADCDynamic*sls_ctrl[0].NElectronInAMip);
  
  StScfStrip *currentStrip=this->first(); 
  while((currentStrip) && (currentStrip->getNStrip()!=iStrip)) currentStrip=this->next(currentStrip);
  if(currentStrip) 
    {
      int sigmaAdc = (int)(iSigma*conversionFactor);
      if (sigmaAdc<NAdcChannel)
	{
	  currentStrip->setSigma(sigmaAdc);
	}
      else 
	{	
	  currentStrip->setSigma(NAdcChannel-1);
	}
    }      
  return;
}


int StScfListStrip::isSorted()
{
  StScfStrip *ptr1 = this->first();
  StScfStrip *ptr2 = 0;

  while(ptr1 != this->last())
    {
      ptr2 = this->next(ptr1);
      if (ptr1->getNStrip()>ptr2->getNStrip()) return 0;
      ptr1 = this->next(ptr1);
    }
  return 1;
}

//$Id: StSstStripList.cc,v 1.4 2016/06/10 19:27:41 bouchet Exp $
//
//$Log: StSstStripList.cc,v $
//Revision 1.4  2016/06/10 19:27:41  bouchet
//coverity : FORWARD_NULL
//
//Revision 1.3  2016/05/30 21:40:29  bouchet
//coverity : REVERSE_INULL fixed
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

#include "St_base/Stiostream.h"
#include "StSstStripList.hh"
#include "StSpaListNoise.hh"
#include "StMessMgr.h"

StSstStripList::StSstStripList()
{
  mListLength = 0;
  mFirstStrip=0;
  mLastStrip=0;
}

StSstStripList::~StSstStripList()
{
  if (mListLength)
    {
      StSstStrip *ptr = mLastStrip;
      StSstStrip *toDele;
      while (mListLength)
	{
	  toDele = ptr;
	  ptr     = prev(ptr);
	  delete toDele;
	  mListLength--;
	}
    }
}


StSstStrip* StSstStripList::next(StSstStrip *ptr)
{  return ptr->getNextStrip(); }

StSstStrip* StSstStripList::prev(StSstStrip *ptr)
{  return ptr->getPrevStrip(); }

StSstStrip* StSstStripList::first()
{  return mFirstStrip; }

StSstStrip* StSstStripList::last()
{  return mLastStrip; }

StSstStrip* StSstStripList::getStrip(Int_t idStrip)
{
  StSstStrip *currentStrip = first();
  while(currentStrip)
    {
      if(currentStrip->getNStrip()==idStrip) return currentStrip;
      currentStrip=next(currentStrip);       
    }
  LOG_INFO <<"No match in getStrip"<<endm;
  return 0;
}

Int_t StSstStripList::getSize()
{  return mListLength; }

Int_t StSstStripList::addNewStrip(StSstStrip *ptr)
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

Int_t StSstStripList::removeStrip(StSstStrip *ptr)
{
  if (!getSize()) return 0;
  StSstStrip *ptBefore = ptr->getPrevStrip();
  StSstStrip *ptAfter  = ptr->getNextStrip();
  
  if (ptBefore == 0)
    {
      if (ptAfter== 0)
	{
	  // taille = 1
	  mFirstStrip =     0;
	  mLastStrip  =     0;
	  mListLength = 0;
	  delete ptr;
	  return 1;
	}
      else
	{
	  mFirstStrip = ptAfter;
	  ptAfter->setPrevStrip(0);
	  mListLength--;
	  delete ptr;
	  return 1;
	}
    }
  else
    {
      if (ptAfter== 0)
	{
	  mLastStrip = ptBefore;
	  ptBefore->setNextStrip(0);
	  mListLength--;
	  delete ptr;
	  return 1;
	}
      else
	{
	  ptBefore->setNextStrip(ptAfter);
	  ptAfter->setPrevStrip(ptBefore);
	  mListLength--;
	  delete ptr;
	  return 1;
	}
    }

}

void StSstStripList::exchangeTwoStrips(StSstStrip *ptr1, StSstStrip *ptr2)
{
  
  StSstStrip *ptrTemp = new StSstStrip(ptr1->getNStrip(), ptr1->getDigitSig(), ptr1->getSigma(),ptr1->getPedestal());
  for(Int_t ii=0;ii<5;ii++) {
    ptrTemp->setIdMcHit(ptr1->getIdMcHit(ii),ii);
    ptrTemp->setIdMcTrack(ptr1->getIdMcTrack(ii),ii);
    ptrTemp->setIdHit(ptr1->getIdHit(ii),ii);
  }
  
  ptr1->setNStrip(ptr2->getNStrip());
  ptr1->setDigitSig(ptr2->getDigitSig());
  ptr1->setPedestal(ptr2->getPedestal());
  ptr1->setSigma(ptr2->getSigma());
  ptr1->setNHits(ptr2->getNHits());
  ptr1->setAnalogSig(ptr2->getAnalogSig());
  ptr1->setMcStrip(ptr2->getMcStrip());
  for( int i= 0 ;i <5 ; i++)
    {
      ptr1->setIdHit(ptr2->getIdHit(i),i);
      ptr1->setIdMcTrack(ptr2->getIdMcTrack(i),i);
      ptr1->setIdMcHit(ptr2->getIdMcHit(i),i);
    }
  
  ptr2->setNStrip(ptrTemp->getNStrip());
  ptr2->setDigitSig(ptrTemp->getDigitSig());
  ptr2->setPedestal(ptrTemp->getPedestal());
  ptr2->setSigma(ptrTemp->getSigma());
  ptr2->setNHits(ptrTemp->getNHits());
  ptr2->setAnalogSig(ptrTemp->getAnalogSig());
  ptr2->setMcStrip(ptrTemp->getMcStrip());
  for( int i= 0 ;i <5 ; i++)
    {
      ptr2->setIdHit(ptrTemp->getIdHit(i),i);
      ptr2->setIdMcTrack(ptrTemp->getIdMcTrack(i),i);
      ptr2->setIdMcHit(ptrTemp->getIdMcHit(i),i);
    }
  delete ptrTemp; 
}

void StSstStripList::sortStrip()
{
  Int_t localSize=getSize();
  if (localSize<2) return;
 
  StSstStrip *ptCurr = first();
  ptCurr = next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSstStrip *ptB1 = ptCurr;
      StSstStrip *ptB2;
      Int_t is_cont = 1;
      while ((ptB1 != first())&&(is_cont))
	{
	  ptB2 = prev(ptB1);
	  if (ptB2->getNStrip() > ptB1->getNStrip())
	    {
	      exchangeTwoStrips(ptB1,ptB2);
	      ptB1 = ptB2;
	    }
	  else
	    {
	      is_cont = 0;
	    }
	}
      ptCurr = next(ptCurr);
      
    }
 
  return;
}

int* StSstStripList::getListAdc(Int_t idStrip, Int_t sizeCluster)
{
  StSstStrip *CurrentStrip = first();
  int* localListAdc = new int[sizeCluster];
  while(CurrentStrip){
    if(CurrentStrip->getNStrip() == idStrip){
      break;
    }
    if(CurrentStrip!=last()){
      CurrentStrip = next(CurrentStrip);
    }
  }
  if (!CurrentStrip) return localListAdc;
  Int_t iStrip = 0;
  for (iStrip=0; iStrip<sizeCluster;iStrip++) 
    {
      localListAdc[iStrip]=CurrentStrip->getDigitSig();
      CurrentStrip = next(CurrentStrip);
    }
  return localListAdc;
}


void StSstStripList::setPedestalSigma(Int_t iStrip, Int_t iPedestal, Int_t iSigma, StSstDynamicControl *dynamicControl)
{
  const Int_t     NAdcChannel             = 1 << (dynamicControl->getnbitEncoding()); // 1 << x == 2^x 
  //  const Float_t   conversionFactor = (float)(NAdcChannel)/(dynamicControl->getadcDynamic()*dynamicControl->getnElectronInAMip());
  //noise is real noise, no more conversion factor needed
  const Float_t   conversionFactor = 1.;
  StSstStrip *currentStrip=first(); 
  while((currentStrip) && (currentStrip->getNStrip()!=iStrip))
    currentStrip=next(currentStrip);
  if(currentStrip) 
    {
      //      Int_t sigmaAdc = (int)(iSigma*conversionFactor);
      Float_t sigmaAdc = iSigma*conversionFactor;
      if (sigmaAdc<NAdcChannel)	currentStrip->setSigma(sigmaAdc);
      else                      currentStrip->setSigma(NAdcChannel-1);
      if (iPedestal<NAdcChannel) currentStrip->setPedestal(iPedestal);
      else                       currentStrip->setPedestal(NAdcChannel-1);
    }
  return;
}

Int_t StSstStripList::isSorted()
{
  StSstStrip *ptr1 = first();
  StSstStrip *ptr2 = 0;

  while(ptr1 != last())
    {
      ptr2 = next(ptr1);
      if (ptr1->getNStrip()>ptr2->getNStrip()) return 0;
      ptr1 = next(ptr1);
    }
  return 1;
}

StSstStripList::StSstStripList(const StSstStripList & originalStripList)
{
  mListLength      = originalStripList.mListLength;
  mFirstStrip      = originalStripList.mFirstStrip;
  mLastStrip       = originalStripList.mLastStrip;
}

StSstStripList& StSstStripList::operator=(const StSstStripList originalStripList)
{
  mListLength      = originalStripList.mListLength;
  mFirstStrip      = originalStripList.mFirstStrip;
  mLastStrip       = originalStripList.mLastStrip;

  return *this;
}

void StSstStripList::updateStrip(StSstStrip *ptr)
{
  Int_t localSize  = getSize();
  if (!localSize)
    {
     addNewStrip(ptr);
     return;
    }
  StSstStrip *stripScan = first();
  while ((stripScan != last())&&(stripScan->getNStrip()!=ptr->getNStrip())) stripScan = next(stripScan);
  if (stripScan->getNStrip()!=ptr->getNStrip()) 
    {
      addNewStrip(ptr);
      return;
    }
  else
    {
      Int_t   dum        = stripScan->getNHits();
      Float_t tmpSig     = stripScan->getAnalogSig();
      stripScan->setNHits(dum+1);
      stripScan->setAnalogSig(ptr->getAnalogSig()+tmpSig) ;
      if (dum<5)
	{
	  if(ptr->getAnalogSig()>tmpSig){
	    stripScan->setIdHit(ptr->getIdHit(0), 0);
	    stripScan->setIdMcHit(ptr->getIdMcHit(0), 0);
	    stripScan->setIdMcTrack(ptr->getIdMcTrack(0), 0);
	  }
	  else{
	    stripScan->setIdMcTrack(stripScan->getIdMcTrack(0), 0);
	    stripScan->setIdHit(stripScan->getIdHit(0),0);
	    stripScan->setIdMcHit(stripScan->getIdMcHit(0),0);
	  }
	}
      delete ptr;
      return;
    }
}
//________________________________________________________________________________
void StSstStripList::updateStripList(StSpaListNoise *ptr)
{
  StSstStrip *pt1      = first();
  StSpaNoise *pt2      = ptr->first();
  Int_t adcCount   = 0; 

  while (pt1)
    {
      pt2 = ptr->first();
      while (pt2&&((pt2->getNStrip())!=(pt1->getNStrip())))
	{
	  pt2 = ptr->next(pt2);
	}
      if (!pt2) 
	{
	  StSstStrip *tmp = pt1;
	  pt1 = next(pt1);
	  removeStrip(tmp);  
	}
      else
	{
	  adcCount = pt2->getNoiseValue();
	  pt1->setDigitSig(adcCount);
	  pt2->setNStrip(-1);
	  pt1 = next(pt1);  
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
 	  StSstStrip *tmpStrip = new StSstStrip(pt2->getNStrip(), pt2->getNoiseValue());
	  addNewStrip(tmpStrip);
 	  pt2              = ptr->next(pt2);  
 	}
    }
}

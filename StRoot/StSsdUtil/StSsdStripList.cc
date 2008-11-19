// $Id: StSsdStripList.cc,v 1.3 2008/10/20 19:29:37 bouchet Exp $
//
// $Log: StSsdStripList.cc,v $
// Revision 1.3  2008/10/20 19:29:37  bouchet
// Fill only one signal contribution (the higher) of GEANT hit per strip
//
// Revision 1.2  2008/05/07 22:48:36  bouchet
// calculation of quality of hits used embedding
//
// Revision 1.1  2006/10/16 16:43:30  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.6  2006/09/15 21:03:14  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.5  2005/03/18 15:02:37  lmartin
// setPedestalSigma method added, setSigma removed
//
// Revision 1.4  2005/03/18 14:17:39  lmartin
// missing CVS header added
//

#include <Stiostream.h>
#include "StSsdStripList.hh"
#include "StSpaListNoise.hh"


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

StSsdStrip* StSsdStripList::getStrip(Int_t idStrip)
{
  StSsdStrip *currentStrip = first();
  while(currentStrip)
    {
      if(currentStrip->getNStrip()==idStrip) return currentStrip;
      currentStrip=next(currentStrip);       
    }
  cout<<"No match in getStrip"<<endl;
  return 0;
}

Int_t StSsdStripList::getSize()
{  return mListLength; }

Int_t StSsdStripList::addNewStrip(StSsdStrip *ptr)
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

Int_t StSsdStripList::removeStrip(StSsdStrip *ptr)
{
  if (!getSize()) return 0;
  StSsdStrip *ptBefore = ptr->getPrevStrip();
  StSsdStrip *ptAfter  = ptr->getNextStrip();
  
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

void StSsdStripList::exchangeTwoStrips(StSsdStrip *ptr1, StSsdStrip *ptr2)
{
  
  StSsdStrip *ptrTemp = new StSsdStrip(ptr1->getNStrip(), ptr1->getDigitSig(), ptr1->getSigma(),ptr1->getPedestal());
  for(Int_t ii=0;ii<5;ii++) {
    ptrTemp->setIdMcHit(ptr1->getIdMcHit(ii),ii);
    ptrTemp->setIdMcTrack(ptr1->getIdMcTrack(ii),ii);
    ptrTemp->setIdHit(ptr1->getIdHit(ii),ii);
  }
  /*
    if(ptr1->getNStrip()){
    ptrTemp->setPrevStrip(ptr1->getPrevStrip());}
    else ptrTemp->setPrevStrip(0);
  */
  
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
  /*
    if(ptr2->getPrevStrip()){
    ptr1->setPrevStrip(ptr2->getPrevStrip());}
    else ptr1->setPrevStrip(0);
  */
  
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

void StSsdStripList::sortStrip()
{
  Int_t localSize=getSize();
  if (localSize<2) return;
 
  StSsdStrip *ptCurr = first();
  ptCurr = next(ptCurr);
  for ( ; ptCurr!=0 ; )
    
    {
      StSsdStrip *ptB1 = ptCurr;
      StSsdStrip *ptB2;
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

int* StSsdStripList::getListAdc(Int_t idStrip, Int_t sizeCluster)
{
  StSsdStrip *CurrentStrip = first();
  int* localListAdc = new int[sizeCluster];
  while((CurrentStrip->getNStrip()!=idStrip)&&(CurrentStrip)) CurrentStrip = next(CurrentStrip);
  if (!CurrentStrip) return localListAdc;
  Int_t iStrip = 0;
  for (iStrip=0; iStrip<sizeCluster;iStrip++) 
    {
      localListAdc[iStrip]=CurrentStrip->getDigitSig();
      CurrentStrip = next(CurrentStrip);
    }
  return localListAdc;
}


void StSsdStripList::setPedestalSigma(Int_t iStrip, Int_t iPedestal, Int_t iSigma, StSsdDynamicControl *dynamicControl)
{
  const Int_t     NAdcChannel             = 1 << (dynamicControl->getnbitEncoding()); // 1 << x == 2^x 
  //  const Float_t   conversionFactor = (float)(NAdcChannel)/(dynamicControl->getadcDynamic()*dynamicControl->getnElectronInAMip());
  const Float_t   conversionFactor = 1./16.;
  StSsdStrip *currentStrip=first(); 
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

Int_t StSsdStripList::isSorted()
{
  StSsdStrip *ptr1 = first();
  StSsdStrip *ptr2 = 0;

  while(ptr1 != last())
    {
      ptr2 = next(ptr1);
      if (ptr1->getNStrip()>ptr2->getNStrip()) return 0;
      ptr1 = next(ptr1);
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

void StSsdStripList::updateStrip(StSsdStrip *ptr)
{
  Int_t localSize  = getSize();
  if (!localSize)
    {
     addNewStrip(ptr);
     return;
    }
  StSsdStrip *stripScan = first();
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

StSsdStripList* StSsdStripList::addStripList(StSsdStripList *list)
{
  Int_t size2 = list->getSize();
  if (!size2) return this;
  
  StSsdStrip *st1 =0 ;
  StSsdStrip *st2 = list->first();
  Int_t i = 0;
  for (i=0 ; i < size2 ; i++)
    {
      st2->copyTo(st1);
      addNewStrip(st1);
      st2 = list->next(st2);
    }
  return this;  
}
//________________________________________________________________________________
void StSsdStripList::updateStripList(StSpaListNoise *ptr)
{
  StSsdStrip *pt1      = first();
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
	  StSsdStrip *tmp = pt1;
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
 	  StSsdStrip *tmpStrip = new StSsdStrip(pt2->getNStrip(), pt2->getNoiseValue());
	  addNewStrip(tmpStrip);
 	  pt2              = ptr->next(pt2);  
 	}
    }
}

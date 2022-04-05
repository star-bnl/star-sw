//$Id: StSstWafer.cc,v 1.10 2016/06/10 19:28:31 bouchet Exp $
//
//$Log: StSstWafer.cc,v $
//Revision 1.10  2016/06/10 19:28:31  bouchet
//coverity : REVERSE_INULL
//
//Revision 1.9  2016/06/08 20:53:54  bouchet
//coverity : PASS_BY_VALUE, FORWARD_NULL
//
//Revision 1.8  2016/06/07 21:44:07  bouchet
//coverity : multiple RESOURCE_LEAK fixed ; add point to list after having set the flag ; cleanup
//
//Revision 1.7  2016/05/30 23:52:22  bouchet
//forget cleanup at previous commit
//
//Revision 1.6  2016/05/30 21:39:21  bouchet
//coverity : FORWARD_NULL fixed ; cleanup + simplified method
//
//Revision 1.5  2015/11/16 19:18:47  bouchet
//revert back the cut on signal strip : using DB entry, not constant
//
//Revision 1.4  2015/08/06 17:46:54  smirnovd
//Removed unused local variables
//
//Revision 1.3  2015/07/21 14:54:23  bouchet
//removed unused variables ; Int_t doLorentzShiftSide moved to void()
//
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.2  2015/05/01 12:12:14  bouchet
//add fixed threshold for cluster finder decision
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein
#include "StSstUtil/StSstWafer.hh"
#include "St_base/Stiostream.h"
#include "TMath.h"
#include "StMessMgr.h"
//________________________________________________________________________________
StSstWafer::StSstWafer(Int_t nid) : TGeoHMatrix(), mDebug(0) {
  memset(first, 0, last-first);
  mId       = nid;
  
  mStripP   = new StSstStripList();
  mStripN   = new StSstStripList();
  mNoiseP   = new StSpaListNoise();
  mNoiseN   = new StSpaListNoise();
  mClusterP = new StSstClusterList();
  mClusterN = new StSstClusterList();
  mPackage  = new StSstPackageList();
  mPoint    = new StSstPointList();
}
//________________________________________________________________________________
StSstWafer::~StSstWafer() {
  delete    mStripP;
  delete    mStripN;
  delete    mNoiseP;
  delete    mNoiseN;
  delete    mClusterP;
  delete    mClusterN;
  delete    mPackage;
  delete    mPoint;
}
//________________________________________________________________________________
void StSstWafer::Reset() {
  delete    mStripP;
  delete    mStripN;
  delete    mNoiseP;
  delete    mNoiseN;
  delete    mClusterP;
  delete    mClusterN;
  delete    mPackage;
  delete    mPoint;
  mStripP   = new StSstStripList();
  mStripN   = new StSstStripList();
  mNoiseP   = new StSpaListNoise();
  mNoiseN   = new StSpaListNoise();
  mClusterP = new StSstClusterList();
  mClusterN = new StSstClusterList();
  mPackage  = new StSstPackageList();
  mPoint    = new StSstPointList();  
}
//________________________________________________________________________________
void StSstWafer::init(Int_t rId, Double_t *rD, Double_t *rN, Double_t *rT, Double_t *rX)
{
  if (rId != mId) {LOG_INFO<<" Can not initialize wafer number : "<<mId<<" with "<<rId<<"\n";}
  else {
    SetName(Form("R%04i",rId));
    Double_t rot[9] = {
      rD[0],  rN[0], rT[0],
      rD[1],  rN[1], rT[1],
      rD[2],  rN[2], rT[2]
    };
    SetRotation(rot);
    SetTranslation(rX);
  }
}
//________________________________________________________________________________
void StSstWafer::debugStrips()
{
  StSstStrip *currentStripP = 0;
  LOG_INFO<<"List of "<<mStripP->getSize()<<" strips on the P side "<<endm;
  if (mStripP->getSize()>0) currentStripP = mStripP->first();
  for (Int_t i=0;i<mStripP->getSize();i++) {
        LOG_INFO<<" id,sig,noise : "<<currentStripP->getNStrip()
     	<<" "<<currentStripP->getDigitSig()
    	<<" "<<currentStripP->getSigma()<<endm;
    if (currentStripP!=mStripP->last()) currentStripP = mStripP->next(currentStripP);
  }

  StSstStrip *currentStripN = 0;
  LOG_INFO<<"List of "<<mStripN->getSize()<<" strips on the N side "<<endm;
  if (mStripN->getSize()>0) currentStripN = mStripN->first();
  for (Int_t i=0;i<mStripN->getSize();i++) {
     LOG_INFO<<" id,sig,noise : "<<currentStripN->getNStrip()
    <<" "<<currentStripN->getDigitSig()
    <<" "<<currentStripN->getSigma()<<endm;
    if (currentStripN!=mStripN->last()) currentStripN = mStripN->next(currentStripN);
  }
}
//________________________________________________________________________________
void StSstWafer::debugClusters()
{
  StSstCluster *currentClusterP = 0;
  LOG_INFO<<"List of "<<mClusterP->getSize()<<" clusters on the P side "<<endm;
  if (mClusterP->getSize()>0) currentClusterP = mClusterP->first();
  for (Int_t i=0;i<mClusterP->getSize();i++) {
      LOG_INFO<<"N,Size,FirstStrip,StripMean,TotAdc,FirstAdc,LastAdc,TotNoise : "<<currentClusterP->getNCluster()
   <<" "<<currentClusterP->getClusterSize()
    <<" "<<currentClusterP->getFirstStrip()
    <<" "<<currentClusterP->getStripMean()
    <<" "<<currentClusterP->getTotAdc()
    <<" "<<currentClusterP->getFirstAdc()
    <<" "<<currentClusterP->getLastAdc()
    <<" "<<currentClusterP->getTotNoise()<<endm;
    if (currentClusterP!=mClusterP->last()) currentClusterP = mClusterP->next(currentClusterP);
  }

  StSstCluster *currentClusterN = 0;
  LOG_INFO<<"List of "<<mClusterN->getSize()<<" clusters on the P side "<<endm;
  if (mClusterN->getSize()>0) currentClusterN = mClusterN->first();
  for (Int_t i=0;i<mClusterN->getSize();i++) {
     LOG_INFO<<"N,Size,FirstStrip,StripMean,TotAdc,FirstAdc,LastAdc,TotNoise : "<<currentClusterN->getNCluster()
    	<<" "<<currentClusterN->getClusterSize()
    <<" "<<currentClusterN->getFirstStrip()
   <<" "<<currentClusterN->getStripMean()
    <<" "<<currentClusterN->getTotAdc()
     <<" "<<currentClusterN->getFirstAdc()
    <<" "<<currentClusterN->getLastAdc()
    <<" "<<currentClusterN->getTotNoise()<<endm;
    if (currentClusterN!=mClusterN->last()) currentClusterN = mClusterN->next(currentClusterN);
  }
}
//________________________________________________________________________________
/*!
A new strip is added to the wafer by calling the StripList method.
 */
void StSstWafer::addStrip(StSstStrip *ptr, Int_t iSide)
{
  if (iSide)
    { mStripN->addNewStrip(ptr); }
  else
    { mStripP->addNewStrip(ptr); }
}
//________________________________________________________________________________
/*!
A new cluster is added to the wafer by calling the ClusterList method.
 */
void StSstWafer::addCluster(StSstCluster *ptr, Int_t iSide)
{
  if (iSide)
    { mClusterN->addNewCluster(ptr); }
  else
    { mClusterP->addNewCluster(ptr); }
}
//________________________________________________________________________________
/*!
A new package is added to the wafer by calling the PackageList method.
 */
void StSstWafer::addPackage(StSstPackage *ptr)
{  mPackage->addNewPackage(ptr); }
//________________________________________________________________________________
/*!
A new point is added to the wafer by calling the PointList method.
 */
void StSstWafer::addPoint(StSstPoint *ptr)
{  mPoint->addNewPoint(ptr); }
//________________________________________________________________________________
void StSstWafer::setPedestalSigmaStrip(Int_t iStrip, Int_t iSide, Int_t iPedestal, Int_t iSigma, StSstDynamicControl *dynamicControl)
{
  if (iSide)
    { mStripN->setPedestalSigma(iStrip, iPedestal, iSigma, dynamicControl); }
  else
    { mStripP->setPedestalSigma(iStrip, iPedestal, iSigma, dynamicControl); }
}
//________________________________________________________________________________
/*!
The strips of both sides are sorted
 */
void StSstWafer::sortStrip()
{
  mStripP->sortStrip();
  mStripN->sortStrip();
}
//________________________________________________________________________________
/*!
The clusters of both sides are sorted
 */
void StSstWafer::sortCluster()
{
  mClusterP->sortCluster();
  mClusterN->sortCluster();
}
//________________________________________________________________________________
/*!
The points are sorted by their oder in the StSstPointList
 */
void StSstWafer::sortPoint()
{  mPoint->sortPoint(); }
//________________________________________________________________________________
/*!
- a cluster finding
- a cluster splitting  
 */
void StSstWafer::doClusterisation(Int_t *NClusterPerSide, StSstClusterControl *clusterControl)
{
  Int_t iSide = 0;
  doFindCluster(clusterControl, iSide);
  NClusterPerSide[0] = doClusterSplitting(clusterControl, iSide); 
  iSide = 1;
  doFindCluster(clusterControl, iSide); 
  NClusterPerSide[1] = doClusterSplitting(clusterControl, iSide);
}
//______________________________________________________________
void  StSstWafer::doCleanListStrip(StSstStripList *myStripList)
{
  Int_t lowCut = 3;
  
  StSstStripList *cleanListStrip;
  //  cleanListStrip = new StSstStripList();
  cleanListStrip = myStripList; 
  StSstStrip *myStrip   = 0; 
  StSstStrip *copyStrip = cleanListStrip->first(); 
  int        size       = cleanListStrip->getSize();   
  for(Int_t i=0;i<size;i++){  
    myStrip = copyStrip; 
    copyStrip = myStripList->next(copyStrip); 
    if((myStrip->getSigma()==0)||(myStrip->getDigitSig()<lowCut*myStrip->getSigma())){
      cleanListStrip->removeStrip(myStrip);  
    }
  }
}
//________________________________________________________________________________
/*!
- Does the cluster finding on the iSide. 
The strip list is scanned by increasing order (the list is assumed to be sorted). 
For a given strip (CurrentStrip) if its signal is above a given number of time its noise value a cluster list is formed. 
The strip list is scanned backward then forward to add the consecutive strips with a signal. The new cluster is updated 
each time a new strip is added. The scan starts with the first strip of the list and ends with the last strip. 
The threshold for the creation of a new cluster is given by SsdClusterControl.highCut.
The SsdClusterControl table seems to be useless in this method (cleaning needed ?) 

- Update for 2014 data :
1) there are no intrinsic strip rms
2) decision based on (signal/rms) > highCut is somehow obsolete
3) use decision based on (signal) > threshold to decide whether a strip is a clusterSeed
4) use 2 threshold for P and N side separately

- Update for 2015 data :
1) remove the threshold constants because cut is too strong for run 15
2) instead we use as originally highCut from clusterControl table
3) DB entries have been updated accordingly : 10 for run 14, 5 for run 15
 */
Int_t StSstWafer::doFindCluster(StSstClusterControl *clusterControl, Int_t iSide)
{
  StSstStripList   *CurrentStripList   =  0;
  StSstClusterList *CurrentClusterList =  0;

  switch (iSide)
    {
    case 0:
     CurrentStripList   =  mStripP;
     CurrentClusterList =  mClusterP;
     break;
    case 1:
     CurrentStripList   =  mStripN;
     CurrentClusterList =  mClusterN;
     break;
    }

  if(!CurrentStripList) return 0;
  if(!CurrentStripList->getSize()) return 0;
  
  Int_t nCluster = 0;
  Int_t atTheEnd = 0;

  //doCleanListStrip(CurrentStripList);

  StSstStrip *CurrentStrip  = CurrentStripList->first();
  StSstStrip *ScanStrip     = 0;
  StSstStrip *LastScanStrip = 0;
  StSstStrip *tmpStrip      = CurrentStripList->first();
  for(Int_t i = 0; i<CurrentStripList->getSize(); i++)
    { tmpStrip = CurrentStripList->next(tmpStrip); }
  
  while(CurrentStrip) 
    {
      if(CurrentStrip->getDigitSig()> clusterControl->getHighCut())
	{
	  LastScanStrip = 0;
	  StSstCluster *newCluster = new StSstCluster(CurrentClusterList->getSize());
	  nCluster++;
	  newCluster->update(CurrentStrip,1.);
	  ScanStrip = CurrentStripList->prev(CurrentStrip);  
	  while(ScanStrip)
	    {
	      if(((ScanStrip->getNStrip())-((CurrentStripList->next(ScanStrip))->getNStrip()))==-1)
		{
		  newCluster->update(ScanStrip,1.);
		  ScanStrip = CurrentStripList->prev(ScanStrip);
		}
	      else
		{	ScanStrip = 0; }
	    }
	  ScanStrip = CurrentStripList->next(CurrentStrip);  	  
	  while(ScanStrip)
	    {
	      if(((ScanStrip->getNStrip())-((CurrentStripList->prev(ScanStrip))->getNStrip()))==1)
		{
		  newCluster->update(ScanStrip,1.);
		  ScanStrip = CurrentStripList->next(ScanStrip);
		  if (!ScanStrip) atTheEnd = 1;
		}
	      else
		{
		  LastScanStrip = ScanStrip;
		  ScanStrip     = 0;
		}
	    }
	  CurrentClusterList->addNewCluster(newCluster);
	  if(LastScanStrip)
	    { CurrentStrip = LastScanStrip; }
	  else
	    {
	      if (atTheEnd)
		{ CurrentStrip = 0; }
	      else
		{ CurrentStrip = CurrentStripList->next(CurrentStrip); }
	    }
	}
      else
	{ CurrentStrip = CurrentStripList->next(CurrentStrip); }
    }
  return nCluster;
}
//________________________________________________________________________________
/*!
Does the cluster splitting on the side iSide. For each cluster in the cluster list, the array of signal values is built 
and transmitted to the splitCluster method. This method seems to remove clusters and not to add new clusters resulting 
from a cluster splitting into several.
 */
Int_t StSstWafer::doClusterSplitting(StSstClusterControl *clusterControl, Int_t iSide)
{
  StSstStripList   *CurrentStripList   =  0;
  StSstClusterList *CurrentClusterList =  0;

  switch (iSide)
    {
    case 0:
      CurrentStripList   =  mStripP;
      CurrentClusterList =  mClusterP;
      break;
    case 1:
      CurrentStripList   =  mStripN;
      CurrentClusterList =  mClusterN;
      break;
    }
  if(!CurrentClusterList) return 0;
  Int_t ClusterListSize = CurrentClusterList->getSize();
  if(!ClusterListSize) return 0;
  
  Int_t iCluster = 0;
  StSstCluster *CurrentCluster = CurrentClusterList->first();
  
  for(iCluster = 0 ; iCluster < ClusterListSize ; iCluster++)
    {   
      
      Int_t *ListAdc = CurrentStripList->getListAdc(CurrentCluster->getFirstStrip(),CurrentCluster->getClusterSize());
      Int_t toBeDeleted = CurrentClusterList->splitCluster(clusterControl,CurrentCluster,ListAdc,CurrentStripList);
      if(toBeDeleted)
	{
	  StSstCluster *TempCluster = CurrentCluster;
	  CurrentCluster = CurrentClusterList->next(CurrentCluster);
	  CurrentClusterList->removeCluster(TempCluster);
	}
      else
	{
	  CurrentCluster = CurrentClusterList->next(CurrentCluster);
	} 

      delete [] ListAdc;
    }
  CurrentClusterList->renumCluster();
  return CurrentClusterList->getSize();
}

//________________________________________________________________________________
/*!
Does the loretnz shift of the mean strip of the cluster
 */
void StSstWafer::doLorentzShift(sstDimensions_st *dimensions,Float_t mShift_hole,Float_t mShift_elec)
{
  Float_t pitch = dimensions[0].stripPitch;
  //side P
  doLorentzShiftSide(mShift_hole, pitch, mClusterP);
  //side N
  doLorentzShiftSide(mShift_elec, pitch, mClusterN);
}
//___________________________________________________________________________________________
void StSstWafer::doLorentzShiftSide(Float_t shift,Float_t pitch, StSstClusterList *currentList){
  Int_t iCluster = 0;
  StSstCluster *CurrentCluster = currentList->first();
    for(iCluster = 0 ; iCluster < currentList->getSize(); iCluster++){   
      Float_t StripMean = CurrentCluster->getStripMean();
      CurrentCluster->setStripMean(StripMean-(shift/pitch));
      CurrentCluster = currentList->next(CurrentCluster);
    }
    delete CurrentCluster;
}
//________________________________________________________________________________
/*!
Determines the packages by comparing the cluster lists built for both sides. 
 */
Int_t StSstWafer::doFindPackage(sstDimensions_st *dimensions, StSstClusterControl *clusterControl)
{
  StSstPackageList *currentPackageList = 0;
  StSstCluster     *currentClusterP    = 0;
  StSstCluster     *currentClusterN    = 0;
  StSstCluster     *scanClusterP       = 0;
  StSstCluster     *scanClusterN       = 0;
  StSstCluster     *lastMatchedN       = 0;
  StSstCluster     *nextMatchedN       = 0;

  Int_t maxMatchedInPackage = clusterControl->getClusterTreat();
  Int_t numPackage         = 0;
  Int_t numUnMatched       = 0;
  Int_t numCurrentClusterP = 0;
  Int_t numCurrentClusterN = 0;
  Int_t numScanClusterP    = 0;
  Int_t numScanClusterN    = 0;
  Int_t numLastMatchedN    = 0;
  Int_t numNextMatchedN    = 0;
  Int_t matchedOk          = 0;
  Int_t keepPackage        = 0;

  currentPackageList = mPackage;
  currentClusterP    = mClusterP->first();
  currentClusterN    = mClusterN->first();

  if (!mClusterP->getSize() || !mClusterN->getSize()) return 0;
  StSstPackage *currentPackage = new StSstPackage(0, clusterControl);

  while (currentClusterP)
    {
      matchedOk = 0;
      keepPackage = 0;
      scanClusterP = currentClusterP ;
      currentPackage->addNewMatched(currentClusterP, maxMatchedInPackage);
      currentPackage->addKindPackage(numCurrentClusterP+1,0, maxMatchedInPackage);
      scanClusterN = currentClusterN ;
      while (scanClusterN)
	{
	  if (geoMatched(dimensions, scanClusterP, scanClusterN))
	    {
	      matchedOk++;
	      currentPackage->addNewMatched(scanClusterN, maxMatchedInPackage);
	      currentPackage->addKindPackage(numLastMatchedN+1,1, maxMatchedInPackage);
	      lastMatchedN = scanClusterN;
	      numLastMatchedN++;
	    }
	  scanClusterN = mClusterN->next(scanClusterN);
	}
      if (!(numScanClusterP == mClusterP->getSize()-1))
	{ 
	  scanClusterP = mClusterP->next(scanClusterP);
	  numScanClusterP++;
	  scanClusterN = lastMatchedN;
	  numScanClusterN = numLastMatchedN;
	  while (scanClusterN)
	    {
	      if (geoMatched(dimensions, scanClusterP, scanClusterN)) 
		{
		  keepPackage = 1;
		  nextMatchedN = scanClusterN;
		  numScanClusterN--;
		  numNextMatchedN = numScanClusterN;
		}
	      scanClusterN = mClusterN->prev(scanClusterN);
	    }
	  if (!keepPackage)
	    {
	      numCurrentClusterP = 0;
	      numCurrentClusterN = 0;
	      numScanClusterP    = 0;
	      numScanClusterN    = 0;
	      numLastMatchedN    = 0;
	      if (!matchedOk)
		{
		  numUnMatched++;
		  currentPackage->purgePackage();
		}
	      else
		{
		  currentClusterN = mClusterN->next(lastMatchedN);
		  StSstPackage *newPackage = new StSstPackage(currentPackageList->getSize(), currentPackage->getSize());
		  newPackage->takeMatcheds(currentPackage);
		  currentPackageList->addNewPackage(newPackage);
		  currentPackage->purgePackage();
		  numPackage++;
		}
	    }
	  else
	    {
	      currentClusterN = nextMatchedN;
	      numCurrentClusterP++;
	      numCurrentClusterN++;
	      numLastMatchedN = numNextMatchedN;              
	    } 
	}
      else
	{
	  StSstPackage *newPackage = new StSstPackage(currentPackageList->getSize(), currentPackage->getSize());
	  newPackage->takeMatcheds(currentPackage);
	  currentPackageList->addNewPackage(newPackage);
	  currentPackage->purgePackage();
	  numPackage++;
	}
      currentClusterP = mClusterP->next(currentClusterP);
    }
  delete currentPackage;
  return numPackage;
}
//________________________________________________________________________________
Int_t StSstWafer::doSolvePerfect(sstDimensions_st *dimensions, StSstClusterControl *clusterControl,Float_t CalibArray)
{
  Int_t nPerfect = 0;
  StSstPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      Int_t numMatched = strlen(currentKind)/2;
      Float_t *Adc     = new float[numMatched];
      for(Int_t i=0;i<numMatched;i++) 
	Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 *********************************************************************
      if(!strcmp(currentKind,"1p1n"))//                   case (1-1) checked
	{
          StSstPoint *newPoint = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 11);
          newPoint->setFlag(100);
          setMatcheds(dimensions, newPoint, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPoint->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	  mPoint->addNewPoint(newPoint);
          nPerfect++;
	}
//  *********************************************************************
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nPerfect;
}
//________________________________________________________________________________
void StSstWafer::doStatPerfect(Int_t nPerfectPoint, StSstClusterControl *clusterControl)
{
  Float_t store = 0;
  StSstPoint *currentPerfect = 0;
  currentPerfect = mPoint->first();
  while(currentPerfect)
    {
      store += currentPerfect->getDe(1);
      currentPerfect = mPoint->next(currentPerfect);
    }
  mPerfectMean = store/nPerfectPoint;

  store = 0;
  currentPerfect = mPoint->first();
  while(currentPerfect)
    {
      store += (currentPerfect->getDe(1)-mPerfectMean)*(currentPerfect->getDe(1)-mPerfectMean);
      currentPerfect=mPoint->next(currentPerfect);
    }
  mPerfectSigma = store/nPerfectPoint;
}
//________________________________________________________________________________
Int_t StSstWafer::doSolvePackage(sstDimensions_st *dimensions, StSstClusterControl *clusterControl,Float_t CalibArray)
{
  Int_t nSolved = 0;
  StSstPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      Int_t numMatched = strlen(currentKind)/2;
      Float_t *Adc     = new float[numMatched];
      for(Int_t i=0;i<numMatched;i++) Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 ********************************************************************
      if(!strcmp(currentKind,"1p1n"))//  case (1-1) done in doSolvePerfect 
	{
	}
// 2 ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n"))// case (1-2)A final check Ok
 	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 12);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0]-Adc[2], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  12);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[1], Adc[2],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
 	}
// 3 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n"))// case (1-2)AS final check Ok
  	{
 	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1]-Adc[2],CalibArray);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLossCorrected(Adc[2], Adc[1]-Adc[0],CalibArray);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
//  ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n3n"))// case (1-3)A checked
 	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 13);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0]-Adc[2]-Adc[3], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[1]-Adc[3], Adc[2],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLossCorrected(Adc[0]-Adc[1]-Adc[2], Adc[3],CalibArray);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);
	  nSolved++;
 	}
//  *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n"))// case (1-3)AS checked
  	{
 	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1]-Adc[2]-Adc[4],CalibArray);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLossCorrected(Adc[2], Adc[1]-Adc[0]-Adc[4],CalibArray);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[4], Adc[1]-Adc[0]-Adc[2],CalibArray);
          newPointC->setFlag(100);
 	  mPoint->addNewPoint(newPointC);
          nSolved++;
  	}
// 4 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n"))//        case (2-2)A checked
  	{
 	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
  	}
// 5 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n"))//        case (2-2)AP checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointB->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
// 6 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n"))//        case (2-2)B checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

// traitement propre aux space points..(probabilite)
 	  Double_t setA[2], setB[2], setC[2], setD[2];
          Double_t probAD, probBC;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  if ((setA[1]*setD[1])||(setB[1]*setC[1]))
	    {
	      Double_t tmp = 3e-33+(setA[1]*setD[1]+setB[1]*setC[1]);
	      probAD = (setA[1]*setD[1])/tmp;
	      probBC = (setB[1]*setC[1])/tmp;
	      if(probAD>probBC)// we store only pointA and pointD because this configuration has the higher probability 
		{
		  newPointA->setFlag(int(100*probAD));
		  newPointD->setFlag(int(100*probAD));
		  mPoint->addNewPoint(newPointA);
		  mPoint->addNewPoint(newPointD);
		  delete newPointC;
		  delete newPointB;
		}
	      else
		{
		  newPointB->setFlag(int(100*probBC));
		  newPointC->setFlag(int(100*probBC));
		  mPoint->addNewPoint(newPointB);
		  mPoint->addNewPoint(newPointC);
		  delete newPointA;
		  delete newPointD;
		}
	    }
	  else
	    {
	      probAD = 0.5;
	      probBC = 0.5;
	      newPointA->setFlag(int(100*probAD));
	      newPointB->setFlag(int(100*probBC));
	      newPointC->setFlag(int(100*probBC));
	      newPointD->setFlag(int(100*probAD));	      
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	    }
          nSolved++;
	}
// 7 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n"))//        case (2-3)A checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[1], Adc[2],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointC->setEnergyLossCorrected(Adc[3]-Adc[5], Adc[2],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
          newPointD->setFlag(100);

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setD[2];
	  Double_t setAB[2], setCD[2];
          Double_t probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] - (Adc[1]+Adc[2]))/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setCD[0] = (Adc[3] - (Adc[2]+Adc[5]))/sqrt(2.0);
          setCD[1] = matchDistr(clusterControl, setCD[0]);
          setD[0]  = (Adc[3] - Adc[5])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
	  Double_t tmp  = 3e-33+(setAB[1]*setD[1]+setA[1]*setCD[1]+1e-10);
	  probABD  = (setAB[1]*setD[1])/tmp;
	  probACD  = (setA[1]*setCD[1])/tmp;
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLossCorrected(Adc[0]-Adc[2],Adc[1],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[3], Adc[5],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointD);
	      delete newPointC;
	    }
	  else
	    {
	      newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[3]-Adc[2], Adc[5],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      delete newPointD;
	    }
          nSolved++;
  	}
// 8 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n"))//        case (2-3)AP checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
   	  newPointB->setEnergyLossCorrected(Adc[2], Adc[1]-Adc[0],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
   	  newPointC->setEnergyLossCorrected(Adc[2], Adc[4]-Adc[5],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(4));
          newPointD->setFlag(100);

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setD[2];
	  Double_t setAB[2], setCD[2];
          Double_t probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] + Adc[2] - Adc[1])/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setCD[0] = (Adc[2] + Adc[5] - Adc[4])/sqrt(2.0);
          setCD[1] = matchDistr(clusterControl, setCD[0]);
          setD[0]  = (Adc[5] - Adc[4])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
	  Double_t tmp = 3e-33+(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  probABD  = (setAB[1]*setD[1])/tmp;
	  probACD  = (setA[1]*setCD[1])/tmp;
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLossCorrected(Adc[0], Adc[1]-Adc[2],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[5], Adc[4],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointD);
	      delete newPointC;
	    }
	  else
	    {
	      newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[5], Adc[4]-Adc[2],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      delete newPointD;
	    }
          nSolved++;
  	}
// 9 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n"))//        case (2-3)B
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[1], Adc[2],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
  	  newPointC->setEnergyLossCorrected(Adc[0]-Adc[1], Adc[3],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setD[2], setE[2];
	  Double_t setAB[2], setAC[2], setDE[2];
          Double_t probABE, probACD, probADE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] - (Adc[1] + Adc[2]))/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setAC[0] = (Adc[0] - (Adc[1] + Adc[3]))/sqrt(2.0);
          setAC[1] = matchDistr(clusterControl, setAC[0]);
          setDE[0] = (Adc[4] - (Adc[2] + Adc[3]))/sqrt(2.0);
          setDE[1] = matchDistr(clusterControl, setDE[0]);
          setD[0]  = (Adc[4] - Adc[2])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
          setE[0]  = (Adc[4] - Adc[3])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
	  Double_t tmp = 3e-33+(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  probABE  = (setAB[1]*setE[1])/tmp;
	  probACD  = (setAC[1]*setD[1])/tmp;
	  probADE  = (setA[1]*setDE[1])/tmp;
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*probACD));
	  newPointD->setFlag(int(100*(probACD+probADE)));
	  newPointE->setFlag(int(100*(probABE+probADE)));
	  if ((probABE > probACD)&&(probABE > probADE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0]-Adc[2],Adc[1],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[4],Adc[3],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointC;
	      //delete newPointD;
	    }
	  else if ((probACD > probABE)&&(probACD > probADE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0]-Adc[3],Adc[1],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[4],Adc[2],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointD);
	      //delete newPointB;
	      //delete newPointE;
	    }
	  else if ((probADE > probABE)&&(probADE > probACD))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1],CalibArray);
	      newPointD->setEnergyLossCorrected(Adc[4]-Adc[3],Adc[2],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[4]-Adc[2],Adc[3],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointC;
	      //delete newPointD;
	    }
          nSolved++;
  	}
// 10 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n"))//        case (3-2)BP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
  	  newPointB->setEnergyLossCorrected(Adc[2], Adc[1]-Adc[0],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(1));
  	  newPointD->setEnergyLossCorrected(Adc[5], Adc[1]-Adc[0],CalibArray);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(4));

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setC[2], setE[2];
          Double_t setAB[2], setAD[2], setCE[2];
          Double_t probABE, probACD, probACE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] + Adc[2] - Adc[1])/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setAD[0] = (Adc[0] + Adc[5] - Adc[1])/sqrt(2.0);
          setAD[1] = matchDistr(clusterControl, setAD[0]);
          setCE[0] = (Adc[2] + Adc[5] + Adc[4])/sqrt(2.0);
          setCE[1] = matchDistr(clusterControl, setCE[0]);
          setC[0]  = (Adc[2] - Adc[4])/sqrt(2.0);
          setC[1]  = matchDistr(clusterControl, setC[0]);
          setE[0]  = (Adc[5] - Adc[4])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
	  Double_t tmp = 3e-33+(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  probABE  = (setAB[1]*setE[1])/tmp;
	  probACD  = (setAD[1]*setC[1])/tmp;
	  probACE  = (setA[1]*setCE[1])/tmp;
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*(probACD+probACE)));
	  newPointD->setFlag(int(100*probACD));
	  newPointE->setFlag(int(100*(probABE+probACE)));
	  if ((probABE > probACD)&&(probABE > probACE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1]-Adc[2],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[5],Adc[4],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointC;
	      //delete newPointD;
	    }
	  else if ((probACD > probABE)&&(probACD > probACE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1]-Adc[5],CalibArray);
	      newPointC->setEnergyLossCorrected(Adc[2],Adc[4],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointD);
	      //delete newPointB;
	      //delete newPointE;
	    }
	  else if ((probACE > probABE)&&(probACE > probACD))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1],CalibArray);
	      newPointC->setEnergyLossCorrected(Adc[2],Adc[4]-Adc[5],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[5],Adc[4]-Adc[2],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointC;
	      //delete newPointD;
	    }
          nSolved++;
  	}
// 11 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n"))//        case (2-3)BS
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
  	  newPointC->setEnergyLossCorrected(Adc[3]-Adc[6],Adc[1],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointD->setEnergyLossCorrected(Adc[3]-Adc[6], Adc[2],CalibArray);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
          newPointE->setFlag(100);

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setB[2], setE[2];
          Double_t setAB[2], setCE[2], setDE[2];
          Double_t probABE, probADE, probBCE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1]  = matchDistr(clusterControl, setB[0]);
          setAB[0] = (Adc[0] - (Adc[1] + Adc[2]))/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setCE[0] = (Adc[3] - (Adc[1] - Adc[6]))/sqrt(2.0);
          setCE[1] = matchDistr(clusterControl, setCE[0]);
          setDE[0] = (Adc[3] - (Adc[2] + Adc[6]))/sqrt(2.0);
          setDE[1] = matchDistr(clusterControl, setDE[0]);
          setE[0]  = (Adc[3] - Adc[6])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
	  Double_t tmp = 3e-33+(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  probABE  = (setAB[1]*setE[1])/tmp;
	  probADE  = (setA[1]*setDE[1])/tmp;
	  probBCE  = (setB[1]*setCE[1])/tmp;
	  newPointA->setFlag(int(100*(probABE+probADE)));
	  newPointB->setFlag(int(100*(probABE+probBCE)));
	  newPointC->setFlag(int(100*probBCE));
	  newPointD->setFlag(int(100*probADE));
	  if ((probABE > probADE)&&(probABE > probBCE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0]-Adc[2],Adc[1],CalibArray);
	      newPointB->setEnergyLossCorrected(Adc[0]-Adc[1],Adc[2],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[3],Adc[6],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointC;
	      //delete newPointD;
	    }
	  if ((probADE > probABE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[3]-Adc[2],Adc[6],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointE);
	      //delete newPointB;
	      //delete newPointC;
	    }
	  if ((probBCE > probABE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLossCorrected(Adc[0],Adc[2],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[3]-Adc[1],Adc[6],CalibArray);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointE); 
	      //delete newPointA;
	      //delete newPointD;
	    }
          nSolved++;
  	}
      // 12 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n"))//        case (3-2)BSP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[6], Adc[2],CalibArray);

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3]-Adc[6], Adc[2],CalibArray);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
          newPointE->setFlag(100);

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setC[2], setE[2];
          Double_t setAC[2], setBE[2], setDE[2];
          Double_t probACE, probADE, probBCE;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAC[0] = (Adc[0] + Adc[3] - Adc[1])/sqrt(2.0);
          setAC[1] = matchDistr(clusterControl, setAC[0]);
          setBE[0] = (Adc[0] + Adc[6] - Adc[2])/sqrt(2.0);
          setBE[1] = matchDistr(clusterControl, setBE[0]);
          setC[0]  = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1]  = matchDistr(clusterControl, setC[0]);
          setDE[0] = (Adc[3] + Adc[6] - Adc[2])/sqrt(2.0);
          setDE[1] = matchDistr(clusterControl, setDE[0]);
          setE[0]  = (Adc[6] - Adc[2])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
	  Double_t tmp = 3e-33+(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  probACE  = (setAC[1]*setE[1])/tmp;
	  probADE  = (setA[1]*setDE[1])/tmp;
	  probBCE  = (setBE[1]*setC[1])/tmp;
	  newPointA->setFlag(int(100*(probACE+probADE)));
	  newPointB->setFlag(int(100*probBCE));
	  newPointC->setFlag(int(100*(probACE+probBCE)));
	  newPointD->setFlag(int(100*probADE));
	  if ((probACE > probADE)&&(probACE > probBCE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1]-Adc[3],CalibArray);
	      newPointC->setEnergyLossCorrected(Adc[3],Adc[1]-Adc[0],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[6],Adc[2],CalibArray);
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointE); 
	      delete newPointB;
	      delete newPointD;
	    }
	  else if ((probADE > probACE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLossCorrected(Adc[0],Adc[1],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[6],Adc[2]-Adc[3],CalibArray); 
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointE); 
	      delete newPointB;
	      delete newPointC;
	    }
	  else if ((probBCE > probACE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLossCorrected(Adc[3],Adc[1],CalibArray);
	      newPointE->setEnergyLossCorrected(Adc[6],Adc[2]-Adc[0],CalibArray);
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointE); 
	      delete newPointA;
	      delete newPointD;
	    }
          nSolved++;
  	}
// 13 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n"))//        case (2-3)C checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLossCorrected(Adc[2]-Adc[5], Adc[4],CalibArray);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLossCorrected(Adc[2]-Adc[4], Adc[5],CalibArray);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 14 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n"))//       case (3-2)CP checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[2]-Adc[5],CalibArray);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(2)); // Fixed thanks to Lilian !
 	  newPointD->setEnergyLossCorrected(Adc[5], Adc[2]-Adc[3],CalibArray);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 15 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p3n"))//        case (2-3)CS checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0]-Adc[2], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0]-Adc[1], Adc[2],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(5));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[3],CalibArray);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 16 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n2n"))//      case (3-2)CPS checked
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1]-Adc[2],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLossCorrected(Adc[2], Adc[1]-Adc[0],CalibArray);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(6));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[6],CalibArray);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 17 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n"))//        case (2-3)D
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));

 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          Double_t setAB[2], setAC[2], setBC[2], setDE[2], setDF[2], setEF[2];
          Double_t prob[6];

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1]  = matchDistr(clusterControl, setB[0]);
          setC[0]  = (Adc[0] - Adc[3])/sqrt(2.0);
          setC[1]  = matchDistr(clusterControl, setC[0]);
          setD[0]  = (Adc[4] - Adc[1])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
          setE[0]  = (Adc[4] - Adc[2])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
          setF[0]  = (Adc[4] - Adc[3])/sqrt(2.0);
          setF[1]  = matchDistr(clusterControl, setF[0]);
          setAB[0] = (Adc[0] - Adc[1] - Adc[2])/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setAC[0] = (Adc[0] - Adc[1] - Adc[3])/sqrt(2.0);
          setAC[1] = matchDistr(clusterControl, setAC[0]);
          setBC[0] = (Adc[0] - Adc[2] - Adc[3])/sqrt(2.0);
          setBC[1] = matchDistr(clusterControl, setBC[0]);
          setDE[0] = (Adc[4] - Adc[1] - Adc[2])/sqrt(2.0);
          setDE[1] = matchDistr(clusterControl, setDE[0]);
          setDF[0] = (Adc[4] - Adc[1] - Adc[3])/sqrt(2.0);
          setDF[1] = matchDistr(clusterControl, setDF[0]);
          setEF[0] = (Adc[4] - Adc[2] - Adc[3])/sqrt(2.0);
          setEF[1] = matchDistr(clusterControl, setEF[0]);
	  Double_t tmp = 3e-33+(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[0]  = (setAC[1]*setE[1]+setAB[1]*setF[1]+setEF[1]*setA[1])/tmp;
	  prob[1]  = (setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1])/tmp;
	  prob[2]  = (setAC[1]*setE[1]+setBC[1]*setD[1]+setDE[1]*setC[1])/tmp;
	  prob[3]  = (setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1])/tmp;
	  prob[4]  = (setAC[1]*setE[1]+setDE[1]*setC[1]+setEF[1]*setA[1])/tmp;
	  prob[5]  = (setAB[1]*setF[1]+setDF[1]*setB[1]+setEF[1]*setA[1])/tmp;
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
	  mPoint->addNewPoint(newPointA);
	  mPoint->addNewPoint(newPointB);
	  mPoint->addNewPoint(newPointC);
	  mPoint->addNewPoint(newPointD);
	  mPoint->addNewPoint(newPointE);
	  mPoint->addNewPoint(newPointF);
          nSolved++;
  	}
// 18 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n"))//        case (3-2)DP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));

 	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(1));

 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(2));

// traitement propre aux space points..(probabilite)
	  Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          Double_t setAC[2], setBD[2], setAE[2], setBF[2], setCE[2], setDF[2];
          Double_t prob[6];

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setB[0]  = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1]  = matchDistr(clusterControl, setB[0]);
          setC[0]  = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1]  = matchDistr(clusterControl, setC[0]);
          setD[0]  = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
          setE[0]  = (Adc[6] - Adc[1])/sqrt(2.0);
          setE[1]  = matchDistr(clusterControl, setE[0]);
          setF[0]  = (Adc[6] - Adc[2])/sqrt(2.0);
          setF[1]  = matchDistr(clusterControl, setF[0]);
          setAC[0] = (Adc[0] + Adc[3] - Adc[1])/sqrt(2.0);
          setAC[1] = matchDistr(clusterControl, setAC[0]);
          setBD[0] = (Adc[0] + Adc[3] - Adc[2])/sqrt(2.0);
          setBD[1] = matchDistr(clusterControl, setBD[0]);
          setAE[0] = (Adc[0] + Adc[6] - Adc[1])/sqrt(2.0);
          setAE[1] = matchDistr(clusterControl, setAE[0]);
          setBF[0] = (Adc[0] + Adc[6] - Adc[2])/sqrt(2.0);
          setBF[1] = matchDistr(clusterControl, setBF[0]);
          setCE[0] = (Adc[3] + Adc[6] - Adc[1])/sqrt(2.0);
          setCE[1] = matchDistr(clusterControl, setCE[0]);
          setDF[0] = (Adc[3] + Adc[6] - Adc[2])/sqrt(2.0);
          setDF[1] = matchDistr(clusterControl, setDF[0]);
	  Double_t tmp = 3e-33+(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[0]  = (setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1])/tmp;
	  prob[1]  = (setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1])/tmp;
	  prob[2]  = (setAC[1]*setF[1]+setCE[1]*setB[1]+setBF[1]*setC[1])/tmp;
	  prob[3]  = (setAE[1]*setD[1]+setCE[1]*setB[1]+setBD[1]*setE[1])/tmp;
	  prob[4]  = (setAE[1]*setD[1]+setDF[1]*setA[1]+setBD[1]*setE[1])/tmp;
	  prob[5]  = (setAC[1]*setF[1]+setDF[1]*setA[1]+setBF[1]*setC[1])/tmp;
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
	  mPoint->addNewPoint(newPointA);
	  mPoint->addNewPoint(newPointB);
	  mPoint->addNewPoint(newPointC);
	  mPoint->addNewPoint(newPointD);
	  mPoint->addNewPoint(newPointE);
	  mPoint->addNewPoint(newPointF);
          nSolved++;
  	}
// 19 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p3n"))//        case (3-3)A
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointE->setEnergyLossCorrected(Adc[6], Adc[5],CalibArray);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 20 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n3n"))//        case (3-3)AP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLossCorrected(Adc[5], Adc[7],CalibArray);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 21 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p2n3n"))//    case (3-3)B
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	  newPointA->setFlag(100);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[5],CalibArray);

 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
 	  newPointE->setEnergyLossCorrected(Adc[6], Adc[2],CalibArray);

 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLossCorrected(Adc[6], Adc[5],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setC[2], setD[2], setE[2], setF[2];
          Double_t probACF, probADE;

          setC[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[6] - Adc[2])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
	  Double_t tmp = 3e-33+ setC[1]*setF[1]+setD[1]*setE[1];
	  probACF = (setC[1]*setF[1])/tmp;
	  probADE = (setD[1]*setE[1])/tmp;
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));

	  if(probACF>probADE)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointF); 
	      delete newPointD;
	      delete newPointE;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointE);  
	      delete newPointC;
	      delete newPointF;
	    }
          nSolved++;
  	}
// 22 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p2n3n"))//    case (3-3)BP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	  newPointA->setFlag(100);
	
	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);
	
	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLossCorrected(Adc[2], Adc[5],CalibArray);
	
 	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointE->setEnergyLossCorrected(Adc[6], Adc[4],CalibArray);
 	
 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLossCorrected(Adc[6], Adc[5],CalibArray);
 	
// traitement propre aux space points..(probabilite)
          Double_t setC[2], setD[2], setE[2], setF[2];
          Double_t probACF, probADE;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[6] - Adc[4])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
	  Double_t tmp = 3e-33+ setC[1]*setF[1]+setD[1]*setE[1];
	  probACF = (setC[1]*setF[1])/tmp;
	  probADE = (setD[1]*setE[1])/tmp;
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));
	  if(probACF>probADE)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointF);
	      delete newPointD; 
	      delete newPointE; 
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointE);  
	      delete newPointC; 
	      delete newPointF; 
	    }	  
          nSolved++;
  	}
// 23 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n3n"))//    case (3-3)BS
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
	
	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);
	
	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);
	
 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);
 	
 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(8));
 	  newPointF->setEnergyLossCorrected(Adc[6], Adc[8],CalibArray);
 	  newPointF->setFlag(100);
 	
// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2];
          Double_t probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  Double_t tmp = 3e-33+(setA[1]*setD[1]+setB[1]*setC[1]);
	  probADF = (setA[1]*setD[1])/tmp;
	  probBCF = (setB[1]*setC[1])/tmp;
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
	  if(probADF>probBCF)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointF); 
	      delete newPointB;
	      delete newPointC;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointF);  
	      delete newPointA;
	      delete newPointD;
	    }
	  nSolved++;
  	}
// 24 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p3n"))//    case (3-3)BSP
 	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);

 	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

 	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointF->setEnergyLossCorrected(Adc[7], Adc[6],CalibArray);
          newPointF->setNMatched(33);
 	  newPointF->setFlag(100);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2];
          Double_t probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  Double_t tmp = 3e-33+(setA[1]*setD[1]+setB[1]*setC[1]);
	  probADF = (setA[1]*setD[1])/tmp;
	  probBCF = (setB[1]*setC[1])/tmp;
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
	  if(probADF>probBCF)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointF); 
	      delete newPointB;
	      delete newPointC;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointF);  
	      delete newPointA;
	      delete newPointD;
	    }
          nSolved++;
  	}
// 25 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p3n"))//    case (3-3)C
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[2],CalibArray);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointF->setEnergyLossCorrected(Adc[7], Adc[3],CalibArray);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 26 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n3n"))//    case (3-3)CS
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointD->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(5), currentPackage->getMatched(8));
 	  newPointF->setEnergyLossCorrected(Adc[5], Adc[8],CalibArray);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 27 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p2n3n"))//   case (3-3)D
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[2],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointE->setEnergyLossCorrected(Adc[4], Adc[3],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLossCorrected(Adc[7], Adc[2],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointG->setEnergyLossCorrected(Adc[7], Adc[3],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setD[2], setE[2], setF[2], setG[2];
          Double_t probADG, probAEF;

          setD[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[3])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[7] - Adc[2])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[7] - Adc[3])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
	  Double_t tmp = 3e-33+(setD[1]*setG[1]+setE[1]*setF[1]);
	  probADG = (setD[1]*setG[1])/tmp;
	  probAEF = (setE[1]*setF[1])/tmp;
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*probADG));
	  if(probADG>probAEF)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointG); 
	      delete newPointE;
	      delete newPointF;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointE);
	      mPoint->addNewPoint(newPointF);
	      delete newPointD;
	      delete newPointG;  
	    }
          nSolved++;
  	}
// 28 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p1n2n3n"))// case (3-3)DP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLossCorrected(Adc[2], Adc[5],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointF->setEnergyLossCorrected(Adc[6], Adc[4],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointG->setEnergyLossCorrected(Adc[6], Adc[5],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setC[2], setD[2], setF[2], setG[2];
          Double_t probACG, probADF;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setF[0] = (Adc[6] - Adc[4])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
	  Double_t tmp = 3e-33+(setC[1]*setG[1]+setD[1]*setF[1]);
	  probACG = (setC[1]*setG[1])/tmp;
	  probADF = (setD[1]*setF[1])/tmp;
	  newPointC->setFlag(int(100*probACG));
	  newPointD->setFlag(int(100*probADF));
	  newPointF->setFlag(int(100*probADF));
	  newPointG->setFlag(int(100*probACG));
	  if(probACG>probADF)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointG); 
	      delete newPointD;
	      delete newPointF;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointF);  
	      delete newPointC;
	      delete newPointG;
	    }
          nSolved++;
  	}
// 29 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n3n"))// case (3-3)DS
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(9));
 	  newPointG->setEnergyLossCorrected(Adc[6], Adc[9],CalibArray);
          newPointG->setFlag(100);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2];
          Double_t probADG, probBCG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  Double_t tmp = 3e-33+(setA[1]*setD[1]+setB[1]*setC[1]);
	  probADG = (setA[1]*setD[1])/tmp;
	  probBCG = (setB[1]*setC[1])/tmp;
	  newPointA->setFlag(int(100*probADG));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));
	  if(probADG>probBCG)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointG); 
	      delete newPointB;
	      delete newPointC;
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointG);  
	      delete newPointA;
	      delete newPointD;
	    }
          nSolved++;
  	}
// 30 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p3n"))//case (3-3)DSP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[1],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLossCorrected(Adc[4], Adc[2],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointG->setEnergyLossCorrected(Adc[8], Adc[3],CalibArray);
          newPointG->setFlag(100);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setD[2], setE[2];
          Double_t probAEG, probBDG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setD[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[1])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
	  Double_t tmp = 3e-33+(setA[1]*setE[1]+setB[1]*setD[1]);
	  probAEG = (setA[1]*setE[1])/tmp;
	  probBDG = (setB[1]*setD[1])/tmp;
	  newPointA->setFlag(int(100*probAEG));
	  newPointB->setFlag(int(100*probBDG));
	  newPointD->setFlag(int(100*probBDG));
	  newPointE->setFlag(int(100*probAEG));

	  if(probAEG>probBDG)
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointE);
	      mPoint->addNewPoint(newPointG);
	      delete newPointB; 
	      delete newPointD; 
	    }
	  else
	    {
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointG);  
	      delete newPointA; 
	      delete newPointE; 
	    }
          nSolved++;
  	}
// 31 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p3n"))//   case (3-3)E
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLossCorrected(Adc[2], Adc[4],CalibArray);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointE->setEnergyLossCorrected(Adc[6], Adc[5],CalibArray);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 32 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n3n"))//   case (3-3)EP
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLossCorrected(Adc[5], Adc[7],CalibArray);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 33 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p2n3n"))//  case (3-3)F
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLossCorrected(Adc[3], Adc[6],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLossCorrected(Adc[7], Adc[2],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointG->setEnergyLossCorrected(Adc[7], Adc[6],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2];
          Double_t probADG, probAEF, probBCG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[3] - Adc[6])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[7] - Adc[2])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[7] - Adc[6])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
	  Double_t tmp  = 3e-33+(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  probADG = (setA[1]*setD[1]*setG[1])/tmp;
	  probAEF = (setA[1]*setE[1]*setF[1])/tmp;
	  probBCG = (setB[1]*setC[1]*setG[1])/tmp;
	  newPointA->setFlag(int(100*(probADG+probAEF)));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*(probADG+probBCG)));

	  if ((probADG > probAEF)&&(probADG > probBCG))
	    {
	     
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointD);
	      mPoint->addNewPoint(newPointG); 
	      delete newPointB;
	      delete newPointC;
	      delete newPointE;
	      delete newPointF;
	    }
	  else if ((probAEF > probADG)&&(probAEF > probBCG))
	    {
	      mPoint->addNewPoint(newPointA);
	      mPoint->addNewPoint(newPointE);
	      mPoint->addNewPoint(newPointF); 
	      delete newPointB;
	      delete newPointC;
	      delete newPointD;
	      delete newPointG;
	    }
	  else if ((probBCG > probADG)&&(probBCG > probAEF))
	    {
	      mPoint->addNewPoint(newPointB);
	      mPoint->addNewPoint(newPointC);
	      mPoint->addNewPoint(newPointG); 
	      delete newPointA;
	      delete newPointD;
	      delete newPointE;
	      delete newPointF;
	    }
          nSolved++;
  	}
// 34 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p2n3n"))//  case (3-3)G
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLossCorrected(Adc[0], Adc[3],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[1],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLossCorrected(Adc[4], Adc[2],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLossCorrected(Adc[4], Adc[3],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointG->setEnergyLossCorrected(Adc[8], Adc[2],CalibArray);

	  StSstPoint *newPointH = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointH->setEnergyLossCorrected(Adc[8], Adc[3],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          Double_t probAEH, probAFG, probBDH, probCDG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[0] - Adc[3])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[4] - Adc[1])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[4] - Adc[3])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[8] - Adc[2])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
          setH[0] = (Adc[8] - Adc[3])/sqrt(2.0);
          setH[1] = matchDistr(clusterControl, setH[0]);
	  Double_t tmp = 3e-33+(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probAEH = (setA[1]*setE[1]*setH[1])/tmp;
	  probAFG = (setA[1]*setF[1]*setG[1])/tmp;
	  probBDH = (setB[1]*setD[1]*setH[1])/tmp;
	  probCDG = (setC[1]*setD[1]*setG[1])/tmp;
	  newPointA->setFlag(int(100*(probAEH+probAFG)));
	  newPointB->setFlag(int(100*probBDH));
	  newPointC->setFlag(int(100*probCDG));
	  newPointD->setFlag(int(100*(probBDH+probCDG)));
	  newPointE->setFlag(int(100*probAEH));
	  newPointF->setFlag(int(100*probAFG));
	  newPointG->setFlag(int(100*(probAFG+probCDG)));
	  newPointH->setFlag(int(100*(probAEH+probBDH)));
	  mPoint->addNewPoint(newPointA);
	  mPoint->addNewPoint(newPointB);
	  mPoint->addNewPoint(newPointC);
	  mPoint->addNewPoint(newPointD);
	  mPoint->addNewPoint(newPointE);
	  mPoint->addNewPoint(newPointF);
	  mPoint->addNewPoint(newPointG);
	  mPoint->addNewPoint(newPointH);
          nSolved++;
  	}
// 35 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p1n2n3n"))//  case (3-3)GS
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0],  Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLossCorrected(Adc[3], Adc[1],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLossCorrected(Adc[3], Adc[2],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLossCorrected(Adc[3], Adc[6],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(1));
 	  newPointF->setEnergyLossCorrected(Adc[7], Adc[1],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointG->setEnergyLossCorrected(Adc[7], Adc[2],CalibArray);

	  StSstPoint *newPointH = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointH->setEnergyLossCorrected(Adc[7], Adc[6],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          Double_t probADH, probAEG, probBCH, probBEF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[3] - Adc[6])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[7] - Adc[1])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[7] - Adc[2])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
          setH[0] = (Adc[7] - Adc[6])/sqrt(2.0);
          setH[1] = matchDistr(clusterControl, setH[0]);
          Double_t tmp = 3e-33+(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probADH = (setA[1]*setD[1]*setH[1])/tmp;
	  probAEG = (setA[1]*setE[1]*setG[1])/tmp;
	  probBCH = (setB[1]*setC[1]*setH[1])/tmp;
	  probBEF = (setB[1]*setE[1]*setF[1])/tmp;
	  newPointA->setFlag(int(100*(probADH+probAEG)));
	  newPointB->setFlag(int(100*(probBCH+probBEF)));
	  newPointC->setFlag(int(100*probBCH));
	  newPointD->setFlag(int(100*probADH));
	  newPointE->setFlag(int(100*(probAEG+probBEF)));
	  newPointF->setFlag(int(100*probBEF));
	  newPointG->setFlag(int(100*probAEG));
	  newPointH->setFlag(int(100*(probADH+probBCH)));
	  mPoint->addNewPoint(newPointA);
	  mPoint->addNewPoint(newPointB);
	  mPoint->addNewPoint(newPointC);
	  mPoint->addNewPoint(newPointD);
	  mPoint->addNewPoint(newPointE);
	  mPoint->addNewPoint(newPointF);
	  mPoint->addNewPoint(newPointG);
	  mPoint->addNewPoint(newPointH);

          nSolved++;
  	}
// 36 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p1n2n3n"))// case (3-3)H
  	{
	  StSstPoint *newPointA = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLossCorrected(Adc[0], Adc[1],CalibArray);

	  StSstPoint *newPointB = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLossCorrected(Adc[0], Adc[2],CalibArray);

	  StSstPoint *newPointC = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLossCorrected(Adc[0], Adc[3],CalibArray);

	  StSstPoint *newPointD = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLossCorrected(Adc[4], Adc[1],CalibArray);

	  StSstPoint *newPointE = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLossCorrected(Adc[4], Adc[2],CalibArray);

	  StSstPoint *newPointF = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLossCorrected(Adc[4], Adc[3],CalibArray);

	  StSstPoint *newPointG = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(1));
 	  newPointG->setEnergyLossCorrected(Adc[8], Adc[1],CalibArray);

	  StSstPoint *newPointH = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointH->setEnergyLossCorrected(Adc[8], Adc[2],CalibArray);

	  StSstPoint *newPointI = new StSstPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointI, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointI->setEnergyLossCorrected(Adc[8], Adc[3],CalibArray);

// traitement propre aux space points..(probabilite)
          Double_t setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2], setI[2];
          Double_t probAEI, probCEG, probAFH, probBDI, probCDH, probBFG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[0] - Adc[3])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[4] - Adc[1])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[4] - Adc[3])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[8] - Adc[1])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
          setH[0] = (Adc[8] - Adc[2])/sqrt(2.0);
          setH[1] = matchDistr(clusterControl, setH[0]);
          setI[0] = (Adc[8] - Adc[3])/sqrt(2.0);
          setI[1] = matchDistr(clusterControl, setI[0]);
          Double_t tmp = (3e-33+setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probAEI = (setA[1]*setE[1]*setI[1])/(tmp);
	  probCEG = (setC[1]*setE[1]*setG[1])/(tmp);
	  probAFH = (setA[1]*setF[1]*setH[1])/(tmp);
	  probBDI = (setB[1]*setD[1]*setI[1])/(tmp);
	  probCDH = (setC[1]*setD[1]*setH[1])/(tmp);
	  probBFG = (setB[1]*setF[1]*setG[1])/(tmp);
	  newPointA->setFlag(int(100*(probAEI+probAFH)));
	  newPointB->setFlag(int(100*(probBDI+probBFG)));
	  newPointC->setFlag(int(100*(probCEG+probCDH)));
	  newPointD->setFlag(int(100*(probBDI+probCDH)));
	  newPointE->setFlag(int(100*(probAEI+probCEG)));
	  newPointF->setFlag(int(100*(probAFH+probBFG)));
	  newPointG->setFlag(int(100*(probCEG+probBFG)));
	  newPointH->setFlag(int(100*(probAFH+probCDH)));
	  newPointI->setFlag(int(100*(probAEI+probBDI)));
	  mPoint->addNewPoint(newPointA);
	  mPoint->addNewPoint(newPointB);
	  mPoint->addNewPoint(newPointC);
	  mPoint->addNewPoint(newPointD);
	  mPoint->addNewPoint(newPointE);
	  mPoint->addNewPoint(newPointF);
	  mPoint->addNewPoint(newPointG);
	  mPoint->addNewPoint(newPointH);
	  mPoint->addNewPoint(newPointI);	
          nSolved++;
  	}
      else LOG_INFO<<" Warning unsolved case ("<<currentKind<<")\n";// other cases
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nSolved;
}
//________________________________________________________________________________
Int_t StSstWafer::convertDigitToAnalog(Double_t pairCreationEnergy)
{
  StSstPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      currentPoint->setDe(currentPoint->getDe(0)*pairCreationEnergy,0);
      currentPoint->setDe(currentPoint->getDe(1)*pairCreationEnergy,1);
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}
//________________________________________________________________________________
Int_t StSstWafer::convertUFrameToLocal(sstDimensions_st *dimensions)
{
  StSstPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      //      printf("posU(0)=%f posU(1)=%f\n",currentPoint->getPositionU(0),currentPoint->getPositionU(1));
      currentPoint->setXl(currentPoint->getPositionU(0)/2.+currentPoint->getPositionU(1)/2.-dimensions[0].waferHalfActLength+dimensions[0].waferHalfActWidth*tan(dimensions[0].stereoAngle),0);
      currentPoint->setXl((currentPoint->getPositionU(1)-currentPoint->getPositionU(0))/(2*tan(dimensions[0].stereoAngle)),2);
      currentPoint->setXl(0.0,1); //yL =0
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}
//________________________________________________________________________________
Int_t StSstWafer::convertLocalToGlobal() {
  StSstPoint *currentPoint = mPoint->first();
  Double_t xg[3];
  while(currentPoint) {
    Double_t  xl[3] = {currentPoint->getXl(0), currentPoint->getXl(1), currentPoint->getXl(2)};
    LocalToMaster(xl,xg);
    currentPoint->setXg(xg[0],0);
    currentPoint->setXg(xg[1],1);
    currentPoint->setXg(xg[2],2);
    currentPoint = mPoint->next(currentPoint);
  }
  return 1;
}
//________________________________________________________________________________
Int_t StSstWafer::convertGlobalToLocal() {
  Int_t localSize = (this->mPoint)->getSize();
  if (!localSize) return 0;
  StSstPoint *temp = mPoint->first();
  for (Int_t i = 0; i < localSize; i++) {
    Double_t xg[3] = {temp->getXg(0), temp->getXg(1), temp->getXg(2)};
    Double_t xl[3];
    MasterToLocal(xg,xl);
    temp->setXl( xl[0], 0);
    temp->setXl( xl[1], 1);
    temp->setXl( xl[2], 2);
    temp = mPoint->next(temp);
  }
  return 1;
}
//________________________________________________________________________________
Int_t StSstWafer::convertLocalToUFrame(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge, Float_t Theta) 
{
  Int_t localSize = (mPoint)->getSize();
  if (!localSize) return 0;

  StSstPoint *temp = (mPoint)->first();
  for (Int_t i = 0; i < localSize; i++)

    {
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))-(temp->getXl(1)+(ActiveSmallEdge/2.))*tan(Theta), 0);
      temp->setUpos((temp->getXl(0)+(ActiveLargeEdge/2.))+(temp->getXl(1)-(ActiveSmallEdge/2.))*tan(Theta), 1); 
      temp = mPoint->next(temp);
    }
  //return 1; 
  return localSize;
} 
//________________________________________________________________________________
StSstPointList* StSstWafer::getDeadHits(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge,Float_t Test)
{
  StSstPointList *listDeadBorder   = getNonActivePointBorder(ActiveLargeEdge,ActiveSmallEdge);
  StSstPointList *listDeadTriangle = getNonActivePointTriangle(Test);
  StSstPointList *listDeadTotal    = new StSstPointList();
  listDeadTotal = listDeadTotal->addPointList(listDeadBorder);
  listDeadTotal = listDeadTotal->addPointList(listDeadTriangle);
  listDeadTotal = listDeadTotal->removeMultipleCount();
  (mPoint)->substractPointList(listDeadTotal);
  delete listDeadBorder;
  delete listDeadTriangle;
  return listDeadTotal;
}
//________________________________________________________________________________
void StSstWafer::convertToStrip(Float_t Pitch, 
				Int_t nStripPerSide,
				Double_t pairCreationEnergy,
				Int_t nstripInACluster,
				Double_t parDiffP,
				Double_t parDiffN,
				Double_t parIndRightP,
				Double_t parIndRightN,
				Double_t parIndLeftP,
				Double_t parIndLeftN,
				Float_t  mShift_hole,
				Float_t  mShift_elec)
{
  convertHitToStrip(Pitch, 
		    nStripPerSide,
		    nstripInACluster,
		    parDiffP,
		    parDiffN,
		    parIndRightP,
		    parIndRightN,
		    parIndLeftP,
		    parIndLeftN,
		    mShift_hole,
		    mShift_elec);
  convertAnalogToDigit(pairCreationEnergy);
  mStripP->sortStrip();
  mStripN->sortStrip();
}
//________________________________________________________________________________
Int_t StSstWafer::printborder()
{
  if (mId==7101){
    printf("Wafer = %d \n",mId);
    Double_t actives[4][3],templs[4][3];
    Double_t activee[4][3],temple[4][3];

    actives[0][0]=-3.65,actives[1][0]= 3.65,actives[2][0]= 3.65,actives[3][0]=-3.65;
    actives[0][1]= 2.00,actives[1][1]= 2.00,actives[2][1]=-2.00,actives[3][1]=-2.00;
    actives[0][2]= 0.00,actives[1][2]= 0.00,actives[2][2]= 0.00,actives[3][2]= 0.00;

    activee[0][0]= 3.65,activee[1][0]= 3.65,activee[2][0]=-3.65,activee[3][0]=-3.65;
    activee[0][1]= 2.00,activee[1][1]=-2.00,activee[2][1]=-2.00,activee[3][1]= 2.00;
    activee[0][2]= 0.00,activee[1][2]= 0.00,activee[2][2]= 0.00,activee[3][2]= 0.00;
    for (Int_t j = 0; j < 4; j++) {
      LocalToMaster(actives[j], templs[j]);
      LocalToMaster(activee[j], temple[j]);
    }
    const Char_t *xyz[3] = {"x","y","z"};
    for (Int_t i = 0; i < 3; i++) {
      printf("%ssSsdLadder%d set {",xyz[i],mId-7100);
      for (Int_t j = 0; j < 4; j++)  printf("%.2f ",templs[j][i]);
      printf("}\n");
      printf("%seSsdLadder%d set {",xyz[i],mId-7100);
      for (Int_t j = 0; j < 4; j++)  printf("%.2f ",temple[j][i]);
      printf("}\n");
    }
  }
  return 1;
}
//________________________________________________________________________________
StSstWafer::StSstWafer(const StSstWafer & originalWafer)
{
  memcpy (first, originalWafer.first, last-first); 
  mStripP   = new StSstStripList();
  mStripN   = new StSstStripList();
  mNoiseP   = new StSpaListNoise();
  mNoiseN   = new StSpaListNoise();
  mClusterP = new StSstClusterList();
  mClusterN = new StSstClusterList();
  mPackage  = new StSstPackageList();
  mPoint    = new StSstPointList();
}

StSstWafer& StSstWafer::operator=(const StSstWafer & originalWafer) {
  memset(first, 0, last-first);
  mId         = originalWafer.mId;
  SetName(originalWafer.GetName());
  SetRotation(originalWafer.GetRotationMatrix());
  SetTranslation(originalWafer.GetTranslation());
  return *this;
}
//________________________________________________________________________________
/*!
Determines if two clusters can be matched based on geometrical considerations
 */
Int_t StSstWafer::geoMatched(sstDimensions_st *dimensions, StSstCluster *ptr1, StSstCluster *ptr2)
{
  Int_t geomatched = 0;
  Int_t numStrip = int((2*dimensions[0].waferHalfActWidth*tan(dimensions[0].stereoAngle)/dimensions[0].stripPitch)+1);
  if ( (!ptr1) || (!ptr2) )
    geomatched = 0;
  else if((ptr2->getStripMean() > ( ptr1->getStripMean() - numStrip))
     && (ptr2->getStripMean() < (ptr1->getStripMean()  + numStrip)))
    geomatched = 1;
  return geomatched;
}
//________________________________________________________________________________
/*!
Must be useful but for what ???
 */
void StSstWafer::setMatcheds(sstDimensions_st *dimensions, StSstPoint *Point, StSstCluster *pMatched, StSstCluster *nMatched)
{// strip(1) -> Upos(0)...
  Point->setPositionU((pMatched->getStripMean()-1)*dimensions[0].stripPitch,0);
  Point->setPositionU((nMatched->getStripMean()-1)*dimensions[0].stripPitch,1);
  Point->setIdClusterP(pMatched->getNCluster());
  Point->setIdClusterN(nMatched->getNCluster());

  // for evaluation only !!!
  Int_t pHitIndex   = 0;
  Int_t nHitIndex   = 0;
  Int_t sptHitIndex = 0;
  for (pHitIndex = 0; pHitIndex < SST_MAXIDMCHIT; pHitIndex++)
    {
      for (nHitIndex = 0; nHitIndex < SST_MAXIDMCHIT; nHitIndex++)
	{
	  if ((pMatched->getIdMcHit(pHitIndex))
	      &&(nMatched->getIdMcHit(nHitIndex))
              &&(pMatched->getIdMcHit(pHitIndex) == nMatched->getIdMcHit(nHitIndex))
	      &&(sptHitIndex < SST_MAXIDMCHIT))
	      Point->setNMchit(pMatched->getIdMcHit(pHitIndex),sptHitIndex++);
	}
    }
}
//________________________________________________________________________________
Double_t StSstWafer::matchDistr(StSstClusterControl *clusterControl, Double_t x)
{
  Double_t mean = clusterControl->getMatchMean();
  Double_t sigm = clusterControl->getMatchSigma();
  return TMath::Gaus(x, mean, sigm, 1);
}
//________________________________________________________________________________
void StSstWafer::addHit(Int_t rNId , Int_t rMcHit, Int_t rMcTrack, Float_t *rXg , Float_t rDe, Float_t *p)
{
  Float_t *alpha = new float[2];
  alpha[0]  = 0.;
  alpha[1]  = 0.;
  StSstPoint *tmpPoint = new StSstPoint(rNId, rMcHit, rMcTrack, rXg, rDe, findAngle(p,alpha));
  (mPoint)->addNewPoint(tmpPoint);
  delete [] alpha;
}
//________________________________________________________________________________
StSstPointList*  StSstWafer::getNonActivePointBorder(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge) 
{
  Int_t localSize = (mPoint)->getSize();
  
  StSstPointList *deadPoints = new StSstPointList();
  if (!localSize) return deadPoints;
  
  StSstPoint *temp = (mPoint)->first();
  for (Int_t i = 0; i < localSize; i++)
    {
      if((temp->getXl(0) >(ActiveLargeEdge/2.)) || (temp->getXl(0) < (-ActiveLargeEdge/2.)) || 
	 (temp->getXl(2) >(ActiveSmallEdge/2.)) || (temp->getXl(2) < (-ActiveSmallEdge/2.)))
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StSstPoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (mPoint)->next(temp);
    }
  return deadPoints;
}
//________________________________________________________________________________
  StSstPointList* StSstWafer::getNonActivePointTriangle(Float_t Test)
    //typically, test=pitch
{
  Int_t localSize = (mPoint)->getSize();
  StSstPointList *deadPoints = new StSstPointList();  
  
  if (!localSize) return deadPoints;
  
  StSstPoint *temp = (mPoint)->first();
  for (Int_t i = 0; i < localSize; i++)
    {
      if (temp->getPositionU(0) < -1.*Test && temp->getPositionU(1) < -1.*Test) 
	{
	  // tempo : I can remove the hit now, just to keep information.
	  StSstPoint *badPoint = temp->giveCopy();
	  deadPoints->addNewPoint(badPoint);
	}
      temp = (mPoint)->next(temp);
    }
  return deadPoints;
} 

//________________________________________________________________________________
Double_t StSstWafer::myErf(Double_t x)
{
  const Double_t a1 = -1.26551223,   a2 = 1.00002368,
               a3 =  0.37409196,   a4 = 0.09678418,
               a5 = -0.18628806,   a6 = 0.27886807,
               a7 = -1.13520398,   a8 = 1.48851587,
               a9 = -0.82215223,  a10 = 0.17087277;
  
  Double_t v = 1.;
  Double_t z = ((x) < 0. ? -(x) : (x));
  
  if (z <= 0) return (1.-v); // erfc(0)=1
  Double_t t = 1./(1.+0.5*z);
  v = t*exp((-z*z) +a1+t*(a2+t*(a3+t*(a4+t*(a5+t*(a6+t*(a7+t*(a8+t*(a9+t*a10)))))))));
  
  if (x < 0) v = 2.-v; // erfc(-x)=2-erfc(x)
  return (1.-v);
}
//________________________________________________________________________________
void StSstWafer::convertHitToStrip(Float_t Pitch, 
				   Int_t nStripPerSide,
				   Int_t nstripInACluster,
				   Double_t parDiffP,
				   Double_t parDiffN,
				   Double_t parIndRightP,
				   Double_t parIndRightN,
				   Double_t parIndLeftP,
				   Double_t parIndLeftN,
				   Float_t mShift_hole,
				   Float_t mShift_elec)
{
  const Double_t parDiff[2]={parDiffP/Pitch,parDiffN/Pitch};
  const Double_t parIndRight[2]={parIndRightP,parIndRightN};
  const Double_t parIndLeft[2]={parIndLeftP,parIndLeftN};

  Int_t   *tabInd   = new Int_t[nstripInACluster];
  Float_t *tabDe    = new Float_t[nstripInACluster];

  StSstPoint *ptr = (mPoint)->first();
  Int_t localSize = (mPoint)->getSize();
  for (Int_t iPoint = 0; iPoint < localSize; iPoint++)
    {
      for (Int_t iSide = 0; iSide < 2; iSide++)
	{
	  for (Int_t v = 0 ; v < nstripInACluster; v++) 
	    {
	      tabInd[v] = 0 ;
	      tabDe[v]  = 0.;
	    }
	  if (Debug())	  LOG_DEBUG<<Form("Before Lorentz Shift")<<endm;
	  if (Debug())	  LOG_DEBUG<<Form("position of the hit : strip P=%f stripN=%f Pitch=%f",ptr->getPositionU(0),ptr->getPositionU(1),Pitch)<<endm;
	  UndoLorentzShift(ptr,iSide,mShift_hole,mShift_elec,Pitch);
	  if (Debug())	  LOG_DEBUG<<Form("After Lorentz Shift\n");
	  if (Debug())	  LOG_DEBUG<<Form("position of the hit : strip P=%f stripN=%f Pitch=%f",ptr->getPositionU(0),ptr->getPositionU(1),Pitch)<<endm;
	  tabInd[0] = (int)(ptr->getPositionU(iSide)/Pitch + 1.);//getPositionU(iSide)->getPositionU(iSide)+shift
	  tabInd[1] = tabInd[0]+1;
	  tabInd[2] = tabInd[0]-1;
	  tabInd[3] = tabInd[0]+2;
	  if (Debug())	  LOG_DEBUG<<Form("Mean strip=%d strip1=%d strip2=%d strip3=%d",tabInd[0],tabInd[1],tabInd[2],tabInd[3])<<endm;
	  Double_t rest = (double)(ptr->getPositionU(iSide)/Pitch) - (double)(tabInd[0]-1);
	  Double_t Result=0.5*(1.+myErf((rest-0.5)/sqrt(2.)/parDiff[iSide]) );
	  Float_t TmpDe0 = 0.;
	  Float_t TmpDe1 = 0.;
	  tabDe[0] = (1.-Result)*ptr->getDe();
	  tabDe[1] = Result*ptr->getDe();
	  tabDe[2] = tabDe[0]*parIndLeft[iSide];
	  tabDe[3] = tabDe[1]*parIndRight[iSide];
	  TmpDe0 = tabDe[1]*parIndLeft[iSide];
	  TmpDe1 = tabDe[0]*parIndRight[iSide];
	  tabDe[0] += TmpDe0;
	  tabDe[1] += TmpDe1;
	  for (Int_t st = 0; st <  nstripInACluster; st++)
	    {
	      if ( tabInd[st] > 0 && tabInd[st] < nStripPerSide+1 )
		{
		  if (Debug())printf("st =%d id =%d tabDe(%d)=%f charge=%f NId=%d McHit=%d MvTrack=%d\n",st,tabInd[st],st,tabDe[st],ptr->getDe(),ptr->getNId(),ptr->getMcHit(),ptr->getMcTrack());
		  StSstStrip *newStrip = new StSstStrip(tabInd[st], ptr->getNId(), ptr->getMcHit(), ptr->getMcTrack(), tabDe[st]);
		  switch (iSide)
		    {
		    case 0:
		      mStripP->updateStrip(newStrip);
		      break;
		    case 1:
		      mStripN->updateStrip(newStrip);
		      break;
		    }
		}
	    }
	}
      ptr = (mPoint)->next(ptr);
    }
  delete [] tabInd;
  delete [] tabDe;
}
//________________________________________________________________________________
void StSstWafer::convertAnalogToDigit(Double_t pairCreationEnergy)
{
  const Double_t ConversionFactor = 1./pairCreationEnergy;//GeV

  Int_t localSize    = mStripP->getSize();
  StSstStrip *curr = mStripP->first();
  Int_t i = 0;
  for (i = 0; i < localSize; i++)
    {
      curr->setDigitSig((int)(curr->getAnalogSig()*ConversionFactor));
      curr = mStripP->next(curr);
    }
  localSize  = mStripN->getSize();
  curr = mStripN->first();
  for (i = 0; i < localSize; i++)
    {
      curr->setDigitSig((int)(curr->getAnalogSig()*ConversionFactor));
      curr = mStripN->next(curr);
    }
}
//________________________________________________________________________________
float* StSstWafer::findAngle(Float_t *p, Float_t *alpha)
{
  Int_t i = 0;
  Float_t pT[3],pN[3],pD[3];

  Float_t spT = 0.;
  Float_t spN = 0.;
  Float_t spD = 0.;

  Float_t npN = 0.;
  Float_t npD = 0.;

  for (i = 0; i < 3; i++)
    {
      spN  += n(i)*p[i]  ;
      spT  += t(i)*p[i]  ;
      spD  += d(i)*p[i]  ;
    }
  for (i = 0; i < 3; i++)
    {
      pN[i] = n(i)*spN  ;
      pT[i] = t(i)*spT  ;
      pD[i] = d(i)*spD  ;
    }

  npD = sqrt(pD[0]*pD[0]+pD[1]*pD[1]+pD[2]*pD[2]);
  npN = sqrt(pN[0]*pN[0]+pN[1]*pN[1]+pN[2]*pN[2]);

//   Float_t np    = sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  Float_t npDN  = sqrt((pN[0]+pD[0])*(pN[0]+pD[0])+(pN[1]+pD[1])*(pN[1]+pD[1])+(pN[2]+pD[2])*(pN[2]+pD[2]));
  Float_t npTD  = sqrt((pT[0]+pD[0])*(pT[0]+pD[0])+(pT[1]+pD[1])*(pT[1]+pD[1])+(pT[2]+pD[2])*(pT[2]+pD[2]));

  alpha[0] = acos(npN/npDN);
  Float_t sSign = 0.;
  sSign = pD[0]*pN[0]+pD[1]*pN[1]+pD[2]*pN[2]+npN*npN;
  if (sSign<0.) alpha[0] = -1.*alpha[0];

  alpha[1] = acos(npD/npTD);
  sSign = pD[0]*pT[0]+pD[1]*pT[1]+pD[2]*pT[2]+npD*npD;
  if (sSign<0.) alpha[1] = -1.*alpha[1];

  return alpha;
}
//________________________________________________________________________________
void StSstWafer::addNoiseToStripSignal(StSpaNoise *ptr, Int_t iSide)
{
  if (iSide)
    { mNoiseN->addNewNoise(ptr); }
  else
    { mNoiseP->addNewNoise(ptr); }
}
//________________________________________________________________________________
void StSstWafer::setIsActive(Int_t rIsActive, Int_t iSide, Int_t rNStrip)
{
  if (iSide)
    { mNoiseN->setIsActive(rIsActive, rNStrip); }
  else
    { mNoiseP->setIsActive(rIsActive, rNStrip); }
}
//________________________________________________________________________________
void StSstWafer::sortNoise()
{
  mNoiseP->sortStrip();
  mNoiseN->sortStrip();
}
//________________________________________________________________________________
void StSstWafer::addNoiseToStripSignal(long nElectronInAMip,long adcDynamic)
{
  mNoiseP->addSignal(mStripP, nElectronInAMip, adcDynamic);
  mNoiseN->addSignal(mStripN, nElectronInAMip, adcDynamic);
}
//________________________________________________________________________________
void StSstWafer::pedestalSubstraction()
{
  mNoiseP->substractPedestal();
  mNoiseN->substractPedestal();
}
//________________________________________________________________________________
void StSstWafer::zeroSubstraction()
{
  mNoiseP->zeroSubstraction();
  mNoiseN->zeroSubstraction();
}
//________________________________________________________________________________
void StSstWafer::convertAnalogToDigit(Long_t nElectronInAMip,Long_t adcDynamic,
				      Long_t nbitEncoding, Float_t daqCutValue)
{
  mNoiseP->convertAnalogToDigit(nElectronInAMip, adcDynamic,
					nbitEncoding,daqCutValue);
  mNoiseN->convertAnalogToDigit(nElectronInAMip, adcDynamic,
					nbitEncoding,daqCutValue);
}
//________________________________________________________________________________
void StSstWafer::updateStripList()
{
  mStripP->updateStripList(mNoiseP); 
  mStripN->updateStripList(mNoiseN); 
}
//________________________________________________________________________________
void StSstWafer::UndoLorentzShift(StSstPoint *ptr,Int_t iSide,Float_t mShift_hole,Float_t mShift_elec,Float_t pitch)
{
  Float_t tempPosition = ptr->getPositionU(iSide);
  if(iSide==0){
    ptr->setPositionU(tempPosition+mShift_hole,iSide);
  }
  else
    if(iSide==1){
      ptr->setPositionU(tempPosition+mShift_elec,iSide);
    }
}


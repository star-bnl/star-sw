#include "StSsdWafer.hh"

#include <Stiostream.h>
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_ssdConfiguration_Table.h"

#include "StSsdStripList.hh"
#include "StSsdStrip.hh"
#include "StSsdClusterList.hh"
#include "StSsdCluster.hh"
#include "StSsdPackageList.hh"
#include "StSsdPackage.hh"
#include "StSsdPointList.hh"
#include "StSsdPoint.hh"
#include "StSsdClusterControl.h"
#include "StSsdDynamicControl.h"

// StSsdWafer::StSsdWafer(int nid ,int *nDeadStripP, int *nDeadStripN)
StSsdWafer::StSsdWafer(int nid)
{
  mId       = nid;
  mD        = new float[3];
  mT        = new float[3];
  mN        = new float[3];
  mX        = new float[3];
//   mDeadStripP = nDeadStripP;
//   mDeadStripN = nDeadStripN;

  mStripP   = new StSsdStripList();
  mStripN   = new StSsdStripList();
  mClusterP = new StSsdClusterList();
  mClusterN = new StSsdClusterList();
  mPackage  = new StSsdPackageList();
  mPoint    = new StSsdPointList();

  for(int i = 0; i < 3; i++)
    {
      mD[i] = 0; 
      mT[i] = 0; 
      mN[i] = 0; 
      mX[i] = 0;
    } 
}
StSsdWafer::~StSsdWafer()
{
  delete[]  mD;
  delete[]  mT;
  delete[]  mN;
  delete[]  mX;

//   delete    mDeadStripP;
//   delete    mDeadStripN;

  delete    mStripP;
  delete    mStripN;
  delete    mClusterP;
  delete    mClusterN;
  delete    mPackage;
  delete    mPoint;
}

/*!
The wafer initialisation. The geom info is taken from the method arguments. The lists are built.
Only the WafersPosition are used at the moment. This part should be updated to take into account
the relative position of the ladders, sectors and barrel.
 */
void StSsdWafer::init(int rId, double *rD, double *rT, double *rN, double *rX)
{
  if (rId != mId)
    cout<<" Can not initialize wafer number : "<<mId<<" with "<<rId<<"\n";
  else
    {
//       cout<<" Okay to initialize wafer number : "<<mId<<" with "<<rId<<"\n";
      for (int i = 0; i < 3; i++)
	{
	  mD[i] = (float)rD[i];
	  mT[i] = (float)rT[i];
	  mN[i] = (float)rN[i];
	  mX[i] = (float)rX[i];
	}
    }
}

StSsdStripList* StSsdWafer::getStripP()
{ return mStripP; }   

StSsdStripList* StSsdWafer::getStripN()
{ return mStripN; }   

StSsdClusterList* StSsdWafer::getClusterP()
{ return mClusterP; }   

StSsdClusterList* StSsdWafer::getClusterN()
{ return mClusterN; }   

StSsdPackageList* StSsdWafer::getPackage()
{  return mPackage; }   

StSsdPointList*   StSsdWafer::getPoint()
{  return mPoint; }  

void StSsdWafer::debugStrips()
{
  StSsdStrip *currentStripP;
  cout<<"List of "<<mStripP->getSize()<<" strips on the P side "<<endl;
  if (mStripP->getSize()>0) currentStripP = mStripP->first();
  for (int i=0;i<mStripP->getSize();i++) {
    cout<<" id,sig,noise : "<<currentStripP->getNStrip()
	<<" "<<currentStripP->getDigitSig()
	<<" "<<currentStripP->getSigma()<<endl;
    if (currentStripP!=mStripP->last()) currentStripP = mStripP->next(currentStripP);
  }

  StSsdStrip *currentStripN;
  cout<<"List of "<<mStripN->getSize()<<" strips on the N side "<<endl;
  if (mStripN->getSize()>0) currentStripN = mStripN->first();
  for (int i=0;i<mStripN->getSize();i++) {
    cout<<" id,sig,noise : "<<currentStripN->getNStrip()
	<<" "<<currentStripN->getDigitSig()
	<<" "<<currentStripN->getSigma()<<endl;
    if (currentStripN!=mStripN->last()) currentStripN = mStripN->next(currentStripN);
  }
}
void StSsdWafer::debugClusters()
{
  StSsdCluster *currentClusterP;
  cout<<"List of "<<mClusterP->getSize()<<" clusters on the P side "<<endl;
  if (mClusterP->getSize()>0) currentClusterP = mClusterP->first();
  for (int i=0;i<mClusterP->getSize();i++) {
    cout<<"N,Size,FirstStrip,StripMean,TotAdc,FirstAdc,LastAdc,TotNoise : "<<currentClusterP->getNCluster()
	<<" "<<currentClusterP->getClusterSize()
	<<" "<<currentClusterP->getFirstStrip()
	<<" "<<currentClusterP->getStripMean()
	<<" "<<currentClusterP->getTotAdc()
	<<" "<<currentClusterP->getFirstAdc()
	<<" "<<currentClusterP->getLastAdc()
	<<" "<<currentClusterP->getTotNoise()<<endl;
    if (currentClusterP!=mClusterP->last()) currentClusterP = mClusterP->next(currentClusterP);
  }

  StSsdCluster *currentClusterN;
  cout<<"List of "<<mClusterN->getSize()<<" clusters on the P side "<<endl;
  if (mClusterN->getSize()>0) currentClusterN = mClusterN->first();
  for (int i=0;i<mClusterN->getSize();i++) {
    cout<<"N,Size,FirstStrip,StripMean,TotAdc,FirstAdc,LastAdc,TotNoise : "<<currentClusterN->getNCluster()
	<<" "<<currentClusterN->getClusterSize()
	<<" "<<currentClusterN->getFirstStrip()
	<<" "<<currentClusterN->getStripMean()
	<<" "<<currentClusterN->getTotAdc()
	<<" "<<currentClusterN->getFirstAdc()
	<<" "<<currentClusterN->getLastAdc()
	<<" "<<currentClusterN->getTotNoise()<<endl;
    if (currentClusterN!=mClusterN->last()) currentClusterN = mClusterN->next(currentClusterN);
  }

}
/*!
A new strip is added to the wafer by calling the StripList method.
 */
void StSsdWafer::addStrip(StSsdStrip *ptr, int iSide)
{
  if (iSide)
    { mStripN->addNewStrip(ptr); }
  else
    { mStripP->addNewStrip(ptr); }
}

/*!
A new cluster is added to the wafer by calling the ClusterList method.
 */
void StSsdWafer::addCluster(StSsdCluster *ptr, int iSide)
{
  if (iSide)
    { mClusterN->addNewCluster(ptr); }
  else
    { mClusterP->addNewCluster(ptr); }
}

/*!
A new package is added to the wafer by calling the PackageList method.
 */
void StSsdWafer::addPackage(StSsdPackage *ptr)
{  mPackage->addNewPackage(ptr); }

/*!
A new point is added to the wafer by calling the PointList method.
 */
void StSsdWafer::addPoint(StSsdPoint *ptr)
{  mPoint->addNewPoint(ptr); }

void StSsdWafer::setSigmaStrip(int iStrip, int iSide, int iSigma, StSsdDynamicControl *dynamicControl)
{
  if (iSide)
    { mStripN->setSigma(iStrip, iSigma, dynamicControl); }
  else
    { mStripP->setSigma(iStrip, iSigma, dynamicControl); }
}

/*!
The strips of both sides are sorted
 */
void StSsdWafer::sortStrip()
{
  mStripP->sortStrip();
  mStripN->sortStrip();
}

/*!
The clusters of both sides are sorted
 */
void StSsdWafer::sortCluster()
{
  mClusterP->sortCluster();
  mClusterN->sortCluster();
}

/*!
The points are sorted by what ??
 */
void StSsdWafer::sortPoint()
{  mPoint->sortPoint(); }


/*!
Does the cluster finding in two steps for both sides :

- a cluster finding
- a cluster splitting  
 */
void StSsdWafer::doClusterisation(int *NClusterPerSide, StSsdClusterControl *clusterControl)
{
  int iSide = 0;
  doFindCluster(clusterControl, iSide);
  NClusterPerSide[0] = doClusterSplitting(clusterControl, iSide); 
  iSide = 1;
  doFindCluster(clusterControl, iSide); 
  NClusterPerSide[1] = doClusterSplitting(clusterControl, iSide);
}

/*!
Does the cluster finding on the iSide. 
The strip list is scanned by increasing order (the list is assumed to be sorted). 
For a given strip (CurrentStrip) if its signal is above a given number of time its noise value a cluster list is formed. 
The strip list is scanned backward then forward to add the consecutive strips with a signal. The new cluster is updated 
each time a new strip is added. The scan starts with the first strip of the list and ends with the last strip. 
The threshold for the creation of a new cluster is given by SsdClusterControl.highCut.
The SsdClusterControl table seems to be useless in this method (cleaning needed ?) 
 */
int StSsdWafer::doFindCluster(StSsdClusterControl *clusterControl, int iSide)
{
  StSsdStripList   *CurrentStripList   =  0;
  StSsdClusterList *CurrentClusterList =  0;

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
  
  if(!CurrentStripList->getSize()) return 0;
  
  int nCluster = 0;
  int atTheEnd = 0;
  StSsdStrip *CurrentStrip = CurrentStripList->first();
  StSsdStrip *ScanStrip  = 0;
  StSsdStrip *LastScanStrip = 0;
  StSsdStrip *tmpStrip = CurrentStripList->first();
  for(int i = 0; i<CurrentStripList->getSize(); i++)
    { tmpStrip = CurrentStripList->next(tmpStrip); }
  
  while(CurrentStrip) 
    {
      if(CurrentStrip->getDigitSig()>(clusterControl->getHighCut()*CurrentStrip->getSigma()))
	{
	  LastScanStrip = 0;
	  StSsdCluster *newCluster = new StSsdCluster(CurrentClusterList->getSize());
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

/*!
Does the cluster splitting on the side iSide. For each cluster in the cluster list, the array of signal values is built 
and transmitted to the splitCluster method. This method seems to remove clusters and not to add new clusters resulting 
from a cluster splitting into several.
 */
int StSsdWafer::doClusterSplitting(StSsdClusterControl *clusterControl, int iSide)
{
  StSsdStripList   *CurrentStripList   =  0;
  StSsdClusterList *CurrentClusterList =  0;

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

  int ClusterListSize = CurrentClusterList->getSize();
  if(!ClusterListSize) return 0;
  
  int iCluster = 0;
  StSsdCluster *CurrentCluster = CurrentClusterList->first();
  
  for(iCluster = 0 ; iCluster < ClusterListSize ; iCluster++)
    {   
      
      int *ListAdc = CurrentStripList->getListAdc(CurrentCluster->getFirstStrip(),CurrentCluster->getClusterSize());
      int toBeDeleted = CurrentClusterList->splitCluster(clusterControl,CurrentCluster,ListAdc,CurrentStripList);
      if(toBeDeleted)
	{
	  StSsdCluster *TempCluster = CurrentCluster;
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

/*!
Determines the packages by comparing the cluster lists built for both sides. 
Still a mystery for me...
 */
int StSsdWafer::doFindPackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl)
{
  StSsdPackageList *currentPackageList = 0;
  StSsdCluster     *currentClusterP    = 0;
  StSsdCluster     *currentClusterN    = 0;
  StSsdCluster     *scanClusterP       = 0;
  StSsdCluster     *scanClusterN       = 0;
  StSsdCluster     *lastMatchedN       = 0;
  StSsdCluster     *nextMatchedN       = 0;

  int maxMatchedInPackage = clusterControl->getClusterTreat();
  int numPackage         = 0;
  int numUnMatched       = 0;
  int numCurrentClusterP = 0;
  int numCurrentClusterN = 0;
  int numScanClusterP    = 0;
  int numScanClusterN    = 0;
  int numLastMatchedN    = 0;
  int numNextMatchedN    = 0;
  int matchedOk          = 0;
  int keepPackage        = 0;

  currentPackageList = mPackage;
  currentClusterP    = mClusterP->first();
  currentClusterN    = mClusterN->first();

  if (!mClusterP->getSize() || !mClusterN->getSize()) return 0;
  StSsdPackage *currentPackage = new StSsdPackage(0, clusterControl);

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
		  if (currentPackage)
		    {
		      StSsdPackage *newPackage = new StSsdPackage(currentPackageList->getSize(), currentPackage->getSize());
		      newPackage->takeMatcheds(currentPackage);
		      currentPackageList->addNewPackage(newPackage);
		      currentPackage->purgePackage();
		      numPackage++;
		    }
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
	  if (currentPackage)
	    {
	      StSsdPackage *newPackage = new StSsdPackage(currentPackageList->getSize(), currentPackage->getSize());
	      newPackage->takeMatcheds(currentPackage);
	      currentPackageList->addNewPackage(newPackage);
	      currentPackage->purgePackage();
	      numPackage++;
	    }
	}
      currentClusterP = mClusterP->next(currentClusterP);
    }
  delete currentPackage;
  return numPackage;
}

int StSsdWafer::doSolvePerfect(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl)
{
  int nPerfect = 0;
  StSsdPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      int numMatched = strlen(currentKind)/2;
      float *Adc     = new float[numMatched];
      for(int i=0;i<numMatched;i++) 
	Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 *********************************************************************
      if(!strcmp(currentKind,"1p1n"))//                   case (1-1) checked
	{
          StSsdPoint *newPoint = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 11);
          newPoint->setFlag(100);
          setMatcheds(dimensions, newPoint, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPoint->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPoint);
          nPerfect++;
	}
//  *********************************************************************
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nPerfect;
}

void StSsdWafer::doStatPerfect(int nPerfectPoint, StSsdClusterControl *clusterControl)
{
  float store = 0;
  StSsdPoint *currentPerfect = 0;
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

int StSsdWafer::doSolvePackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl)
{
  int nSolved = 0;
  StSsdPackage *currentPackage = 0;
  char         *currentKind    = 0;
  currentPackage = mPackage->first();
  while(currentPackage)
    {
      currentKind    = currentPackage->getKind();
      int numMatched = strlen(currentKind)/2;
      float *Adc     = new float[numMatched];
      for(int i=0;i<numMatched;i++) Adc[i]=(currentPackage->getMatched(i))->getTotAdc();
// 1 ********************************************************************
      if(!strcmp(currentKind,"1p1n"))//  case (1-1) done in doSolvePerfect 
	{
	}
// 2 ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n"))// case (1-2)A final check Ok
 	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 12);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  12);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
 	}
// 3 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n"))// case (1-2)AS final check Ok
  	{
 	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  21);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
//  ********************************************************************
      else if(!strcmp(currentKind,"1p1n2n3n"))// case (1-3)A
 	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 13);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2]-Adc[3], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1]-Adc[3], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  13);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0]-Adc[1]-Adc[2], Adc[3]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);
	  nSolved++;
 	}
//  *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n"))// case (1-3)AS
  	{
 	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]-Adc[3]);
          newPointA->setFlag(100);
 	  mPoint->addNewPoint(newPointA);

 	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]-Adc[3]);
          newPointB->setFlag(100);
 	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  31);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]-Adc[0]-Adc[2]);
          newPointC->setFlag(100);
 	  mPoint->addNewPoint(newPointC);
          nSolved++;
  	}
// 4 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n"))//        case (2-2)A checked
  	{
 	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  221);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[3], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
	  nSolved++;
  	}
// 5 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n"))//        case (2-2)AP checked
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  222);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointB->setEnergyLoss(Adc[2], Adc[4]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);
          nSolved++;
  	}
// 6 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n"))//        case (2-2)B checked
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  223);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
 	  double setA[2], setB[2], setC[2], setD[2];
          double probAD, probBC;

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
	      probAD = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	      probBC = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	    }
	  else
	    {
	      probAD = 0.5;
	      probBC = 0.5;
	    }
	  newPointA->setFlag(int(100*probAD));
	  newPointB->setFlag(int(100*probBC));
	  newPointC->setFlag(int(100*probBC));
	  newPointD->setFlag(int(100*probAD));
          nSolved++;

  	}
// 7 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n"))//        case (2-3)A checked
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointC->setEnergyLoss(Adc[3]-Adc[5], Adc[2]);
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2];
	  double setAB[2], setCD[2];
          double probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] - (Adc[1]+Adc[2]))/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setCD[0] = (Adc[3] - (Adc[2]+Adc[5]))/sqrt(2.0);
          setCD[1] = matchDistr(clusterControl, setCD[0]);
          setD[0]  = (Adc[3] - Adc[5])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
	  probABD  = (setAB[1]*setD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  probACD  = (setA[1]*setCD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointD->setEnergyLoss(Adc[3], Adc[5]);
	    }
	  else
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]);
	      newPointD->setEnergyLoss(Adc[3]-Adc[2], Adc[5]);
	    }
          nSolved++;
  	}
// 8 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n"))//        case (2-3)AP checked
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
   	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
   	  newPointC->setEnergyLoss(Adc[2], Adc[4]-Adc[5]);
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(4));
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2];
	  double setAB[2], setCD[2];
          double probABD, probACD;

          setA[0]  = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1]  = matchDistr(clusterControl, setA[0]);
          setAB[0] = (Adc[0] + Adc[2] - Adc[1])/sqrt(2.0);
          setAB[1] = matchDistr(clusterControl, setAB[0]);
          setCD[0] = (Adc[2] + Adc[5] - Adc[4])/sqrt(2.0);
          setCD[1] = matchDistr(clusterControl, setCD[0]);
          setD[0]  = (Adc[5] - Adc[4])/sqrt(2.0);
          setD[1]  = matchDistr(clusterControl, setD[0]);
	  probABD  = (setAB[1]*setD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  probACD  = (setA[1]*setCD[1])/(setAB[1]*setD[1]+setA[1]*setCD[1]);
	  newPointB->setFlag(int(100*probABD));
	  newPointC->setFlag(int(100*probACD));
	  if (probABD > probACD)
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
	      newPointD->setEnergyLoss(Adc[5], Adc[4]);
	    }
	  else
	    {
	      newPointA->setEnergyLoss(Adc[0], Adc[1]);
	      newPointD->setEnergyLoss(Adc[5], Adc[4]-Adc[2]);
	    }
          nSolved++;
  	}
// 9 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n"))//        case (2-3)B
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
  	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
  	  newPointC->setEnergyLoss(Adc[0]-Adc[1], Adc[3]);
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setD[2], setE[2];
	  double setAB[2], setAC[2], setDE[2];
          double probABE, probACD, probADE;

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
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  probACD  = (setAC[1]*setD[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  probADE  = (setA[1]*setDE[1])/(setAB[1]*setE[1]+setAC[1]*setD[1]+setA[1]*setDE[1]);
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*probACD));
	  newPointD->setFlag(int(100*(probACD+probADE)));
	  newPointE->setFlag(int(100*(probABE+probADE)));
	  if ((probABE > probACD)&&(probABE > probADE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointE->setEnergyLoss(Adc[4],Adc[3]);
	    }
	  else if ((probACD > probABE)&&(probACD > probADE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[3],Adc[1]);
	      newPointD->setEnergyLoss(Adc[4],Adc[2]);
	    }
	  else if ((probADE > probABE)&&(probADE > probACD))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointD->setEnergyLoss(Adc[4]-Adc[3],Adc[2]);
	      newPointE->setEnergyLoss(Adc[4]-Adc[2],Adc[3]);
	    }
          nSolved++;
  	}
// 10 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n"))//        case (3-2)BP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
  	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(5), currentPackage->getMatched(1));
  	  newPointD->setEnergyLoss(Adc[5], Adc[1]-Adc[0]);
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(4));
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setC[2], setE[2];
          double setAB[2], setAD[2], setCE[2];
          double probABE, probACD, probACE;

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
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  probACD  = (setAD[1]*setC[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  probACE  = (setA[1]*setCE[1])/(setAB[1]*setE[1]+setAD[1]*setC[1]+setA[1]*setCE[1]);
	  newPointB->setFlag(int(100*probABE));
	  newPointC->setFlag(int(100*(probACD+probACE)));
	  newPointD->setFlag(int(100*probACD));
	  newPointE->setFlag(int(100*(probABE+probACE)));
	  if ((probABE > probACD)&&(probABE > probACE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[2]);
	      newPointE->setEnergyLoss(Adc[5],Adc[4]);
	    }
	  else if ((probACD > probABE)&&(probACD > probACE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[5]);
	      newPointC->setEnergyLoss(Adc[2],Adc[4]);
	    }
	  else if ((probACE > probABE)&&(probACE > probACD))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointC->setEnergyLoss(Adc[2],Adc[4]-Adc[5]);
	      newPointE->setEnergyLoss(Adc[5],Adc[4]-Adc[2]);
	    }
          nSolved++;
  	}
// 11 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n"))//        case (2-3)BS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
  	  newPointC->setEnergyLoss(Adc[3]-Adc[6],Adc[1]);
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
  	  newPointD->setEnergyLoss(Adc[3]-Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
          newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setE[2];
          double setAB[2], setCE[2], setDE[2];
          double probABE, probADE, probBCE;

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
	  probABE  = (setAB[1]*setE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  probADE  = (setA[1]*setDE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  probBCE  = (setB[1]*setCE[1])/(setAB[1]*setE[1]+setA[1]*setDE[1]+setB[1]*setCE[1]);
	  newPointA->setFlag(int(100*(probABE+probADE)));
	  newPointB->setFlag(int(100*(probABE+probBCE)));
	  newPointC->setFlag(int(100*probBCE));
	  newPointD->setFlag(int(100*probADE));
	  if ((probABE > probADE)&&(probABE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0]-Adc[2],Adc[1]);
	      newPointB->setEnergyLoss(Adc[0]-Adc[1],Adc[2]);
	      newPointE->setEnergyLoss(Adc[3],Adc[6]);
	    }
	  else if ((probADE > probABE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointE->setEnergyLoss(Adc[3]-Adc[2],Adc[6]);
	    }
	  else if ((probBCE > probABE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLoss(Adc[0],Adc[2]);
	      newPointE->setEnergyLoss(Adc[3]-Adc[1],Adc[6]);
	    }
          nSolved++;
  	}
// 12 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n"))//        case (3-2)BSP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[6], Adc[2]);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3]-Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
          newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

// traitement propre aux space points..(probabilite)
	  double setA[2], setC[2], setE[2];
          double setAC[2], setBE[2], setDE[2];
          double probACE, probADE, probBCE;

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
	  probACE  = (setAC[1]*setE[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  probADE  = (setA[1]*setDE[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  probBCE  = (setBE[1]*setC[1])/(setAC[1]*setE[1]+setA[1]*setDE[1]+setBE[1]*setC[1]);
	  newPointA->setFlag(int(100*(probACE+probADE)));
	  newPointB->setFlag(int(100*probBCE));
	  newPointC->setFlag(int(100*(probACE+probBCE)));
	  newPointD->setFlag(int(100*probADE));
	  if ((probACE > probADE)&&(probACE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]-Adc[3]);
	      newPointC->setEnergyLoss(Adc[3],Adc[1]-Adc[0]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]);
	    }
	  else if ((probADE > probACE)&&(probADE > probBCE))
	    {
	      newPointA->setEnergyLoss(Adc[0],Adc[1]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]-Adc[3]);
	    }
	  else if ((probBCE > probACE)&&(probBCE > probADE))
	    {
	      newPointB->setEnergyLoss(Adc[3],Adc[1]);
	      newPointE->setEnergyLoss(Adc[6],Adc[2]-Adc[0]);
	    }
          nSolved++;
  	}
// 13 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n"))//        case (2-3)C
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2]-Adc[5], Adc[4]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2]-Adc[4], Adc[5]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 14 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n"))//       case (3-2)CP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]-Adc[5]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[5], Adc[2]-Adc[3]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 15 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p3n"))//        case (2-3)CS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0]-Adc[2], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0]-Adc[1], Adc[2]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[4], Adc[3]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 16 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n3p1n2n"))//      case (3-2)CPS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]-Adc[2]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(2), currentPackage->getMatched(1));
 	  newPointB->setEnergyLoss(Adc[2], Adc[1]-Adc[0]);
          newPointB->setFlag(100);
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  32);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(6));
 	  newPointD->setEnergyLoss(Adc[4], Adc[6]);
          newPointD->setFlag(100);
 	  mPoint->addNewPoint(newPointD);

          nSolved++;
  	}
// 17 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n"))//        case (2-3)D
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointE);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          double setAB[2], setAC[2], setBC[2], setDE[2], setDF[2], setEF[2];
          double prob[6];

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
	  prob[0]  = (setAC[1]*setE[1]+setAB[1]*setF[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[1]  = (setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[2]  = (setAC[1]*setE[1]+setBC[1]*setD[1]+setDE[1]*setC[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[3]  = (setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[4]  = (setAC[1]*setE[1]+setDE[1]*setC[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  prob[5]  = (setAB[1]*setF[1]+setDF[1]*setB[1]+setEF[1]*setA[1])/(setAC[1]*setE[1]+setAB[1]*setF[1]+setBC[1]*setD[1]+setDF[1]*setB[1]+setDE[1]*setC[1]+setEF[1]*setA[1]);
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
          nSolved++;
  	}
// 18 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n"))//        case (3-2)DP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
	  mPoint->addNewPoint(newPointB);

 	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(1));
 	  mPoint->addNewPoint(newPointE);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  23);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(2));
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
	  double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2];
          double setAC[2], setBD[2], setAE[2], setBF[2], setCE[2], setDF[2];
          double prob[6];

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
	  prob[0]  = (setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[1]  = (setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[2]  = (setAC[1]*setF[1]+setCE[1]*setB[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[3]  = (setAE[1]*setD[1]+setCE[1]*setB[1]+setBD[1]*setE[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[4]  = (setAE[1]*setD[1]+setDF[1]*setA[1]+setBD[1]*setE[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  prob[5]  = (setAC[1]*setF[1]+setDF[1]*setA[1]+setBF[1]*setC[1])/(setAC[1]*setF[1]+setAE[1]*setD[1]+setDF[1]*setA[1]+setCE[1]*setB[1]+setBD[1]*setE[1]+setBF[1]*setC[1]);
	  newPointA->setFlag(int(100*prob[0]));
	  newPointB->setFlag(int(100*prob[1]));
	  newPointC->setFlag(int(100*prob[2]));
	  newPointD->setFlag(int(100*prob[3]));
	  newPointE->setFlag(int(100*prob[4]));
	  newPointF->setFlag(int(100*prob[5]));
          nSolved++;
  	}
// 19 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p3n"))//        case (3-3)A
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(3));
 	  newPointE->setEnergyLoss(Adc[6], Adc[3]);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 20 ********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p2n3n"))//        case (3-3)AP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLoss(Adc[5], Adc[7]);
	  newPointE->setFlag(100);
 	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 21 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3n3p2n3n"))//    case (3-3)B
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[3], Adc[5]);
	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[6], Adc[2]);
 	  mPoint->addNewPoint(newPointE);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLoss(Adc[6], Adc[5]);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setE[2], setF[2];
          double probACF, probADE;

          setC[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[6] - Adc[2])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
	  probACF = (setC[1]*setF[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  probADE = (setD[1]*setE[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));
          nSolved++;
  	}
// 22 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p2n3n"))//    case (3-3)BP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2], Adc[5]);
	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointE->setEnergyLoss(Adc[6], Adc[4]);
 	  mPoint->addNewPoint(newPointE);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointF->setEnergyLoss(Adc[6], Adc[5]);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setE[2], setF[2];
          double probACF, probADE;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[6] - Adc[4])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
	  probACF = (setC[1]*setF[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  probADE = (setD[1]*setE[1])/(setC[1]*setF[1]+setD[1]*setE[1]);
	  newPointC->setFlag(int(100*probACF));
	  newPointD->setFlag(int(100*probADE));
	  newPointE->setFlag(int(100*probADE));
	  newPointF->setFlag(int(100*probACF));
          nSolved++;
  	}
// 23 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p2n3n"))//    case (3-3)BS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(8));
 	  newPointF->setEnergyLoss(Adc[6], Adc[8]);
 	  newPointF->setFlag(100);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  probADF = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCF = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
          nSolved++;
  	}
// 24 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p3n"))//    case (3-3)BSP
 	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

 	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
 	  mPoint->addNewPoint(newPointD);

 	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointF->setEnergyLoss(Adc[7], Adc[6]);
          newPointF->setNMatched(33);
 	  newPointF->setFlag(100);
 	  mPoint->addNewPoint(newPointF);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADF, probBCF;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  probADF = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCF = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADF));
	  newPointB->setFlag(int(100*probBCF));
	  newPointC->setFlag(int(100*probBCF));
	  newPointD->setFlag(int(100*probADF));
          nSolved++;
  	}
// 25 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p3n"))//    case (3-3)C
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[4], Adc[2]);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[7], Adc[3]);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 26 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3p1n2n3n"))//    case (3-3)CS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointD->setEnergyLoss(Adc[2], Adc[4]);
          newPointD->setFlag(100);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(5), currentPackage->getMatched(8));
 	  newPointF->setEnergyLoss(Adc[5], Adc[8]);
          newPointF->setFlag(100);
	  mPoint->addNewPoint(newPointF);

          nSolved++;
  	}

// 27 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p2n3n3p2n3n"))//   case (3-3)D
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointE->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(3));
 	  newPointG->setEnergyLoss(Adc[7], Adc[3]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setD[2], setE[2], setF[2], setG[2];
          double probADG, probAEF;

          setD[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[3])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
          setF[0] = (Adc[7] - Adc[2])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[7] - Adc[3])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
	  probADG = (setD[1]*setG[1])/(setD[1]*setG[1]+setE[1]*setF[1]);
	  probAEF = (setE[1]*setF[1])/(setD[1]*setG[1]+setE[1]*setF[1]);
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*probADG));

          nSolved++;
  	}
// 28 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p1n2n3n"))// case (3-3)DP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(2), currentPackage->getMatched(5));
 	  newPointD->setEnergyLoss(Adc[2], Adc[5]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(6), currentPackage->getMatched(4));
 	  newPointF->setEnergyLoss(Adc[6], Adc[4]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointG->setEnergyLoss(Adc[6], Adc[5]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setC[2], setD[2], setF[2], setG[2];
          double probACG, probADF;

          setC[0] = (Adc[2] - Adc[4])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[2] - Adc[5])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setF[0] = (Adc[6] - Adc[4])/sqrt(2.0);
          setF[1] = matchDistr(clusterControl, setF[0]);
          setG[0] = (Adc[6] - Adc[5])/sqrt(2.0);
          setG[1] = matchDistr(clusterControl, setG[0]);
	  probACG = (setC[1]*setG[1])/(setC[1]*setG[1]+setD[1]*setF[1]);
	  probADF = (setD[1]*setF[1])/(setC[1]*setG[1]+setD[1]*setF[1]);
	  newPointC->setFlag(int(100*probACG));
	  newPointD->setFlag(int(100*probADF));
	  newPointF->setFlag(int(100*probADF));
	  newPointG->setFlag(int(100*probACG));

          nSolved++;
  	}
// 29 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3p1n2n3n"))// case (3-3)DS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(6), currentPackage->getMatched(9));
 	  newPointG->setEnergyLoss(Adc[6], Adc[9]);
          newPointG->setFlag(100);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2];
          double probADG, probBCG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setC[0] = (Adc[3] - Adc[1])/sqrt(2.0);
          setC[1] = matchDistr(clusterControl, setC[0]);
          setD[0] = (Adc[3] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
	  probADG = (setA[1]*setD[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  probBCG = (setB[1]*setC[1])/(setA[1]*setD[1]+setB[1]*setC[1]);
	  newPointA->setFlag(int(100*probADG));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));

          nSolved++;
  	}
// 30 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p3n"))//case (3-3)DSP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointG->setEnergyLoss(Adc[8], Adc[3]);
          newPointG->setFlag(100);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setD[2], setE[2];
          double probAEG, probBDG;

          setA[0] = (Adc[0] - Adc[1])/sqrt(2.0);
          setA[1] = matchDistr(clusterControl, setA[0]);
          setB[0] = (Adc[0] - Adc[2])/sqrt(2.0);
          setB[1] = matchDistr(clusterControl, setB[0]);
          setD[0] = (Adc[4] - Adc[2])/sqrt(2.0);
          setD[1] = matchDistr(clusterControl, setD[0]);
          setE[0] = (Adc[4] - Adc[1])/sqrt(2.0);
          setE[1] = matchDistr(clusterControl, setE[0]);
	  probAEG = (setA[1]*setE[1])/(setA[1]*setE[1]+setB[1]*setD[1]);
	  probBDG = (setB[1]*setD[1])/(setA[1]*setE[1]+setB[1]*setD[1]);
	  newPointA->setFlag(int(100*probAEG));
	  newPointB->setFlag(int(100*probBDG));
	  newPointD->setFlag(int(100*probBDG));
	  newPointE->setFlag(int(100*probAEG));

          nSolved++;
  	}
// 31 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2p1n2n3n3p3n"))//   case (3-3)E
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(2), currentPackage->getMatched(4));
 	  newPointC->setEnergyLoss(Adc[2], Adc[4]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(6), currentPackage->getMatched(5));
 	  newPointE->setEnergyLoss(Adc[6], Adc[5]);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 32 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p2n3p2n3n"))//   case (3-3)EP
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
          newPointA->setFlag(100);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointC->setEnergyLoss(Adc[3], Adc[2]);
          newPointC->setFlag(100);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(5), currentPackage->getMatched(7));
 	  newPointE->setEnergyLoss(Adc[5], Adc[7]);
          newPointE->setFlag(100);
	  mPoint->addNewPoint(newPointE);

          nSolved++;
  	}
// 33 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p2n3n"))//  case (3-3)F
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLoss(Adc[3], Adc[6]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointF->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointG->setEnergyLoss(Adc[7], Adc[6]);
	  mPoint->addNewPoint(newPointG);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2];
          double probADG, probAEF, probBCG;

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
	  probADG = (setA[1]*setD[1]*setG[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  probAEF = (setA[1]*setE[1]*setF[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  probBCG = (setB[1]*setC[1]*setG[1])/(setA[1]*setD[1]*setG[1]+setA[1]*setE[1]*setF[1]+setB[1]*setC[1]*setG[1]);
	  newPointA->setFlag(int(100*(probADG+probAEF)));
	  newPointB->setFlag(int(100*probBCG));
	  newPointC->setFlag(int(100*probBCG));
	  newPointD->setFlag(int(100*probADG));
	  newPointE->setFlag(int(100*probAEF));
	  newPointF->setFlag(int(100*probAEF));
	  newPointG->setFlag(int(100*(probADG+probBCG)));

          nSolved++;
  	}
// 34 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p2n3n"))//  case (3-3)G
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0], Adc[3]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointG->setEnergyLoss(Adc[8], Adc[2]);
	  mPoint->addNewPoint(newPointG);

	  StSsdPoint *newPointH = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointH->setEnergyLoss(Adc[8], Adc[3]);
	  mPoint->addNewPoint(newPointH);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          double probAEH, probAFG, probBDH, probCDG;

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
	  probAEH = (setA[1]*setE[1]*setH[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probAFG = (setA[1]*setF[1]*setG[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probBDH = (setB[1]*setD[1]*setH[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  probCDG = (setC[1]*setD[1]*setG[1])/(setA[1]*setE[1]*setH[1]+setA[1]*setF[1]*setG[1]+setB[1]*setD[1]*setH[1]+setC[1]*setD[1]*setG[1]);
	  newPointA->setFlag(int(100*(probAEH+probAFG)));
	  newPointB->setFlag(int(100*probBDH));
	  newPointC->setFlag(int(100*probCDG));
	  newPointD->setFlag(int(100*(probBDH+probCDG)));
	  newPointE->setFlag(int(100*probAEH));
	  newPointF->setFlag(int(100*probAFG));
	  newPointG->setFlag(int(100*(probAFG+probCDG)));
	  newPointH->setFlag(int(100*(probAEH+probBDH)));

          nSolved++;
  	}
// 35 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n2p1n2n3n3p1n2n3n"))//  case (3-3)GS
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0],  Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(3), currentPackage->getMatched(1));
 	  newPointC->setEnergyLoss(Adc[3], Adc[1]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(3), currentPackage->getMatched(2));
 	  newPointD->setEnergyLoss(Adc[3], Adc[2]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(3), currentPackage->getMatched(6));
 	  newPointE->setEnergyLoss(Adc[3], Adc[6]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(7), currentPackage->getMatched(1));
 	  newPointF->setEnergyLoss(Adc[7], Adc[1]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(7), currentPackage->getMatched(2));
 	  newPointG->setEnergyLoss(Adc[7], Adc[2]);
	  mPoint->addNewPoint(newPointG);

	  StSsdPoint *newPointH = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(),  33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(7), currentPackage->getMatched(6));
 	  newPointH->setEnergyLoss(Adc[7], Adc[6]);
	  mPoint->addNewPoint(newPointH);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2];
          double probADH, probAEG, probBCH, probBEF;

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
	  probADH = (setA[1]*setD[1]*setH[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probAEG = (setA[1]*setE[1]*setG[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probBCH = (setB[1]*setC[1]*setH[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  probBEF = (setB[1]*setE[1]*setF[1])/(setA[1]*setD[1]*setH[1]+setA[1]*setE[1]*setG[1]+setB[1]*setC[1]*setH[1]+setB[1]*setE[1]*setF[1]);
	  newPointA->setFlag(int(100*(probADH+probAEG)));
	  newPointB->setFlag(int(100*(probBCH+probBEF)));
	  newPointC->setFlag(int(100*probBCH));
	  newPointD->setFlag(int(100*probADH));
	  newPointE->setFlag(int(100*(probAEG+probBEF)));
	  newPointF->setFlag(int(100*probBEF));
	  newPointG->setFlag(int(100*probAEG));
	  newPointH->setFlag(int(100*(probADH+probBCH)));

          nSolved++;
  	}
// 36 *********************************************************************
        else if(!strcmp(currentKind,"1p1n2n3n2p1n2n3n3p1n2n3n"))// case (3-3)H
  	{
	  StSsdPoint *newPointA = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointA, currentPackage->getMatched(0), currentPackage->getMatched(1));
 	  newPointA->setEnergyLoss(Adc[0], Adc[1]);
	  mPoint->addNewPoint(newPointA);

	  StSsdPoint *newPointB = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointB, currentPackage->getMatched(0), currentPackage->getMatched(2));
 	  newPointB->setEnergyLoss(Adc[0], Adc[2]);
	  mPoint->addNewPoint(newPointB);

	  StSsdPoint *newPointC = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointC, currentPackage->getMatched(0), currentPackage->getMatched(3));
 	  newPointC->setEnergyLoss(Adc[0], Adc[3]);
	  mPoint->addNewPoint(newPointC);

	  StSsdPoint *newPointD = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointD, currentPackage->getMatched(4), currentPackage->getMatched(1));
 	  newPointD->setEnergyLoss(Adc[4], Adc[1]);
	  mPoint->addNewPoint(newPointD);

	  StSsdPoint *newPointE = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointE, currentPackage->getMatched(4), currentPackage->getMatched(2));
 	  newPointE->setEnergyLoss(Adc[4], Adc[2]);
	  mPoint->addNewPoint(newPointE);

	  StSsdPoint *newPointF = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointF, currentPackage->getMatched(4), currentPackage->getMatched(3));
 	  newPointF->setEnergyLoss(Adc[4], Adc[3]);
	  mPoint->addNewPoint(newPointF);

	  StSsdPoint *newPointG = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointG, currentPackage->getMatched(8), currentPackage->getMatched(1));
 	  newPointG->setEnergyLoss(Adc[8], Adc[1]);
	  mPoint->addNewPoint(newPointG);

	  StSsdPoint *newPointH = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointH, currentPackage->getMatched(8), currentPackage->getMatched(2));
 	  newPointH->setEnergyLoss(Adc[8], Adc[2]);
	  mPoint->addNewPoint(newPointH);

	  StSsdPoint *newPointI = new StSsdPoint(mPoint->getSize(), mId, currentPackage->getNPackage(), 33);
          setMatcheds(dimensions, newPointI, currentPackage->getMatched(8), currentPackage->getMatched(3));
 	  newPointI->setEnergyLoss(Adc[8], Adc[3]);
	  mPoint->addNewPoint(newPointI);

// traitement propre aux space points..(probabilite)
          double setA[2], setB[2], setC[2], setD[2], setE[2], setF[2], setG[2], setH[2], setI[2];
          double probAEI, probCEG, probAFH, probBDI, probCDH, probBFG;

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
	  probAEI = (setA[1]*setE[1]*setI[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probCEG = (setC[1]*setE[1]*setG[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probAFH = (setA[1]*setF[1]*setH[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probBDI = (setB[1]*setD[1]*setI[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probCDH = (setC[1]*setD[1]*setH[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  probBFG = (setB[1]*setF[1]*setG[1])/(setA[1]*setE[1]*setI[1]+setC[1]*setE[1]*setG[1]+setA[1]*setF[1]*setH[1]+setB[1]*setD[1]*setI[1]+setC[1]*setD[1]*setH[1]+setB[1]*setF[1]*setG[1]);
	  newPointA->setFlag(int(100*(probAEI+probAFH)));
	  newPointB->setFlag(int(100*(probBDI+probBFG)));
	  newPointC->setFlag(int(100*(probCEG+probCDH)));
	  newPointD->setFlag(int(100*(probBDI+probCDH)));
	  newPointE->setFlag(int(100*(probAEI+probCEG)));
	  newPointF->setFlag(int(100*(probAFH+probBFG)));
	  newPointG->setFlag(int(100*(probCEG+probBFG)));
	  newPointH->setFlag(int(100*(probAFH+probCDH)));
	  newPointI->setFlag(int(100*(probAEI+probBDI)));

          nSolved++;
  	}
      else cout<<" Warning unsolved case ("<<currentKind<<")\n";// other cases
      delete [] Adc;
      currentPackage=mPackage->next(currentPackage);
    }
  return nSolved;
}

int StSsdWafer::convertDigitToAnalog(double PairCreationEnergy)
{
  StSsdPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      currentPoint->setDe(currentPoint->getDe(0)*PairCreationEnergy,0);
      currentPoint->setDe(currentPoint->getDe(1)*PairCreationEnergy,1);
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}


int StSsdWafer::convertUFrameToLocal(ssdDimensions_st *dimensions)
{
  StSsdPoint *currentPoint = mPoint->first();
  while(currentPoint)
    {
      currentPoint->setXl(currentPoint->getPositionU(0)/2.+currentPoint->getPositionU(1)/2.-dimensions[0].waferHalfActLength+dimensions[0].waferHalfActWidth*tan(dimensions[0].stereoAngle),0);
      currentPoint->setXl((currentPoint->getPositionU(1)-currentPoint->getPositionU(0))/(2*tan(dimensions[0].stereoAngle)),1);
      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}

int StSsdWafer::convertLocalToGlobal()
{
  StSsdPoint *currentPoint = mPoint->first();
  float B[3],wD[3],wT[3],wN[3],tempXl[3];
  float delta = 0;
  while(currentPoint)
    {
      // sign (-) of B[0] : temporarily fixed - order of strip readout has to be reversed 
      B[0]=-currentPoint->getXl(0)+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
      B[1]=currentPoint->getXl(1)+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
      B[2]=currentPoint->getXl(2)+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

//   Xl[0]=mDv.Xgv-mDv.Xv        ( mD[0] mD[1] mD[2] ) (Xg[0]) = (B[0])
//   Xl[1]=mTv.Xgv-mTv.Xv  i.e.  ( mT[0] mT[1] mT[2] ) (Xg[1])   (B[1])
//   Xl[2]=mNv.Xgv-mNv.Xv        ( mN[0] mN[1] mN[2] ) (Xg[2])   (B[2])

//   ( wD[0] wD[1] wD[2] ) (B[0]) = (tempXl[0])
//   ( wT[0] wT[1] wT[2] ) (B[1]) = (tempXl[1])
//   ( wN[0] wN[1] wN[2] ) (B[2]) = (tempXl[2])

      delta = mD[0]*mT[1]*mN[2]-mD[0]*mT[2]*mN[1]-mD[1]*mT[0]*mN[2]+mD[2]*mT[0]*mN[1]+mD[1]*mT[2]*mN[0]-mD[2]*mT[1]*mN[0]; // change frame =>delta = -1 !..

      wD[0] = mT[1]*mN[2]-mT[2]*mN[1];
      wD[1] = mD[2]*mN[1]-mD[1]*mN[2];
      wD[2] = mD[1]*mT[2]-mD[2]*mT[1];
      wT[0] = mT[2]*mN[0]-mT[0]*mN[2];
      wT[1] = mD[0]*mN[2]-mD[2]*mN[0];
      wT[2] = mD[2]*mT[0]-mD[0]*mT[2];
      wN[0] = mT[0]*mN[1]-mT[1]*mN[0];
      wN[1] = mD[1]*mN[0]-mD[0]*mN[1];
      wN[2] = mD[0]*mT[1]-mD[1]*mT[0];

      tempXl[0] = wD[0]*B[0]+wD[1]*B[1]+wD[2]*B[2];
      tempXl[1] = wT[0]*B[0]+wT[1]*B[1]+wT[2]*B[2];
      tempXl[2] = wN[0]*B[0]+wN[1]*B[1]+wN[2]*B[2];

      currentPoint->setXg(tempXl[0]/delta,0);
      currentPoint->setXg(tempXl[1]/delta,1);
      currentPoint->setXg(tempXl[2]/delta,2);

      currentPoint = mPoint->next(currentPoint);
    }
  return 1;
}


int StSsdWafer::printborder()
{
  if (mId==7101){
    printf("Wafer = %d \n",mId);
    float activeXs[4],BXs[4],tempXls[4];
    float activeYs[4],BYs[4],tempYls[4];
    float activeZs[4],BZs[4],tempZls[4];
    float activeXe[4],BXe[4],tempXle[4];
    float activeYe[4],BYe[4],tempYle[4];
    float activeZe[4],BZe[4],tempZle[4];
    float wD[3],wT[3],wN[3];
    float delta = 0;

    activeXs[0]=-3.65,activeXs[1]= 3.65,activeXs[2]= 3.65,activeXs[3]=-3.65;
    activeYs[0]= 2.00,activeYs[1]= 2.00,activeYs[2]=-2.00,activeYs[3]=-2.00;
    activeZs[0]= 0.00,activeZs[1]= 0.00,activeZs[2]= 0.00,activeZs[3]= 0.00;

    activeXe[0]= 3.65,activeXe[1]= 3.65,activeXe[2]=-3.65,activeXe[3]=-3.65;
    activeYe[0]= 2.00,activeYe[1]=-2.00,activeYe[2]=-2.00,activeYe[3]= 2.00;
    activeZe[0]= 0.00,activeZe[1]= 0.00,activeZe[2]= 0.00,activeZe[3]= 0.00;

    wD[0] = mT[1]*mN[2]-mT[2]*mN[1];
    wD[1] = mD[2]*mN[1]-mD[1]*mN[2];
    wD[2] = mD[1]*mT[2]-mD[2]*mT[1];
    wT[0] = mT[2]*mN[0]-mT[0]*mN[2];
    wT[1] = mD[0]*mN[2]-mD[2]*mN[0];
    wT[2] = mD[2]*mT[0]-mD[0]*mT[2];
    wN[0] = mT[0]*mN[1]-mT[1]*mN[0];
    wN[1] = mD[1]*mN[0]-mD[0]*mN[1];
    wN[2] = mD[0]*mT[1]-mD[1]*mT[0];
    for(int i=0;i<5;i++)
      {
	BXs[i]=activeXs[i]+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
	BYs[i]=activeYs[i]+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
	BZs[i]=activeZs[i]+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

	BXe[i]=activeXe[i]+mD[0]*mX[0]+mD[1]*mX[1]+mD[2]*mX[2];
	BYe[i]=activeYe[i]+mT[0]*mX[0]+mT[1]*mX[1]+mT[2]*mX[2]; 
	BZe[i]=activeZe[i]+mN[0]*mX[0]+mN[1]*mX[1]+mN[2]*mX[2];

	tempXls[i] = wD[0]*BXs[i]+wD[1]*BYs[i]+wD[2]*BZs[i];
	tempYls[i] = wT[0]*BXs[i]+wT[1]*BYs[i]+wT[2]*BZs[i];
	tempZls[i] = wN[0]*BXs[i]+wN[1]*BYs[i]+wN[2]*BZs[i];

	tempXle[i] = wD[0]*BXe[i]+wD[1]*BYe[i]+wD[2]*BZe[i];
	tempYle[i] = wT[0]*BXe[i]+wT[1]*BYe[i]+wT[2]*BZe[i];
	tempZle[i] = wN[0]*BXe[i]+wN[1]*BYe[i]+wN[2]*BZe[i];

      }
    delta = mD[0]*mT[1]*mN[2]-mD[0]*mT[2]*mN[1]-mD[1]*mT[0]*mN[2]+mD[2]*mT[0]*mN[1]+mD[1]*mT[2]*mN[0]-mD[2]*mT[1]*mN[0]; // change frame =>delta = -1 !..



    printf("xsSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempXls[0]/delta,tempXls[1]/delta,tempXls[2]/delta,tempXls[3]/delta);
    printf("xeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempXle[0]/delta,tempXle[1]/delta,tempXle[2]/delta,tempXle[3]/delta);
    printf("ysSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempYls[0]/delta,tempYls[1]/delta,tempYls[2]/delta,tempYls[3]/delta);
    printf("yeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempYle[0]/delta,tempYle[1]/delta,tempYle[2]/delta,tempYle[3]/delta);
    printf("zsSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempZls[0]/delta,tempZls[1]/delta,tempZls[2]/delta,tempZls[3]/delta);
    printf("zeSsdLadder%d set {%.2f %.2f %.2f %.2f}\n",mId-7100,tempZle[0]/delta,tempZle[1]/delta,tempZle[2]/delta,tempZle[3]/delta);

  }
  return 1;
}

StSsdWafer::StSsdWafer(const StSsdWafer & originalWafer)
{
  mId         = originalWafer.mId;
  mD          = new float[3];
  mT          = new float[3];
  mN          = new float[3];
  mX          = new float[3];
//   mDeadStripP = originalWafer.mDeadStripP;
//   mDeadStripN = originalWafer.mDeadStripN;

  mClusterP   = new StSsdClusterList();
  mClusterN   = new StSsdClusterList();
  mPackage    = new StSsdPackageList();
  mPoint      = new StSsdPointList();

  for(int i = 0; i < 3; i++)
    {
      mD[i]   = originalWafer.mD[i]; 
      mT[i]   = originalWafer.mT[i]; 
      mN[i]   = originalWafer.mN[i]; 
      mX[i]   = originalWafer.mX[i];
    } 
}

StSsdWafer& StSsdWafer::operator=(const StSsdWafer originalWafer)
{
  mId         = originalWafer.mId;
//   mDeadStripP = originalWafer.mDeadStripP;
//   mDeadStripN = originalWafer.mDeadStripN;

  for(int i = 0; i < 3; i++)
    {
      mD[i]   = originalWafer.mD[i]; 
      mT[i]   = originalWafer.mT[i]; 
      mN[i]   = originalWafer.mN[i]; 
      mX[i]   = originalWafer.mX[i];
    } 
  return *this;
}

/*!
Determines if two clusters can be matched based on geometrical considerations
 */
int StSsdWafer::geoMatched(ssdDimensions_st *dimensions, StSsdCluster *ptr1, StSsdCluster *ptr2)
{
  int geomatched = 0;
  int numStrip = int((2*dimensions[0].waferHalfActWidth*tan(dimensions[0].stereoAngle)/dimensions[0].stripPitch)+1);
  if ( (!ptr1) || (!ptr2) )
    geomatched = 0;
  else if((ptr2->getStripMean() > ( ptr1->getStripMean() - numStrip))
     && (ptr2->getStripMean() < (ptr1->getStripMean()  + numStrip)))
    geomatched = 1;
  return geomatched;
}

/*!
Must be useful but for what ???
 */
int StSsdWafer::setMatcheds(ssdDimensions_st *dimensions, StSsdPoint *Point, StSsdCluster *pMatched, StSsdCluster *nMatched)
{// strip(1) -> Upos(0)...
  Point->setPositionU((pMatched->getStripMean()-1)*dimensions[0].stripPitch,0);
  Point->setPositionU((nMatched->getStripMean()-1)*dimensions[0].stripPitch,1);

  // for evaluation only !!!
  int pHitIndex   = 0;
  int nHitIndex   = 0;
  int sptHitIndex = 0;
  for (pHitIndex = 0; pHitIndex < SSD_MAXIDMCHIT; pHitIndex++)
    {
      for (nHitIndex = 0; nHitIndex < SSD_MAXIDMCHIT; nHitIndex++)
	{
	  if ((pMatched->getIdMcHit(pHitIndex))
	      &&(nMatched->getIdMcHit(nHitIndex))
              &&(pMatched->getIdMcHit(pHitIndex) == nMatched->getIdMcHit(nHitIndex))
	      &&(sptHitIndex < SSD_MAXIDMCHIT))
	      Point->setNMchit(pMatched->getIdMcHit(pHitIndex),sptHitIndex++);
	}
    }
  return 1;
}

double StSsdWafer::matchDistr(StSsdClusterControl *clusterControl, double x)
{
  double mean = clusterControl->getMatchMean();
  double sigm = clusterControl->getMatchSigma();
  return (1/(sigm*sqrt(2.0*M_PI)))*exp(-0.5*((x-mean)*(x-mean))/(sigm*sigm));
}

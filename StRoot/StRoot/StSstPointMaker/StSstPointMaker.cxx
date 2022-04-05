//$Id: StSstPointMaker.cxx,v 1.3 2017/04/26 20:17:45 perev Exp $
//
//$Log: StSstPointMaker.cxx,v $
//Revision 1.3  2017/04/26 20:17:45  perev
//Hide m_DataSet
//
//Revision 1.2  2016/05/27 15:20:38  bouchet
//coverity DEAD_CODE fixed ; cleanup cout
//
//Revision 1.1  2015/06/23 16:29:04  jeromel
//Version of the SSD code for the SST - strated revision 1
//
//Revision 1.16  2015/06/18 22:29:29  bouchet
//CPP-CHECK : C-style coding ; cleanup : removed unused libraries and variables ; init ctor ; replace ROOT types by C++ types
//
//Revision 1.15  2015/06/10 14:06:58  bouchet
//reflect changes in StSstDbMaker : slsCtrl table retrieved from StSstDbMaker
//
//Revision 1.14  2015/05/23 20:53:13  bouchet
//writePointToContainer() needs StSstDynamicControl for ADC hit calculation
//
//Revision 1.13  2015/05/15 18:33:05  bouchet
//possible infinite loop fixed
//
//Revision 1.12  2015/05/08 14:16:32  bouchet
//typo ssd --> sst for sstStripCalib path in DB ; cosmetic
//
//Revision 1.11  2015/05/02 15:36:27  bouchet
//StSstBarrel object is cleared at the end of the current event ONLY if StSstTupleMaker is not in the chain
//
//Revision 1.10  2015/05/01 19:11:09  bouchet
//cleanup
//
//Revision 1.9  2015/04/30 18:16:46  bouchet
//add protection against null spa_strip, some cosmetics and cleanup
//
//Revision 1.7  2015/04/28 15:59:54  bouchet
//remove old methods, update ctor, update methods to fill gain calib and wafer status, cleanup
//
//Revision 1.6  2015/04/27 14:07:38  bouchet
//remove mode member from sstStripCalib (was first version) ; cleanup
//
//Revision 1.5  2015/04/26 17:56:36  bouchet
//ChipGain calibration methods removed ; cleanup
//
//Revision 1.4  2015/04/21 22:05:48  bouchet
//prototype methods to use sstGainCalibChip, remove unused arrays, typos ssd -> sst, print the # of events processed by the maker
//
//Revision 1.2  2015/04/20 19:02:34  bouchet
//remove spa_strip (unused) when writing points to container
//
//Revision 1.1  2015/04/19 18:52:10  bouchet
//initial commit ; SST classes only
//
//fork from the SSD code, move along - see history therein
#include "StSstPointMaker.h"
#include "TDataSetIter.h"
#include "StMessMgr.h"
#include "StSstUtil/StSstPoint.hh"
#include "StSstUtil/StSstPackage.hh"
#include "StSstUtil/StSstCluster.hh"
#include "StSstUtil/StSstStripList.hh"
#include "StSstUtil/StSstClusterList.hh"
#include "StSstUtil/StSstPointList.hh"
#include "StSstUtil/StSstPackageList.hh"
#include "StSstUtil/StSstWafer.hh"
#include "StSstUtil/StSstLadder.hh"
#include "StSstUtil/StSstBarrel.hh"
#include "StSstUtil/StSstStrip.hh"
#include "tables/St_spa_strip_Table.h" 
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sstSlsCtrl_Table.h"
#include "tables/St_sstClusterControl_Table.h"
#include "tables/St_sstDimensions_Table.h"
#include "tables/St_sstStripCalib_Table.h"
#include "tables/St_sstGainCalibWafer_Table.h"
#include "tables/St_sstNoise_Table.h"
#include "tables/St_sstWaferConfiguration_Table.h"
#include "StEvent.h"
#include "StSstHitCollection.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSstUtil/StSstConsts.h"
#include "TMath.h"
ClassImp(StSstPointMaker);

StSstPointMaker::StSstPointMaker(const char *name):
  StMaker(name), mGain(0), mWafConfig(0), mPedRmsData(0),
  mCtrl(0), mClusterCtrl(0),  
  mDynamicControl(0), mClusterControl(0)
{
  memset(mCalibArray,0,sizeof(mCalibArray));
  memset(mWaferStatus,0,sizeof(mWaferStatus));
  mEventCounter =0;
}  
//_____________________________________________________________________________
Int_t StSstPointMaker::Init(){
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSstPointMaker::InitRun(int runumber) {
  mCtrl = gStSstDbMaker->getSlsCtrl();
  if(!mCtrl){LOG_ERROR << "InitRun : No access to slsCtrl table" << endm;}
  else  {
    mDynamicControl = new StSstDynamicControl();
    mDynamicControl -> setnElectronInAMip(mCtrl->nElectronInAMip);
    mDynamicControl -> setadcDynamic(mCtrl->adcDynamic);
    mDynamicControl -> seta128Dynamic(mCtrl->a128Dynamic);
    mDynamicControl -> setnbitEncoding(mCtrl->nbitEncoding);
    mDynamicControl -> setnstripInACluster(mCtrl->nstripInACluster);
    mDynamicControl -> setpairCreationEnergy(mCtrl->pairCreationEnergy);
    mDynamicControl -> setparDiffP(mCtrl->parDiffP);
    mDynamicControl -> setparDiffN(mCtrl->parDiffN);
    mDynamicControl -> setparIndRightP(mCtrl->parIndRightP);
    mDynamicControl -> setparIndRightN(mCtrl->parIndRightN);
    mDynamicControl -> setparIndLeftP(mCtrl->parIndLeftP);
    mDynamicControl -> setparIndLeftN(mCtrl->parIndLeftN);
    mDynamicControl -> setdaqCutValue(mCtrl->daqCutValue);
    mDynamicControl -> printParameters();
  }
  mClusterCtrl = ((St_sstClusterControl *) GetInputDB("Geometry/sst/sstClusterControl"))->GetTable();
  if (!mClusterCtrl) {LOG_ERROR << "InitRun : No access to clusterControl table" << endm;}
  else {
    mClusterControl = new StSstClusterControl();
    mClusterControl -> setHighCut(mClusterCtrl->highCut);  
    mClusterControl -> setTestTolerance(mClusterCtrl->testTolerance);
    mClusterControl -> setClusterTreat(mClusterCtrl->clusterTreat);
    mClusterControl -> setAdcTolerance(mClusterCtrl->adcTolerance);
    mClusterControl -> setMatchMean(mClusterCtrl->matchMean);
    mClusterControl -> setMatchSigma(mClusterCtrl->matchSigma);
    mClusterControl -> printParameters();
  }  
  
  St_sstStripCalib *mPedRms = (St_sstStripCalib*) GetDataBase("Calibrations/sst/sstStripCalib");
  if (!mPedRms) {LOG_ERROR << "InitRun : No access to sstStripCalib - will use the default noise and pedestal values" << endm;}
  else { 
    LOG_INFO<<"InitRun for  : (sstStripCalib) is used"<<endm; 
    mPedRmsData = mPedRms->GetTable();
    std::cout << " size of sstStripTable : " << mPedRms->GetSize() << std::endl;
  }
  
  FillCalibTable();
  FillWaferTable();

  return kStOk;
}
//_____________________________________________________________________________
Int_t StSstPointMaker::Make()
{
  int res = 0; 
  TDataSet *SpaStrip = GetDataSet("sst_raw");
  
  if(! SpaStrip) {
    LOG_ERROR << "no input data set, wrong chain option" << endm;
    return kStErr;
  }
  St_spa_strip *spa_strip = dynamic_cast<St_spa_strip *> (SpaStrip->Find("spa_strip"));

  if(!spa_strip || spa_strip->GetNRows()==0){
      LOG_WARN << "no input (fired strip for the SST)"<<endm;
  }

  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  AddData(scm_spt); 
  
  St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",5000);//09/13
  AddData(scf_cluster);
  
  StEvent *pEvent = (StEvent*) GetInputDS("StEvent");
  //  StSstHitCollection *sstHitCollection;
  if (!pEvent) {
    LOG_ERROR << "StSstPointMaker::Make(): There is no StEvent " << endm;
    return kStErr;
  }
  StSstHitCollection *sstHitCollection = pEvent->sstHitCollection();
  if (!sstHitCollection){
    LOG_WARN << "The SST hit collection does not exist  - creating a new one" << endm;
    sstHitCollection = new StSstHitCollection;
    pEvent->setSstHitCollection(sstHitCollection);
  }
  LOG_INFO<<"#################################################"<<endm;
  LOG_INFO<<"####     START OF NEW SST POINT MAKER        ####"<<endm;
  LOG_INFO<<"####        SST BARREL INITIALIZATION        ####"<<endm;
  LOG_INFO<<"####          BEGIN INITIALIZATION           ####"<<endm; 
  StSstBarrel *mySst =gStSstDbMaker->getSst();
  mySst->setClusterControl(mClusterControl);
  //The full SST object is built only if we are processing physics data
  if(spa_strip && spa_strip->GetNRows()!=0){
    int stripTableSize = mySst->readStripFromTable(spa_strip);
    LOG_INFO<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endm;
    mySst->sortListStrip();
    //PrintStripSummary(mySst);
    LOG_INFO<<"####       NUMBER OF DB ENTRIES "<< ReadNoiseTable(mySst) <<"       ####"<<endm;
    int nClusterPerSide[2]={0,0};
    mySst->doSideClusterisation(nClusterPerSide,mWaferStatus);
    LOG_INFO<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endm;
    LOG_INFO<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endm;
    mySst->sortListCluster();
    int nClusterWritten = mySst->writeClusterToTable(scf_cluster,spa_strip);
    LOG_INFO<<"####   -> "<<nClusterWritten<<" CLUSTERS WRITTEN INTO TABLE       ####"<<endm;
    //PrintClusterSummary(mySst);
    //PrintStripDetails(mySst,8310);
    //PrintClusterDetails(mySst,8310); 
    //debugUnPeu(mySst);
    int nPackage = mySst->doClusterMatching(mCalibArray);
    LOG_INFO<<"####   -> "<<nPackage<<" PACKAGES IN THE SST           ####"<<endm;
    mySst->convertDigitToAnalog(mDynamicControl);
    mySst->convertUFrameToOther();
    //PrintPointSummary(mySst);

    //get McEvent here
    int nSptWritten    = 0;
    StMcEvent* mcEvent = 0;
    mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
    if(mcEvent){	
      LOG_DEBUG << " mcEvent exists " << endm;
      nSptWritten = mySst->writePointToContainer(scm_spt,sstHitCollection,scf_cluster,mDynamicControl,mcEvent);
    }
    else{
      nSptWritten = mySst->writePointToContainer(scm_spt,sstHitCollection,scf_cluster,mDynamicControl);
    }
    LOG_INFO<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endm;
    if(sstHitCollection){
      if (sstHitCollection->numberOfHits()>0){
	mEventCounter++;
	LOG_INFO<<"####   -> "<<sstHitCollection->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endm;
	//clean data stream
	scf_cluster->Purge();
	scm_spt->Purge();
      }
      else {
	LOG_INFO<<" ######### NO SST HITS WRITTEN INTO CONTAINER   ####"<<endm;
      }
    }
    LOG_INFO<<"####        END OF SST NEW POINT MAKER       ####"<<endm;
    LOG_INFO<<"#################################################"<<endm;
    if (nSptWritten) res = kStOK;
  }
  if (!GetMaker("SstTuple")) {
    //do not delete mySst object since it should be used in StSstMyMONMaker
  mySst->Reset();
  }
  if(res!=kStOK){
    LOG_WARN <<"Make : no output" << endm;
    return kStWarn;
  }
  return kStOK;
}
//_____________________________________________________________________________

void StSstPointMaker::PrintStripSummary(StSstBarrel *mySst)
{
  int ladderCountN[20]={0};
  int ladderCountP[20]={0};
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySst->mLadders[i]->mWafers[j]->getStripP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySst->mLadders[i]->mWafers[j]->getStripN()->getSize();
      }
    }
  
  LOG_INFO <<"PrintStripSummary : Number of raw data in the SST" << endm;
  LOG_INFO <<"PrintStripSummary : Active Ladders : " << endm;
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintStripSummary : Counts (p-side): " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountP[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintStripSummary : Counts (n-side): " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountN[i];
    }
  LOG_DEBUG<<endm;
}

//_____________________________________________________________________________
void StSstPointMaker::debugUnPeu(StSstBarrel *mySst)
{
  int monladder,monwafer;
  monladder=7;
  monwafer=6;
  mySst->debugUnPeu(monladder,monwafer);
}

//_____________________________________________________________________________
void StSstPointMaker::PrintClusterSummary(StSstBarrel *mySst)
{
  int ladderCountN[20]={0};
  int ladderCountP[20]={0};
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
	ladderCountP[i]=ladderCountP[i]+mySst->mLadders[i]->mWafers[j]->getClusterP()->getSize();
	ladderCountN[i]=ladderCountN[i]+mySst->mLadders[i]->mWafers[j]->getClusterN()->getSize();
      }
    }
  
  LOG_INFO <<"PrintClusterSummary : Number of clusters in the SST" << endm;
  LOG_INFO << "PrintClusterSummary : Active Ladders : " << endm;
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (p-side): " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountP[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintClusterSummary : Counts (n-side): " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCountN[i];
    }
  LOG_DEBUG<<endm;
}
//_____________________________________________________________________________
void StSstPointMaker::PrintPointSummary(StSstBarrel *mySst)
{
  int ladderCount[20]={0};
  int ladderCount11[20]={0};
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
	ladderCount[i]=ladderCount[i]+mySst->mLadders[i]->mWafers[j]->getPoint()->getSize();
	StSstPoint *pSpt = mySst->mLadders[i]->mWafers[j]->getPoint()->first();
	while (pSpt){	
	  if (pSpt->getNMatched()==11) ladderCount11[i]++;
	  pSpt    = mySst->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
	}
      }
    }
  
  LOG_INFO<<"PrintPointSummary : Number of hits in the SST" << endm;
  LOG_INFO<< "PrintPointSummary : Active Ladders : " << endm;
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG<<i+1;
    }
  
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintPointSummary : Counts         : " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCount[i];
    }
  LOG_DEBUG<<endm;
  LOG_INFO << "PrintPointSummary : Counts  (11)   : " << endm;
  for (int i=0;i<20;i++)
    if (mySst->isActiveLadder(i)>0) {
      LOG_DEBUG.width(5);
      LOG_DEBUG <<ladderCount11[i];
    }
  LOG_DEBUG<<endm;
}
//_____________________________________________________________________________
void StSstPointMaker::PrintStripDetails(StSstBarrel *mySst, int mywafer)
{
  int found = 0 ;
  LOG_DEBUG <<"PrintStripDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
        if (mySst->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side strip informations
          if (mySst->mLadders[i]->mWafers[j]->getStripP()->getSize()==0) {
            LOG_DEBUG <<"PrintStripDetails() - No strip on the P-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintStripDetails() - "
		     <<mySst->mLadders[i]->mWafers[j]->getStripP()->getSize()<<" strip(s) on the P-side of this wafer "<< endm;  
            LOG_DEBUG<<"PrintStripDetails() - Strip/Adc/Ped/Noise/Analog"<< endm;  
            StSstStrip *pStripP = mySst->mLadders[i]->mWafers[j]->getStripP()->first();
            while (pStripP){
              LOG_DEBUG<<"PrintStripDetails() - "
		       <<pStripP->getNStrip()<<" "
		       <<pStripP->getDigitSig()<<" "
		       <<pStripP->getPedestal()<<" "
		       <<pStripP->getSigma()<<" "
		       <<pStripP->getAnalogSig()<<" "
		       <<endm;  
	      for(int e=0;e<5;e++){printf("e=%d idMcHit=%d idMcTrack=%d\n",e,pStripP->getIdMcHit(e),pStripP->getIdMcTrack(e));}
              pStripP    = mySst->mLadders[i]->mWafers[j]->getStripP()->next(pStripP);
            }
	  }
          //Looking for the N-side strip informations
          if (mySst->mLadders[i]->mWafers[j]->getStripN()->getSize()==0) {
            LOG_DEBUG <<"PrintStripDetails() - No strip on the N-side of this wafer "<< endm;  
          }
          else {
            LOG_DEBUG<<"PrintStripDetails() - "
		     <<mySst->mLadders[i]->mWafers[j]->getStripN()->getSize()<<" strip(s) on the N-side of this wafer "<< endm;  
            LOG_DEBUG <<"StSstPointMaker::PrintStripDetails() - Strip/Adc/Ped/Noise/Analog"<< endm;  
            StSstStrip *pStripN = mySst->mLadders[i]->mWafers[j]->getStripN()->first();
            while (pStripN){
              LOG_DEBUG<<"PrintStripDetails() - "
		       <<pStripN->getNStrip()<<" "
		       <<pStripN->getDigitSig()<<" "
		       <<pStripN->getPedestal()<<" "
		       <<pStripN->getSigma()<<" "
		       <<pStripN->getAnalogSig()<<" "
		       <<endm;  
	      for(int e=0;e<5;e++){printf("e=%d idMcHit=%d idMcTrack=%d\n",e,pStripN->getIdMcHit(e),pStripN->getIdMcTrack(e));}
              pStripN    = mySst->mLadders[i]->mWafers[j]->getStripN()->next(pStripN);
            }     
          }
        }
      }
    } 
  if (found==0) {LOG_DEBUG <<"PrintStripDetails() - Wafer not found !!!"<<endm;}
}
//_____________________________________________________________________________
void StSstPointMaker::PrintClusterDetails(StSstBarrel *mySst, int mywafer)
{
  int found = 0;
  LOG_INFO <<"PrintClusterDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
        if (mySst->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          //Looking for the P-side cluster informations
          if (mySst->mLadders[i]->mWafers[j]->getClusterP()->getSize()==0) {
            LOG_INFO <<"PrintClusterDetails() - No cluster on the P-side of this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintClusterDetails() - "
		    <<mySst->mLadders[i]->mWafers[j]->getClusterP()->getSize()<<" cluster(s) on the P-side of this wafer "<< endm;  
            LOG_INFO<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSstCluster *pClusterP = mySst->mLadders[i]->mWafers[j]->getClusterP()->first();
            while (pClusterP){
              LOG_INFO<<"PrintClusterDetails() - "
		      <<pClusterP->getNCluster()<<" "
		      <<pClusterP->getFlag()<<" "
		      <<pClusterP->getClusterSize()<<" "
		      <<pClusterP->getFirstStrip()<<" "
		      <<pClusterP->getStripMean()<<" "
		      <<pClusterP->getTotAdc()<<" "
		      <<pClusterP->getFirstAdc()<<" "
		      <<pClusterP->getLastAdc()<<" "
		      <<pClusterP->getTotNoise()<<" "
		      <<endm;  
	      for(int e=0;e<5;e++){printf("e=%d idMcHit=%d \n",e,pClusterP->getIdMcHit(e));}
              pClusterP    = mySst->mLadders[i]->mWafers[j]->getClusterP()->next(pClusterP);
            }
	  }
          //Looking for the N-side cluster informations
          if (mySst->mLadders[i]->mWafers[j]->getClusterN()->getSize()==0) {
            LOG_INFO <<"PrintClusterDetails() - No cluster on the N-side of this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintClusterDetails() - "
		    <<mySst->mLadders[i]->mWafers[j]->getClusterN()->getSize()<<" cluster(s) on the N-side of this wafer "<< endm;  
            LOG_INFO<<"PrintClusterDetails() - Cluster/Flag/Size/1st Strip/Strip Mean/TotAdc/1st Adc/Last Adc/TotNoise"<< endm;  
            StSstCluster *pClusterN = mySst->mLadders[i]->mWafers[j]->getClusterN()->first();
            while (pClusterN){
              LOG_INFO<<"PrintClusterDetails() - "
		      <<pClusterN->getNCluster()<<" "
		      <<pClusterN->getFlag()<<" "
		      <<pClusterN->getClusterSize()<<" "
		      <<pClusterN->getFirstStrip()<<" "
		      <<pClusterN->getStripMean()<<" "
		      <<pClusterN->getTotAdc()<<" "
		      <<pClusterN->getFirstAdc()<<" "
		      <<pClusterN->getLastAdc()<<" "
		      <<pClusterN->getTotNoise()<<" "
		      <<endm;  
	      for(int e=0;e<5;e++){printf("e=%d idMcHit=%d \n",e,pClusterN->getIdMcHit(e));}
              pClusterN    = mySst->mLadders[i]->mWafers[j]->getClusterN()->next(pClusterN);
            }     
          }
        }
      }
    }
  if (found==0){ LOG_INFO <<"PrintClusterDetails() - Wafer not found !!!"<<endm; }
}

//_____________________________________________________________________________
void StSstPointMaker::PrintPackageDetails(StSstBarrel *mySst, int mywafer)
{
  int found = 0;
  LOG_INFO <<"PrintPackageDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
        if (mySst->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySst->mLadders[i]->mWafers[j]->getPackage()->getSize()==0) {
            LOG_INFO <<"PrintPackageDetails() - No package in this wafer "<< endm;  
          }
          else {
            LOG_INFO <<"PrintPackageDetails() - "<<mySst->mLadders[i]->mWafers[j]->getPackage()->getSize()<<" package(s) in this wafer "<< endm;  
            LOG_INFO <<"PrintPackageDetails() - Package/Kind/Size"<< endm;  
            StSstPackage *pPack = mySst->mLadders[i]->mWafers[j]->getPackage()->first();
            while (pPack){
              LOG_INFO<<"PrintPackageDetails() - "<<pPack->getNPackage()<<" "
		      <<pPack->getKind()<<" "
		      <<pPack->getSize()<<" "<<endm;
              for (int k=0;k<pPack->getSize();k++) {
                LOG_INFO<<"PrintPackageDetails() - "<<k<<" "<<pPack->getMatched(k)<<" "<<pPack->getMatched(k)->getNCluster()<<endm;
              }
              pPack    = mySst->mLadders[i]->mWafers[j]->getPackage()->next(pPack);
            }     
          }
        }
      }
    }
  if (found==0){ LOG_INFO <<"PrintPackageDetails() - Wafer not found !!!"<<endm;}
}

//_____________________________________________________________________________
void StSstPointMaker::PrintPointDetails(StSstBarrel *mySst, int mywafer)
{
  int found = 0;
  float convMeVToAdc = (int)pow(2.0,mDynamicControl->getnbitEncoding())/(mDynamicControl->getpairCreationEnergy()*mDynamicControl->getadcDynamic()*mDynamicControl->getnElectronInAMip());
  LOG_INFO <<"PrintPointDetails() - Wafer "<<mywafer<< endm;  
  for (int i=0;i<20;i++) 
    if (mySst->isActiveLadder(i)>0) {
      for (int j=0; j<mySst->mLadders[i]->getWaferPerLadder();j++) {
        if (mySst->mLadders[i]->mWafers[j]->getIdWafer()==mywafer) {
          found=1;
          if (mySst->mLadders[i]->mWafers[j]->getPoint()->getSize()==0) {
            LOG_INFO <<"PrintPointDetails() - No hit in this wafer "<< endm;  
          }
          else {
            LOG_INFO<<"PrintPointDetails() - "<<mySst->mLadders[i]->mWafers[j]->getPoint()->getSize()<<" hit(s) in this wafer "<< endm; 
 
            LOG_INFO<<"PrintPointDetails() - Hit/Flag/NMatched/IdClusP/IdClusN/idMcHit[0]/idMcHit[1]/idMcHit[2]/idMcHit[3]/idMcHit[4]/Xg[0]/Xg[1]/Xg[2]/Xl[0]/Xl[1]/Xl[2]/a/b"<<endm;  
            StSstPoint *pSpt = mySst->mLadders[i]->mWafers[j]->getPoint()->first();
            while (pSpt){
	      float a = 0, b = 0;
	      a = convMeVToAdc*(pSpt->getDe(0)+pSpt->getDe(1));
	      b = convMeVToAdc*(pSpt->getDe(0)-pSpt->getDe(1));
              LOG_INFO<<"PrintPointDetails() - "
		      <<pSpt->getNPoint()    <<" "
		      <<pSpt->getFlag()      <<" "
		      <<pSpt->getNMatched()  <<" "
		      <<pSpt->getIdClusterP()<<" "
		      <<pSpt->getIdClusterN()<<" "
		      <<pSpt->getNMchit(0)   <<" "
		      <<pSpt->getNMchit(1)   <<" "
		      <<pSpt->getNMchit(2)   <<" "
		      <<pSpt->getNMchit(3)   <<" "
		      <<pSpt->getNMchit(4)   <<" "
		      <<pSpt->getXg(0)       <<" "
		      <<pSpt->getXg(1)       <<" "
		      <<pSpt->getXg(2)       <<" "
		      <<pSpt->getXl(0)       <<" "
		      <<pSpt->getXl(1)       <<" "
		      <<pSpt->getXl(2)       <<" "
		      <<a                    <<" "
		      <<b                    <<" "
		      <<endm;
	      printf("pulseP =%f pulseN = %f\n",a,b);  
              pSpt    = mySst->mLadders[i]->mWafers[j]->getPoint()->next(pSpt);
            }     
          }
        }
      }
    }
  if (found==0) {LOG_INFO <<"PrintPointDetails() - Wafer not found !!!"<<endm; }
}
//_____________________________________________________________________________
void StSstPointMaker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void StSstPointMaker::FillCalibTable(){
  mGain = ((St_sstGainCalibWafer*)GetDataBase("Calibrations/sst/sstGainCalibWafer"))->GetTable();
  if(mGain){ 
    for(int i=0; i<320;i++){
      LOG_DEBUG<< " ladder : " << i/16
	       << " wafer  : " << i%16 
	       << " status : " << mGain[0].nGain[i] << endm;
      mCalibArray[i] = mGain[0].nGain[i];
    }
  }
  else { 
    LOG_WARN << "InitRun : No access to Gain Calib - will use the default gain" << endm;
    LOG_WARN << "We will use the default table" <<endm;
    for(int i=0; i<320;i++){
      mCalibArray[i] = 1;
    }
  }
}
//_____________________________________________________________________________
void StSstPointMaker::FillWaferTable(){
  mWafConfig = ((St_sstWaferConfiguration*) GetDataBase("Geometry/sst/sstWaferConfiguration"))->GetTable();
  if(mWafConfig){ 
    for(int i=0; i<320;i++){
      LOG_DEBUG<< " ladder : " << i/16
	       << " wafer  : " << i%16 
	       << " status : " << (int)mWafConfig[0].nStatus[i] << endm;
      mWaferStatus[i/16][i%16] = (int)mWafConfig[0].nStatus[i]; 
    }
  }
  else { 
    LOG_WARN << "InitRun : No access to Wafer Config - will use the default wafer config" << endm;
    LOG_WARN << "We will use the default table" <<endm;
    for(int i=0; i<20;i++){
      for(int j=0; j<16;j++){ 
	mWaferStatus[i][j] = 1;
      }
    }
  }
}
//_____________________________________________________________________________
Int_t StSstPointMaker::ReadNoiseTable(StSstBarrel *mySst){
  int noiseTableSize = 0; 
  if(!mPedRmsData){
    LOG_WARN << "Make : No pedestal and noise values (sstStripCalib table missing), will use default values" <<endm;
    noiseTableSize = mySst->readNoiseDefault(mDynamicControl);
  }
  else{
    //temporary
    noiseTableSize = mySst->readNoiseFromTable(mPedRmsData,mDynamicControl);
  }
  return noiseTableSize;  
}
//____________________________________________________________________________
Int_t StSstPointMaker::Finish() {
  LOG_INFO << "StSstPointMaker : Finish() with " << mEventCounter <<" processed"  << endm;
   return kStOK;
}


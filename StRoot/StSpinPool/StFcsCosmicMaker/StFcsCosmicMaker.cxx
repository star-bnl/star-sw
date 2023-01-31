/*
 *
 * \class StFcsCosmicMaker
 *
 */

#include "StFcsCosmicMaker.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StFcsCluster.h"
#include "StRoot/StFcsDbMaker/StFcsDb.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"

StFcsCosmicMaker::StFcsCosmicMaker(const Char_t* name) : StMaker(name) {
  mNTowerThre[0]=10;
  mSigmaMaxThre[0]=3.0;
  mSigmaMinThre[0]=0.5;
  mThetaThre[0]=15;
  mNTowerThre[1]=8;
  mSigmaMaxThre[1]=2.0;
  mSigmaMinThre[1]=0.5;
  mThetaThre[1]=15;
};

StFcsCosmicMaker::~StFcsCosmicMaker(){};

Int_t StFcsCosmicMaker::Init(){
  SetAttr(".Privilege",1); //give Privilege to skip a event

  mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
  if(!mFcsDb){
    LOG_FATAL << "Error finding StFcsDb"<< endm;
    return kStFatal;
  }

  if(mSetFile==0){
      int yday=mRun/1000;
      sprintf(mFilename,"%d/%d.cosmic.root",yday,mRun);
      printf("StFcsCosmicMaker::Init - Opening %s\n",mFilename);
  }else{
      sprintf(mFilename,"%s",mSetFile);
  }
  mFile=new TFile(mFilename,"RECREATE");

  const char* nameEHP[kFcsEHP] = {"Ecal","Hcal","Pres"};
  char t[100],t2[100];
  for(int ehp=0; ehp<2; ehp++){
    sprintf(t,"%4s_ADC",nameEHP[ehp]);
    mAdc[ehp] = new TH1F(t,t,100,0.0,100.0);
    sprintf(t,"%4s_NTower",nameEHP[ehp]);
    mNTower[ehp] = new TH1F(t,t,100,0.0,100.0);
    sprintf(t,"%4s_SigmaMax",nameEHP[ehp]);
    mSigmaMax[ehp] = new TH1F(t,t,100,0.0,10.0);
    sprintf(t,"%4s_SigmaMin",nameEHP[ehp]);
    mSigmaMin[ehp] = new TH1F(t,t,100,0.0,3.0);
    sprintf(t,"%4s_Sigma",nameEHP[ehp]);    
    sprintf(t2,"%4s; SigmaMax; SigmaMin",nameEHP[ehp]);
    mSigma[ehp] = new TH2F(t,t2,100,0.0,10.0,100,0.0,3.0);
    sprintf(t,"%4s_SigmaNtow",nameEHP[ehp]);    
    sprintf(t2,"%4s; SigmaMax; NTower",nameEHP[ehp]);
    mSigmaNtow[ehp] = new TH2F(t,t2,100,0.0,10.0,100,0.0,100.0);
  }    
  return kStOK;
};

Int_t StFcsCosmicMaker::Make() {
  LOG_INFO << "StFcsCosmicMaker::Make()" << endm;
  mFcsCollection=0;
  
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) { 
      LOG_INFO << "No StEvent found" << endm;
  }else{ 
    mFcsCollection=event->fcsCollection();
  } 
  if(!mFcsCollection){
    LOG_INFO << "No StFcsCollection found" << endm;
    return kStErr;
  }
  
  static int nEvent=0;
  static int nCosmic=0;  
  int flag=0;
  for(int det=0; det<4; det++){  
    int ehp=det/2;
    int nCluster=mFcsCollection->numberOfClusters(det);
    StSPtrVecFcsCluster& clusters = mFcsCollection->clusters(det); 
    for (int i=0; i<nCluster; i++){
      StFcsCluster* clu=clusters[i];
      int nt = clu->nTowers();
      float smax = clu->sigmaMax();
      float smin= clu->sigmaMin();
      float theta = clu->theta() * 180.0 / 3.141592654;
      while(theta>90.0) theta-=180.0;  //reduce to +-90 degree
      theta = fabs(theta); //reduce to 0~90 degree from x axis
      mNTower[ehp]->Fill(float(nt));
      mSigmaMax[ehp]->Fill(smax);
      mSigmaMin[ehp]->Fill(smin);
      mSigma[ehp]->Fill(smax,smin);      
      mSigmaNtow[ehp]->Fill(smax,nt);      
      if(nt > mNTowerThre[ehp]){
	LOG_INFO << Form("StFcsCosmicMaker NTow=%3d SMax=%8.5f SMin=%8.5f",nt,smax,smin)<<endm;
	if(smax > mSigmaMaxThre[ehp] && smin < mSigmaMinThre[ehp] && theta>mThetaThre[ehp]){
	  //cosmic candidate
	  for(int j=0; j<nt; j++){
	    StFcsHit* hit = clu->hits()[j];
	    int adc = hit->adcSum();
	    mAdc[ehp]->Fill(adc);
	  }
	  flag=1;
	}
      }
    }
  }
  nEvent++;
  if(flag==1){ //found cosmic candidate
    nCosmic++;
    LOG_INFO << Form("StFcsCosmicMaker found cosmic candidate %d / %d events",nCosmic,nEvent)<<endm;
    return kStOK;
  }else{ // oterwise skip event display
    LOG_INFO << Form("StFcsCosmicMaker did not found cosmic candidate %d / %d events, skipping",nCosmic,nEvent)<<endm;
    return kStSkip;
  }
};

Int_t StFcsCosmicMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsCosmicMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsCosmicMaker);

/*
 * $Id: StFcsCosmicMaker.cxx,v 1.9 2021/03/30 13:29:27 akio Exp $
 * $Log: StFcsCosmicMaker.cxx,v $
 *
 */

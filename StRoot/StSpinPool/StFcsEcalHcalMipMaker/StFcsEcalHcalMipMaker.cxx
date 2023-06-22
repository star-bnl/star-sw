#include "StFcsEcalHcalMipMaker.h"
#include "TDataSetIter.h"
#include "StDAQMaker/StDAQReader.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StFcsCluster.h"
#include "StRoot/StFcsDbMaker/StFcsDb.h"
#include "StRoot/StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"
#include "TCanvas.h"

#include <string.h>
#include <time.h>

//ClassImp(StFcsEcalHcalMipMaker)

//_____________________________________________________________________________
/// constructor

StFcsEcalHcalMipMaker::StFcsEcalHcalMipMaker(const char *name):StMaker(name){

}

//_____________________________________________________________________________
/// destructor

StFcsEcalHcalMipMaker::~StFcsEcalHcalMipMaker(){};
 

//_____________________________________________________________________________
/// Init - the top level StChain calls to initialize all its makers 
Int_t StFcsEcalHcalMipMaker::Init(){  
  mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
  
  sprintf(mFilename,"%8d.mip.root",mRun);
  LOG_INFO << "StFcsEcalHcalMipMaker::Init - Opening "<<mFilename<<endm;
  mFile=new TFile(mFilename,"RECREATE");
  
  const char* nameEH[2] = {"Ecal","Hcal"};
  const char* nameNS[kFcsNorthSouth] = {"N","S"};
  char f[100], t[100]; //histogram file names
  
  for(int eh=0; eh<2; eh++){
    mNClu[eh] = new TH1F(Form("NCluster_%s",nameEH[eh]),
			 Form("NCluster_%s",nameEH[eh]),
			 50,0,50);
    mNTowClu[eh] = new TH1F(Form("NTowerCluster_%s",nameEH[eh]),
			    Form("NTowerCluster_%s",nameEH[eh]),
			    30,0,30);
    mNNeiClu[eh] = new TH1F(Form("NNeiCluster_%s",nameEH[eh]),
			    Form("NNeiCluster_%s",nameEH[eh]),
			    20,0,20);
  }
  const int nbin=256;
  const float max=3.0;
  for(int det=0; det<4; det++){
    int ns  = mFcsDb->northSouth(det);
    int eh = mFcsDb->ecalHcalPres(det);
    int maxid = mFcsDb->maxId(det);    
    mAdc[det] = new TH2F(Form("ADC_%s%s",nameEH[eh],nameNS[ns]),
			 Form("ADC %s%s",nameEH[eh],nameNS[ns]),
			 maxid,0,maxid,nbin,0,max);
    mAdcSingleTower[det] = new TH2F(Form("ADCSingle_%s%s",nameEH[eh],nameNS[ns]),
				    Form("ADCSingle %s%s",nameEH[eh],nameNS[ns]),
				    maxid,0,maxid,nbin,0,max);
    mAdcEcalMatch[det] = new TH2F(Form("ADCEcal_%s%s",nameEH[eh],nameNS[ns]),
				  Form("ADCEcalCoin %s%s",nameEH[eh],nameNS[ns]),
				  maxid,0,maxid,nbin,0,max);
  }
  mX = new TH2F("X","X; XEcal; XHcal",100,-100,100,100,-100,100);
  mY = new TH2F("Y","Y; YEcal; YHcal",100,   0,200,100,   0,200);
  mDXX= new TH2F("DXX","DX; XHcal; DX",100,-100,100,100,-30,30);
  mDXY= new TH2F("DXY","DX; YHcal; DX",100,   0,200,100,-30,30);
  mDYX= new TH2F("DYX","DY; XHcal; DY",100,-100,100,100,-30,30);
  mDYY= new TH2F("DYY","DY; YHcal; DY",100,   0,200,100,-30,30);
  mDR= new TH2F("DR","DR; RHcal; DR",100,0,100,100,0,30);
  return kStOk;
}

//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StFcsEcalHcalMipMaker::Make(){
  mFcsCollection=0;
  StEvent* event= (StEvent*)GetInputDS("StEvent");  
  if(!event) {
    LOG_INFO << "No StEvent found" << endm;
  }else{
    mFcsCollection=event->fcsCollection();
  }
  if(!mFcsCollection){
    LOG_INFO << "No StFcsCollection found" << endm;
    return kStErr;
  }
  
  const int maxTower=1;
  const float ecalELow=0.15;
  const float ecalEHigh=0.50;
  const float dRCut=15.0;

  for(int ns=0; ns<2; ns++){
    StSPtrVecFcsCluster& ecal= mFcsCollection->clusters(ns);
    StSPtrVecFcsCluster& hcal= mFcsCollection->clusters(ns+2);
    int nEcal=mFcsCollection->numberOfClusters(ns); 
    int nHcal=mFcsCollection->numberOfClusters(ns+2); 
    mNClu[0]->Fill(nEcal);
    mNClu[1]->Fill(nHcal);
      
    //loop over Ecal clusters
    for(int i=0; i<nEcal;i++){      
      StFcsCluster* clu=ecal[i];
      int id=clu->hits()[0]->id();
      int ntow=clu->nTowers();
      int nnei=clu->nNeighbor();
      mNTowClu[0]->Fill(ntow);
      mNNeiClu[0]->Fill(nnei);
      mAdc[ns]->Fill(id,clu->energy());
      if(ntow<=maxTower && nnei==0){
	mAdcSingleTower[ns]->Fill(id,clu->energy());
      }
    }
    
    //loop over Hcal clusters
    for(int i=0; i<nHcal;i++){
      StFcsCluster* clu=hcal[i];
      int id=clu->hits()[0]->id();
      int ntow=clu->nTowers();
      int nnei=clu->nNeighbor();
      mNTowClu[1]->Fill(ntow);
      mNNeiClu[1]->Fill(nnei);
      mAdc[ns+2]->Fill(id,clu->energy());
      if(ntow<=2 && nnei==0){
        mAdcSingleTower[ns+2]->Fill(id,clu->energy());
	for(int i=0; i<nEcal;i++){ //loop over Ecal
	  StFcsCluster* eclu=ecal[i];
	  int entow=eclu->nTowers();
	  int ennei=eclu->nNeighbor();
	  if(entow<=maxTower && ennei==0 && eclu->energy()>ecalELow && eclu->energy()<ecalEHigh){
	    double px = mFcsDb->getHcalProjectedToEcalX(ns,clu->x()) * (ns*2-1);
	    double py = mFcsDb->getHcalProjectedToEcalY(ns,clu->y());
	    double ex = eclu->x() * mFcsDb->getXWidth(ns) * (ns*2-1);
	    double ey = eclu->y() * mFcsDb->getYWidth(ns);
	    double dx=ex-px;
	    double dy=ey-py;
	    double pr=sqrt(px*px+py*py);
	    double dr=sqrt(dx*dx+dy*dy);
	    mX->Fill(ex,px);
	    mY->Fill(ey,py);
	    if(abs(dy)<dRCut){
	      mDXX->Fill(px,dx);
	      mDXY->Fill(py,dx);
	    }
	    if(abs(dx)<dRCut){
	      mDYX->Fill(px,dy);
	      mDYY->Fill(py,dy);
	    }
	    mDR->Fill(pr,dr);
	    if(dr<dRCut){
	      mAdcEcalMatch[ns+2]->Fill(id,clu->energy());
	    }
	  }
	}	
      }
    }
  }
  return kStOK; 
}
  
Int_t StFcsEcalHcalMipMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsEcalHcalMipMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsEcalHcalMipMaker);


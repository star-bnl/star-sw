#include "StFcsMIPMaker.h"
#include "TDataSetIter.h"
#include "StDAQMaker/StDAQReader.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StFcsCluster.h"
#include "StRoot/StFcsDbMaker/StFcsDbMaker.h"
#include "StRoot/StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"
#include "TCanvas.h"

#include <string.h>
#include <time.h>

//ClassImp(StFcsMIPMaker)

//_____________________________________________________________________________
/// constructor

StFcsMIPMaker::StFcsMIPMaker(const char *name):StMaker(name){

}

//_____________________________________________________________________________
/// destructor

StFcsMIPMaker::~StFcsMIPMaker(){};
 

//_____________________________________________________________________________
/// Init - the top level StChain calls to initialize all its makers 
Int_t StFcsMIPMaker::Init(){
 
  
      mFcsDbMkr = static_cast< StFcsDbMaker*>(GetMaker("fcsDb"));
      
      if(mPedSub>0) mFcsDbMkr->readPedFromText();

      sprintf(mFilename,"%d_%d.root",mRun,mSub);
      printf("StFcsMIPMaker::Init - Opening %s\n",mFilename);
      mFile=new TFile(mFilename,"RECREATE");
      
      char* nameEHP[kFcsEHP] = {"Ecal","Hcal","Pres"};
      char* nameNS[kFcsNorthSouth] = {"N","S"};
      char f[100], t[100]; //histogram file names
      char adcsum_name[100];//1D hist names
      char adcsum_title[100]; //1D hist titles

      hitsperevent= new TH1F("hitsperevent","EventHits",100,0,200);
      
      for(int det=0; det<kFcsNDet; det++){
	int ns  = mFcsDbMkr->northSouth(det);
	int ehp = mFcsDbMkr->ecalHcalPres(det);
      	int maxid = mFcsDbMkr->maxId(det);

	if(maxid==0) continue; 
      
	for(int id=0; id<maxid; id++){      
	  
	  int ehp2,ns2,crt,sub,dep,ch;
	  
	  mFcsDbMkr->getDepfromId(det,id,ehp2,ns2,crt,sub,dep,ch);
	  sprintf(f,"%4s_%1s_TbinAdc_id%03d",nameEHP[ehp],nameNS[ns],id);	
	  sprintf(t,"%4s_%1s_TbinAdc_id%03d",nameEHP[ehp],nameNS[ns],id);
	  
	  //declare hist to count # of minbias events	  
	  minbias_events= new TH1F("minbias","minbias events",5,0,5);
	  
	  //DEFINE ADC SUM HISTS -- FOR MIPS
	  sprintf(adcsum_title,"adcsum det%d_id%d",det,id);
	  sprintf(adcsum_name, "adcsum_det%d_id%d",det,id);
	  towadcsum[det][id]= new TH1F(adcsum_name,adcsum_title,7000,0,5000);
	  towadcsum[det][id]->Sumw2();
 
	}
      }      
  
 return kStOk;
}




//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StFcsMIPMaker::Make(){
  
  mFcsCollection=0;
  StTriggerData* trg=0;
  
  //Getting StFcsRawDaqReader and TriggerData
  StFcsRawDaqReader* fcsraw=(StFcsRawDaqReader*)GetMaker("daqReader");
  StEvent* event= (StEvent*)GetInputDS("StEvent");  
  if(fcsraw){
    //Getting trigger data (if daq file)
    trg = fcsraw->trgdata();
    if(!trg){
      LOG_INFO << "Cannot find Trigger Data from StFcsRawDaqReader" << endm;
    }
  }else if(event){
    trg=event->triggerData();
    if(!trg){
      LOG_INFO << "Cannot find Trigger Data from StEvent" << endm;
    }
  }
  
  if(!event) {
    LOG_INFO << "No StEvent found" << endm;
  }else{
    mFcsCollection=event->fcsCollection();
  }
  if(!mFcsCollection){
    LOG_INFO << "No StFcsCollection found" << endm;
    return kStErr;
  }
  
  minbias_events->Fill(1);//count # of minbias events
  
  int nh[kFcsNDet]; memset(nh,0,sizeof(nh));
  int sum[kFcsNDet][kFcsEcalMaxId]; memset(sum,0,sizeof(sum));
  float etot[kFcsNDet]; memset(etot,0,sizeof(etot));
  memset(Hcal_array,0,sizeof(Hcal_array)); //initialize hcal array
  
  
  /////////////////////////////////
  //FIND A MIP
  
  //Appy nhits cut on Ecal
  if (mFcsCollection-> numberOfHits(1) < 50)// <---multiplicity/nhit cut
    {
      StSPtrVecFcsCluster& clusters= mFcsCollection->clusters(1);
      int nEclu=mFcsCollection->numberOfClusters(1); 
      
      //loop over Ecal clusters
      for(int i=0; i<nEclu;i++){
	StFcsCluster* clusterE=clusters[i];
	StThreeVectorF xyzE=mFcsDbMkr->getStarXYZfromColumnRow(1,clusterE->x(),clusterE->y());      
	int ntow=clusterE->nTowers();
	int nnei=clusterE->nNeighbor();
	
	//apply cluster cuts
	if(ntow==1 and nnei==0){
	  
	  //associate Hcal/Ecal clusters by finding shortest dist btwn cals (deltarmin)
	  deltarmin=1000000;
	  matchHcalClusE=0;
	  
	  //get corresponding Hcal cluster
	  int nHclu=mFcsCollection->numberOfClusters(3);//<--- det=3, Hcal
	  StSPtrVecFcsCluster& Hclusters=mFcsCollection->clusters(3);
	  
	  //loop over Hcal clusters
	  for(int j=0; j<nHclu; j++){
	    StFcsCluster* clusterH=Hclusters[j];
	    StThreeVectorF xyzH=mFcsDbMkr->getStarXYZfromColumnRow(3,clusterH->x(),clusterH->y());
	    deltax=xyzH.x()-xyzE.x();
	    deltay=xyzH.y()-xyzE.y();
	    deltar=pow(pow(deltax,2)+pow(deltay,2),0.5);
	    
	    if(deltar<deltarmin){
	      matchHcalClusE=clusterH->energy();
	      
	      deltar=deltarmin;
	    }	 
	  }
	  
	  StPtrVecFcsHit &EClusterHits=clusterE->hits();
	  
	  if(matchHcalClusE>6){//only keep events w/ Hcal energy>6GeV
	    for(size_t j=0; j<EClusterHits.size(); j++)//loop over # hits/clust
	      {
		int id=EClusterHits[j]->id();
		float adc=EClusterHits[j]->adcSum();
		
		towadcsum[1][id]->Fill(adc);//fill adcsum 1D histogram for MIPs
	      }
	  }
	}
      }
    }
  
  return kStOK; 
}

Int_t StFcsMIPMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsMIPMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsMIPMaker);


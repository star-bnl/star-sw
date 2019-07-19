/*
 *
 * \class StFcsQaMaker
 *
 */

#include "StFcsQaMaker.h"

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

StFcsQaMaker::StFcsQaMaker(const Char_t* name) : StMaker(name) {};

StFcsQaMaker::~StFcsQaMaker(){};

Int_t StFcsQaMaker::Init(){
  mFcsDbMkr = static_cast< StFcsDbMaker*>(GetMaker("fcsDb"));
  if(!mFcsDbMkr){
    LOG_FATAL << "Error finding StFcsDbMaker"<< endm;
    return kStFatal;
  }

  if(mPedSub>0 || mDump>0) mFcsDbMkr->readPedFromText();

  int yday=mRun/1000;
  sprintf(mFilename,"%d/%d.root",yday,mRun);
  printf("StFcsQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");

  char* nameEHP[kFcsEHP] = {"Ecal","Hcal","Pres"};
  char* nameNS[kFcsNorthSouth] = {"N","S"};
  char t[100],t2[100];

  mDataSize = new TH1F("DataSize","DataSize",100,-1.0,7.0);
  for(int det=0; det<kFcsNDet; det++){
    int ns  = mFcsDbMkr->northSouth(det);
    int ehp = mFcsDbMkr->ecalHcalPres(det);
    int maxid = mFcsDbMkr->maxId(det);

    if(maxid==0) continue; 
    
    sprintf(t,"%4s_%1s_IdTbinAdc",nameEHP[ehp],nameNS[ns]);
    mAdcTb2[det] = new TH2F(t,t,
			    maxid, 0.0, float(maxid),
			    mNTimeBins, 0.0, float(mNTimeBins));
    for(int id=0; id<maxid; id++){      
      int c=mFcsDbMkr->getColumnNumber(det,id);
      int r=mFcsDbMkr->getRowNumber(det,id);
      int ehp2,ns2,crt,sub,dep,ch;
      mFcsDbMkr->getDepfromId(det,id,ehp2,ns2,crt,sub,dep,ch);
      sprintf(t ,"%4s_%1s_TbinAdc_id%03d",nameEHP[ehp],nameNS[ns],id);
      sprintf(t2,"%1s%1s%03d_r%02dc%02d_Dep%02dCh%02d",
	      nameEHP[ehp],nameNS[ns],id,r,c,dep,ch);
      printf("%s %s\n",t,t2);
      mAdcTb[det][id] = new TH2F(t,t2,
				 mNTimeBins, 0.0, float(mNTimeBins),
				 mMaxAdc, 0.0, float(mMaxAdc));
    }
    sprintf(t,"%4s_%1s_Adc",nameEHP[ehp],nameNS[ns]);
    int maxdep = mFcsDbMkr->getNDep(ehp,ns);
    int maxdepch = maxdep * kFcsMaxDepCh;
    mAdcId[det] = new TH2F(t,t,
			   maxdepch, 0.0, float(maxdepch),
			   mMaxAdc, 0.0, float(mMaxAdc));
    sprintf(t,"%4s_%1s_IdSum",nameEHP[ehp],nameNS[ns]);
    mAdcSumId[det] = new TH2F(t,t,
			      maxid, 0.0, float(maxid),
			      mMaxAdcSum, 0.0, float(mMaxAdcSum));
    sprintf(t,"%4s_%1s_NHit",nameEHP[ehp],nameNS[ns]);
    mNHit[det] = new TH1F(t,t,maxid,0.0,float(maxid+1));
    
    if(ehp<2){ //clusters for Ecasl/Hcal
      sprintf(t,"%4s_%1s_NCluster",nameEHP[ehp],nameNS[ns]);
      mNClu[det] = new TH1F(t,t,10,0.0,10.0);

      sprintf(t,"%4s_%1s_NTowClu",nameEHP[ehp],nameNS[ns]);
      mNTowClu[det] = new TH1F(t,t,10,0.0,10.0);

      sprintf(t,"%4s_%1s_NNeiClu",nameEHP[ehp],nameNS[ns]);
      mNNeiClu[det] = new TH1F(t,t,10,0.0,10.0);

      for(int id=0; id<maxid; id++){      
	mFcsDbMkr->getName(det,id,t2);	
	sprintf(t ,"%1s%1s%03d_NTowClu_E",nameEHP[ehp],nameNS[ns],id);
	sprintf(t2 ,"%22s_NTowClu_E",t2);
	mNTowEClu[det][id] =  new TH2F(t,t2,10,1.0,11.0,100,0.0,10.0);

	sprintf(t ,"%1s%1s%03d_NTowCluIso_E",nameEHP[ehp],nameNS[ns],id);
	sprintf(t2 ,"%22s_NTowCluIso_E",t2);
	mNTowECluIso[det][id] =  new TH2F(t,t2,10,1.0,11.0,100,0.0,10.0);

	sprintf(t ,"%1s%1s%03d_NTowCluIsoH_E",nameEHP[ehp],nameNS[ns],id);
	sprintf(t2 ,"%22s_NTowCluIsoH_E",t2);
	mNTowECluIsoH[det][id] =  new TH2F(t,t2,10,1.0,11.0,100,0.0,10.0);
      }      
    }
  }  
  return kStOK;
};

Int_t StFcsQaMaker::Make() {
  mFcsCollection=0;
  StTriggerData* trg=0;

  //Getting StFcsRawDaqReader and TriggerData
  StFcsRawDaqReader* fcsraw=(StFcsRawDaqReader*)GetMaker("daqReader");
  StEvent* event= (StEvent*)GetInputDS("StEvent");  
  if(fcsraw){
      //Getting trigger data (if daq file)
      trg = fcsraw->trgdata();
      if(!trg){
	  LOG_INFO << "Canot find Trigger Data from StFcsRawDaqReader" << endm;
      }
  }else if(event){
      trg=event->triggerData();
      if(!trg){
	  LOG_INFO << "Canot find Trigger Data from StEvent" << endm;
      }
  }

  //tof multiplicity from trigger data
  int tofmult = trg->tofMultiplicity();

  //check if FCS was readout for this event
  if(trg){
      unsigned short detmask=trg->getTrgDetMask();
      printf("TrgDetMask = %4x\n",detmask);
      if(! ((detmask >> 30) & 0x1)){   //FCS_ID=30 but detmask is 16bit:O
	  printf("No FCS readout for this event detmask=%x\n",detmask);
	  //return kStOK;
      }
      unsigned short lastdsm4 = trg->lastDSM(4);
      unsigned short fcs2019 = (lastdsm4 >> 10) & 0x1;
      printf("fcs2019=%1d\n",fcs2019);
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
  
  static int nevt=0;
  int nfcsdata=0;
  int nh[kFcsNDet]; memset(nh,0,sizeof(nh));
  int sum[kFcsNDet][kFcsEcalMaxId]; memset(sum,0,sizeof(sum));
  float etot[kFcsNDet]; memset(etot,0,sizeof(etot));

  for(int det=0; det<kFcsNDet+1; det++){  //det==kFcsDet is for empty channel
    int nhit=mFcsCollection->numberOfHits(det);
    printf("StFcsQaMaker found %d hits for det=%d in event#=%d\n",nhit,det,nevt);
    if(nhit<=0) continue;       
    StSPtrVecFcsHit& hits = mFcsCollection->hits(det); 
    for (int i=0; i<nhit; i++){
      nfcsdata++;          
      int id  = hits[i]->id();
      int ehp = hits[i]->ehp();
      int ns  = hits[i]->ns();
      int dep = hits[i]->dep();
      int ch  = hits[i]->channel();
      int depch=mFcsDbMkr->getDepCh(dep,ch);
      int ntb = hits[i]->nTimeBin();
      float ped=0.0;
      if(mPedSub>0)  ped=mFcsDbMkr->pedestal(ehp,ns,dep,ch);
      for(int j=0; j<ntb; j++){
	unsigned short adc = hits[i]->adc(j);
	unsigned short tb  = hits[i]->timebin(j);
	if(det<kFcsNDet){	//connected channel
	  //printf("%1d %1d %2d %2d %4d %f %f\n",ehp,ns,dep,ch,adc,ped,adc-ped);
	  mAdcTb2[det]->Fill(id,tb,adc);
	  mAdcTb[det][id]->Fill(tb,adc);
	  if(tb>=mMinTB && tb<=mMaxTB) sum[det][id] += (adc - ped);
	  mAdcId[det]->Fill(float(depch),float(adc));
	}else{ // empty channel
	  if(ch<32){
	    int ns2 = hits[i]->ns();
	    int ehp2= hits[i]->ehp();
	    int det2=mFcsDbMkr->detectorId(ehp2,ns2);
	    if(det2>=0 && det2<kFcsNDet)
	      mAdcId[det2]->Fill(float(depch),float(adc));
	  }
	}
      }
    }
    if(det<kFcsNDet){
      int maxid = mFcsDbMkr->maxId(det);
      for(int id=0; id<maxid; id++){
	if(sum[det][id]>0){
	  mAdcSumId[det]->Fill((float)id, float(sum[det][id]));
	  nh[det]++;
	  etot[det]+=sum[det][id];
	}
      }
      mNHit[det]->Fill(float(nh[det]));
      
      if(mDump>0){
	int oldid=-1;
	for (int i=0; i<nhit; i++){
	  int id  = hits[i]->id();
	  int ehp = hits[i]->ehp();
	  int ns  = hits[i]->ns();
	  int dep = hits[i]->dep();
	  int ch  = hits[i]->channel();
	  int ntb = hits[i]->nTimeBin();
	  float ped = mFcsDbMkr->pedestal(ehp,ns,dep,ch);
	  char name[22];
	  mFcsDbMkr->getName(det,id,name);
	  if(mDump==1 && sum[det][id]>250){
	    printf("\nFCSDump %5d %22s %5.1f %5d ",nevt,name,ped,sum[det][id]);
	    for(int j=0; j<ntb; j++) printf("%4d ",hits[i]->adc(j));
	    //	  }else if(mDump==2 && det==1 && id==50){
	    //	  }else if(mDump==2 && det==1 && (id==32 || id==18)){
	  }else if(mDump==2 && det==1 && id==12){
	    printf("\nFCSDump %5d %22s %5.1f %5d ",nevt,name,ped,sum[det][id]);
	    int max=0, min=4000;
	    for(int j=0; j<ntb; j++){
	      unsigned int adc=hits[i]->adc(j);
	      unsigned int tb =hits[i]->timebin(j);
	      if(adc>max) max=adc;
	      if(adc<min) min=adc;
	      printf("%4d ",adc);	    		    
	    }
	    if(max-min>6) printf(" !!! min=%d max=%d diff=%d",min,max,max-min);
	  }
	}
	printf("\n");
      }
    }
  }

  //for Ecal/Hcal only for clusters
  for(int det=0; det<kFcsNDet; det++){        
    if(det<=kFcsHcalSouthDetId){ 
      int nclu=mFcsCollection->numberOfClusters(det);
      printf("StFcsQaMaker found %d cluster for det=%d in event#=%d\n",nclu,det,nevt);
      if(nclu<=0) continue;       
      StSPtrVecFcsCluster& clusters = mFcsCollection->clusters(det); 
      mNClu[det]->Fill(nclu);
      for (int i=0; i<nclu; i++){
	StFcsCluster* cluster=clusters[i];
	int ntow=cluster->nTowers();
	int nnei=cluster->nNeighbor();
	mNTowClu[det]->Fill(ntow);
	mNNeiClu[det]->Fill(nnei);
	StPtrVecFcsHit& hits = cluster->hits();	
	int htid = hits[0]->id();
	mNTowEClu[det][htid]->Fill(ntow,cluster->energy());
	if(nnei==0) {
	  mNTowECluIso[det][htid]->Fill(ntow,cluster->energy());
	  if(etot[3]>100){
	    mNTowECluIsoH[det][htid]->Fill(ntow,cluster->energy());
	  }
	}
      }
    }    
  }

  mDataSize->Fill(log10(nfcsdata));
  nevt++;
  return kStOK;
};

Int_t StFcsQaMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsQaMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsQaMaker);

/*
 * $Id: StFcsQaMaker.cxx,v 1.5 2019/07/19 15:16:29 akio Exp $
 * $Log: StFcsQaMaker.cxx,v $
 * Revision 1.5  2019/07/19 15:16:29  akio
 * add tofmult
 *
 * Revision 1.4  2019/07/10 03:18:36  akio
 * fix fcs_id to 30
 *
 * Revision 1.3  2019/07/10 03:10:21  akio
 * added run19 fcs trigger bit
 *
 * Revision 1.2  2019/06/21 17:44:46  akio
 * added cluster plots
 *
 * Revision 1.1  2019/06/07 19:06:44  akio
 * *** empty log message ***
 *
 * Revision 1.6  2017/02/25 04:43:08  akio
 * added checking for trgDetMask for sparse readout (skipping event when FCS was NOT readout)
 *
 * Revision 1.5  2017/02/20 19:18:53  akio
 * added check if we get StFcsRawDawReader
 *
 * Revision 1.4  2017/02/18 18:28:21  akio
 * adding RCC-TCU check
 *
 * Revision 1.3  2015/05/30 16:08:00  akio
 * *** empty log message ***
 *
 * Revision 1.2  2015/02/28 02:55:35  akio
 * fix a bug
 *
 * Revision 1.1  2015/02/25 20:03:26  akio
 * new fcs qa maker
 *
 */

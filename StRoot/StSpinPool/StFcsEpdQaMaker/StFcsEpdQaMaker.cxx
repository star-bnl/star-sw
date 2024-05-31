/*
 *
 * \class StFcsEpdQaMaker
 *
 */

#include "StFcsEpdQaMaker.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StEpdCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StEpdHit.h"
#include "StRoot/StFcsDbMaker/StFcsDb.h"
#include "StRoot/StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"
#include "TCanvas.h"

#include <string.h>
#include <time.h>

StFcsEpdQaMaker::StFcsEpdQaMaker(const Char_t* name) : StMaker(name) {
  sprintf(mFilename,"0.epdqa.root");
}

StFcsEpdQaMaker::~StFcsEpdQaMaker(){};

Int_t StFcsEpdQaMaker::Init(){
  mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
  if(!mFcsDb){
    LOG_FATAL << "Error finding StFcsDb"<< endm;
    return kStFatal;
  }
  
  if(mFilename[0]=='0' && mRun>0){
      int yday=mRun/1000;
      sprintf(mFilename,"%d/%d.epdqa.root",yday,mRun);
      printf("StFcsEpdQaMaker::Init - Opening %s\n",mFilename);
  }
  mFile=new TFile(mFilename,"RECREATE");
  
  char t[100], n[100];
  char *cNS[2]={"N","S"};
  for(int det=kFcsPresNorthDetId; det<=kFcsPresSouthDetId; det++){
      for(int id=0; id<kFcsPresMaxId; id++){
	  char name[100];
	  int ns= mFcsDb->northSouth(det);
	  mFcsDb->getName(det,id,name);	  
	  sprintf(t,"EPDADC_%1s%03d",cNS[ns],id);
	  sprintf(n,"%s; QTADC; DEP Fit Integral",name);
	  mQtDepA[ns][id] = new TH2F(t,n,64,0,1024,64,0,1024*4);
	  sprintf(t,"EPDTAC_%1s%03d",cNS[ns],id);
	  sprintf(n,"%s; QTTAC; DEP Fit Peak Timebin",name);
	  mQtDepT[ns][id] = new TH2F(t,n,50,0,3000,50,45,56);
          sprintf(t,"EPDRatio_%1s%03d",cNS[ns],id);
          sprintf(n,"%s; DEP Peak Timebin; QTADC/DEPIntg",name);
	  mQtDepR[ns][id] = new TH2F(t,n,50,44,57,50,0.0,0.8);
      }
  }
  mQtDepA[0][kFcsPresMaxId] = new TH2F("EPDADCc","EPDADC QTc; QTc ADC; DEP Fit Integral",64,0,1024,64,0,1024*4);
  mQtDepT[0][kFcsPresMaxId] = new TH2F("EPDTACc","EPDTAC QTc; QTc TAC; DEP Fit Peak Timebin",50,0,3000,50,45,56);
  mQtDepR[0][kFcsPresMaxId] = new TH2F("EPDRatioc","EPDRatio QTc; DEP Peak Timebin; QTcADC/DEPIntg",50,44,57,50,0.0,0.8);
  mQtDepA[1][kFcsPresMaxId] = new TH2F("EPDADCbmqtad","EPDADC QTb; QTb ADC; DEP Fit Integral",64,0,1024,64,0,1024*4);
  mQtDepT[1][kFcsPresMaxId] = new TH2F("EPDTACb","EPDTAC QTb; QTb TAC; DEP Fit Peak Timebin",50,0,3000,50,45,56);
  mQtDepR[1][kFcsPresMaxId] = new TH2F("EPDRatiob","EPDRatio QTb; DEP Peak Timebin; QTbADC/DEPIntg",50,44,57,50,0.0,0.8);
  return kStOK;
};

Int_t StFcsEpdQaMaker::Make(){
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
  int tofmult = 0;
  //check if FCS was readout for this event
  if(trg){
      tofmult = trg->tofMultiplicity(); 
      //unsigned short detmask=trg->getTrgDetMask();
      //printf("TrgDetMask = %4x\n",detmask);
      //if(! ((detmask >> 30) & 0x1)){   //FCS_ID=30 but detmask is 16bit:O
      //printf("No FCS readout for this event detmask=%x\n",detmask);
	  //return kStOK;
      //}
      //unsigned short lastdsm4 = trg->lastDSM(4);
      //unsigned short fcs2019 = (lastdsm4 >> 10) & 0x1;
      //printf("fcs2019=%1d\n",fcs2019);
      unsigned short lastdsm2 = trg->lastDSM(2);
      unsigned short lastdsm5 = trg->lastDSM(5);
      printf("lastdsm2=%04x lastdsm5=%04x tofmult=%d\n",lastdsm2,lastdsm5,tofmult);
  }
  
  if(!event) { 
      LOG_INFO << "No StEvent found" << endm;
  }else{ 
      mFcsCollection=event->fcsCollection();
      mEpdCollection=event->epdCollection();
  } 
  if(!mFcsCollection){
    LOG_INFO << "No StFcsCollection found" << endm;
    return kStErr;
  }
  if(!mEpdCollection){
    LOG_INFO << "No StEpdCollection found" << endm;
    return kStErr;
  }
  
  for(int det=kFcsPresNorthDetId; det<kFcsNDet; det++){  
    int nhit=mFcsCollection->numberOfHits(det);
    printf("StFcsEpdQaMaker found %d hits for det=%d\n",nhit,det);
    if(nhit<=0) continue;       
    StSPtrVecFcsHit& hits = mFcsCollection->hits(det); 
    for (int i=0; i<nhit; i++){
      int id  = hits[i]->id();
      //int ehp = hits[i]->ehp();
      int ns  = hits[i]->ns();
      int dep = hits[i]->dep();
      int ch  = hits[i]->channel();
      //int ntb = hits[i]->nTimeBin();
      int pp,tt;
      mFcsDb->getEPDfromId(det,id,pp,tt);
      int QTcQRb = tt<=9?0:1;

      //from fits
      float fititeg=0;
      float fitpeak=0;
      fititeg = hits[i]->adcSum();
      fitpeak = hits[i]->fitPeak();      
      //      printf("Dep=%02d Ch=%02d PP=%02d TT=%02d   DEP=%6d PEAK=%f",
      //	     dep,ch,pp,tt,fititeg,fitpeak);
      
      //get EPD data from epd collection      
      int nepd = mEpdCollection->epdHits().size();
      int adc=0, tac=0;
      for(int j=0; j<nepd; j++){
	StEpdHit* epd = mEpdCollection->epdHits()[j];
	int ew = epd->side();
	int epp = epd->position();
	int ett = epd->tile();
	if(ew==1 && epp==pp && ett==tt){
	  adc = epd->adc();
	  tac = epd->tac();	
	  break;
	}
      }
      if(Debug()) printf("  Dep=%02d Ch=%02d PP=%02d TT=%02d  QT=%4d DEP=%6.1f  TAC=%4d PEAK=%4.2f\n",
			 dep,ch,pp,tt,adc,fititeg,tac,fitpeak);           

      mQtDepA[ns][id]->Fill(adc,fititeg);
      mQtDepA[QTcQRb][kFcsPresMaxId]->Fill(adc,fititeg);
      if(fititeg>100) {
	mQtDepT[ns][id]->Fill(tac,fitpeak);
	mQtDepT[QTcQRb][kFcsPresMaxId]->Fill(tac,fitpeak);	
	mQtDepR[ns][id]->Fill(fitpeak,float(adc)/fititeg);
	mQtDepR[QTcQRb][kFcsPresMaxId]->Fill(fitpeak,float(adc)/fititeg);
      }
    }
  }

  return kStOK;
};

Int_t StFcsEpdQaMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsEpdQaMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsEpdQaMaker)

/*
 * $Id: StFcsEpdQaMaker.cxx,v 1.1 2021/05/30 21:33:05 akio Exp $
 * $Log: StFcsEpdQaMaker.cxx,v $
 * Revision 1.1  2021/05/30 21:33:05  akio
 * QA for EPD West from DEP and QT comparison
 *
 */

/*
 *
 * \class StFcsTrgQaMaker
 *
 */

#include "StFcsTrgQaMaker.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StEvent/StFcsCluster.h"
#include "StRoot/StFcsDbMaker/StFcsDbMaker.h"
#include "StRoot/StFcsTriggerSimMaker/StFcsTriggerSimMaker.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"

#include <string.h>
#include <time.h>

StFcsTrgQaMaker::StFcsTrgQaMaker(const Char_t* name) : StMaker(name) {};

StFcsTrgQaMaker::~StFcsTrgQaMaker(){};

Int_t StFcsTrgQaMaker::Init(){
  mFcsDbMkr = static_cast< StFcsDbMaker*>(GetMaker("fcsDb"));
  if(!mFcsDbMkr){
    LOG_FATAL << "Error finding StFcsDbMaker"<< endm;
    return kStFatal;
  }
  mFcsTrgSimMkr = static_cast< StFcsTriggerSimMaker*>(GetMaker("FcsTrgSim"));
  if(!mFcsTrgSimMkr){
    LOG_FATAL << "Error finding StFcsTrgSimMaker"<< endm;
    return kStFatal;
  }

  if(mRun>0){
      int yday=mRun/1000;
      sprintf(mFilename,"%d/%d.trgQa.root",yday,mRun);
  }else if(mFilename==0){
      static char* fname = "fcs.trgqa.root";
      mFilename=fname;
  }
  printf("StFcsTrgQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");

  char* NS[22]={"N","S"};
  for(int ns=0; ns<kFcsNorthSouth; ns++){
      mEcal[ns] = new TH2F(Form("Ecal4x4%s",NS[ns]),Form("Ecal4x4%s",NS[ns]),
			   kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow,
			   64,0.0,6.4);
      mHcal[ns] = new TH2F(Form("Hcal4x4%s",NS[ns]),Form("Hcal4x4%s",NS[ns]),
			   kFcsHcal4x4NCol*kFcsHcal4x4NRow,0.0,kFcsHcal4x4NCol*kFcsHcal4x4NRow,
			   64,0.0,6.4);
      mPres[ns] = new TH2F(Form("PresAdc%s",NS[ns]),Form("PresAdc%s",NS[ns]),
			   kFcsPresNCol*kFcsPresNRow,0.0,kFcsPresNCol*kFcsPresNRow,
			   64,0.0,6.4);      
      mEPmap[ns]= new TH2F(Form("EPmap%s",NS[ns]),Form("EPmap%s",NS[ns]),
			   kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow,
			   kFcsPresNCol*kFcsPresNRow,0.0,kFcsPresNCol*kFcsPresNRow);
      mEcalNorm[ns] = new TH1F(Form("NormEcal%s",NS[ns]),Form("NormEcal4x4%s",NS[ns]),
			       kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow);
  }
  mDsmOut = new TH1F("DsmOut","DsmOut",16,0,16);
  return kStOK;
};

Int_t StFcsTrgQaMaker::Make() {
    fcs_trg_base* trg=mFcsTrgSimMkr->getTriggerEmu();
    if(!trg){
	LOG_FATAL << "Error finding fcs_trg_base from StFcsTrgSimMaker"<< endm;
	return kStFatal;
    }
    
    int max=0,maxns,maxc,maxr;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
	for(int c=0; c<kFcsEcal4x4NCol; c++){
	    for(int r=0; r<kFcsEcal4x4NRow; r++){
		if(trg->esum[ns][r][c]>max){  //find max Ecal 4x4
		    max=trg->esum[ns][r][c];
		    maxns=ns;
		    maxr=r;
		    maxc=c;		    
		}
	    }
	}
    }	    
    //Fill only for max ecal 4x4
    if(max>0){
	int id=maxr*kFcsEcal4x4NCol + maxc;
	float pt=max*mPtCh;
	mEcal[maxns]->Fill(id,pt);
	if(pt>mEcalPtThr){
	    mEcalNorm[maxns]->Fill(id);
	    for(int dep=0; dep<6; dep++){
		for(int ch=0; ch<32; ch++){
		    if(trg->phit[maxns][dep][ch]>0){
			int idp=dep*32 + ch;
			mEPmap[maxns]->Fill(id,idp);
		    }
		}
	    }
	}
    }
    //hcal
    for(int ns=0; ns<kFcsNorthSouth; ns++){
	for(int c=0; c<kFcsHcal4x4NCol; c++){
	    for(int r=0; r<kFcsHcal4x4NRow; r++){
		if(trg->hsum[ns][r][c]==0) continue;
		int id=r*kFcsHcal4x4NCol + c;
		mHcal[ns]->Fill(id,trg->hsum[ns][r][c]*0.0316);
	    }
	}
    }
    //Pres ADC
    for(int ns=0; ns<kFcsNorthSouth; ns++){
	for(int dep=0; dep<6; dep++){
	    for(int ch=0; ch<32; ch++){
		if(trg->padc[ns][dep][ch]>0){
		    int idp=dep*32 + ch;
		    mPres[ns]->Fill(idp,trg->padc[ns][dep][ch]/150.0);
		}
	    }
	}	
    }

    //DSM out
    for(int i=0; i<16; i++){
	if( (trg->dsmout >> i) & 0x1) mDsmOut->Fill(i);
    }

    return kStOK;
};

Int_t StFcsTrgQaMaker::Finish(){
  mFile->Write();
  mFile->Close();
  printf("StFcsTrgQaMaker::Finish - Closing %s\n",mFilename);
  return kStOK;
};

ClassImp(StFcsTrgQaMaker);

/*
 * $Id: StFcsTrgQaMaker.cxx,v 1.3 2020/07/24 17:21:31 akio Exp $
 * $Log: StFcsTrgQaMaker.cxx,v $
 * Revision 1.3  2020/07/24 17:21:31  akio
 * adding hist for DsmOut
 *
 * Revision 1.2  2020/06/01 19:34:41  akio
 * adding normarization histo
 *
 * Revision 1.1  2020/05/29 18:59:32  akio
 * Initial version of FCS Trigger QA maker
 *
 *
 */

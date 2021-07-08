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
#include "StRoot/StFcsDbMaker/StFcsDb.h"
#include "StRoot/StSpinPool/StFcsTriggerSimMaker/StFcsTriggerSimMaker.h"

#include "StRoot/RTS/src/TRG_FCS/fcs_trg_base.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TString.h"
#include "TFile.h"

#include <string.h>
#include <time.h>

StFcsTrgQaMaker::StFcsTrgQaMaker(const Char_t* name) : StMaker(name) {};

StFcsTrgQaMaker::~StFcsTrgQaMaker(){};

Int_t StFcsTrgQaMaker::Init(){
  mFcsDb = static_cast< StFcsDb*>(GetDataSet("fcsDb"));
  if(!mFcsDb){
    LOG_FATAL << "Error finding StFcsDb"<< endm;
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

  const char* cNS[2]={"N","S"};
  const char* cName[4]={"All","SIM","DEP","TCU"};
  const char* cJet[6]={"A","B","C","D","E","F"};
  for(int ns=0; ns<kFcsNorthSouth; ns++){
    for(int i=0; i<4; i++){
      mETot[ns][i] = new TH1F(Form("ETot%s%s",cNS[ns],cName[i]), Form("ETot%s%s",cNS[ns],cName[i]),256,0,512);
      mHTot[ns][i] = new TH1F(Form("HTot%s%s",cNS[ns],cName[i]), Form("HTot%s%s",cNS[ns],cName[i]),256,0,512);

      mEHT [ns][i] = new TH1F(Form("EHT%s%s",cNS[ns],cName[i]), Form("EHT%s%s",cNS[ns],cName[i]),256,0,256);
      mHHT [ns][i] = new TH1F(Form("HHT%s%s",cNS[ns],cName[i]), Form("HHT%s%s",cNS[ns],cName[i]),256,0,256);

      for(int j=0; j<6; j++){
	mJP[ns][j][i] = new TH1F(Form("JP%s%s%s",cJet[j],cNS[ns],cName[i]), Form("JP%s%s%s",cJet[j],cNS[ns],cName[i]),256,0,256);
      }
   
      mPOR [ns][i] = new TH1F(Form("POR%s%s", cNS[ns],cName[i]), Form("POR%s%s", cNS[ns],cName[i]),256,0,512);
      mE4b4[ns][i] = new TH1F(Form("E4b4%s%s",cNS[ns],cName[i]), Form("E4b4%s%s",cNS[ns],cName[i]),256,0,256);
      mH4b4[ns][i] = new TH1F(Form("H4b4%s%s",cNS[ns],cName[i]), Form("H4b4%s%s",cNS[ns],cName[i]),256,0,256);

      mSum[ns][0][i] = new TH1F(Form("Sum%s%s", cNS[ns],cName[i]), Form("E+H%s%s",     cNS[ns],cName[i]),256,0,256);
      mEHR[ns][0][i] = new TH1F(Form("EHR%s%s", cNS[ns],cName[i]), Form("EHRatio%s%s", cNS[ns],cName[i]), 64,0,1.05);
      mSum[ns][1][i] = new TH1F(Form("Had%s%s", cNS[ns],cName[i]), Form("E+H%s%s",     cNS[ns],cName[i]),256,0,256);
      mEHR[ns][1][i] = new TH1F(Form("RHad%s%s",cNS[ns],cName[i]), Form("EHRatio%s%s", cNS[ns],cName[i]), 64,0,1.05);
      mSum[ns][2][i] = new TH1F(Form("EM%s%s",  cNS[ns],cName[i]), Form("E+H%s%s",     cNS[ns],cName[i]),256,0,256);
      mEHR[ns][2][i] = new TH1F(Form("REM%s%s", cNS[ns],cName[i]), Form("EHRatio%s%s", cNS[ns],cName[i]), 64,0,1.05);
      mSum[ns][3][i] = new TH1F(Form("Gam%s%s", cNS[ns],cName[i]), Form("E+H%s%s",     cNS[ns],cName[i]),256,0,256);
      mEHR[ns][3][i] = new TH1F(Form("RGam%s%s",cNS[ns],cName[i]), Form("EHRatio%s%s", cNS[ns],cName[i]), 64,0,1.05);
      mSum[ns][4][i] = new TH1F(Form("Ele%s%s", cNS[ns],cName[i]), Form("E+H%s%s",     cNS[ns],cName[i]),256,0,256);
      mEHR[ns][4][i] = new TH1F(Form("REle%s%s",cNS[ns],cName[i]), Form("EHRatio%s%s", cNS[ns],cName[i]), 64,0,1.05);
      
      mDEm [ns][i] = new TH2F(Form("DEm%s%s", cNS[ns],cName[i]),Form("DiEM%s%s",      cNS[ns],cName[i]),256,0,256,256,0,256);
      mDHad[ns][i] = new TH2F(Form("DHad%s%s",cNS[ns],cName[i]),Form("DiHadron%s%s",  cNS[ns],cName[i]),256,0,256,256,0,256);
      mDGam[ns][i] = new TH2F(Form("DGam%s%s",cNS[ns],cName[i]),Form("DiGamma%s%s",   cNS[ns],cName[i]),256,0,256,256,0,256);
      mDEle[ns][i] = new TH2F(Form("DEle%s%s",cNS[ns],cName[i]),Form("DiElectron%s%s",cNS[ns],cName[i]),256,0,256,256,0,256);
      mDJP [ns][i] = new TH2F(Form("DJP%s%s", cNS[ns],cName[i]),Form("DiJP%s%s",      cNS[ns],cName[i]),256,0,256,256,0,256);
    }
    mSumTot[ns][0] = new TH2F(Form("SumEtot%s",cNS[ns]),Form("SumEtot%s;E4x4+H4x4;Etot",cNS[ns]),256,0,256,256,0,1024);
    mSumTot[ns][1] = new TH2F(Form("SumHtot%s",cNS[ns]),Form("SumHtot%s;E4x4+H4x4;Htot",cNS[ns]),256,0,256,256,0,512);

    mEcal[ns] = new TH2F(Form("Ecal4x4%s",cNS[ns]),Form("Ecal4x4%s",cNS[ns]),
			 kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow,
			 64,0.0,6.4);
    mHcal[ns] = new TH2F(Form("Hcal4x4%s",cNS[ns]),Form("Hcal4x4%s",cNS[ns]),
			 kFcsHcal4x4NCol*kFcsHcal4x4NRow,0.0,kFcsHcal4x4NCol*kFcsHcal4x4NRow,
			 64,0.0,6.4);
    mPres[ns] = new TH2F(Form("PresAdc%s",cNS[ns]),Form("PresAdc%s",cNS[ns]),
			 kFcsPresNCol*kFcsPresNRow,0.0,kFcsPresNCol*kFcsPresNRow,
			 64,0.0,6.4);      
    mEPmap[ns]= new TH2F(Form("EPmap%s",cNS[ns]),Form("EPmap%s",cNS[ns]),
			 kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow,
			 kFcsPresNCol*kFcsPresNRow,0.0,kFcsPresNCol*kFcsPresNRow);
    mEcalNorm[ns] = new TH1F(Form("NormEcal%s",cNS[ns]),Form("NormEcal4x4%s",cNS[ns]),
			     kFcsEcal4x4NCol*kFcsEcal4x4NRow,0.0,kFcsEcal4x4NCol*kFcsEcal4x4NRow);
  }

  mDsmOut = new TH1F("DsmOut","DsmOut",16,0,16);
  mDepOut = new TH1F("DepOut","DepOut",16,0,16);
  mTcuBit = new TH1F("TcuBit","TcuBit",16,0,16);
  mTcuDep = new TH1F("TcuDep","TcuDepMismatch",16,0,16);
  mSimDep = new TH1F("SimDep","SimDepMismatch",16,0,16);

  return kStOK;
};

Int_t StFcsTrgQaMaker::Make() {
    fcs_trg_base* trg=mFcsTrgSimMkr->getTriggerEmu();
    if(!trg){
	LOG_FATAL << "Error finding fcs_trg_base from StFcsTrgSimMaker"<< endm;
	return kStFatal;
    }
    
    //Getting Trigger Data
    TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
    StTriggerData* trgd=0;
    if(os){
      trgd = (StTriggerData*)os->GetObject();
      if(trgd){
	//LOG_INFO << "got StTriggerData addr="<<trgd<<endm;
      }else{
	LOG_ERROR << "could not get StTriggerData from DataSet."<<endm;
	//return kStErr;
      }
    }else{
      LOG_ERROR << "could not get StTriggerData DataSet."<<endm;
      //return kStErr;
    }    
    unsigned short tcubit = 0;
    if(trgd) tcubit = trgd->lastDSM(5);

    //TCU bits
    for(int i=0; i<16; i++) if((tcubit >> i) & 0x1) mTcuBit->Fill(i);

    //DSM out
    int dsmout=trg->dsmout;
    for(int i=0; i<16; i++) if((dsmout >> i) & 0x1) mDsmOut->Fill(i);

    //Getting StEvent and FcsCollection
    StEvent* event= (StEvent*)GetInputDS("StEvent");
    StFcsCollection *fcs = 0;
    if(!event) {
      LOG_INFO << "No StEvent found" << endm;
    }else{
      fcs=event->fcsCollection();
    }
    if(!fcs){
      LOG_INFO << "No StFcsCollection found" << endm;
      return kStErr;
    }

    //DEP out
    int depout=0;
    int det=6; //access DEPIO   
    int nhit=fcs->numberOfHits(det);
    StSPtrVecFcsHit& hits = fcs->hits(det); 
    //printf("DEP TRG data n = %d\n",nhit);
    for (int i=0; i<nhit; i++){
      int ehp = hits[i]->ehp(); 
      int ns  = hits[i]->ns();
      int dep = hits[i]->dep();
      int ch  = hits[i]->channel();
      //printf("DEP TRG data = %1d %1d %02d %02d\n",ehp,ns,dep,ch);      
      if(ehp!=3) continue;
      if(ns!=0) continue;
      if(dep!=0) continue;
      if(ch==4 || ch==5){
	int ntb = hits[i]->nTimeBin();
	for(int j=0; j<ntb; j++){
	  int            adc = hits[i]->adc(j);
	  unsigned short tb  = hits[i]->timebin(j);
	  //if(tb>=96-8 && tb<=96+8) 
	  //if(tb>=96-8){ 
	  if(0){
	    printf("DEPIO data = d%1d ns%1d dep%2d ch%2d tb%3d  0x%x",ehp,ns,dep,ch,tb,adc);
	    if(ch==5 && (adc&0x1)) printf(" ELE2");
	    printf("\n");;
	  }
	  if(tb!=mS3off) continue;
	  if(ch==4) depout += adc;
	  if(ch==5) depout += (adc << 8);
	}
      }      
    }
    for(int i=0; i<16; i++) if((depout >> i) & 0x1) mDepOut->Fill(i);

    //Mismatch
    for(int i=0; i<16; i++) if((depout >> i) != (tcubit >> i)) mTcuDep->Fill(i);
    for(int i=0; i<16; i++) if((depout >> i) != (dsmout >> i)) mSimDep->Fill(i);

    LOG_INFO << Form("FCS SIM/DEP/TCU Bits = %04x  %04x  %04x",dsmout,depout,tcubit)<<endm;

    unsigned int max;
    int maxns,maxc,maxr,maxj;    

    //Ecal HT 2x2 
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int c=0; c<kFcsEcal4x4NCol+1; c++){
	for(int r=0; r<kFcsEcal4x4NRow+1; r++){
	  if(trg->e2x2[ns][r][c]>max){  //find max Ecal 2x2
	    max=trg->e2x2[ns][r][c]; maxns=ns; maxr=r; maxc=c;		    
	  }
	}
      }
    }	    
    if(max>0){
      mEHT[maxns][0]->Fill(max);
      if((dsmout>>0)&0x1) mEHT[maxns][1]->Fill(max);
      if((depout>>0)&0x1) mEHT[maxns][2]->Fill(max);
      if((tcubit>>0)&0x1) mEHT[maxns][3]->Fill(max);
    }

    //Hcal HT 2x2 
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int c=0; c<kFcsHcal4x4NCol+1; c++){
	for(int r=0; r<kFcsHcal4x4NRow+1; r++){
	  if(trg->h2x2[ns][r][c]>max){  //find max Hcal 2x2
	    max=trg->h2x2[ns][r][c]; maxns=ns; maxr=r; maxc=c;		    
	  }
	}
      }
    }
    if(max>0){
      mHHT[maxns][0]->Fill(max);
      if((dsmout>>1)&0x1) mHHT[maxns][1]->Fill(max);
      if((depout>>1)&0x1) mHHT[maxns][2]->Fill(max);
      if((tcubit>>1)&0x1) mHHT[maxns][3]->Fill(max);
    }

    //Ecal Tot
    max=trg->etot[0]; maxns=0;
    if(trg->etot[1] > max) {max=trg->etot[1]; maxns=1;}
    mETot[0][0]->Fill(trg->etot[0]);
    mETot[1][0]->Fill(trg->etot[1]);
    if(max>0){
      if((dsmout>>2)&0x1) mETot[maxns][1]->Fill(max);
      if((depout>>2)&0x1) mETot[maxns][2]->Fill(max);
      if((tcubit>>2)&0x1) mETot[maxns][3]->Fill(max);
    }

    //Hcal Tot
    max=trg->htot[0]; maxns=0;
    if(trg->htot[1] > max) {max=trg->htot[1]; maxns=1;}
    mHTot[0][0]->Fill(trg->htot[0]);
    mHTot[1][0]->Fill(trg->htot[1]);
    if(max>0){
      if((dsmout>>3)&0x1) mHTot[maxns][1]->Fill(max);
      if((depout>>3)&0x1) mHTot[maxns][2]->Fill(max);
      if((tcubit>>3)&0x1) mHTot[maxns][3]->Fill(max);
    }

    //JP
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      printf("StFcsTrgQaMaker JP  ns=%d : ",ns);
      for(int j=0; j<6; j++){
	if(trg->jet[ns][j]>max){  //find max JP
	  max=trg->jet[ns][j]; maxns=ns; maxj=j;		    
	}
	mJP[ns][j][0]->Fill(trg->jet[ns][j]);
	printf("%3d ",trg->jet[ns][j]);
      }
      printf("\n");
    }	    
    if(max>0){
      if((dsmout>>4)&0x1) mJP[maxns][maxj][1]->Fill(max);
      if((depout>>4)&0x1) mJP[maxns][maxj][2]->Fill(max);
      if((tcubit>>4)&0x1) mJP[maxns][maxj][3]->Fill(max);
      //mJP[1][0]->Fill(max);
      //if((dsmout>>9)&0x1) mJP[1][1]->Fill(max);
      //if((depout>>9)&0x1) mJP[1][2]->Fill(max);
      //if((tcubit>>9)&0x1) mJP[1][3]->Fill(max);
    }

    //ecal 4x4
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int c=0; c<kFcsEcal4x4NCol; c++){
	for(int r=0; r<kFcsEcal4x4NRow; r++){
	  if(trg->esum[ns][r][c]>max){  //find max Ecal 4x4
	    max=trg->esum[ns][r][c]; maxns=ns; maxr=r; maxc=c;		    
	  }
	}
      }
    }	    
    if(max>0){
      mE4b4[maxns][0]->Fill(max);
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
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int c=0; c<kFcsHcal4x4NCol; c++){
	for(int r=0; r<kFcsHcal4x4NRow; r++){
	  //if(trg->hsum[ns][r][c]==0) continue;
	  int id=r*kFcsHcal4x4NCol + c;
	  mHcal[ns]->Fill(id,trg->hsum[ns][r][c]*0.0316);
	  if(trg->hsum[ns][r][c]>max){  //find max Ecal 4x4
	    max=trg->esum[ns][r][c]; maxns=ns; maxr=r; maxc=c;		    
	  }
	}
      }
    }
    if(max>0){
      mH4b4[maxns][0]->Fill(max);
    }

    //Pres ADC 
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int dep=0; dep<6; dep++){
	for(int ch=0; ch<32; ch++){
	  if(trg->padc[ns][dep][ch]>0){
	    int idp=dep*32 + ch;
	    mPres[ns]->Fill(idp,trg->padc[ns][dep][ch]/150.0);
	  }
	  if(trg->padc[ns][dep][ch]>max){  //find max Ecal 4x4
	    max=trg->padc[ns][dep][ch]; maxns=ns; maxr=dep; maxc=ch;		    
	  }
	}
      }	
    }
    if(max>0){
      mPOR[maxns][0]->Fill(max);
    }
    
    //ecal+hcal 4x4
    max=0; maxns=0;
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int c=0; c<kFcsEcal4x4NCol; c++){
	for(int r=0; r<kFcsEcal4x4NRow; r++){
	  if(trg->sum[ns][r][c]>max){  //find max Ecal 4x4
	    max=trg->sum[ns][r][c]; maxns=ns; maxr=r; maxc=c;		    
	  }
	}
      }
    }	    
    if(max>0){
      mSum[maxns][0][0]->Fill(max);
      mSumTot[maxns][0]->Fill(max,trg->etot[maxns]);
      mSumTot[maxns][1]->Fill(max,trg->htot[maxns]);
      mEHR[maxns][0][0]->Fill(trg->ratio[maxns][maxr][maxc]);
      if(trg->had[maxns][maxr][maxc]){
	mSum[maxns][1][0]->Fill(max);
	mEHR[maxns][1][0]->Fill(trg->ratio[maxns][maxr][maxc]);
      }
      if(trg->em[maxns][maxr][maxc]){
	mSum[maxns][2][0]->Fill(max);
	mEHR[maxns][2][0]->Fill(trg->ratio[maxns][maxr][maxc]); 
	if(trg->epdcoin[maxns][maxr][maxc]==0){
	  mSum[maxns][3][0]->Fill(max);
	  mEHR[maxns][3][0]->Fill(trg->ratio[maxns][maxr][maxc]);
	}
	if(trg->epdcoin[maxns][maxr][maxc]==1){
	  mSum[maxns][4][0]->Fill(max);
	  mEHR[maxns][4][0]->Fill(trg->ratio[maxns][maxr][maxc]);
	}
      }
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
 * $Id: StFcsTrgQaMaker.cxx,v 1.6 2021/05/30 21:41:58 akio Exp $
 * $Log: StFcsTrgQaMaker.cxx,v $
 * Revision 1.6  2021/05/30 21:41:58  akio
 * A lots of update for trigger comissionong Run21 OO200
 *
 * Revision 1.5  2021/03/30 13:36:51  akio
 * change to StSpinPoll for include
 *
 * Revision 1.4  2021/03/30 13:31:27  akio
 * StFcsDbMAker->StFcsDB
 *
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

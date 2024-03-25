
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
  fcs_trg_base* trg=mFcsTrgSimMkr->getTriggerEmu();
  if(!trg){
    LOG_FATAL << "Error finding fcs_trg_base from StFcsTrgSimMaker"<< endm;
    return kStFatal;
  }

  if(mRun>0){
      int yday=mRun/1000;
      sprintf(mFilename,"%d/%d.trgQa.root",yday,mRun);
  }else if(mFilename==0){
    char* fname = "fcs.trgqa.root";
    mFilename=fname;
  }
  printf("StFcsTrgQaMaker::Init - Opening %s\n",mFilename);
  mFile=new TFile(mFilename,"RECREATE");

  const char* NS[2]={"N","S"};
  const char* JP[5]={"A","B","C","D","E"};  

  mTow[0] = new TH2F("ETow","Ecal; +-col; -row",22*2+1,-22.5,22.5,34,-34.5,-0.5);
  mTow[1] = new TH2F("HTow","Hcal; +-col; -row",13*2+1,-13.5,13.5,20,-20.5,-0.5);

  mETot[0] = new TH1F("ETotN", "ETotN", 256,0,512);
  mETot[1] = new TH1F("ETotS", "ETotS", 256,0,512);
  mHTot[0] = new TH1F("HTotN", "HTotN", 256,0,256);
  mHTot[1] = new TH1F("HTotS", "HTotS", 256,0,256);

  mEHT[0] = new TH1F("EHTN"  ,"MAX EHT",    256,0,256);
  mEHT[1] = new TH1F("EHTS"  ,"MAX EHT",    256,0,256);
  mEHTMap = new TH2F("EHTMap","MAX E2x2Map",21,-10.5,10.5,16,-16.5,-0.5);
  mHHT[0] = new TH1F("HHTN"  ,"MAX HHT",    256,0,256);
  mHHT[1] = new TH1F("HHTS"  ,"MAX HHT",    256,0,256);
  mHHTMap = new TH2F("HHTMap","MAX H2x2Map",13,-6.5,6.5,10,-10.5,-0.5);
		     
  for(int ns=0; ns<2; ns++){
    for(int j=0; j<5; j++){
      mJP[ns][j] = new TH1F(Form("JP%1s_%1s",JP[j],NS[ns]),
			    Form("JP%1s_%1s",JP[j],NS[ns]),256,0,512); 
    }
  }
  mJPMap[0] = new TH1F("JP2",Form("JP2 THR A=%d,BC=%d,DE=%d",trg->JPATHR2,trg->JPBCTHR2,trg->JPDETHR2),11,-5.5,5.5);
  mJPMap[1] = new TH1F("JP1",Form("JP1 THR A=%d,BC=%d,DE=%d",trg->JPATHR1,trg->JPBCTHR1,trg->JPDETHR1),11,-5.5,5.5);
  mJPMap[2] = new TH1F("JP0",Form("JP0 THR A=%d,BC=%d,DE=%d",trg->JPATHR0,trg->JPBCTHR0,trg->JPDETHR0),11,-5.5,5.5);

  mE4x4      = new TH1F("E4x4",    "MAX E4x4",    256,0,256);
  mEM4x4     = new TH1F("EM4x4",   "MAX EM4x4",   256,0,256);
  mELE4x4    = new TH1F("ELE4x4",  "MAX ELE4x4",  256,0,256);
  mERatio    = new TH1F("EMRatio", "MAX EM Ratio=E/(E+Hmax)",  64,0,1.05);  
  mELERatio  = new TH1F("ELERatio","MAX ELE Ratio=E/(E+Hmax)", 64,0,1.05);  

  mH4x4      = new TH1F("H4x4",    "MAX H4x4",   256,0,256);
  mEH4x4     = new TH1F("EH4x4",   "MAX E+H4x4", 256,0,256);
  mHAD4x4    = new TH1F("HAD4x4",  "MAX Had4x4", 256,0,256);
  mHRatio    = new TH1F("HRatio",  "MAX E+H Ratio=E/(E+H)",  64,0,1.05);
  mHADRatio  = new TH1F("HADRatio","MAX Had Ratio=E/(E+H)",  64,0,1.05);

  mE4x4Map[0]   = new TH2F("EM4x4MapEM2",  Form("EM EM2=%d",trg->EMTHR2),  19,-9.5,9.5,15,-15.5,-0.5);
  mE4x4Map[1]   = new TH2F("EM4x4MapEM1",  Form("EM EM1=%d",trg->EMTHR1),  19,-9.5,9.5,15,-15.5,-0.5); 
  mE4x4Map[2]   = new TH2F("EM4x4MapEM0",  Form("EM EM0=%d",trg->EMTHR0),  19,-9.5,9.5,15,-15.5,-0.5);  
  mE4x4Map[3]   = new TH2F("EM4x4MapELE2", Form("EM ELE2=%d",trg->ELETHR2),19,-9.5,9.5,15,-15.5,-0.5);  

  mELE4x4Map[0] = new TH2F("ELE4x4MapELE2",Form("ELE ELE2=%d",trg->ELETHR2),  19,-9.5,9.5,15,-15.5,-0.5);  
  mELE4x4Map[1] = new TH2F("ELE4x4MapELE1",Form("ELE ELE1=%d",trg->ELETHR1),  19,-9.5,9.5,15,-15.5,-0.5);  

  mEH4x4Map[0]  = new TH2F("EHMapHAD2",   Form("E+H HAD2=%d",trg->HADTHR2),    19,-9.5,9.5,15,-15.5,-0.5);  
  mEH4x4Map[1]  = new TH2F("EHMapHAD1",   Form("E+H HAD1=%d",trg->HADTHR1),    19,-9.5,9.5,15,-15.5,-0.5);  
  mEH4x4Map[2]  = new TH2F("EHMapHAD0",   Form("E+H HAD0=%d",trg->HADTHR0),    19,-9.5,9.5,15,-15.5,-0.5);   

  mDsmOut = new TH1F("DsmOut","DsmOut",33,0,33);
  mDepOut = new TH1F("DepOut","DepOut",33,0,33);
  mTcuBit = new TH1F("TcuBit","TcuBit",33,0,33);
  mTcuDep = new TH1F("TcuDep","TcuDepMismatch",33,0,33);
  mSimDep = new TH1F("SimDep","SimDepMismatch",33,0,33);

  const float maxadc=500;
  mAdc[0]=new TH2F("EN_Id_Adc","EcalNorth; Id; ADC",mFcsDb->maxId(0),0.0,mFcsDb->maxId(0),500,0.0,maxadc);
  mAdc[1]=new TH2F("ES_Id_Adc","EcalSouth; Id; ADC",mFcsDb->maxId(1),0.0,mFcsDb->maxId(1),500,0.0,maxadc);
  mAdc[2]=new TH2F("HN_Id_Adc","HcalNorth; Id; ADC",mFcsDb->maxId(2),0.0,mFcsDb->maxId(2),500,0.0,maxadc);
  mAdc[3]=new TH2F("HS_Id_Adc","HcalSouth; Id; ADC",mFcsDb->maxId(3),0.0,mFcsDb->maxId(3),500,0.0,maxadc);
  mAdc[4]=new TH2F("PN_Id_Adc","PresNorth; Id; ADC",mFcsDb->maxId(4),0.0,mFcsDb->maxId(4),500,0.0,maxadc);
  mAdc[5]=new TH2F("PS_Id_Adc","PresSouth; Id; ADC",mFcsDb->maxId(5),0.0,mFcsDb->maxId(5),500,0.0,maxadc);
  return kStOK;
};

Int_t StFcsTrgQaMaker::Make() {
    fcs_trg_base* trg=mFcsTrgSimMkr->getTriggerEmu();
    if(!trg){
	LOG_FATAL << "Error finding fcs_trg_base from StFcsTrgSimMaker"<< endm;
	return kStFatal;
    }
    
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
    if(fcs->isDataExist()==0) {
      LOG_INFO << "No FCS data in this event" << endm;
      return kStOk;
    }
    //Getting Trigger Data
    StTriggerData* trgd = event->triggerData();
    if(!trgd){
      TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
      if(os){
	trgd = (StTriggerData*)os->GetObject();
	if(trgd){
	  //LOG_INFO << "got StTriggerData addr="<<trgd<<endm;
	}else{
	  LOG_ERROR << "could not get StTriggerData from DataSet."<<endm;
	  return kStErr;
	}
      }else{
	LOG_ERROR << "could not get StTriggerData DataSet."<<endm;
	return kStErr;
      }    
    }
    unsigned int tcubit = trgd->lastDSM(2) + (trgd->lastDSM(5)<<16);

    //TCU bits
    for(int i=0; i<32; i++) if((tcubit >> i) & 0x1) mTcuBit->Fill(i);

    //DSM out
    unsigned int dsmout=trg->dsmout;
    for(int i=0; i<32; i++) if((dsmout >> i) & 0x1) mDsmOut->Fill(i);
    mDsmOut->Fill(32);

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
      if(ehp!=3) continue; //DEPIO boards only
      if(dep==0 && (ch==4 || ch==5) ){ //stage3 DEP to TCU
	int ntb = hits[i]->nTimeBin();
	for(int j=0; j<ntb; j++){
	  int            adc = hits[i]->adc(j);
	  unsigned short tb  = hits[i]->timebin(j);
	  //printf("DEPIO3 data = ehp%1d ns%1d dep%2d ch%2d tb%3d  0x%x\n",ehp,ns,dep,ch,tb,adc);
	  if(tb!=mS3off) continue;
	  if(ch==4) depout += adc;
	  if(ch==5) depout += (adc << 8);
	}	
      }  
      if(dep==1 && ch==36){ //stage2 north/south DEP to TCU
	int ntb = hits[i]->nTimeBin();
        for(int j=0; j<ntb; j++){
          int            adc = hits[i]->adc(j);
          unsigned short tb  = hits[i]->timebin(j);
	  //printf("DEPIO2 data = ehp%1d ns%1d dep%02d ch%02d tb%03d  0x%x\n",ehp,ns,dep,ch,tb,adc);
          if(tb!=mS2off) continue;
	  if(ns==0) depout += (adc&0xff) << 16;
	  if(ns==1) depout += (adc&0xff) << 24;
	}
      }
    }
    for(int i=0; i<32; i++) if((depout >> i) & 0x1) mDepOut->Fill(i);

    const char* BIT[32]={"HAD0","HAD1","HAD2",
			 "EM0","EM1","EM2",
			 "JP2","JPA1","JPBC1","JPDE1","JPA0","JPBC0","JPDE0",
			 "DiJP","DiJPAsy","DiELEA",
			 "ELE0-N","ELE1-N","ELE2-N","EM3-N","EHT-N","HHT-N","ETOT-N","HTOT-N",
			 "ELE0-S","ELE1-S","ELE2-S","EM3-S","EHT-S","HHT-S","ETOT-S","HTOT-S"};
    if(GetDebug()){
      LOG_INFO << Form("FCSBits SIM = %08x ",dsmout);
      for(int i=0; i<32; i++){ if((dsmout>>i)&1) {LOG_INFO << BIT[i] << " ";} } LOG_INFO<<endm;
      LOG_INFO << Form("FCSBits DEP = %08x ",depout);
      for(int i=0; i<32; i++){ if((depout>>i)&1) {LOG_INFO << BIT[i] << " ";} } LOG_INFO<<endm;
      LOG_INFO << Form("FCSBits TCU = %08x ",tcubit);
      for(int i=0; i<32; i++){ if((tcubit>>i)&1) {LOG_INFO << BIT[i] << " ";} } LOG_INFO<<endm;
    }

    //Mismatch
    LOG_INFO << Form("FCSBitsMM DEP-TCU Mismatch ");
    for(int i=0; i<32; i++) if(((depout>> i)&1) != ((tcubit>>i)&1)) {mTcuDep->Fill(i); LOG_INFO << BIT[i] << " ";}
    LOG_INFO<<endm;
    LOG_INFO << Form("FCSBitsMM SIM-DEP Mismatch ");
    for(int i=0; i<32; i++) if(((dsmout >>i)&1) != ((depout>>i)&1)) {mSimDep->Fill(i); LOG_INFO << BIT[i] << " ";}
    LOG_INFO<<endm;

    //Tower
    for(int det=0; det<4; det++){
      int nhit=fcs->numberOfHits(det);
      StSPtrVecFcsHit& hits = fcs->hits(det); 
      for (int i=0; i<nhit; i++){
	int id  = hits[i]->id();
	int ehp = hits[i]->ehp(); 
	int ns  = hits[i]->ns();
	int c = mFcsDb->getColumnNumber(det,id);
	int r = mFcsDb->getRowNumber(det,id);
        float cc = c * (ns*2-1);
        float rr = - r;	
	mTow[ehp]->Fill(cc,rr,hits[i]->energy());
      }
    }

    unsigned int max;
    int maxns,maxc,maxr;    
    int maxns2,maxc2,maxr2;    
    int maxns3,maxc3,maxr3;    
    int maxns4,maxc4,maxr4;    
    int maxns5,maxc5,maxr5;    
    //Ecal HT 2x2 
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      max=0;
      for(int c=0; c<kFcsEcal4x4NCol+1; c++){
	for(int r=0; r<kFcsEcal4x4NRow+1; r++){
	  if(trg->e2x2[ns][r][c]>max){  //find max Ecal 2x2
	    max=trg->e2x2[ns][r][c]; maxr=r; maxc=c;		    
	  }
	}
      }
      if(max>0){
	mEHT[ns]->Fill(max);
	mEHTMap->Fill((maxc+1)*(ns*2-1),-maxr-1);
      }
    }	    

    //Hcal HT 2x2 
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      max=0;
      for(int c=0; c<kFcsHcal4x4NCol+1; c++){
	for(int r=0; r<kFcsHcal4x4NRow+1; r++){
	  if(trg->h2x2[ns][r][c]>max){  //find max Hcal 2x2
	    max=trg->h2x2[ns][r][c]; maxr=r; maxc=c;		    
	  }
	}
      }
      if(max>0){
	mHHT[ns]->Fill(max);
	mHHTMap->Fill((maxc+1)*(ns*2-1),-maxr-1);
      }
    }	    
    
    //Ecal Tot
    mETot[0]->Fill(trg->etot[0]);
    mETot[1]->Fill(trg->etot[1]);
    
    //Hcal Tot
    mHTot[0]->Fill(trg->htot[0]);
    mHTot[1]->Fill(trg->htot[1]);
    
    //JP
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      for(int j=0; j<5; j++){
	if(trg->jet[ns][j]>max){  //find max JP
	  mJP[ns][j]->Fill(trg->jet[ns][j]);
	  if((trg->d_out.s2[ns].s2_to_s3[0].d[1]>>j)&0x1) mJPMap[0]->Fill((j+1)*(ns*2-1));
	  if((trg->d_out.s2[ns].s2_to_s3[1].d[1]>>j)&0x1) mJPMap[1]->Fill((j+1)*(ns*2-1));
	  if((trg->d_out.s2[ns].s2_to_s3[0].d[2]>>j)&0x1) mJPMap[2]->Fill((j+1)*(ns*2-1));
	}
      }
    }	        

    //Ecal 4x4
    for(int ns=0; ns<kFcsNorthSouth; ns++){
      max=0;
      int emmax=0, elemax=0, summax=0, hadmax=0;
      for(int c=0; c<kFcsEcal4x4NCol; c++){
	for(int r=0; r<kFcsEcal4x4NRow; r++){
	  int esum=trg->esum[ns][r][c];	  
	  int sum=trg->sum[ns][r][c];
	  if(esum>max){  //find max Ecal 4x4
	    max=esum; maxns=ns; maxr=r; maxc=c;		    	    
	  }
	  if(esum>emmax && trg->ratiomax[ns][r][c] > 1.0/(1.0+(float)trg->EM_HERATIO_THR/128)){
	    emmax=esum; maxns2=ns; maxr2=r; maxc2=c;  
	  }
	  if(esum>elemax && trg->ratiomax[ns][r][c] > 1.0/(1.0+(float)trg->EM_HERATIO_THR/128) && trg->epdcoin[ns][r][c]){
	    elemax=esum; maxns3=ns; maxr3=r; maxc3=c;  
	  }
	  if(sum>summax){
	    summax=sum; maxns4=ns; maxr4=r; maxc4=c; 
	  }
	  if(sum>hadmax && trg->ratio[ns][r][c] < 1.0/(1.0+(float)trg->HAD_HERATIO_THR/128)){
	    hadmax=sum; maxns5=ns; maxr5=r; maxc5=c; 
	  }
	  int cc=(c+1)*(ns*2-1);
	  int rr=-r-1;
	  if(esum > trg->EMTHR2)  mE4x4Map[0]->Fill(cc,rr);
	  if(esum > trg->EMTHR1)  mE4x4Map[1]->Fill(cc,rr);
	  if(esum > trg->EMTHR0)  mE4x4Map[2]->Fill(cc,rr);
	  if(esum > trg->ELETHR2) mE4x4Map[3]->Fill(cc,rr);
	  if(trg->epdcoin[ns][r][c]){
	    if(esum > trg->ELETHR2) mELE4x4Map[0]->Fill(cc,rr);
	    if(esum > trg->ELETHR1) mELE4x4Map[1]->Fill(cc,rr);
	  }
	  if(sum > trg->HADTHR2)  mEH4x4Map[0]->Fill(cc,rr);
	  if(sum > trg->HADTHR1)  mEH4x4Map[1]->Fill(cc,rr);
	  if(sum > trg->HADTHR0)  mEH4x4Map[2]->Fill(cc,rr);
	}
      }
      if(max>0)    mE4x4->Fill(max);
      if(emmax>0)  mEM4x4->Fill(emmax);
      if(elemax>0) mELE4x4->Fill(elemax);
      if(summax>0) mEH4x4->Fill(summax);
      if(hadmax>0) mHAD4x4->Fill(summax);
      if(max>0)    mERatio->Fill(trg->ratiomax[maxns][maxr][maxc]);      
      if(elemax>0) mELERatio->Fill(trg->ratiomax[maxns3][maxr3][maxc3]);      
      if(summax>0) mHRatio->Fill(trg->ratio[maxns4][maxr4][maxc4]);      
      if(hadmax>0) mHADRatio->Fill(trg->ratio[maxns5][maxr5][maxc5]);      
    }

    for(int det=0; det<kFcsNDet; det++){
      StSPtrVecFcsHit& hits = fcs->hits(det);
      int nh=fcs->numberOfHits(det);
      for(int i=0; i<nh; i++) mAdc[det]->Fill(hits[i]->id(),(float)hits[i]->adcSum());
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

// \class StFmsTriggerSimMaker
// \author Akio Ogawa
//
//  $Id: StFcsTriggerSimMaker.cxx,v 1.2 2021/05/30 21:40:56 akio Exp $
//  $Log: StFcsTriggerSimMaker.cxx,v $
//  Revision 1.2  2021/05/30 21:40:56  akio
//  Many updates from trigger commissioning on Run21 OO data
//
//  Revision 1.1  2021/03/30 13:33:53  akio
//  Moved from $CVSROOT/offline/upgrade/akio/ to $CVSROOT/StRoot/StSpinPool/
//
//  Revision 1.7  2021/02/25 21:56:10  akio
//  Int_t -> int
//
//  Revision 1.6  2020/07/24 17:22:39  akio
//  adding option to reading in EPD masks
//
//  Revision 1.5  2020/06/01 20:33:42  akio
//  adapt for DAQ_FCS change
//
//  Revision 1.4  2020/05/29 18:55:47  akio
//  Modiying to make it run with Tonko's wrapper
//
//  Revision 1.3  2019/06/26 18:01:06  akio
//  assuming first timebin from MC has ADC
//
//  Revision 1.2  2019/05/17 15:58:56  akio
//  updates
//
//  Revision 1.1  2018/11/12 13:15:58  akio
//  Initial version
//

#include "StFcsTriggerSimMaker.h"

#include "TTree.h"
#include "TFile.h"

#include "StMessMgr.h"
#include "Stypes.h"
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"

#include "StThreeVectorF.hh"
#include "StEvent/StEventTypes.h"
#include "StEvent/StFcsHit.h"
#include "StFcsDbMaker/StFcsDb.h"

#include "RTS/include/rtsLog.h"

#include "StRoot/RTS/src/TRG_FCS/fcs_trg_base.h"

#include "StMaker.h"
#include "StChain.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"

namespace {
  enum {kMaxNS=2, kMaxDet=3, kMaxDep=24, kMaxCh=32, kMaxEcalDep=24, kMaxHcalDep=8, kMaxPresDep=4, kMaxLink2=2};
  uint32_t   fcs_trg_sim_adc[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;
  float   fcs_trg_pt_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
  float   fcs_trg_gain_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
  uint16_t fcs_trg_pedestal[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;

  static const int mNTRG=21;
  static const char* ctrg[mNTRG]={"JP2", "JPA1", "JPA0", "JPBC1", "JPBC0", "JPDE1", "JPDE0",
				  "DiJP", "DiJPAsy",
				  "DY","JPsi","DYNoEpd","DYAsy",
				  "Had2","Had1","Had0",
				  "EM2","EM1","EM0",
				  "ELE2","EM3"};
  int NTRG[mNTRG+1];
}

ClassImp(StFcsTriggerSimMaker);

StFcsTriggerSimMaker::StFcsTriggerSimMaker(const char* name): StMaker(name) {}

StFcsTriggerSimMaker::~StFcsTriggerSimMaker(){}

int StFcsTriggerSimMaker::Init(){  
    LOG_INFO << "StFcsTriggerSimMaker::Init" << endm;

    mFcsDb=static_cast<StFcsDb*>(GetDataSet("fcsDb"));  
    if(!mFcsDb){
	LOG_ERROR  << "StFcsTriggerSimMaker::Init Failed to get StFcsDb" << endm;
	return kStFatal;
    }

    rtsLogOutput(RTS_LOG_STDERR) ;

    mTrgSim = new fcs_trg_base();
    mTrgSim->sim_mode=1;
    mTrgSim->init(".");
    mTrgSim->run_start(0);
    mTrgSim->fcs_trgDebug=mDebug;
    
    //trigegr versions
    if(mTrgSelect==201900){
	mTrgSim->stage_version[0]=0;
	mTrgSim->stage_version[1]=0;
	mTrgSim->stage_version[2]=0;
	mTrgSim->stage_version[3]=0;
    }else if(mTrgSelect==202201){
	mTrgSim->stage_version[0]=0;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=1;
	mTrgSim->stage_version[3]=1;
    }else if(mTrgSelect==202203){
	mTrgSim->stage_version[0]=2;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=3;
	mTrgSim->stage_version[3]=3;
    }else if(mTrgSelect==202204){
	mTrgSim->stage_version[0]=2;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=4;
	mTrgSim->stage_version[3]=3;
    }else if(mTrgSelect==202205){
	mTrgSim->stage_version[0]=2;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=5;
	mTrgSim->stage_version[3]=3;
    }else if(mTrgSelect==202206){
	mTrgSim->stage_version[0]=2;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=6;
	mTrgSim->stage_version[3]=3;
    }else if(mTrgSelect==202207){
	mTrgSim->stage_version[0]=2;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=7;
	mTrgSim->stage_version[3]=7;
    }else if(mTrgSelect==202209){
	mTrgSim->stage_version[0]=3;
	mTrgSim->stage_version[1]=1;
	mTrgSim->stage_version[2]=7;
	mTrgSim->stage_version[3]=7;
    }

    //Thresholds
    if(mThresholdFile){
	readThresholdFile();
    }else if(mThresholdDb){
	readThresholdDb();
    }
    //mTrgSim->EM_HERATIO_THR = 32;
    //mTrgSim->HAD_HERATIO_THR = 32;

    //EPD mask
    if(mPresMask){
	printf("Reading PresMask from %s\n",mPresMask);
	FILE* F=fopen(mPresMask,"r");
	if(F==NULL){
	    printf("Cannot open %s\n",mPresMask);	    
	}else{
	    char line[512];
	    int r,c,m[6];
	    while(fgets(line,sizeof(line),F)){
		if(line[0]=='#') {
		    printf("%s",line);
		    continue;
		}
		sscanf(line,"%d %d %x %x %x %x %x %x",&r,&c,&m[0],&m[1],&m[2],&m[3],&m[4],&m[5]);
		printf("%2d %1d %08x %08x %08x %08x %08x %08x\n",r,c,m[0],m[1],m[2],m[3],m[4],m[5]);
		for(int i=0; i<6; i++) mTrgSim->PRES_MASK[r-1][c-1][i]=m[i];
	    }
	    mTrgSim->fcs_readPresMaskFromText=1;	
	}
    }

    memset(NTRG,0,sizeof(NTRG));
    return kStOK;
}

int StFcsTriggerSimMaker::InitRun(int runNumber){
    LOG_INFO << "StFcsTriggerSimMaker::InitRun" << endm;
    //print out 4x4 and JP info
    if(mDebug>0){
	print4B4();
	printJP();
    }

    //QA root file    
    if(mQaTreeFilename){
	mQaTreeFile=new TFile(mQaTreeFilename,"RECREATE");
	mTree = new TTree("trgsim","trigger sim QA");
	mTree->Branch("flt",&mFlt,"flt/I");
	mTree->Branch("trg",&mTrg,"trg/I");
    }
    if(mQaHistFilename){
       mQaHistFile=new TFile(mQaHistFilename,"RECREATE");
       mTrgRate = new TH1F("FcsTrgRate","FcsTrgRate",mNTRG+1,0,mNTRG+1);
    }

    //Write Text event file & gainfile
    FILE* gainfile=0;
    FILE* gainfile2=0;
    if(mFilename){
      mFile = fopen(mFilename,"w");
      gainfile=fopen("fcs_et_gain.txt","w");
      gainfile2=fopen("fcs_et_gain2.txt","w");
    }
    
    //Fill ETgain, GainCorr and Pedestal 
    for(int det=0; det<=kFcsNDet; det++){
	int nid=mFcsDb->maxId(det);
	for(int id=0; id<nid; id++){
	    int ehp,ns,crt,sub,dep,ch;
	    mFcsDb->getDepfromId(det,id,ehp,ns,crt,sub,dep,ch);
	    if(det<4){
		fcs_trg_pt_correction[ns][ehp][dep][ch] = mFcsDb->getEtGain(det,id,mEtFactor);
		fcs_trg_gain_correction[ns][ehp][dep][ch] = mFcsDb->getGainCorrection(det,id);
	    }else{
		fcs_trg_pt_correction[ns][ehp][dep][ch] = 1.0;
		fcs_trg_gain_correction[ns][ehp][dep][ch] = 1.0;
	    }
	    fcs_trg_pedestal[ns][ehp][dep][ch] = 0;
	    
	    mTrgSim->p_g[ns][ehp][dep][ch].ped  = fcs_trg_pedestal[ns][ehp][dep][ch];

	    float ggg = fcs_trg_pt_correction[ns][ehp][dep][ch];
	    //float ggg = (fcs_trg_pt_correction[ns][ehp][dep][ch]-1.0)/2.0 + 1.0;
	    float gg = ggg * fcs_trg_gain_correction[ns][ehp][dep][ch];
	    int g = (uint32_t)(gg*256.0+0.5) ;
	    mTrgSim->p_g[ns][ehp][dep][ch].gain = g;

	    /*
	      printf("AAAGAIN %1d %1d %2d %2d pT=%6.3f corr=%6.3f ped=%4d\n",ns,ehp,dep,ch,
	      fcs_trg_pt_correction[ns][ehp][dep][ch],
	      fcs_trg_gain_correction[ns][ehp][dep][ch],
	      fcs_trg_pedestal[ns][ehp][dep][ch]);
	    */

	    if(gainfile) 
		fprintf(gainfile,"%2d %2d %2d %2d %8.3f\n",ns,ehp,dep,ch,
			fcs_trg_pt_correction[ns][ehp][dep][ch]);
	    if(gainfile2) 
		fprintf(gainfile2,"%2d %2d %2d %2d %8.3f\n",ns,ehp,dep,ch,
		(fcs_trg_pt_correction[ns][ehp][dep][ch]-1.0)/2.0 + 1.0);
	}
    }
    if(gainfile)  fclose(gainfile);
    if(gainfile2) fclose(gainfile2);
    return kStOK;
}

int StFcsTriggerSimMaker::Finish(){
    mTrgSim->run_stop();
    if(mFile) {
	printf("Closing %s\n",mFilename);
	fclose(mFile);
    }
    if(mQaTreeFile){
	printf("Closing %s\n",mQaTreeFilename);
	mTree->Write();
	mQaTreeFile->Close();
    }
    if(mQaHistFile){
	printf("Closing %s\n",mQaHistFilename);
	mTrgRate->Write();
	mQaHistFile->Close();
    }

    int tot = NTRG[mNTRG];
    LOG_INFO << "Triggers counts/"<<tot<<" (rate at 5MHz BBC)"<<endm; 
    for(int i=0; i<mNTRG; i++)
	LOG_INFO << Form("%8s  %9d (%12.2f)",ctrg[i],NTRG[i],double(NTRG[i])/tot*5.0e6)<<endm;

    return kStOK;
}

int StFcsTriggerSimMaker::Make(){
    StEvent* event = nullptr;
    event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_INFO << "StFcsTriggerSimMaker::Make did not find StEvent"<<endm;}
    mFcsColl = event->fcsCollection();
    if(!mFcsColl) {LOG_INFO << "StFcsTriggerSimMaker::Make did not find StEvent->StFcsCollection"<<endm;}
    
    StMuEvent* muevent = nullptr;
    if((!event)||(!mFcsColl)){
        
        LOG_INFO<<"No StEvent info available for StFcsTriggerSimMaker. "<< endm;
    
        muevent = StMuDst::event();
        mMuFcsColl = StMuDst::muFcsCollection();
        if(muevent && mMuFcsColl){
            LOG_INFO <<"Proceeding with StMuDst info to be used with StfcsTriggerSimMaker."<< endm;
        }else{
            LOG_ERROR << "StFcsTriggerSimMaker::Make did not find StEvent and MuEvent." << endm;
            return kStErr;
        }
    }
        
    mTrgSim->start_event();

    //Feed ADC
    static uint16_t data[8]; 
    memset(data,0,sizeof(data)) ;
    memset(fcs_trg_sim_adc,0,sizeof(fcs_trg_sim_adc));
    int n=0;
    for(int det=0; det<=kFcsNDet; det++){
        
        int ns  = mFcsDb->northSouth(det);
        int ehp = mFcsDb->ecalHcalPres(det);
                
        if((event)&&(mFcsColl)){
            StSPtrVecFcsHit& hits = mFcsColl->hits(det);
        
            int nh  = mFcsColl->numberOfHits(det);
          
            for(int i=0; i<nh; i++){
                StFcsHit* hit=hits[i];
                unsigned short ch  = hit->channel();

                if(ehp<0 || ch>=32) continue;
                feedADC(hit, ns, ehp, data);
                n++;
            }
            
        }else if((muevent)&&(mMuFcsColl)){ //Use StMuDst info instead of StEvent for Trigger Reconstruction
            
            int nh = mMuFcsColl->numberOfHits(det);
            int det_hit_index = mMuFcsColl->indexOfFirstHit(det);

            for(int i=0; i<nh; i++){
                
                int hit_index = i + det_hit_index;
                StMuFcsHit* hit = mMuFcsColl->getHit(hit_index);
                unsigned short ch  = hit->channel();

                if(ehp<0 || ch>=32) continue;
                feedADC(hit, ns, ehp, data);
                n++;
            }
        }
    }
    if(mFile) fprintf(mFile,"%2d %2d %2d %2d %5d\n",-1,0,0,0,0);
    LOG_INFO << Form("StFcsTriggerSimMaker feeded %d hits",n) << endm;;

    //Run Trigger Simulation
    //   uint16_t dsm_out = fcs_trg_run(mTrgSelect, mDebug);
    uint32_t dsm_out = mTrgSim->end_event();
  
    //QA Tree
    mFlt=0;
    StarPrimaryMaker* pmkr= static_cast<StarPrimaryMaker*>(GetMaker("PrimaryMaker"));   
    if(pmkr){
	StarGenEvent *ge = pmkr->event();
	if(ge){
	    mFlt=ge->GetFilterResult();
	}
    }
    mTcu=dsm_out;
    if(mQaTreeFile) mTree->Fill();
    
    //Results
    LOG_INFO << Form("Output to TCU = 0x%08x  Filter=0x%08x",mTcu,mFlt)<<endm;

    //TCU Trigger Emulation 
    int trg[mNTRG]; 
    mTrg=0;
    memset(trg,0,sizeof(trg));
    if((dsm_out >> 6) & 0x1)       {trg[ 0]=1; mTrg+=1<<0;}  //JP2
    if((dsm_out >> 7) & 0x1)       {trg[ 1]=1; mTrg+=1<<1;}  //JPA1
    if((dsm_out >>10) & 0x1)       {trg[ 2]=1; mTrg+=1<<2;}  //JPA0
    if((dsm_out >> 8) & 0x1)       {trg[ 3]=1; mTrg+=1<<3;}  //JPBC1
    if((dsm_out >>11) & 0x1)       {trg[ 4]=1; mTrg+=1<<4;}  //JPBC0
    if((dsm_out >> 9) & 0x1)       {trg[ 5]=1; mTrg+=1<<5;}  //JPDE1
    if((dsm_out >>12) & 0x1)       {trg[ 6]=1; mTrg+=1<<6;}  //JPDE0
    if((dsm_out >>13) & 0x1)       {trg[ 7]=1; mTrg+=1<<7;}  //DiJP
    if((dsm_out >>14) & 0x1)       {trg[ 8]=1; mTrg+=1<<8;}  //DiJpAsy
    if( ((dsm_out >>18) & 0x1) && 
        ((dsm_out >>26) & 0x1) )   {trg[ 9]=1; mTrg+=1<<9;}  //DY 
    if( ((dsm_out >>17) & 0x1) && 
        ((dsm_out >>25) & 0x1) )   {trg[10]=1;  mTrg+=1<<10;} //Jpsi
    if( ((dsm_out >>19) & 0x1) && 
        ((dsm_out >>27) & 0x1) )   {trg[11]=1;  mTrg+=1<<11;} //DYNoEpd
    if((dsm_out >>15) & 0x1)       {trg[12]=1;  mTrg+=1<<12;} //DYAsy     
    if((dsm_out >> 2) & 0x1)       {trg[13]=1;  mTrg+=1<<13;} //Had2
    if((dsm_out >> 1) & 0x1)       {trg[14]=1;  mTrg+=1<<14;} //Had1
    if((dsm_out >> 0) & 0x1)       {trg[15]=1;  mTrg+=1<<15;} //Had0
    if((dsm_out >> 5) & 0x1)       {trg[16]=1;  mTrg+=1<<16;} //EM2
    if((dsm_out >> 4) & 0x1)       {trg[17]=1;  mTrg+=1<<17;} //EM1
    if((dsm_out >> 3) & 0x1)       {trg[18]=1;  mTrg+=1<<18;} //EM0
    if( ((dsm_out >>18) & 0x1) ||
        ((dsm_out >>26) & 0x1) )   {trg[19]=1;  mTrg+=1<<19;} //ELE2
    if( ((dsm_out >>19) & 0x1) ||
        ((dsm_out >>27) & 0x1) )   {trg[20]=1;  mTrg+=1<<20;} //EM3

    if(mTrgRate) mTrgRate->Fill(mNTRG);
    NTRG[mNTRG]++;
    for(int i=0; i<mNTRG; i++){
      if(trg[i]) {
        if(mTrgRate) mTrgRate->Fill(i);
        NTRG[i]++;
      }
    }
  
    LOG_INFO << "Triggers = ";
    for(int i=0; i<mNTRG; i++){ if(trg[i]) LOG_INFO << ctrg[i] << " ";}
    LOG_INFO << endm;
    
    return kStOK;
}    

void StFcsTriggerSimMaker::runStage2(link_t ecal[], link_t hcal[], link_t pres[], geom_t& geo, link_t output[]){
  uint16_t s2_to_dsm;
  mTrgSim->stage_2(ecal,hcal,pres,geo,output,&s2_to_dsm);
}

void StFcsTriggerSimMaker::print4B4(){
    //printout ecal 4x4
    FILE* f1=fopen("EH4by4.txt","w");
    FILE* f2=fopen("EH4by4dist.txt","w");
    FILE* f3=fopen("EH4by4map.txt","w");
    FILE* f4=fopen("EH2by2dist.txt","w");
    FILE* f5=fopen("EH2by2map.txt","w");
    FILE* f6=fopen("EH2by2map2.txt","w");
    
    // v1 with hcal top2/bottom2 rows not in trigger
    //enum {EX2B2=10,EY2B2=16,HX2B2=6,HY2B2=8};
    //enum {EXOFF=1,EYOFF=1,HXOFF=0,HYOFF=2};
    //v2 with hcal top2/bottom2 rows in trigger
    enum {EX2B2=10,EY2B2=16,HX2B2=6,HY2B2=10};
    enum {EXOFF=1,EYOFF=1,HXOFF=0,HYOFF=0};
    // v3 with hcal top2/bottom2 rows in trigger, put offset of 1 in HX
    //enum {EX2B2=10,EY2B2=16,HX2B2=6,HY2B2=10};
    //enum {EXOFF=1,EYOFF=1,HXOFF=1,HYOFF=0};
    StThreeVectorF exyz[2][EX2B2-1][EY2B2-1];
    StThreeVectorF hxyz[2][HX2B2-1][HY2B2-1]; //4x4
    StThreeVectorF Hxyz[2][HX2B2][HY2B2];     //2x2
    StThreeVectorF sxyz[2][EX2B2-1][EY2B2-1]; //ecal 4x4 extraprated at hcal
    StThreeVectorF eoff = mFcsDb->getDetectorOffset(1);
    StThreeVectorF hoff = mFcsDb->getDetectorOffset(3);
    float esmx=mFcsDb->getShowerMaxZ(1);
    float hsmx=mFcsDb->getShowerMaxZ(3);
    float r1 = sqrt(eoff.x()*eoff.x()+eoff.z()*eoff.z()); //distance from IP to ecal surface
    float r2 = r1 + esmx;                                 //distance from IP to ecal SMax
    float r3 = sqrt(hoff.x()*hoff.x()+hoff.z()*hoff.z()); //distance from IP to hcal surface
    float r4 = r3 + hsmx;                                 //distance from IP to hcal SMax
    float sf = r4/r2;  // scale factor for extraporating ecal point to hcal plane
    fprintf(f1,"Distannce from IP to EcalSmax=%8.3f HcalSMax=%8.3f Ratio=%6.3f\n",r2,r4,sf);

    fprintf(f1,"\nHcal 4x4\n");
    fprintf(f1,"   C  R  XY[cell]     XYZ[cm]\n");
    for(int ns=1; ns<2; ns++){
	for(int j=0; j<HY2B2-1; j++){
	    float y=j*2 + HYOFF + 2;
	    for(int i=0; i<HX2B2-1; i++){
		float x=i*2 + HXOFF + 2;
		hxyz[ns][i][j] = mFcsDb->getStarXYZfromColumnRow(ns+2,x,y);
		fprintf(f1,"H %2d %2d %4.1f %4.1f  %8.2f %8.2f %8.2f\n",
			i,j,x,y,hxyz[ns][i][j].x(),hxyz[ns][i][j].y(),hxyz[ns][i][j].z());
	    }
	}
    }

    fprintf(f1,"\nHcal 2x2\n");
    fprintf(f1,"   C  R  XY[cell]     XYZ[cm]\n");
    for(int ns=1; ns<2; ns++){
	for(int j=0; j<HY2B2; j++){
	    float y=j*2 + HYOFF + 1;
	    for(int i=0; i<HX2B2; i++){
		float x=i*2 + HXOFF + 1;
		Hxyz[ns][i][j] = mFcsDb->getStarXYZfromColumnRow(ns+2,x,y);
		fprintf(f1,"H %2d %2d %4.1f %4.1f  %8.2f %8.2f %8.2f\n",
			i,j,x,y,Hxyz[ns][i][j].x(),Hxyz[ns][i][j].y(),Hxyz[ns][i][j].z());
	    }
	}
    }

    fprintf(f1,"\nEcal 4x4\n");
    fprintf(f1,"   C  R  XY[cell]    XYZ[cm]                 | XYZ at HCAL                |  C  R  distance  XYZ of closest Hcal 4x4\n");
    
    fprintf(f3,"static const int EtoHmap[%d][%d][2] = {\n",EY2B2-1,EX2B2-1);
    fprintf(f5,"static const int EtoH2map[%d][%d][2] = {\n",EY2B2-1,EX2B2-1);
    fprintf(f6,"static const int EtoH3map[%d][%d][4] = {\n",EY2B2-1,EX2B2-1);
    for(int ns=1; ns<2; ns++){
	for(int j=0; j<EY2B2-1; j++){
	    float y=j*2 + EYOFF + 2;
	    fprintf(f3,"    { ");
	    for(int i=0; i<EX2B2-1; i++){
		float x=i*2 + EXOFF + 2;
		exyz[ns][i][j] = mFcsDb->getStarXYZfromColumnRow(ns,x,y);
		sxyz[ns][i][j] = sf * exyz[ns][i][j];
		//search for closest hcal 4x4
		float dmin=999.0;
		int k,l;
		for(int ii=0; ii<HX2B2-1; ii++){
		    for(int jj=0; jj<HY2B2-1; jj++){
			StThreeVectorF diff = sxyz[ns][i][j]-hxyz[ns][ii][jj];
			float d = diff.mag();
			//printf("%2d %2d %2d %2d d=%6.2f  diff=%8.2f %8.2f %8.2f\n",
			//       i,j,ii,jj,d,diff.x(),diff.y(),diff.z());
			if(d<dmin){
			    dmin=d;
			    k=ii;
			    l=jj;
			}
		    }
		}
		fprintf(f1,"E %2d %2d %4.1f %4.1f %8.2f %8.2f %8.2f | %8.2f %8.2f %8.2f | %2d %2d %6.2f %8.2f %8.2f %8.2f\n",
			i,j,x,y,
			exyz[ns][i][j].x(),exyz[ns][i][j].y(),exyz[ns][i][j].z(),
			sxyz[ns][i][j].x(),sxyz[ns][i][j].y(),sxyz[ns][i][j].z(),
			k,l,dmin,
			hxyz[ns][k][l].x(),hxyz[ns][k][l].y(),hxyz[ns][k][l].z());
		fprintf(f2,"%6.2f  ",dmin);
		if(i==EX2B2-2) fprintf(f2,"\n");
		fprintf(f3,"{%2d,%2d}",l,k);
		if(i<EX2B2-2)  fprintf(f3,",");
		if(i==EX2B2-2) fprintf(f3,"}");

		//hcal 2x2
		float dmin2=999.0;
                int kk,ll;
		for(int ii=0; ii<HX2B2; ii++){
		    for(int jj=0; jj<HY2B2; jj++){
			StThreeVectorF diff = sxyz[ns][i][j]-Hxyz[ns][ii][jj];
			float d = diff.mag();
			//printf("%2d %2d %2d %2d d=%6.2f  diff=%8.2f %8.2f %8.2f\n",
			//       i,j,ii,jj,d,diff.x(),diff.y(),diff.z());
			if(d<dmin2){
			    dmin2=d;
			    kk=ii;
			    ll=jj;
			}
		    }
		}
		fprintf(f1,"E %2d %2d %4.1f %4.1f %8.2f %8.2f %8.2f | %8.2f %8.2f %8.2f | %2d %2d %6.2f %8.2f %8.2f %8.2f\n",
			i,j,x,y,
			exyz[ns][i][j].x(),exyz[ns][i][j].y(),exyz[ns][i][j].z(),
			sxyz[ns][i][j].x(),sxyz[ns][i][j].y(),sxyz[ns][i][j].z(),
			kk,ll,dmin2,
			Hxyz[ns][kk][ll].x(),Hxyz[ns][kk][ll].y(),Hxyz[ns][kk][ll].z());
		fprintf(f4,"%6.2f  ",dmin2);
		if(i==EX2B2-2) fprintf(f4,"\n");

		fprintf(f5,"{%2d,%2d}",ll,kk);
		if(i<EX2B2-2)  fprintf(f5,",");
		if(i==EX2B2-2) fprintf(f5,"}");

		int i1=kk-1, i2=kk, j1=ll-1, j2=ll;
		int r1=-1, r2=-1, r3=-1, r4=-1;
		if(i1>=0 && i1<HX2B2-1 && j1>=0 && j1<HY2B2-1) r1=i1+j1*(HX2B2-1);
		if(i2>=0 && i2<HX2B2-1 && j1>=0 && j1<HY2B2-1) r2=i2+j1*(HX2B2-1);
		if(i1>=0 && i1<HX2B2-1 && j2>=0 && j2<HY2B2-1) r3=i1+j2*(HX2B2-1);
		if(i2>=0 && i2<HX2B2-1 && j2>=0 && j1<HY2B2-1) r4=i2+j2*(HX2B2-1);
		fprintf(f6,"{%2d,%2d,%2d,%2d}",r1,r2,r3,r4);
		if(i<EX2B2-2)  fprintf(f6,",");
		if(i==EX2B2-2) fprintf(f6,"}");
	    }
	    if(j<EY2B2-2)  fprintf(f3,",\n");
	    if(j==EY2B2-2) fprintf(f3,"\n");
	    if(j<EY2B2-2)  fprintf(f5,",\n");
	    if(j==EY2B2-2) fprintf(f5,"\n");
	    if(j<EY2B2-2)  fprintf(f6,",\n");
	    if(j==EY2B2-2) fprintf(f6,"\n");
	}
	fprintf(f3,"}\n");
	fprintf(f5,"}\n");
	fprintf(f6,"}\n");
    }
    fclose(f1);
    fclose(f2);
    fclose(f3);
    fclose(f4);
    fclose(f5);
    fclose(f6);
    return;
}


void StFcsTriggerSimMaker::printJP(){
    //printout ecal 4x4
    FILE* f1=fopen("EHJP.txt","w");
    FILE* f2=fopen("EHJPdist.txt","w");
    
    enum {EXOFF=1,EYOFF=1,HXOFF=0,HYOFF=2};
    StThreeVectorF exyz[2][3][5];
    StThreeVectorF hxyz[2][3][5];
    StThreeVectorF sxyz[2][3][5]; //ecal 4x4 extraprated at hcal
    StThreeVectorF eoff = mFcsDb->getDetectorOffset(1);
    StThreeVectorF hoff = mFcsDb->getDetectorOffset(3);
    float esmx=mFcsDb->getShowerMaxZ(1);
    float hsmx=mFcsDb->getShowerMaxZ(3);
    float r1 = sqrt(eoff.x()*eoff.x()+eoff.z()*eoff.z()); //distance from IP to ecal surface
    float r2 = r1 + esmx;                                 //distance from IP to ecal SMax
    float r3 = sqrt(hoff.x()*hoff.x()+hoff.z()*hoff.z()); //distance from IP to hcal surface
    float r4 = r3 + hsmx;                                 //distance from IP to hcal SMax
    float sf = r4/r2;  // scale factor for extraporating ecal point to hcal plane
    fprintf(f1,"Distannce from IP to EcalSmax=%8.3f HcalSMax=%8.3f Ratio=%6.3f\n",r2,r4,sf);
    fprintf(f1,"\nHcal 8x8\n");
    fprintf(f1,"   C  R  XY[cell]     XYZ[cm]\n");
    for(int ns=1; ns<2; ns++){
	for(int j=0; j<5; j++){
	    float y=j*2 + HYOFF + 4;
	    for(int i=0; i<3; i++){
		float x=i*2 + HXOFF + 4;
		hxyz[ns][i][j] = mFcsDb->getStarXYZfromColumnRow(ns+2,x,y);
		fprintf(f1,"H %2d %2d %4.1f %4.1f  %8.2f %8.2f %8.2f\n",
			i,j,x,y,hxyz[ns][i][j].x(),hxyz[ns][i][j].y(),hxyz[ns][i][j].z());
	    }
	}
    }
    fprintf(f1,"\nEcal 12x16\n");
    fprintf(f1,"   C  R  XY[cell]    XYZ[cm]                 | XYZ at HCAL                |  distance  XYZ of Hcal 8x8\n");    
    for(int ns=1; ns<2; ns++){
	for(int j=0; j<5; j++){
	    float y=j*4 + EYOFF + 8;
	    for(int i=0; i<3; i++){
		float x=i*4 + EXOFF + 6;
		exyz[ns][i][j] = mFcsDb->getStarXYZfromColumnRow(ns,x,y);
		sxyz[ns][i][j] = sf * exyz[ns][i][j];
		StThreeVectorF diff = sxyz[ns][i][j]-hxyz[ns][i][j];
		float d = diff.mag();
		fprintf(f1,"E %2d %2d %4.1f %4.1f %8.2f %8.2f %8.2f | %8.2f %8.2f %8.2f | %6.2f %8.2f %8.2f %8.2f\n",
			i,j,x,y,
			exyz[ns][i][j].x(),exyz[ns][i][j].y(),exyz[ns][i][j].z(),
			sxyz[ns][i][j].x(),sxyz[ns][i][j].y(),sxyz[ns][i][j].z(),
			d,
			hxyz[ns][i][j].x(),hxyz[ns][i][j].y(),hxyz[ns][i][j].z());
		fprintf(f2,"%6.2f  ",d);
		if(i==2) fprintf(f2,"\n");
	    }
	}
    }
    fclose(f1);
    fclose(f2);
    return;
}

void StFcsTriggerSimMaker::readThresholdFile(){
    LOG_INFO << Form("Reading Thresholds from %s",mThresholdFile)<<endm;
    FILE* F=fopen(mThresholdFile,"r");
    if(F == NULL){
	LOG_ERROR << Form("Could not open %s",mThresholdFile)<<endm;
	return;
    }
    char f[10],name[100];
    int i,id,v;
    while(fscanf(F,"%s %d %d %s %d",f, &i, &id, name, &v) != EOF){
	if(f[0] == '#') continue;
	TString trg(name);
	printf("Reading Threshold %s %d\n",trg.Data(),v);
	if     (trg=="FCS_PHTTHR")          mTrgSim->PHTTHR=v;
	else if(trg=="FCS_EM-HERATIO-THR")  mTrgSim->EM_HERATIO_THR=v;
	else if(trg=="FCS_HAD-HERATIO-THR") mTrgSim->HAD_HERATIO_THR=v;
	else if(trg=="FCS_EMTHR2")          mTrgSim->EMTHR2=v;
	else if(trg=="FCS_EMTHR1")          mTrgSim->EMTHR1=v;
	else if(trg=="FCS_EMTHR0")          mTrgSim->EMTHR0=v;
	else if(trg=="FCS_ELETHR2")         mTrgSim->ELETHR2=v;
	else if(trg=="FCS_ELETHR1")         mTrgSim->ELETHR1=v;
	else if(trg=="FCS_ELETHR0")         mTrgSim->ELETHR0=v;
	else if(trg=="FCS_HADTHR2")         mTrgSim->HADTHR2=v;
	else if(trg=="FCS_HADTHR1")         mTrgSim->HADTHR1=v;
	else if(trg=="FCS_HADTHR0")         mTrgSim->HADTHR0=v;
	else if(trg=="FCS_JPATHR2")         mTrgSim->JPATHR2=v;
	else if(trg=="FCS_JPATHR1")         mTrgSim->JPATHR2=v;
	else if(trg=="FCS_JPATHR0")         mTrgSim->JPATHR0=v;
	else if(trg=="FCS_JPBCTHR2")        mTrgSim->JPBCTHR2=v;
	else if(trg=="FCS_JPBCTHR1")        mTrgSim->JPBCTHR1=v;
	else if(trg=="FCS_JPBCTHR0")        mTrgSim->JPBCTHR0=v;
	else if(trg=="FCS_JPBCTHRD")        mTrgSim->JPBCTHRD=v;
	else if(trg=="FCS_JPDETHR2")        mTrgSim->JPDETHR2=v;
	else if(trg=="FCS_JPDETHR1")        mTrgSim->JPDETHR1=v;
	else if(trg=="FCS_JPDETHR0")        mTrgSim->JPDETHR0=v;
	else if(trg=="FCS_JPDETHRD")        mTrgSim->JPDETHRD=v;
	else if(trg=="FCS_EHTTHR")          mTrgSim->EHTTHR=v;
	else if(trg=="FCS_HHTTHR")          mTrgSim->HHTTHR=v;
	else if(trg=="FCS_ETOTTHR")         mTrgSim->ETOTTHR=v;
	else if(trg=="FCS_HTOTTHR")         mTrgSim->HTOTTHR=v;
	else{
	    printf("No Threshold called %s %d\n",trg.Data(),v);
	}
    }
    fclose(F);
}

//read from offline copy of online runlog DB
void StFcsTriggerSimMaker::readThresholdDb(){
  //to be implemented before run22 online DB moves to offline  
}

template<typename T> void StFcsTriggerSimMaker::feedADC(T* hit, int ns, int ehp, uint16_t data_array[]){
    
    unsigned short dep = hit->dep();
    unsigned short ch  = hit->channel();
    
//    printf("ns=%1d ehp=%1d dep=%2d ch=%2d adc=%4d sim=%d\n",ns,ehp,dep,ch,hit->adc(0),mSimMode);
    fcs_trg_sim_adc[ns][ehp][dep][ch] = hit->adc(0);
    if(mSimMode==0){
      int ntb=hit->nTimeBin();
      for(int t=0; t<ntb; t++){
    int tb = hit->timebin(t);
    if(tb>=mTrgTimebin-3 && tb<=mTrgTimebin+4){
      data_array[tb-mTrgTimebin+3] = hit->adc(t);
//    printf("tb=%3d i=%2d adc=%4d\n",tb,tb-mTrgTimebin+3,hit->adc(t));
    }
      }
      mTrgSim->fill_event(ehp,ns,dep,ch,data_array,8) ;
    }else{
      data_array[1] = hit->adc(0)-1;  //removing 1 to add at tb6
      data_array[6] = 1;              //add this so tb6>tb7
      mTrgSim->fill_event(ehp,ns,dep,ch,data_array,8) ;
    }
    if(mFile) fprintf(mFile,"%2d %2d %2d %2d %5d\n",ns,ehp,dep,ch,hit->adc(0));

}

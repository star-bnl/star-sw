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

namespace {
  enum {kMaxNS=2, kMaxDet=3, kMaxDep=24, kMaxCh=32, kMaxEcalDep=24, kMaxHcalDep=8, kMaxPresDep=4, kMaxLink2=2};
  u_int   fcs_trg_sim_adc[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;
  float   fcs_trg_pt_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
  float   fcs_trg_gain_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
  u_short fcs_trg_pedestal[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;
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
	//Thresholds
	mTrgSim->EM_HERATIO_THR = 32;
	mTrgSim->HAD_HERATIO_THR = 32;
	if(0){
	  mTrgSim->EMTHR1 = 32;
	  mTrgSim->EMTHR2 = 48;
	  mTrgSim->EMTHR3 = 64;
	  mTrgSim->HADTHR1 = 32;
	  mTrgSim->HADTHR2 = 48;
	  mTrgSim->HADTHR3 = 64;
	  mTrgSim->JETTHR1 = 64;
	  mTrgSim->JETTHR2 = 128;
	  mTrgSim->ETOTTHR = 128;
	  mTrgSim->HTOTTHR = 64;
	  mTrgSim->EHTTHR = 16;
	  mTrgSim->HHTTHR = 16;
	}
	if(0){
	  mTrgSim->EMTHR1 = 64;
	  mTrgSim->EMTHR2 = 128;
	  mTrgSim->EMTHR3 = 192;
	  mTrgSim->HADTHR1 = 64;
	  mTrgSim->HADTHR2 = 128;
	  mTrgSim->HADTHR3 = 192;
	  mTrgSim->JETTHR1 = 128;
	  mTrgSim->JETTHR2 = 192;
	  mTrgSim->ETOTTHR = 192;
	  mTrgSim->HTOTTHR = 64;
	  mTrgSim->EHTTHR = 64;
	  mTrgSim->HHTTHR = 16;
	}
	if(0){
	  mTrgSim->EMTHR1 = 180;
	  mTrgSim->EMTHR2 = 240;
	  mTrgSim->EMTHR3 = 250;
	  mTrgSim->HADTHR1 = 180;
	  mTrgSim->HADTHR2 = 240;
	  mTrgSim->HADTHR3 = 250;
	  mTrgSim->JETTHR1 = 150;
	  mTrgSim->JETTHR2 = 250;
	  mTrgSim->ETOTTHR = 250;
	  mTrgSim->HTOTTHR = 150;
	  mTrgSim->EHTTHR = 200;
	  mTrgSim->HHTTHR = 40;
	}
	if(0){
	  mTrgSim->EMTHR1 = 70;
	  mTrgSim->EMTHR2 = 120;
	  mTrgSim->EMTHR3 = 160;
	  mTrgSim->HADTHR1 = 85;
	  mTrgSim->HADTHR2 = 130;
	  mTrgSim->HADTHR3 = 160;
	  mTrgSim->JETTHR1 = 240;
	  mTrgSim->JETTHR2 = 250;
	  mTrgSim->ETOTTHR = 250;
	  mTrgSim->HTOTTHR = 230;
	  mTrgSim->EHTTHR = 80;
	  mTrgSim->HHTTHR = 70;
	}
	if(1){
	  mTrgSim->EMTHR1 = 70;
	  mTrgSim->EMTHR2 = 50;
	  mTrgSim->EMTHR3 = 160;
	  mTrgSim->HADTHR1 = 85;
	  mTrgSim->HADTHR2 = 50;
	  mTrgSim->HADTHR3 = 160;
	  mTrgSim->JETTHR1 = 240;
	  mTrgSim->JETTHR2 = 250;
	  mTrgSim->ETOTTHR = 100;
	  mTrgSim->HTOTTHR = 80;
	  mTrgSim->EHTTHR = 80;
	  mTrgSim->HHTTHR = 70;
	}
	mTrgSim->PHTTHR = 160;    
    }
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
    if(mQaFilename){
	mQaFile=new TFile(mQaFilename,"RECREATE");
	mTree = new TTree("trgsim","trigger sim QA");
	mTree->Branch("flt",&mFlt,"flt/I");
	mTree->Branch("trg",&mTrg,"trg/I");
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
	    fcs_trg_pt_correction[ns][ehp][dep][ch] = mFcsDb->getEtGain(det,id);
	    fcs_trg_gain_correction[ns][ehp][dep][ch] = mFcsDb->getGainCorrection(det,id);
	    fcs_trg_pedestal[ns][ehp][dep][ch] = 0;
	    
	    mTrgSim->p_g[ns][ehp][dep][ch].ped  = fcs_trg_pedestal[ns][ehp][dep][ch];

	    float ggg = fcs_trg_pt_correction[ns][ehp][dep][ch];
	    //float ggg = (fcs_trg_pt_correction[ns][ehp][dep][ch]-1.0)/2.0 + 1.0;
	    float gg = ggg * fcs_trg_gain_correction[ns][ehp][dep][ch];
	    int g = (u_int)(gg*256.0+0.5) ;
	    mTrgSim->p_g[ns][ehp][dep][ch].gain = g;

	    /*
	      printf("AAA %1d %1d %2d %2d pT=%6.3f corr=%6.3f ped=%4d\n",ns,ehp,dep,ch,
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
    if(mQaFile){
	printf("Closing %s\n",mQaFilename);
	mTree->Write();
	mQaFile->Close();
    }
    return kStOK;
}

int StFcsTriggerSimMaker::Make(){
    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFcsTriggerSimMaker::Make did not find StEvent"<<endm; return kStErr;}
    mFcsColl = event->fcsCollection();
    if(!mFcsColl) {LOG_ERROR << "StFcsTriggerSimMaker::Make did not find StEvent->StFcsCollection"<<endm; return kStErr;}
    
    mTrgSim->start_event();

    //Feed ADC
    static u_short data[8]; 
    memset(data,0,sizeof(data)) ;
    memset(fcs_trg_sim_adc,0,sizeof(fcs_trg_sim_adc));
    int n=0;
    for(int det=0; det<=kFcsNDet; det++){
	StSPtrVecFcsHit& hits = mFcsColl->hits(det);
	int ns  = mFcsDb->northSouth(det);
	int ehp = mFcsDb->ecalHcalPres(det);
	int nh  = mFcsColl->numberOfHits(det);
	for(int i=0; i<nh; i++){
	    StFcsHit* hit=hits[i];
	    unsigned short dep = hit->dep();
	    unsigned short ch  = hit->channel();
	    //printf("ns=%1d ehp=%1d dep=%2d ch=%2d adc=%4d\n",ns,ehp,dep,ch,hit->adc());
	    if(ehp<0 || ch>=32) continue;
	    fcs_trg_sim_adc[ns][ehp][dep][ch] = hit->adc(0);
	    if(mSimMode==0){
	      int ntb=hit->nTimeBin();
	      for(int t=0; t<ntb; t++){
		int tb = hit->timebin(t);
		if(tb>=mTrgTimebin-3 && tb<=mTrgTimebin+4){
		  data[tb-mTrgTimebin+3] = hit->adc(t);
		  //printf("tb=%3d i=%2d adc=%4d\n",tb,tb-mTrgTimebin+3,hit->adc(t));
		}
	      }
	      mTrgSim->fill_event(ehp,ns,dep,ch,data,8) ;
	    }else{
	      data[3] = hit->adc(0);
	      mTrgSim->fill_event(ehp,ns,dep,ch,data,8) ;
	    }
	    if(mFile) fprintf(mFile,"%2d %2d %2d %2d %5d\n",ns,ehp,dep,ch,hit->adc(0));
	    n++;
	}
    }
    if(mFile) fprintf(mFile,"%2d %2d %2d %2d %5d\n",-1,0,0,0,0);
    LOG_INFO << Form("StFcsTriggerSimMaker feeded %d hits",n) << endm;;

    //Run Trigger Simulation
    //   u_short dsm_out = fcs_trg_run(mTrgSelect, mDebug);
    u_short dsm_out = mTrgSim->end_event();

    //QA Tree
    mFlt=0;
    StarPrimaryMaker* pmkr= static_cast<StarPrimaryMaker*>(GetMaker("PrimaryMaker"));   
    if(pmkr){
	StarGenEvent *ge = pmkr->event();
	if(ge){
	    mFlt=ge->GetFilterResult();
	}
    }
    mTrg=dsm_out;
    if(mQaFile) mTree->Fill();

    //Results
    LOG_INFO << Form("Output to DSM = 0x%03x  Filter=0x%03x\n",mTrg,mFlt)<<endm;
    return kStOK;
}    

void StFcsTriggerSimMaker::runStage2(link_t ecal[], link_t hcal[], link_t pres[], geom_t& geo, link_t output[]){
  mTrgSim->stage_2(ecal,hcal,pres,geo,output);
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


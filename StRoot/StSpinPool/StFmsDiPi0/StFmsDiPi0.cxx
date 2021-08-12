// \class StFmsDiPi0
// \author Akio Ogawa
//
//  $Id: StFmsDiPi0.cxx,v 1.5 2017/09/05 17:42:19 akio Exp $
//  $Log: StFmsDiPi0.cxx,v $
//  Revision 1.5  2017/09/05 17:42:19  akio
//  update
//
//  Revision 1.4  2016/11/17 18:57:23  akio
//  Many updates
//
//  Revision 1.3  2016/10/10 19:17:40  akio
//  *** empty log message ***
//
//  Revision 1.2  2016/06/08 16:28:09  akio
//  *** empty log message ***
//
//  Revision 1.1  2016/01/20 19:58:55  akio
//  *** empty log message ***
//
//  Revision 1.1  2016/01/20 19:50:04  akio
//  *** empty log message ***
//
//  Revision 1.1  2015/10/20 19:55:51  akio
//  Initial version of FMS di-pi0 analysis
//

#include "StFmsDiPi0.h"

#include "StMessMgr.h"
#include "Stypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StThreeVectorF.hh"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StEnumerations.h"
//#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StEvent/StTriggerData.h"

#include "StSpinPool/StFmsFastSimMaker/StFmsFastSimMaker.h"
//#include "StarGenerator/BASE/StarPrimaryMaker.h"
//#include "StarGenerator/EVENT/StarGenEvent.h"
//#include "StarGenerator/EVENT/StarGenParticle.h"

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

static const double PI=TMath::Pi();
static const double twoPI=PI*2.0;

const float mPtBin[7]={0.5,1.0,1.5,2.0,2.5,3.0,4.0};
const float energyCut=1.0;
const float ptCut=0.1;
const float ZggCut=0.7;
const float MassCut0=0.07;
const float MassCut1=0.2;
const float MassCut2=0.35;

static const int NTRG=12;  //123=FMS-sm-bs123,456=FMS-lg-bs123,7=FMS-DiBS,8910=FMS-JP012,11=FMS-DiJP,13=LED                              
static const char *CTRG[NTRG] = {"SmBS1","SmBS2","SmBS3","LgBS1",
				 "LgBS2","LgBS3","DiBS" ,"JP2",
				 "JP1"  ,"JP0"  ,"DiJP" ,"LED"};

static const short MAXP=256;
unsigned int   tNEVT;
unsigned short tTRG,tBBCE,tZDCE;
unsigned char tBC, tBBCM, tTOFM, tNP;
unsigned char tDet[MAXP];
unsigned short tId[MAXP];
float tX[MAXP], tY[MAXP];	   
float tPx[MAXP], tPy[MAXP], tPz[MAXP], tE[MAXP];   

int getFmsTrigId(const StTriggerId& trgid, int print=0){
    const int TIDBASE=480800;
    const int NBEAM=4;
    const int MAXVERSION=3;
    int trig=0;
    if(print) LOG_INFO << "TRGID = ";
    for(int k=0; k<NBEAM; k++){
        for(int j=0; j<MAXVERSION; j++){
            for(int i=1; i<=NTRG; i++){
                int l=i;
                if(i==12) l=13;
                int id=TIDBASE + 10000*k + 20*j + l;
                if(trgid.isTrigger(id)){
                    trig |= (1<<(i-1));
                    if(print)LOG_INFO << CTRG[i-1] << " ";
                }
            }
        }
    }
    if(print) LOG_INFO << endm;
    return trig;
}

double wrapAround(double phi){
    double res=phi;
    while(res>=1.5*PI) {res-=twoPI;}
    while(res<-0.5*PI) {res+=twoPI;}
    return res;
}

ClassImp(StFmsDiPi0);

StFmsDiPi0::StFmsDiPi0(const Char_t* name):
    StMaker(name),mFilename((char *)"fmsDiPi0.root") {}

StFmsDiPi0::~StFmsDiPi0(){}

Int_t StFmsDiPi0::Init(){  
    if(!mReadTree && !mPythia){
	mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
	if(!mFmsDbMaker){
	    LOG_ERROR  << "StFmsDiPi0::InitRun Failed to get StFmsDbMaker" << endm;
	    return kStFatal;
	}
    }

    if(!mWriteTree){
	mFile=new TFile(mFilename,"RECREATE");
	char c[100];
	mBC=new TH1F("BC","BC",120,0.0,120.0);
	mBBC=new TH1F("BBCE","BBCE",100,0.0,70000.0);
	mBBCAG=new TH1F("BBCEAG","BBCEAG",100,0.0,70000.0);
	mBBCM=new TH1F("BBCMult","BBCMult",17,0.0,17.0);
	mBBCMAG=new TH1F("BBCMultAG","BBCMultAG",17,0.0,17.0);
	mTOF=new TH1F("TOF","TOF",150,0.0,300.0);
	mTOFAG=new TH1F("TOFAG","TOFAG",150,0.0,300.0);
	mBBCTOF=new TH2F("BBC_TOF","BBC_TOF",50,0.0,70000.0,50,0.0,300.0);
	mBBCMTOF=new TH2F("BBCMult_TOF","BBCMult_TOF",17,0.0,17.0,50,0.0,300.0);
	mBBCBBCM=new TH2F("BBC_BBCMult","BBC_BBCMult",50,0.0,70000.0,17,0.0,17.0);
	mTOFTOF=new TH2F("TOF_TOF","TOF_TOF",50,0.0,300.0,50,0.0,300.0);

	mMass0=new TH2F("Mass0","Mass0",50,0.0,  0.4,16,0.0,16.0);
	mMass1=new TH2F("Mass1","Mass1",50,0.0,  0.4,16,0.0,16.0);
	mMass2=new TH2F("Mass2","Mass2",50,0.0,  0.4,16,0.0,16.0);
	mEne =new TH2F("Ene", "Ene",    50,0.0,100.0,16,0.0,16.0);
	mPt  =new TH2F("pT",  "pT",     50,0.0, 10.0,16,0.0,16.0);

	for(int i=0; i<kNPtBin; i++){
	    for(int j=0; j<=i; j++){
		for(int k=0; k<=kNCut; k++){
		    if(j==0){
			sprintf(c,"m0_%1d_c%d",i,k);    mM0[i][k]=new TH1F(c,c,50,0.0,0.4);
			sprintf(c,"phi0_%1d_c%d",i,k);  mPhi0[i][k]=new TH1F(c,c,128,-0.5*PI,1.5*PI);
			sprintf(c,"etaphi0_%1d_c%d",i,k);  mEtaPhi0[i][k]=new TH2F(c,c,50,2.5,4.5,64,-0.5*PI,1.5*PI);
		    }
		    sprintf(c,"m1_%1d%1d_c%d",i,j,k);   mM1[i][j][k]=new TH1F(c,c,50,0.0,0.4);
		    sprintf(c,"m2_%1d%1d_c%d",i,j,k);   mM2[i][j][k]=new TH1F(c,c,50,0.0,0.4);
		    sprintf(c,"z1_%1d%1d_c%d",i,j,k);   mZ1[i][j][k]=new TH1F(c,c,50,0.0,1.0);
		    sprintf(c,"z2_%1d%1d_c%d",i,j,k);   mZ2[i][j][k]=new TH1F(c,c,50,0.0,1.0);
		    sprintf(c,"e1_%1d%1d_c%d",i,j,k);   mE1[i][j][k]=new TH1F(c,c,50,0.0,100.0);
		    sprintf(c,"e2_%1d%1d_c%d",i,j,k);   mE2[i][j][k]=new TH1F(c,c,50,0.0,100.0);
		    sprintf(c,"pt1_%1d%1d_c%d",i,j,k);  mPt1[i][j][k]=new TH1F(c,c,50,0.0,10.0);
		    sprintf(c,"pt2_%1d%1d_c%d",i,j,k);  mPt2[i][j][k]=new TH1F(c,c,50,0.0,10.0);
		    sprintf(c,"eta1_%1d%1d_c%d",i,j,k); mEta1[i][j][k]=new TH1F(c,c,50,2.0,5.0);
		    sprintf(c,"eta2_%1d%1d_c%d",i,j,k); mEta2[i][j][k]=new TH1F(c,c,50,2.0,5.0);
		    sprintf(c,"phi1_%1d%1d_c%d",i,j,k); mPhi1[i][j][k]=new TH1F(c,c,128,-0.5*PI,1.5*PI);
		    sprintf(c,"phi2_%1d%1d_c%d",i,j,k); mPhi2[i][j][k]=new TH1F(c,c,128,-0.5*PI,1.5*PI);
		    sprintf(c,"dphi_%1d%1d_c%d",i,j,k); mDphi[i][j][k]=new TH1F(c,c,128,-0.5*PI,1.5*PI);
		    sprintf(c,"bbce_%1d%1d_c%d",i,j,k); mBbce[i][j][k]=new TH1F(c,c,50,0.0,70000.0);
		    sprintf(c,"tofm_%1d%1d_c%d",i,j,k); mTofm[i][j][k]=new TH1F(c,c,50,0.0,250.0);
		    sprintf(c,"phi1dphi_%1d%1d_c%d",i,j,k); mPhi1Dphi[i][j][k]=new TH2F(c,c,50,-0.5*PI,1.5*PI,50,-0.5*PI,1.5*PI);
		}
	    }
	}
    }

    if(mWriteTree){
	mTreeFile=new TFile(mTreeFilename,"recreate");
	mTree=new TTree("dipi0","dipi0 tree");
	mTree->Branch("trg",&tTRG,"trg/s");
	mTree->Branch("bbce",&tBBCE,"bbce/s");
	mTree->Branch("zdce",&tZDCE,"zdce/s");
	mTree->Branch("bc",&tBC,"bc/b");
	mTree->Branch("bbcm",&tBBCM,"bbcm/b");
	mTree->Branch("tofm",&tTOFM,"tofm/b");
	mTree->Branch("np",&tNP,"np/b");
	mTree->Branch("det",tDet,"det[np]/b");
	mTree->Branch("id",tId,"id[np]/s");
	mTree->Branch("e",tE,"e[np]/F");
	mTree->Branch("x",tX,"x[np]/F");
	mTree->Branch("y",tY,"y[np]/F");
	mTree->Branch("px",tPx,"px[np]/F");
	mTree->Branch("py",tPy,"py[np]/F");
	mTree->Branch("pz",tPz,"pz[np]/F");
    }

    if(mReadTree){
	//mTreeFile=new TFile(mTreeFilename);
	//mTree = (TTree*)mTreeFile->Get("dipi0");
	//tNEVT=mChain->GetEntries();
	//LOG_INFO << Form("Found %d entries in the input tree",tNEVT) << endm;
        mChain->SetBranchAddress("trg",&tTRG);
        mChain->SetBranchAddress("bbce",&tBBCE);
        mChain->SetBranchAddress("zdce",&tZDCE);
        mChain->SetBranchAddress("bc",&tBC);
        mChain->SetBranchAddress("bbcm",&tBBCM);
        mChain->SetBranchAddress("tofm",&tTOFM);
        mChain->SetBranchAddress("np",&tNP);
        mChain->SetBranchAddress("det",tDet);
        mChain->SetBranchAddress("id",tId);
        mChain->SetBranchAddress("e",tE);
        mChain->SetBranchAddress("x",tX);
        mChain->SetBranchAddress("y",tY);
        mChain->SetBranchAddress("px",tPx);
        mChain->SetBranchAddress("py",tPy);
        mChain->SetBranchAddress("pz",tPz);
        mFile->cd();	
    }
    return kStOK;
}

Int_t StFmsDiPi0::Finish(){
    if(!mWriteTree){
	LOG_INFO << Form("Writing and Closing %s",mFilename) << endm;
	mFile->Write();
	mFile->Close();
    }else{
	LOG_INFO << Form("Writing and Closing %s",mTreeFilename) << endm;
	mTreeFile->cd();
	mTreeFile->Write();
	mTreeFile->Close();
    }
    return kStOK;
}

Int_t StFmsDiPi0::ptbin(float pt){
    if(pt < mPtBin[0]) return -1;
    for(int i=0; i<kNPtBin; i++){
	if(pt >= mPtBin[i] && pt < mPtBin[i+1]) return i;
    }
    return kNPtBin;    
}

Int_t StFmsDiPi0::Make(){
    int print=0;
    int ag=0, bbcEsum=0, zdce=0, bbcmult=0, tofmul1=0, tofmul2=0, bsTrg=0, bsSTrg=0, bsLTrg=0, jetTrg=0, doubleTrg=0;
    int bunch=-1, trigger=-1;
    
    //LOG_INFO << "BBCCuts = " << mBBCCut1 <<" " << mBBCCut2 <<" "<< mBBCCut3 <<" "<< mBBCCut4 <<" "<<endm;
    if(mPythia==1){
	bsTrg=1;
    }else{
	if(mReadTree==1){
	    static unsigned int ievt=0;
	    //if(ievt>tNEVT) return kStEOF;
	    //mTreeFile->cd();
	    int ret=mChain->GetEntry(ievt++);	
	    if(ret==0) return kStEOF;
	    bunch=tBC;
	    bbcEsum=tBBCE;
	    zdce=tZDCE;
	    bbcmult=tBBCM;
	    tofmul2=tTOFM;
	    trigger=tTRG;
	    mFile->cd();
	}else{ //Reading from MuDst
	    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
	    if(!mudst) {LOG_ERROR << "StFmsDiPi0::Make did not find MuDst"<<endm; return kStErr;}
	    StMuEvent* mueve = mudst->event();
	    if(!mueve) {LOG_ERROR << "StFmsDiPi0::Make did not find MuEvent"<<endm; return kStErr;}
	    const StTriggerData* trgd=mueve->triggerData();
	    if(!trgd)  {LOG_ERROR << "StFmsDiPi0::Make did not find StTriggerData from MuDst"<<endm; return kStErr;}
	    //bunch and abort gap
	    bunch = trgd->bunchId7Bit();
	    //bbc east sum
	    for(int i=1; i<=16; i++){
		int tac=trgd->bbcTDC(east,i);
		if(tac>100 && tac<2400){
		    int adc=trgd->bbcADC(east,i);
		    if(adc>50) bbcmult++;
		    bbcEsum += trgd->bbcADC(east,i);
		}
	    } 
	    zdce=trgd->zdcAttenuated(east);
	    tofmul1=trgd->tofMultiplicity();
	    tofmul2=mudst->event()->btofTrayMultiplicity();
	    //triggers
	    trigger = getFmsTrigId(mudst->event()->triggerIdCollection().nominal());
	    if(print)
		LOG_INFO << Form("Run=%8d Event=%10d",mueve->runNumber(),mueve->eventNumber())<<endm;
	}		
	if     (bunch>=30  && bunch< 40) {ag=1;}
	else if(bunch>=110 && bunch<120) {ag=2;}   
	else if(bbcmult==0 || tofmul2<3) {ag=3;}   
	if(trigger & 0x03f) bsTrg=1;
	if(trigger & 0x007) bsSTrg=1;
	if(trigger & 0x038) bsLTrg=1;
	if(trigger & 0x380) jetTrg=1;
	if(trigger & 0x440) doubleTrg=1;

	if(!mWriteTree){
	    mBC->Fill(float(bunch));
	    if(ag==0 || ag==3){
		mBBC->Fill(float(bbcEsum));
		mBBCM->Fill(float(bbcmult));
		mTOF->Fill(float(tofmul2));
	    }else if(ag==1){
		mBBCAG->Fill(float(bbcEsum));
		mBBCMAG->Fill(float(bbcmult));
		mTOFAG->Fill(float(tofmul2));
	    }
	    mBBCTOF->Fill(bbcEsum,tofmul2);
	    mBBCMTOF->Fill(bbcmult,tofmul2);
	    mBBCBBCM->Fill(bbcEsum,bbcmult);
	    mTOFTOF->Fill(tofmul1,tofmul2);
	}
	if(print)
	    LOG_INFO << Form("Bunch=%3d AGap=%1d BbcEsum=%6d Tof=%6d | %6d",			     
			     bunch,ag,bbcEsum,tofmul1,tofmul2) << endm;    
    }

    int np=0;
    StPtrVecFmsPoint pcut;
    if(mReadTree>0){
	np=tNP;
	for(int i=0; i<np; i++){		
	    StFmsPoint* point = new StFmsPoint();
	    point->setDetectorId(tDet[i]);
	    point->setId(tId[i]);
	    point->setEnergy(tE[i]);
	    point->setX(tX[i]);
	    point->setX(tY[i]);
	    point->setFourMomentum(StLorentzVectorF(tPx[i],tPy[i],tPz[i],tE[i]));
	    pcut.push_back(point);
	}
    }else{	
	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if(!event) {LOG_ERROR << "StFmsDiPi0::Make did not find StEvent"<<endm; return kStErr;}
	mFmsColl = event->fmsCollection();
	if(!mFmsColl) {LOG_ERROR << "StFmsDiPi0::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}    
	StSPtrVecFmsPoint &points = mFmsColl->points();
	np=mFmsColl->numberOfPoints();
	if(print) cout << Form("Npoint = %d",np) << endl;
	//FV cut and minimal energy/pT cut for points
	for(int i=0; i<np; i++){
	    int edgeType;
	    float distance=mFmsDbMaker->distanceFromEdge(points[i],edgeType);
	    if(mPythia || distance<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType==4)){ //FV cut
		if(mPythia || (points[i]->energy()>energyCut && points[i]->fourMomentum().perp()>ptCut)){ //minimal E and pT cuts
		    pcut.push_back(points[i]);		
		}
	    }
	}
    }

    if(print) cout << Form("Ncut = %d",pcut.size()) << endl;
    if(pcut.size()>1){
	//sort by pT
	std::sort(pcut.begin(), pcut.end(), [](StFmsPoint* a, StFmsPoint* b) {
		return b->fourMomentum().perp() < a->fourMomentum().perp();
	    });
	//pair
	vector<StFmsPointPair*> pair;
	std::vector<int> pcutuse(pcut.size(), 0);
	for(unsigned int i=0; i<pcut.size()-1; i++){
	    for(unsigned int j=i+1; j<pcut.size(); j++){
		StFmsPointPair* pp = new StFmsPointPair(pcut[i],pcut[j]);
		if(mPythia || (pp->zgg() < ZggCut && pp->pT() > mPtBin[0])){
		    pair.push_back(pp);
		    pcutuse[i]=1;
		    pcutuse[j]=1;
		}else{
		    delete pp;
		}
	    }
	}
	if(print) cout << Form("Npair = %d",pair.size()) << endl;
	if(pair.size()>0) {	
	    
	    // write Tree file
	    if(mWriteTree){
		mTreeFile->cd();
		tBC=bunch;	   		
		tTRG=trigger;	   
		tBBCM=bbcmult;	   
		tTOFM=tofmul2;	   
		tBBCE=bbcEsum;	   
		tZDCE=zdce;	   
		tNP=pcut.size();	   
		for(unsigned int i=0; i<pcut.size(); i++){		
		    if(pcutuse[i]==1){
			tDet[i]=pcut[i]->detectorId();
			tId[i]=pcut[i]->id();
			tE[i]=pcut[i]->energy();;
			tX[i]=pcut[i]->x();
			tY[i]=pcut[i]->y();
			tPx[i]=pcut[i]->fourMomentum().px();
			tPy[i]=pcut[i]->fourMomentum().py();
			tPz[i]=pcut[i]->fourMomentum().pz();
		    }
		}
		mTree->Fill();
		return kStOK; //exit if writing TTree
	    }
	    
	    //sort by pT of pair
	    std::sort(pair.begin(), pair.end(), [](StFmsPointPair* a, StFmsPointPair* b) {
		    return b->pT() < a->pT();
		});
	    if(print){
		for(unsigned int i=0; i<pair.size(); i++){
		    LOG_INFO << Form("%3d m=%6.3f e=%6.2f pt=%6.2f eta=%6.2f phi=%6.3f zgg=%6.3f id=%4d %4d",
				     i,
				     pair[i]->mass(), pair[i]->energy(),pair[i]->pT(), 
				     pair[i]->eta(), pair[i]->phi(), pair[i]->zgg(),
				     pair[i]->point(0)->id(), pair[i]->point(1)->id() ) << endm;
		}
	    }
	    //pair of pair (4 photons or 2 pairs)
	    int ndipi0=0;
	    int firstpair=0;
	    int used1[2000],used2[2000],used3[2000],used4[2000];
	    memset(used1,0,sizeof(used1)); //pi0-pi0
	    memset(used2,0,sizeof(used2)); //pi0-highmass
	    memset(used3,0,sizeof(used3)); //highmass-pi0
	    memset(used4,0,sizeof(used4)); //highmass-highmass

	    for(unsigned int i=0; i<pair.size(); i++){ //loop over 1st pair
		int ptbin0=ptbin(pair[i]->pT());
		if(ptbin0<0) continue; //below lowest pt bin

		int id0=pair[i]->point(0)->id();
		int id1=pair[i]->point(1)->id();
		float m0=pair[i]->mass();
		float z0=pair[i]->zgg();
		float p0=wrapAround(pair[i]->phi());
		int topbot=1; //bottom
		if(p0>=0.0 && p0<PI) topbot=0;//top

		//QA to look for mass/pt/energy for each octet
		float pp=p0;
		while(pp<0.0) pp+=twoPI;
		while(pp>=twoPI) pp-=twoPI;
		int ii=int(8.0*pp/twoPI);
		int dd=pair[i]->point(0)->detectorId();
		if(dd==kFmsNorthSmallDetId || dd==kFmsSouthSmallDetId) ii+=8;
		mMass0->Fill(m0,float(ii));
		if(pair[i]->pT()>1.5) mMass1->Fill(m0,float(ii));
		if(pair[i]->energy()>30.0) mMass2->Fill(m0,float(ii));
		if(m0>=MassCut0 && m0<MassCut1){
		    mEne->Fill(pair[i]->energy(),float(ii));
		    mPt->Fill(pair[i]->pT(),float(ii));
		}

		if(ptbin0>=kNPtBin) continue; //above highest pt bin
		int cut1[kNCut];
		memset(cut1,0,sizeof(cut1));

		//pi0 mass and highmass
		int masscut1=0;
		if(m0>=MassCut0 && m0<MassCut1)      {masscut1=1;}
		else if(m0>=MassCut1 && m0<MassCut2) {masscut1=2;}

		//4 exclusiveness for pi0-pi0, pi0-hightmass, highmass-pi0, highmass-highmass
		int exclusive1=0, exclusive2=0, exclusive3=0, exclusive4=0;
		if(masscut1==1 && ((used1[id0]==0 && used1[id1]==0) || (used1[id0]==id1 && used1[id1]==id0))) {exclusive1=1; used1[id0]=id1; used1[id1]=id0;}	
		if(masscut1==1 && ((used2[id0]==0 && used2[id1]==0) || (used2[id0]==id1 && used2[id1]==id0))) {exclusive2=1; used2[id0]=id1; used2[id1]=id0;} 
		if(masscut1==2 && ((used3[id0]==0 && used3[id1]==0) || (used3[id0]==id1 && used3[id1]==id0))) {exclusive3=1; used3[id0]=id1; used3[id1]=id0;}
		if(masscut1==2 && ((used4[id0]==0 && used4[id1]==0) || (used4[id0]==id1 && used4[id1]==id0))) {exclusive4=1; used4[id0]=id1; used4[id1]=id0;}

		//setup cuts
		if(ag==0 && bsTrg==1){
		    cut1[0]=1;
		    if(masscut1==1){
			cut1[1]=1;
			if(exclusive1==1){
			    cut1[2]=1;
			    if     (bbcEsum<mBBCCut1) {cut1[3]=1;}
			    else if(bbcEsum<mBBCCut2) {cut1[4]=1;}
			    else if(bbcEsum<mBBCCut3) {cut1[5]=1;}
			    else if(bbcEsum<mBBCCut4) {cut1[15]=1;}
			    else                      {cut1[16]=1;}
			}
		    }
		}
		if(ag==1 && bsTrg==1     && exclusive1) cut1[6]=1;
		if(ag==3 && bsTrg==1     && exclusive1) cut1[17]=1;
		if(ag==0 && jetTrg==1    && exclusive1) cut1[7]=1;
		if(ag==0 && doubleTrg==1 && exclusive1) cut1[8]=1;
		if(ag==0 && bsTrg==1     && exclusive2) cut1[9]=1;  //this is for pi0-highmass pair
		if(ag==0 && bsTrg==1     && exclusive3) cut1[10]=1; //this is for highmass-pi0 pair
		if(ag==0 && bsTrg==1     && exclusive4) cut1[11]=1; //this is for highmass-highmass pair
		if(ag==0 && bsSTrg==1    && exclusive1) cut1[12]=1;
		if(ag==0 && bsLTrg==1    && exclusive1) cut1[13]=1;
		if(ag==0 && bsTrg==1     && exclusive1 && firstpair==0) cut1[14]=1;
		if(ag==0 && bsTrg==1     && exclusive1 && topbot==0) cut1[18]=1;
		if(ag==0 && bsTrg==1     && exclusive1 && topbot==1) cut1[19]=1;

		for(unsigned int k=0; k<kNCut; k++){
		    if(cut1[k]==1){
			mM0[ptbin0][k]->Fill(m0); //this is for normalization
			mPhi0[ptbin0][k]->Fill(p0); //this is for mix event
			mEtaPhi0[ptbin0][k]->Fill(pair[i]->eta(),p0);
		    }
		}
		//loop over 2nd pair
		for(unsigned int j=i+1; j<pair.size(); j++){
		    if(i==j) continue; //same pair
		    int ptbin1=ptbin(pair[j]->pT());
		    if(ptbin1<0) continue; //below lowest pt bin
		    int id2=pair[j]->point(0)->id();
		    int id3=pair[j]->point(1)->id();
		    if(id0==id2 || id0==id3 || id1==id2 || id1==id3) continue; //using same photon
		    float dphi = wrapAround(pair[i]->phi() - pair[j]->phi());
		    float m1=pair[j]->mass();
		    float z1=pair[j]->zgg();
		    float p1=wrapAround(pair[j]->phi());
		    int cut2[kNCut];
		    memset(cut2,0,sizeof(cut2));
		    int masscut2=0;
		    if     (m1>=MassCut0 && m1<MassCut1) {masscut2=1;}
		    else if(m1>=MassCut1 && m1<MassCut2) {masscut2=2;}

		    int exc1=0,exc2=0, exc3=0, exc4=0;
		    if(masscut2==1 && exclusive1 &&
		       ((used1[id2]==0 && used1[id3]==0) || (used1[id2]==id3 && used1[id3]==id2)) ){
			used1[id2]=id3; used1[id3]=id2;
			exc1=1;		    
		    }		
		    if(masscut2==2 && exclusive2 &&
		       ((used2[id2]==0 && used2[id3]==0) || (used2[id2]==id3 && used2[id3]==id2)) ){
			used1[id2]=id3; used2[id3]=id2;
			exc2=1;		    
		    }		
		    if(masscut2==1 && exclusive3 &&
		       ((used3[id2]==0 && used3[id3]==0) || (used3[id2]==id3 && used3[id3]==id2)) ){
			used3[id2]=id3; used3[id3]=id2;
			exc3=1;		    
		    }		
		    if(masscut2==2 && exclusive4 &&
		       ((used4[id2]==0 && used4[id3]==0) || (used4[id2]==id3 && used4[id3]==id2)) ){
			used4[id2]=id3; used4[id3]=id2;
			exc4=1;		    
		    }		

		    if(ag==0 && bsTrg==1){
			cut2[0]=1;
			if(masscut2==1){
			    cut2[1]=1;
			    if(exc1==1){
				cut2[2]=1;
				if     (bbcEsum<mBBCCut1) {cut2[3]=1;}
				else if(bbcEsum<mBBCCut2) {cut2[4]=1;}
				else if(bbcEsum<mBBCCut3) {cut2[5]=1;}
				else if(bbcEsum<mBBCCut4) {cut2[15]=1;}
				else                      {cut2[16]=1;}
			    }
			}
		    }
		    if(ag==1 && bsTrg==1     && exc1) cut2[6]=1;
		    if(ag==3 && bsTrg==1     && exc1) cut2[17]=1;
		    if(ag==0 && jetTrg==1    && exc1) cut2[7]=1;
		    if(ag==0 && doubleTrg==1 && exc1) cut2[8]=1;
		    if(ag==0 && bsTrg==1     && exc2) cut2[9]=1;  //this is for pi0-highmass pair         
		    if(ag==0 && bsTrg==1     && exc3) cut2[10]=1; //this is for highmass-pi0 pair         
		    if(ag==0 && bsTrg==1     && exc4) cut2[11]=1; //this is for highmass-highmass pair         
		    if(ag==0 && bsSTrg==1    && exc1) cut2[12]=1;
		    if(ag==0 && bsLTrg==1    && exc1) cut2[13]=1;
		    if(ag==0 && bsTrg==1     && exc1 && firstpair==0) {firstpair=1;cut2[14]=1;}
		    if(ag==0 && bsTrg==1     && exc1 && topbot==0) cut2[18]=1;
		    if(ag==0 && bsTrg==1     && exc1 && topbot==1) cut2[19]=1;
		    
		    //filling histos
		    for(unsigned int k=0; k<kNCut; k++){
			if(cut2[k]==1){
			    mM1[ptbin0][ptbin1][k]->Fill(m0);
			    mM2[ptbin0][ptbin1][k]->Fill(m1);
			    mZ1[ptbin0][ptbin1][k]->Fill(z0);
			    mZ2[ptbin0][ptbin1][k]->Fill(z1);
			    mE1[ptbin0][ptbin1][k]->Fill(pair[i]->energy());
			    mE2[ptbin0][ptbin1][k]->Fill(pair[j]->energy());
			    mPt1[ptbin0][ptbin1][k]->Fill(pair[i]->pT());
			    mPt2[ptbin0][ptbin1][k]->Fill(pair[j]->pT());
			    mEta1[ptbin0][ptbin1][k]->Fill(pair[i]->eta());
			    mEta2[ptbin0][ptbin1][k]->Fill(pair[j]->eta());
			    mPhi1[ptbin0][ptbin1][k]->Fill(p0);
			    mPhi2[ptbin0][ptbin1][k]->Fill(p1);
			    mDphi[ptbin0][ptbin1][k]->Fill(dphi);
			    mBbce[ptbin0][ptbin1][k]->Fill(bbcEsum);
			    mPhi1Dphi[ptbin0][ptbin1][k]->Fill(p0,dphi);
			}
		    } 	    
		    if(print) LOG_INFO << Form("diPair=%3d pair=%2d %2d  dphi=%6.3f ptbin=%1d %1d masscut=%1d ag=%1d exc=%1d",
					       ndipi0,i,j,dphi,ptbin0,ptbin1,masscut2,ag,exclusive2)<<endm;
		    ndipi0++;	    
		}
	    }    	    
	}
	for(unsigned int i=0; i<pair.size(); i++) delete pair[i];
    }
    if(mReadTree){ 
	for(unsigned int i=0; i<pcut.size(); i++) delete pcut[i];
    }

    //Pythia events, so we know real pi0
    if(mPythia){
	StFmsFastSimMaker* fmssim = (StFmsFastSimMaker*)GetMaker("FmsFastSimMaker");
	if(!fmssim){
	    LOG_ERROR << "Did not find FmsFastSimMaker!!!" << endm;
	    return kStErr;
	}
	int n=fmssim->nPi0();
	if(n>1){
	    for(int i=0; i<n; i++){
		int ptbin0=ptbin(fmssim->pi0(i)->perp());
		if(ptbin0<0) continue; //below lowest pt bin
		mM0[ptbin0][kNCut]->Fill(0.135); //this is for normalization
		if(print)
		    cout << Form("RealPi0 n=%2d pt=%6.2f eta=%6.2f phi=%6.3f",
				 n,fmssim->pi0(i)->perp(),
				 fmssim->pi0(i)->pseudoRapidity(),fmssim->pi0(i)->phi())<<endl;
		for(int j=i+1; j<n; j++){
		    if(i==j) continue;
		    int ptbin1=ptbin(fmssim->pi0(j)->perp());
		    if(ptbin1<0) continue; //below lowest pt bin
		    float phi1=wrapAround(fmssim->pi0(i)->phi());
		    float phi2=wrapAround(fmssim->pi0(j)->phi());
		    float dphi=wrapAround(phi1-phi2);
                    mM1[ptbin0][ptbin1][kNCut]->Fill(0.135);
                    mM2[ptbin0][ptbin1][kNCut]->Fill(0.135);
                    mPt1[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(i)->perp());
                    mPt2[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(j)->perp());
		    mE1[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(i)->e());
		    mE2[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(j)->e());
                    mEta1[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(i)->pseudoRapidity());
                    mEta2[ptbin0][ptbin1][kNCut]->Fill(fmssim->pi0(j)->pseudoRapidity());
                    mPhi1[ptbin0][ptbin1][kNCut]->Fill(phi1);
                    mPhi2[ptbin0][ptbin1][kNCut]->Fill(phi2);
                    mDphi[ptbin0][ptbin1][kNCut]->Fill(dphi);
		    if(print) LOG_INFO << Form("RealPi0Pair %d %d dphi=%6.3f",i,j,dphi) << endm;
		}
	    }
	}
    }
    return kStOK;
}

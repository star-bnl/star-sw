// \class StFmsDiPi0
// \author Akio Ogawa
//
//  $Id: StFmsDiPi0.cxx,v 1.2 2016/06/08 16:28:09 akio Exp $
//  $Log: StFmsDiPi0.cxx,v $
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

const float mPtBin[7]={0.5,1.0,1.5,2.0,2.5,3.0,20.0};
const float PI=3.14159265358979323846;
const float energyCut=1.0;
const float ptCut=0.1;
const float ZggCut=0.7;
const float MassCut0=0.07;
const float MassCut1=0.2;
const float MassCut2=0.35;
const float BBCCut1= 3000.0;
const float BBCCut2=30000.0;

static const int NTRG=12;  //123=FMS-sm-bs123,456=FMS-lg-bs123,7=FMS-DiBS,8910=FMS-JP012,11=FMS-DiJP,13=LED                              
static const char *CTRG[NTRG] = {"SmBS1","SmBS2","SmBS3","LgBS1",
				 "LgBS2","LgBS3","DiBS" ,"JP2",
				 "JP1"  ,"JP0"  ,"DiJP" ,"LED"};
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

ClassImp(StFmsDiPi0);

StFmsDiPi0::StFmsDiPi0(const Char_t* name):
    StMaker(name),mFilename((char *)"fmsDiPi0.root") {}

StFmsDiPi0::~StFmsDiPi0(){}

Int_t StFmsDiPi0::Init(){  
    mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
    if(!mFmsDbMaker){
	LOG_ERROR  << "StFmsDiPi0::InitRun Failed to get StFmsDbMaker" << endm;
	if(mPythia==0) return kStFatal;
    }

    mFile=new TFile(mFilename,"RECREATE");
    char c[100];
    mBC=new TH1F("BC","BC",120,0.0,120.0);
    mBBC=new TH1F("BBCE","BBCE",100,0.0,70000.0);
    for(int i=0; i<kNPtBin; i++){
	for(int j=0; j<=i; j++){
	    for(int k=0; k<=kNCut; k++){
		if(j==0){
		    sprintf(c,"m0_%1d_c%d",i,k);  mM0[i][k]=new TH1F(c,c,50,0.0,0.4);
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
		sprintf(c,"phi1_%1d%1d_c%d",i,j,k); mPhi1[i][j][k]=new TH1F(c,c,50,0.0,2*PI);
		sprintf(c,"phi2_%1d%1d_c%d",i,j,k); mPhi2[i][j][k]=new TH1F(c,c,50,0.0,2*PI);
		sprintf(c,"dphi_%1d%1d_c%d",i,j,k); mDphi[i][j][k]=new TH1F(c,c,50,-0.5*PI,1.5*PI);
	    }
	}
    }
    return kStOK;
}

Int_t StFmsDiPi0::Finish(){
    LOG_INFO << Form("Writing and Closing %s",mFilename) << endm;
    mFile->Write();
    mFile->Close();
    return kStOK;
}

Int_t StFmsDiPi0::ptbin(float pt){
    for(int i=0; i<kNPtBin; i++){
	if(pt >= mPtBin[i] && pt < mPtBin[i+1]) return i;
    }
    return -1;
}

Int_t StFmsDiPi0::Make(){
    int print=1;
    int ag=0, bbcEsum=0, bsTrg=0, bsSTrg=0, bsLTrg=0, jetTrg=0, doubleTrg=0;

    if(mPythia==0){
	int bunch=-1;
	StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
	if(!mudst) {LOG_ERROR << "StFmsDiPi0::Make did not find MuDst"<<endm; return kStErr;}
	const StTriggerData* trgd=mudst->event()->triggerData();
	if(!trgd)  {LOG_ERROR << "StFmsDiPi0::Make did not find StTriggerData from MuDst"<<endm; return kStErr;}
	//bunch and abort gap
	bunch = trgd->bunchId7Bit();
	if     (bunch>=30  && bunch< 40) {ag=1;}
	else if(bunch>=110 && bunch<120) {ag=2;}   
	//bbc east sum
	for(int i=0; i<16; i++){
	    int tac=trgd->bbcTDC(east,i);
	    if(tac>100 && tac<2400){
		bbcEsum += trgd->bbcADC(east,i);
	    }
	} 
	mBC->Fill(float(bunch));
	mBBC->Fill(float(bbcEsum));	
	LOG_INFO << Form("Run=%8d Event=%10d Bunch=%3d AGap=%1d BbcEsum=%6d",
			 mudst->event()->runNumber(),mudst->event()->eventNumber(),
			 bunch,ag,bbcEsum) << endm;    
	//triggers
	int trigger = getFmsTrigId(mudst->event()->triggerIdCollection().nominal());
	if(trigger & 0x03f) bsTrg=1;
	if(trigger & 0x007) bsSTrg=1;
	if(trigger & 0x038) bsLTrg=1;
	if(trigger & 0x380) jetTrg=1;
	if(trigger & 0x440) doubleTrg=1;
    }else{
	bsTrg=1;
    }

    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFmsDiPi0::Make did not find StEvent"<<endm; return kStErr;}
    mFmsColl = event->fmsCollection();
    if(!mFmsColl) {LOG_ERROR << "StFmsDiPi0::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}    
    StSPtrVecFmsPoint& points = mFmsColl->points(); 
    int np=mFmsColl->numberOfPoints();
    
    if(print) cout << Form("Npoint = %d",np) << endl;
    //FV cut and minimal energy/pT cut for points
    StPtrVecFmsPoint pcut;
    for(int i=0; i<np; i++){
	int edgeType;
	float distance=mFmsDbMaker->distanceFromEdge(points[i],edgeType);
	if(mPythia || distance<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType==4)){ //FV cut
	    if(mPythia || (points[i]->energy()>energyCut && points[i]->fourMomentum().perp()>ptCut)){ //minimal E and pT cuts
		pcut.push_back(points[i]);		
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
	for(unsigned int i=0; i<pcut.size()-1; i++){
	    for(unsigned int j=i+1; j<pcut.size(); j++){
		StFmsPointPair* pp = new StFmsPointPair(pcut[i],pcut[j]);
		if(mPythia || (pp->zgg() < ZggCut && pp->pT() > mPtBin[0])){
		    pair.push_back(pp);
		}
	    }
	}
	if(print) cout << Form("Npair = %d",pair.size()) << endl;
	if(pair.size()>0) {	
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
			    if     (bbcEsum<BBCCut1) {cut1[3]=1;}
			    else if(bbcEsum<BBCCut2) {cut1[4]=1;}
			    else                     {cut1[5]=1;}
			}
		    }
		}
		if(ag==1 && bsTrg==1     && exclusive1) cut1[6]=1;
		if(ag==0 && jetTrg==1    && exclusive1) cut1[7]=1;
		if(ag==0 && doubleTrg==1 && exclusive1) cut1[8]=1;
		if(ag==0 && bsTrg==1     && exclusive2) cut1[9]=1;  //this is for pi0-highmass pair
		if(ag==0 && bsTrg==1     && exclusive3) cut1[10]=1; //this is for highmass-pi0 pair
		if(ag==0 && bsTrg==1     && exclusive4) cut1[11]=1; //this is for highmass-highmass pair
		if(ag==0 && bsSTrg==1    && exclusive1) cut1[12]=1;
		if(ag==0 && bsLTrg==1    && exclusive1) cut1[13]=1;
		if(ag==0 && bsTrg==1     && exclusive1 && firstpair==0) cut1[14]=1;
		
		for(unsigned int k=0; k<kNCut; k++){
		    if(cut1[k]==1){
			mM0[ptbin0][k]->Fill(m0); //this is for normalization
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
		    float dphi = pair[i]->phi() - pair[j]->phi();
		    while(dphi < 0.5*PI) dphi+=2.0*PI;
		    while(dphi > 1.5*PI) dphi-=2.0*PI;
		    float m1=pair[j]->mass();
		    float z1=pair[j]->zgg();
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
				if     (bbcEsum<BBCCut1) {cut2[3]=1;}
				else if(bbcEsum<BBCCut2) {cut2[4]=1;}
				else                     {cut2[5]=1;}
			    }
			}
		    }
		    if(ag==1 && bsTrg==1     && exc1) cut2[6]=1;
		    if(ag==0 && jetTrg==1    && exc1) cut2[7]=1;
		    if(ag==0 && doubleTrg==1 && exc1) cut2[8]=1;
		    if(ag==0 && bsTrg==1     && exc2) cut2[9]=1;  //this is for pi0-highmass pair         
		    if(ag==0 && bsTrg==1     && exc3) cut2[10]=1; //this is for highmass-pi0 pair         
		    if(ag==0 && bsTrg==1     && exc4) cut2[11]=1; //this is for highmass-highmass pair         
		    if(ag==0 && bsSTrg==1    && exc1) cut2[12]=1;
		    if(ag==0 && bsLTrg==1    && exc1) cut2[13]=1;
		    if(ag==0 && bsTrg==1     && exc1 && firstpair==0) {firstpair=1;cut2[14]=1;}
		    
		    //filling histos
		    float p1=pair[i]->phi();
		    while(p1>2.0*PI) p1-=2.0*PI;
		    while(p1<0.0)    p1+=2.0*PI;
		    float p2=pair[j]->phi();
		    while(p2>2.0*PI) p2-=2.0*PI;
		    while(p2<0.0)    p2+=2.0*PI;
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
			    mPhi1[ptbin0][ptbin1][k]->Fill(p1);
			    mPhi2[ptbin0][ptbin1][k]->Fill(p2);
			    mDphi[ptbin0][ptbin1][k]->Fill(dphi);
			}
		    } 	    
		    if(print) LOG_INFO << Form("diPair=%3d pair=%2d %2d  dphi=%6.3f ptbin=%1d %1d masscut=%1d ag=%1d exc=%1d",
					       ndipi0,i,j,dphi,ptbin0,ptbin1,masscut2,ag,exclusive2)<<endm;
		    ndipi0++;	    
		}
	    }    	    
	}
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
		    float phi1=fmssim->pi0(i)->phi();
		    float phi2=fmssim->pi0(j)->phi();
		    float dphi=phi1-phi2;
		    while(dphi < 0.5*PI) dphi+=2.0*PI;
		    while(dphi > 1.5*PI) dphi-=2.0*PI;
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

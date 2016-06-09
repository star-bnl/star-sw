// \class StFmsFastSimMaker
// \author Akio Ogawa
//
//  $Id: StFmsFastSimMaker.cxx,v 1.1 2016/06/09 12:17:42 akio Exp $
//  $Log: StFmsFastSimMaker.cxx,v $
//  Revision 1.1  2016/06/09 12:17:42  akio
//  First version
//
// 

#include "StFmsFastSimMaker.h"

#include "StMessMgr.h"
//#include "Stypes.h"

#include "StThreeVectorF.hh"
#include "StEnumerations.h"
//#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsPoint.h"

#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include "TRandom2.h"

float projectFMS(float x, float z, float zp, float vz){
    //LOG_INFO << Form("x=%f z=%f zp=%f vz=%f x/z=%f xp=%f",x,z,zp,vz,x/z,x/z*zp)<<endm;
    return x/z*(zp-vz);
}

static const float ZFMS=720.0;
static const float XMINFMS=20.0;
static const float XMAXFMS=100.0;
int fmsAcceptance(StarGenParticle *p){
    //LOG_INFO << Form("pz=%f",p->GetPz())<<endm;
    if(p->GetPz()<1.0) return 0; //cut at +1GeV    
    float x=fabs(projectFMS(p->GetPx(),p->GetPz(),ZFMS,p->GetVz()));
    float y=fabs(projectFMS(p->GetPy(),p->GetPz(),ZFMS,p->GetVz()));
    //LOG_INFO << Form("x=%f y=%f",x,y)<<endm;
    if(x<XMINFMS && y<XMINFMS) return 0;
    if(x>XMAXFMS || y>XMAXFMS) return 0;
    return 1; 
}

ClassImp(StFmsFastSimMaker);

StFmsFastSimMaker::StFmsFastSimMaker(const Char_t* name):StMaker(name),mPrint(0) {}

StFmsFastSimMaker::~StFmsFastSimMaker(){}   

Int_t StFmsFastSimMaker::Init(){  
    return kStOK;
}
 
void StFmsFastSimMaker::Clear(Option_t *option){  
    int n=mRealPi0.size();
    for(int i=0; i<n; i++) delete mRealPi0[i];
    mRealPi0.clear();
}

Float_t StFmsFastSimMaker::hadronResponse(float e, float &f){
    static TRandom2 RND;
    if(e<0.3) {f=1.0; return e;}
    f=0.3/e;
    if(RND.Rndm()<0.5) {
	return 0.3;
    }else{
	while(f<=0.3/e || f>0.95) f = RND.Gaus()*0.3 + 0.35;
	return e*f;
    }
}

Int_t StFmsFastSimMaker::Make(){
    StarPrimaryMaker* primary=(StarPrimaryMaker*)GetMaker("PrimaryMaker");
    if(!primary) {LOG_INFO << "StFmsFastSimMaker cannot find PrimaryMaker"<<endm; return kStErr;}
    StarGenEvent* pevent = primary->event();
    if(!pevent) {LOG_INFO << "StFmsFastSimMaker cannot find PrimaryMaker->event"<<endm; return kStErr;}

    StEvent* stEvent = (StEvent*) GetInputDS("StEvent");
    if(!stEvent){
	stEvent = new StEvent();
	AddData(stEvent);
    }
    StFmsCollection* fmsColl = new StFmsCollection();    
    stEvent->setFmsCollection(fmsColl);
    StSPtrVecFmsPoint& points = fmsColl->points(); 
    
    int n=pevent->GetNumberOfParticles();
    float etot=0.0, emeas=0.0;
    for(int j=0; j<n; j++){	
	StarGenParticle *p = (*pevent)[j];
	int flag=0;
	int pid=p->GetId();
	int stat=p->GetStatus();
	float f=1.0;
	float hadres=0.0;
	if(fmsAcceptance(p)==0) {
	    flag=1;
	}else if(pid==111){ //pi0	    
	    mRealPi0.push_back(new StLorentzVectorF(p->GetPx(),p->GetPy(),p->GetPz(),p->GetEnergy()));
	    flag=2;
	}else if(stat==1){
	    float ene=p->GetEnergy();
	    if(abs(pid)==22 || abs(pid)==11){ //photon & electron
		StFmsPoint* point = new StFmsPoint();
		point->setId(j);
		point->setEnergy(ene);
		point->setFourMomentum(StLorentzVectorF(p->GetPx(),p->GetPy(),p->GetPz(),ene));
		points.push_back(point);
		etot+=ene;
		emeas+=ene;
		flag=3;
	    }else if(abs(pid)==211 || abs(pid)==321 || abs(pid)==2212 || abs(pid)==2112){
		// /*
		StFmsPoint* point = new StFmsPoint();
		hadres = hadronResponse(ene,f);
		point->setId(j);
		point->setEnergy(hadres);
		point->setFourMomentum(StLorentzVectorF(f*p->GetPx(),f*p->GetPy(),f*p->GetPz(),hadres));
		points.push_back(point);		
		etot+=ene;
		emeas+=hadres;
		//  */
		flag=4;
	    }else{
		etot+=ene;
		flag=5;
	    }	
	}else{
	    flag=6;
	}
	if(flag==1 || flag==6) continue;
	if(flag>0 && mPrint>1) {
	    cout << Form("%3d Pid=%5d Stat=%3d m=%8.3f p=%7.3f %7.3f %7.3f eta=%6.2f e=%7.2f meas=%7.2f zvtx=%7.2f",
			 j,pid,stat,p->GetMass(),p->GetPx(),p->GetPy(),p->GetPz(),
			 p->momentum().PseudoRapidity(),
			 p->GetEnergy(), f*p->GetEnergy(), p->GetVz());
	    switch(flag){
	    case 1: cout << " Not heading to FMS" << endl; break;
	    case 2: cout << " pi0" << endl; break;
	    case 3: cout << " photon/electron" << endl; break;
	    case 4: cout << Form(" pi/K/P/N f=%4.2f",f) << endl; break;
	    case 5: cout << " Something else pid=" << pid << endl; break;
	    case 6: cout << " Unstable" << endl; break;
	    default: cout << endl;
	    }
	}
    }

    //sort real pi0 in pT order
    std::sort(mRealPi0.begin(), mRealPi0.end(), [](StLorentzVectorF* a, StLorentzVectorF* b) {
	    return b->perp() < a->perp();
	});    
    
    //merging points
     
    int np=fmsColl->numberOfPoints();
    LOG_INFO << Form("Etot=%f Emeasured=%f Created %d StFmsPoints, and NRealPi0=%d",etot,emeas,np,mRealPi0.size()) << endm;
    
   return kStOK;    
}

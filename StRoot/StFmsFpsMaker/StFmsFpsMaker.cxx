// \class StFmsFpsMaker
// \author Akio Ogawa
//
//  $Id: StFmsFpsMaker.cxx,v 1.8 2016/06/07 18:00:52 akio Exp $
//  $Log: StFmsFpsMaker.cxx,v $
//  Revision 1.8  2016/06/07 18:00:52  akio
//  Removing data member initialization from constructor (moved to c++11 style initialization in .h	file)
//  Moving project() and distance() as private member function (from global scope)
//  Adding "const" for many of local variables
//  Adding "unsigned" for loop variables for consistency
//  Removing hardcoded zvertex=0.0 and replaced with mMeanVertexZ which is defaulted to 0 and settable via setMeanVertexZ(float v)
//  Removed obsoluted lines
//  Added some local const variable for loop condition
//  Remove commented out cout for debugging, or replaced with LOG_DEBUG
//
//  Revision 1.7  2016/01/26 19:53:14  akio
//  Drop QA/alignment histograms and just do the analysis/filling StEvent structure
//  Offline QA and Alignment histograms will be moved to StRoot/StSpinPool/StFmsOfflineQaMaker
//
//  Revision 1.6  2015/12/08 17:00:03  akio
//  updates
//
//  Revision 1.5  2015/10/29 21:22:01  akio
//  more cuts for QA
//
//  Revision 1.4  2015/10/21 15:51:01  akio
//  Add more QA for debugging FMS reconstruciton code
//
//  Revision 1.3  2015/09/18 18:50:06  akio
//  cleaning up and adding QA histos
//
//  Revision 1.2  2015/09/02 16:12:29  akio
//  fix typo
//
//  Revision 1.1  2015/09/02 14:56:12  akio
//  Initial version of FMS-FPS correation analysis
//

#include "StFmsFpsMaker.h"

#include "StMessMgr.h"
#include "Stypes.h"

#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StEnumerations.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

ClassImp(StFmsFpsMaker);

StFmsFpsMaker::StFmsFpsMaker(const Char_t* name): StMaker(name) {}

StFmsFpsMaker::~StFmsFpsMaker(){}

Int_t StFmsFpsMaker::Init(){  
    mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
    if(!mFmsDbMaker){
	LOG_ERROR  << "StFmsFpsMaker::InitRun Failed to get StFmsDbMaker" << endm;
	return kStFatal;
    }
    return kStOK;
}

Int_t StFmsFpsMaker::Make(){
    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFmsFpsMaker::Make did not find StEvent"<<endm; return kStErr;}
    mFmsColl = event->fmsCollection();
    if(!mFmsColl) {LOG_ERROR << "StFmsFpsMaker::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}
    if(mReadMuDST) readMuDST();
    mFmsColl->fillFpsSlat();        //fill StFpsSlat in StFmsCollection
    corrFmsFps();                   //find FMS-FPS correlation, add fps slats info to StFmsPoint 
    mFmsColl->fillFpsAssociation(); //fill StFpsSlat with association info to FmsPoint
    pid(mPidMethod);                //find PID, add to FMS point
    mFmsColl->fillFmsPointPair();   //find pairs of points
    isolationCone();                //calc sum energy within cones
    return kStOk;
}

//Correlation between FMS and FPS
void StFmsFpsMaker::corrFmsFps(){
    const unsigned int npoint=mFmsColl->numberOfPoints();
    StSPtrVecFmsPoint& points = mFmsColl->points(); 
    
    //loop over FMS points
    for(unsigned int i=0; i<npoint; i++) { 
	const float x=points[i]->XYZ().x();
	const float y=points[i]->XYZ().y();
	const float z=points[i]->XYZ().z();
	StLorentzVectorF v1=points[i]->fourMomentum();
	points[i]->resetFps();
	//loop over FPS layers to and project 
	for(unsigned int l=1; l<=kFpsNLayer; l++){
	    int nFpsFound=0;
	    //loop pver FPS quad and slats
	    for(unsigned int q=1; q<=kFpsNQuad; q++){	
		for(unsigned int s=1; s<=kFpsNSlat; s++) {
		    const int slatid = mFmsDbMaker->fpsSlatId(q,l,s);	  
		    if(slatid<0) continue;
		    float xyz[3],width[3];
		    mFmsDbMaker->fpsPosition(slatid,xyz,width);	 
		    const float projx=project(x,z,xyz[2],mMeanVertexZ);
		    const float projy=project(y,z,xyz[2],mMeanVertexZ);	
		    const float d = distance(projx,projy,xyz[0],xyz[1],width[0],width[1]); 
		    if(d<mMaxDistanceToAssociate){
			float mip=mFmsColl->fps(slatid)->mip();
			const unsigned short status=mFmsDbMaker->fpsStatus(slatid);
			if(status != 0) mip=-9.0; //check FPS status from DB
			LOG_DEBUG << Form("%3d proj=%5.1f %5.1f slatid=%4d Q%1dL%1dS%02d d=%4.1f n=%d\n",
					  i,projx,projy,slatid,q,l,s,d,nFpsFound) << endm;
			points[i]->setFps(l,mip,slatid,d);
			nFpsFound++;
		    } // if this hit a fps slat
		} //loop over fps slat
	    } //loop over fps quad
	    if(nFpsFound==0){
		LOG_DEBUG << Form("%3d proj=%5.1f %5.1f E=%5.1f  L%1d not found!!!\n",i,x,y,points[i]->energy(),l) << endm;
	    }else if(nFpsFound>kFpsNCandidate){
		LOG_WARN << Form("found %d FPS slats assosicated with FmsPoint, which is more than %d!!!",nFpsFound,kFpsNCandidate) <<endm;
	    }
	} //loop over fps layer
    }
}

//opt: 0=take closest only, 1=sum of all associated, 2=closest, if mip=0 at closest, take 2nd if available
void StFmsFpsMaker::pid(int opt){  
    const unsigned int npoint=mFmsColl->numberOfPoints();
    StSPtrVecFmsPoint& points = mFmsColl->points();
    
    for(unsigned int i=0; i<npoint; i++) {
	int pid=StFmsPoint::kFpsPidNoFps;
	int i1=0,i2=0,i3=0;
	if(opt==0 || opt==2){
	    i1=int(points[i]->fpsMip(1,0) + 0.5);
	    i2=int(points[i]->fpsMip(2,0) + 0.5); 
	    i3=int(points[i]->fpsMip(3,0) + 0.5);
	}else if(opt==1){
	    i1=int(points[i]->fpsMip(1,kFpsNCandidate+2)+0.5);
	    i2=int(points[i]->fpsMip(2,kFpsNCandidate+2)+0.5);
	    i3=int(points[i]->fpsMip(3,kFpsNCandidate+2)+0.5);
	}
	if(opt==2){
	    if(i1<=0) i1=int(points[i]->fpsMip(1,1) + 0.5);
	    if(i2<=0) i2=int(points[i]->fpsMip(2,1) + 0.5);
	    if(i3<=0) i3=int(points[i]->fpsMip(3,1) + 0.5);
	}      
	if(i1<-5 || i2<-5 || i3<-5) {
	    pid=StFmsPoint::kFpsPidBad;  //bad FPS status
	}else if(i1<0 || i2<0 || i3<0) {
	    pid=StFmsPoint::kFpsPidNoFps;  //no FPS coverage 
	}else{
	    //total 0
	    if     (i1==0 && i2==0 && i3==0) pid=StFmsPoint::kFpsPidGamma1;   // golden gamma which didn't convert in PbC
	    //total 1 hits
	    else if(i1>=1 && i2==0 && i3==0) pid=StFmsPoint::kFpsPidGamma3;   // gamma + accidental?
	    else if(i1==0 && i2>=1 && i3==0) pid=StFmsPoint::kFpsPidGamma4;   // gamma + accidental?
	    else if(i1==0 && i2==0 && i3>=1) pid=StFmsPoint::kFpsPidGamma2;   // golden gamma
	    //total 2 hits
	    else if(i1>=1 && i2>=1 && i3==0) pid=StFmsPoint::kFpsPidUnknown;  // ???
	    else if(i1>=1 && i2==0 && i3>=1) pid=StFmsPoint::kFpsPidGamma5;   // gamma + accidental? 
	    else if(i1==0 && i2>=1 && i3>=1) pid=StFmsPoint::kFpsPidGamma6;   // gamma + accidental? 
	    //total 3 hits
	    else if(i1>=1 && i2>=1 && i3<=4) pid=StFmsPoint::kFpsPidMip;      // MIP
	    else if(i1==1 && i2==1 && i3>=5) pid=StFmsPoint::kFpsPidElectron1;// golden e+e-
	    else if(i1==1 && i2>=2 && i3>=5) pid=StFmsPoint::kFpsPidElectron2;// e+e-
	    else if(i1>=2 && i2==1 && i3>=5) pid=StFmsPoint::kFpsPidElectron3;// e+e-
	    else if(i1>=2 && i2>=2 && i3>=5) pid=StFmsPoint::kFpsPidGamma7;   // gamma converted to e+e- pair?
	    else                             LOG_WARN << Form("Leaking selection : %1d %1d %1d\n",i1,i2,i3)<<endm;
	}    
	points[i]->setFpsPid(pid);
    } // loop over FMS points
}

void StFmsFpsMaker::isolationCone(){  
    const unsigned int nPointPairs = mFmsColl->numberOfPointPairs(); 
    const unsigned int nHits = mFmsColl->numberOfHits();
    for(unsigned int i=0; i<nPointPairs; i++){
	StFmsPointPair* pair=mFmsColl->pointPairs()[i];
	StThreeVectorF v1=pair->fourMomentum().vect();
	float sum[StFmsPointPair::kFmsPointMaxCone];
	memset(sum,0,sizeof(sum));
	for(unsigned int h=0; h<nHits; h++){
	    StFmsHit* hit=mFmsColl->hits()[h];
	    if(hit->detectorId()>=kFmsNorthLargeDetId && hit->detectorId()<=kFmsSouthSmallDetId){
		StThreeVectorF v2=mFmsDbMaker->getStarXYZ(hit);
		float angle=v1.angle(v2);
		for(unsigned int c=0; c<StFmsPointPair::kFmsPointMaxCone; c++){
		    if(angle < pair->coneRadius(c)){
			sum[c]+=hit->energy();
		    }//if within cone radius
		}//loop over difference cone sizes
	    }//if fms hits
	}//loop over hits
	for(unsigned int c=0; c<StFmsPointPair::kFmsPointMaxCone; c++) pair->setConeEnergy(c,sum[c]);     
    }//loop over point pairs
}

//Read MuDST if available, and update FPS hits in StEvent using current DB values
void StFmsFpsMaker::readMuDST(){  
    const unsigned int nh=mFmsColl->numberOfHits();
    StSPtrVecFmsHit& hits = mFmsColl->hits();
    int nfpshit=0;
    for(unsigned int j=0; j<nh; j++){ //count fps hits in StEvent->StFmsCollection->Hit
	if(hits[j]->detectorId()==kFpsDetId) nfpshit++; 
    }
    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
    if(!mudst){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no Mudst"<<endm; return;}
    
    StMuFmsCollection* muFmsColl = StMuDst::muFmsCollection();
    if(!muFmsColl){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no StMuFmsCollection in MuDST"<<endm; return;}
    int nfps=0;
    const unsigned int nhits = muFmsColl->numberOfHits();  
    for(unsigned int i=0; i<nhits; i++){
	StMuFmsHit* h = muFmsColl->getHit(i);
	if(h->detectorId()==15) h->setDetectorId(kFpsDetId); //hack to deal with wrong DetId
	if(h->detectorId()==kFpsDetId) { //only updating FPS hits... StFmsHitMaker deal with the rest
	    int flag=0;
	    const int ch=h->channel();	  
	    const float gain=mFmsDbMaker->fpsGain(ch);
	    const float nmip=h->adc()/gain;
	    LOG_DEBUG << Form("ch=%3d adc=%4d gain=%6.2f nmip=%6.2f\n",ch,h->adc(),gain,nmip)<<endm;
	    if(nfpshit>0){ //only if there were FPS hits in StEnvent...
		for(unsigned int j=0; j<nh; j++){ //loop over fmsHits in StEvent and updating energy with new calibration
		    if(hits[j]->detectorId()==kFpsDetId && hits[j]->channel()==ch){
			if(h->adc()!= hits[j]->adc()) {
			    LOG_ERROR << "StFmsFpsMaker::readMuDst Found inconsistent FPS hit" <<endm;
			    h->print();
			    hits[j]->print();
			    break;
			}
			LOG_DEBUG<<"StFmsFpsMaker::readMuDST found matching hits in StEvent->StFmsCollection, updating energy with new DB value "<<endm;
			hits[j]->setEnergy(nmip);
			flag=1;
			break;
		    }
		}
	    }
	    if(flag==0){ //found no correspinding hit in StEvent->FmsCollection, so adding it
		StFmsHit* hit = new StFmsHit(h->detectorId(),h->channel(),
					     h->qtCrate(),h->qtSlot(),h->qtChannel(),
					     h->adc(), h->tdc(), nmip);
		mFmsColl->addHit(hit);
		LOG_DEBUG<<"StFmsFpsMaker::readMuDST did not find matching hits in StEvent->StFmsCollection. Creating and adding"<<endm;
	    }
	    nfps++;
	}
    }
    LOG_INFO<<"StFmsFpsMaker::readMuDST Found "<<nhits<<" FMS hits in MuDst, updated "<<nfps<<" FPS hits"<<endm;
}

Float_t StFmsFpsMaker::project(float x, float z, float zp, float vz){
    if(z==vz) return 0.0;
    return x/(z-vz)*(zp-vz);
}

//find minimal distance between a point (x,y) and edge of a rectange with center (x0,y0) and width (xw,yw)
//return negative distance if its inside, positive outside
Float_t StFmsFpsMaker::distance(float x, float y, float x0, float y0, float xw, float yw){
    const float xx=x-x0;
    float yy=y-y0;
    float dx1=xx-xw/2.0, adx1=fabs(dx1);
    float dx2=xx+xw/2.0, adx2=fabs(dx2);
    float dy1=yy-yw/2.0, ady1=fabs(dy1);
    float dy2=yy+yw/2.0, ady2=fabs(dy2);
    float adx=(adx1<adx2) ? adx1 : adx2;
    float ady=(ady1<ady2) ? ady1 : ady2;
    float ad =(adx <ady ) ? adx  : ady ; 
    float oix=dx1*dx2;
    float oiy=dy1*dy2;
    if(oix<0.0 && oiy<0.0) return -ad; //inside
    if(oix<0.0 && oiy>0.0) return ady; //outside in y direction
    if(oix>0.0 && oiy<0.0) return adx; //outside in x direction
    return sqrt( pow(fabs(xx)-xw/2.0,2.0) + pow(fabs(yy)-yw/2.0,2.0) ); //outside both ways, return dist from corner
}

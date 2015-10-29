// \class StFmsFpsMaker
// \author Akio Ogawa
//
//  $Id: StFmsFpsMaker.cxx,v 1.5 2015/10/29 21:22:01 akio Exp $
//  $Log: StFmsFpsMaker.cxx,v $
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

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"

static const float EThr=5.0;
static const float ETotThr1=20.0;
static const float ETotThr2=30.0;

inline float project(float x, float z, float zp, float vz){
    return x/(z-vz)*(zp-vz);
}

//find minimal distance between a point (x,y) and a rectange with center (x0,y0) and width (xw,yw)
//return negative distance if its inside, positive outside
float distance(float x, float y, float x0, float y0, float xw, float yw){
    float xx=x-x0;
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

ClassImp(StFmsFpsMaker);

StFmsFpsMaker::StFmsFpsMaker(const Char_t* name):
    StMaker(name),mQA(false),mFilename((char *)"fmsfps.root"),mPrint(0),mReadMuDST(0),
    mMaxDistanceToAssociate(2.0),mPidMethod(1)
{}

StFmsFpsMaker::~StFmsFpsMaker(){}

Int_t StFmsFpsMaker::Init(){  
    mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
    if(!mFmsDbMaker){
	LOG_ERROR  << "StFmsFpsMaker::InitRun Failed to get StFmsDbMaker" << endm;
	return kStFatal;
    }
    if(!mQA) return kStOK;
    
    char c[100];
    mFile=new TFile(mFilename,"RECREATE");
    static const int pm[kFpsNQuad][kFpsNLayer]={{1,1,1},{1,-1,-1},{-1,1,1},{-1,-1,-1}}; //+or- measured coordnates
    
    mERatio[0]= new TH1F("ERatioCluster-Hit","ERatioCH",50.0,0.0,1.2);
    mERatio[1]= new TH1F("ERatioPoint-Cluster","ERatioPC",50.0,0.0,1.2);

    mFmsAdc      = new TH1F("FmsAdc","FmsAdc",4096,0.0,4096.0);
    mFmsHitLarge = new TH2F("FmsHitLarge","FmsHitLarge",34,-17.0,17.0,34,0.0,34.0);
    mFmsHitSmall = new TH2F("FmsHitSmall","FmsHitSmall",24,-12.0,12.0,24,0.0,24.0);
    mFpsMip[0]   = new TH1F("FpsMipL1","FpsMipL1",100,0.0,5.0);
    mFpsMip[1]   = new TH1F("FpsMipL2","FpsMipL2",100,0.0,5.0);
    mFpsMip[2]   = new TH1F("FpsMipL3","FpsMipL3",180,0.0,90.0);
    
    mNTow[0]  = new TH1F("NTowL","NTowL",50,0.0,50.0);
    mNTow[1]  = new TH1F("NTowS","NTowS",50,0.0,50.0);
    mNTowE[0] = new TH2F("NTowEL","NTowEL",50,0.0,50.0,100.0,0.0,100.0);
    mNTowE[1] = new TH2F("NTowES","NTowES",50,0.0,50.0,100.0,0.0,100.0);
    mSigmax[0]= new TH1F("SigMaxL","SigMaxL",100.0,0.0,4.0);
    mSigmax[1]= new TH1F("SigMaxS","SigMaxS",100.0,0.0,4.0);
    mSigmin[0]= new TH1F("SigMinL","SigMinL",100.0,0.0,4.0);
    mSigmin[1]= new TH1F("SigMinS","SigMinS",100.0,0.0,4.0);
    mSigmaxE[0]= new TH2F("SigMaxEL","SigMaxEL",100.0,0.0,100.0,100,0.0,2.0);
    mSigmaxE[1]= new TH2F("SigMaxES","SigMaxES",100.0,0.0,100.0,100,0.0,2.0);
    mChi2[0]  = new TH1F("Chi2L","Chi2L",100.0,0.0,2000.0);
    mChi2[1]  = new TH1F("Chi2S","Chi2S",100.0,0.0,2000.0);
    mCluXY[0]= new TH2F("CluXYL","CluXYL",100.0,0.0,17*5.812,100,0.0,34.0*5.812);
    mCluXY[1]= new TH2F("CluXYS","CluXYS",100.0,0.0,24*3.822,100,0.0,12.0*3.875);

    for(int cut=0; cut<NCUT1; cut++){
	for(int q=0; q<kFpsNQuad; q++){
	    for(int l=0; l<kFpsNLayer; l++){
		float min,max;
		if(pm[q][l]==1){min=-10.0; max=110;}
		else           {min=-110.0; max=10.0;} 
		sprintf(c,"FMSFPS_Q%1dL%1d_c%1d",q+1,l+1,cut);
		mH2[q][l][cut]=new TH2F(c,c,120,min,max,21,0.5,21.5);
		sprintf(c,"FMSFPSd_Q%1dL%1d_c%1d",q+1,l+1,cut);
		mHd2[q][l][cut]=new TH2F(c,c,100,min,max,100,-50.0,50.0);
		sprintf(c,"FMS-FPS_Q%1dL%1d_c%1d",q+1,l+1,cut);
		mHd[q][l][cut]=new TH1F(c,c,100,-50.0,50.0);
	    }//loop over layer
	}//loop over quad
	sprintf(c,"NP_c%d",cut);   mHn  [cut]=new TH1F(c,c, 50,0.0,50.0);
	sprintf(c,"e_c%d",cut);    mHene[cut]=new TH1F(c,c,100,0.0,100.0);
	sprintf(c,"elo_c%d",cut);  mHelo[cut]=new TH1F(c,c,100,0.0,4.0);
	sprintf(c,"pt_c%d",cut);   mHpt [cut]=new TH1F(c,c,100,0.0,10.0);
	sprintf(c,"ept_c%d",cut);  mHept[cut]=new TH2F(c,c,100,0.0,100.0,100,0.0,10.0);
	sprintf(c,"eta_c%d",cut);  mHeta[cut]=new TH1F(c,c,100,2.5,4.5);
	sprintf(c,"phi_c%d",cut);  mHphi[cut]=new TH1F(c,c,100,-3.2,3.2);
	sprintf(c,"x_c%d",cut);    mHx  [cut]=new TH1F(c,c,100,-100.0,100.0);
	sprintf(c,"y_c%d",cut);    mHy  [cut]=new TH1F(c,c,100,-100.0,100.0);
	sprintf(c,"xy_c%d",cut);   mHxy [cut]=new TH2F(c,c,200,-100.0,100.0,200,-100.0,100.0);
	sprintf(c,"pid_c%d",cut);  mHpid[cut]=new TH1F(c,c,41,-0.5,40.5);
	sprintf(c,"pid2_c%d",cut); mHpid2[cut]=new TH1F(c,c,5,-0.5,4.5);
    }//loop over cuts
    
    for(int cut=0; cut<NCUT2; cut++){
	sprintf(c,"p_NP_c%d",cut);   mPn  [cut]=new TH1F(c,c, 16,0.0,16.0);
	sprintf(c,"p_e_c%d",cut);    mPene[cut]=new TH1F(c,c,100,0.0,100.0);
	sprintf(c,"p_pt_c%d",cut);   mPpt [cut]=new TH1F(c,c,100,0.0,10.0);
	sprintf(c,"p_ept_c%d",cut);  mPept[cut]=new TH2F(c,c,100,0.0,100.0,100,0.0,10.0);
	sprintf(c,"p_eta_c%d",cut);  mPeta[cut]=new TH1F(c,c,100,2.5,5.0);
	sprintf(c,"p_phi_c%d",cut);  mPphi[cut]=new TH1F(c,c,100,-3.2,3.2
);
	sprintf(c,"p_pid_c%d",cut);  mPpid[cut]=new TH2F(c,c,5,-0.5,4.5,5,-0.5,4.5);
	sprintf(c,"p_m1_c%d",cut);   mPm1 [cut]=new TH1F(c,c,100,0.0,1.0);
	sprintf(c,"p_m2_c%d",cut);   mPm2 [cut]=new TH1F(c,c,100,0.0,5.0);    
	sprintf(c,"p_zgg_c%d",cut);  mPzgg[cut]=new TH1F(c,c,100,0.0,1.0);    
	sprintf(c,"p_dgg_c%d",cut);  mPdgg[cut]=new TH1F(c,c,200,0.0,20.0);    
	sprintf(c,"p_r30_c%d",cut);  mPr30[cut]=new TH1F(c,c,100,0.0,3.0);    
	sprintf(c,"p_r100_c%d",cut); mPr100[cut]=new TH1F(c,c,100,0.0,2.0);    
	sprintf(c,"p_xy_c%d",cut);   mPxy[cut]=new TH2F(c,c,200,-100.0,100.0,200,-100.0,100.0);    
    }
    return kStOK;
}

Int_t StFmsFpsMaker::Finish(){
    if(mQA){
	mFile->Write();
	mFile->Close();
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
    if(mQA) fmsFpsAlignment();  
    if(mPrint) print();
    return kStOk;
}

//Correlation between FMS and FPS
void StFmsFpsMaker::corrFmsFps(){
    int npoint=mFmsColl->numberOfPoints();
    StSPtrVecFmsPoint& points = mFmsColl->points(); 
    
    //loop over FMS points
    for(int i=0; i<npoint; i++) { 
	float x=points[i]->XYZ().x();
	float y=points[i]->XYZ().y();
	float z=points[i]->XYZ().z();
	StLorentzVectorF v1=points[i]->fourMomentum();
	points[i]->resetFps();
	//loop over FPS layers to and project 
	for(int l=1; l<=kFpsNLayer; l++){
	    int nFpsFound=0;
	    //loop pver FPS quad and slats
	    for(int q=1; q<=kFpsNQuad; q++){	
		for(int s=1; s<=kFpsNSlat; s++) {
		    int slatid = mFmsDbMaker->fpsSlatId(q,l,s);	  
		    if(slatid<0) continue;
		    float xyz[3],width[3];
		    mFmsDbMaker->fpsPosition(slatid,xyz,width);	 
		    static const float zvertex=0.0;
		    float projx=project(x,z,xyz[2],zvertex);
		    float projy=project(y,z,xyz[2],zvertex);	
		    float d = distance(projx,projy,xyz[0],xyz[1],width[0],width[1]); 
		    if(d<mMaxDistanceToAssociate){
			float mip=mFmsColl->fps(slatid)->mip();
			unsigned short status=mFmsDbMaker->fpsStatus(slatid);
			if(status != 0) mip=-9.0; //check FPS status from DB
			//LOG_DEBUG << Form("%3d proj=%5.1f %5.1f slatid=%4d Q%1dL%1dS%02d d=%4.1f n=%d\n",
			//		  i,projx,projy,slatid,q,l,s,d,nFpsFound) << endm;
			points[i]->setFps(l,mip,slatid,d);
			nFpsFound++;
		    } // if this hit a fps slat
		} //loop over fps slat
	    } //loop over fps quad
	    if(nFpsFound==0){
		// LOG_DEBUG << FORM("%3d proj=%5.1f %5.1f E=%5.1f  L%1d not found!!!\n",i,x,y,e,l) << endm;
	    }else if(nFpsFound>kFpsNCandidate){
		LOG_WARN << Form("found %d FPS slats assosicated with FmsPoint, which is more than %d!!!",nFpsFound,kFpsNCandidate) <<endm;
	    }
	} //loop over fps layer
	//points[i]->orderFpsCandidates();
    }
}

//opt: 0=take closest only, 1=sum of all associated, 2=closest, if mip=0 at closest, take 2nd if available
void StFmsFpsMaker::pid(int opt){  
    int npoint=mFmsColl->numberOfPoints();
    StSPtrVecFmsPoint& points = mFmsColl->points();
    
    for(int i=0; i<npoint; i++) {
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
    for(unsigned int i=0; i<mFmsColl->numberOfPointPairs(); i++){
	StFmsPointPair* pair=mFmsColl->pointPairs()[i];
	StThreeVectorF v1=pair->fourMomentum().vect();
	float sum[StFmsPointPair::kFmsPointMaxCone];
	memset(sum,0,sizeof(sum));
	for(unsigned int h=0; h<mFmsColl->numberOfHits(); h++){
	    StFmsHit* hit=mFmsColl->hits()[h];
	    if(hit->detectorId()>=kFmsNorthLargeDetId && hit->detectorId()<=kFmsSouthSmallDetId){
		StThreeVectorF v2=mFmsDbMaker->getStarXYZ(hit);
		float angle=v1.angle(v2);
		for(int c=0; c<StFmsPointPair::kFmsPointMaxCone; c++){
		    if(angle < pair->coneRadius(c)){
			sum[c]+=hit->energy();
		    }//if within cone radius
		}//loop over difference cone sizes
	    }//if fms hits
	}//loop over hits
	for(int c=0; c<StFmsPointPair::kFmsPointMaxCone; c++) pair->setConeEnergy(c,sum[c]);     
    }//loop over point pairs
}

void StFmsFpsMaker::print(){
    mFmsColl->print(4);
    StSPtrVecFpsSlat& slats = mFmsColl->fpsSlats();
    for(int l=1; l<=kFpsNLayer; l++){
	printf("NMIP      FPS layer%1d ",l);
	for(int q=1; q<=kFpsNQuad; q++){
	    printf(" Q%1d ",q);
	    for(int s=1; s<=kFpsNSlat; s++){
		int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
		if(slatid<0) {;
		    printf("x");
		}else{
		    float mip=slats[slatid]->mip();
		    int n=int(mip+0.5);
		    if(n>9) n=9;
		    printf("%1d",n);
		}
	    }      
	}
	printf("\n");
	printf("NFmsPoint FPS layer%1d ",l);
	for(int q=1; q<=kFpsNQuad; q++){
	    printf(" Q%1d ",q);
	    for(int s=1; s<=kFpsNSlat; s++){
		int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
		if(slatid<0) {
		    printf("x");
		}else{
		    int n=slats[slatid]->nPoint(0);
		    if(n>9) n=9;
		    printf("%1d",n);
		}
	    }      
	}
	printf("\n");
    }

    /*
    //testing StFmsDbMaker::distanceFromEdge()
    const int nn=100;
    const float minx=-10.0, maxx=110.0;
    for(int i=0; i<nn; i++){
	float x=minx + i*(maxx-minx)/float(nn);
	float y=20.0;
	int edge=0;
	float d=mFmsDbMaker->distanceFromEdge(8,x,y,edge);
	printf("EDGE i=%4d det=%2d x=%8.2f y=%8.2f d=%8.2f edge=%1d\n",i,8,x,y,d,edge);
    }
    for(int i=0; i<nn; i++){
	float x=minx + i*(maxx-minx)/float(nn);
	float y=80.0;
	int edge=0;
	float d=mFmsDbMaker->distanceFromEdge(8,x,y,edge);
	printf("EDGE i=%4d det=%2d x=%8.2f y=%8.2f d=%8.2f edge=%1d\n",i,8,x,y,d,edge);
    }
    for(int i=0; i<nn; i++){
	float x=minx + i*(maxx-minx)/float(nn);
	float y=10.0;
	int edge=0;
	float d=mFmsDbMaker->distanceFromEdge(10,x,y,edge);
	printf("EDGE i=%4d det=%2d x=%8.2f y=%8.2f d=%8.2f edge=%1d\n",i,10,x,y,d,edge);
    }
    for(int i=0; i<nn; i++){
	float x=minx + i*(maxx-minx)/float(nn);
	float y=40.0;
	int edge=0;
	float d=mFmsDbMaker->distanceFromEdge(10,x,y,edge);
	printf("EDGE i=%4d det=%2d x=%8.2f y=%8.2f d=%8.2f edge=%1d\n",i,10,x,y,d,edge);
    }
    */
}

//QA for Correlation between FMS and FPS and alignment histograms
void StFmsFpsMaker::fmsFpsAlignment(){
    StSPtrVecFmsHit& hits = mFmsColl->hits();
    StSPtrVecFmsCluster& clusters = mFmsColl->clusters();
    StSPtrVecFmsPoint& points = mFmsColl->points(); 
    StSPtrVecFpsSlat& slats = mFmsColl->fpsSlats(); 
    vector<StFmsPointPair*>& pairs = mFmsColl->pointPairs();
    int nh=mFmsColl->numberOfHits();
    int nc=mFmsColl->numberOfClusters();
    int np=mFmsColl->numberOfPoints();
    int ns=slats.size();
    int npair=mFmsColl->numberOfPointPairs();
    float etot[3]={0.0,0.0,0.0};

    //First fill some hists for hits for just QA
    for(int i=0; i<nh; i++){
	StFmsHit* hit=hits[i];
	int det=hit->detectorId();
	int ch=hit->channel();
	if(det>=kFmsNorthLargeDetId && det<=kFmsSouthSmallDetId){
	    mFmsAdc->Fill(float(hit->adc()));
	    float row=mFmsDbMaker->getRowNumber(det,ch)-0.5;
	    float col=mFmsDbMaker->getColumnNumber(det,ch)-0.5;
	    if(mFmsDbMaker->northSouth(det)==0) col*=-1.0;
	    if(mFmsDbMaker->largeSmall(det)==0){
		mFmsHitLarge->Fill(col,row,hit->energy());
	    }else{
		mFmsHitSmall->Fill(col,row,hit->energy());
	    }
	    etot[0]+=hit->energy();
	}else if(det==kFpsDetId){
            int q,l,s;
            mFmsDbMaker->fpsQLSfromSlatId(ch,&q,&l,&s);
	    if(l>=1 && l<=3) mFpsMip[l-1]->Fill(hit->energy());
	}
    }
	    
    //Hists for clusters
    for(int i=0; i<nc; i++){
	StFmsCluster* clu=clusters[i];
	int det=clu->detectorId();
	int ls=mFmsDbMaker->largeSmall(det);
	float nt=float(clu->nTowers());
	mNTow[ls]->Fill(nt);
	mNTowE[ls]->Fill(nt,clu->energy());
	mSigmax[ls]->Fill(clu->sigmaMax());
	mSigmin[ls]->Fill(clu->sigmaMin());
	mSigmaxE[ls]->Fill(clu->energy(),clu->sigmaMax());
	mCluXY[ls]->Fill(clu->x(),clu->y());
	int nphoton=clu->nPhotons();
	float c2=0.0;
	if(nphoton==0)      {c2=0.0;}
	else if(nphoton==1) {c2=clu->chi2Ndf1Photon();}
	else if(nphoton==2) {c2=clu->chi2Ndf2Photon();}
	else {LOG_INFO << Form("NPHOTON=%d ???",nphoton) << endm;}
	mChi2[ls]->Fill(c2);
	etot[1]+=clu->energy();
    }
    
    //Counting points and slats per quad
    int npoint[kFpsNQuad], nfps[kFpsNQuad][kFpsNLayer];
    memset(npoint,0,sizeof(npoint));
    memset(nfps,0,sizeof(nfps));  
    //Get # of FMS point per quad 
    for(int i=0; i<np; i++) {
	float x=points[i]->XYZ().x();
	float y=points[i]->XYZ().y();
	int q=0;
	if     (x>=0.0 && y>=0.0) {q=1;}
	else if(x>=0.0 && y< 0.0) {q=2;}
	else if(x< 0.0 && y>=0.0) {q=3;}
	else if(x< 0.0 && y< 0.0) {q=4;}
	npoint[q-1]++;
	etot[2]+=points[i]->energy();
    }  
    //Get # of FPS slats hitted per quad/layer
    for(int i=0; i<ns; i++) {
	if(slats[i]->mip()>0.5){
	    int slatid=slats[i]->slatId();
	    int q,l,s;
	    mFmsDbMaker->fpsQLSfromSlatId(slatid,&q,&l,&s);
	    nfps[q-1][l-1]++;
	}
    }
    if(etot[0]>0.0) mERatio[0]->Fill(etot[1]/etot[0]);
    if(etot[1]>0.0) mERatio[1]->Fill(etot[2]/etot[1]);

    //Loop over FMS points to set up cuts and counts 
    int cut1[NCUT1], n1[NCUT1];
    memset(n1,0,sizeof(n1));
    for(int i=0; i<np; i++) {
	memset(cut1,0,sizeof(cut1));
	float e=points[i]->energy();
	float x=points[i]->XYZ().x();
	float y=points[i]->XYZ().y();
	float z=points[i]->XYZ().z();
	int pid=points[i]->fpsPid();
	int pid2=pid/10;
	StLorentzVectorF v1=points[i]->fourMomentum();
	int q=0;
	if     (x>=0.0 && y>=0.0) {q=1;}
	else if(x>=0.0 && y< 0.0) {q=2;}
	else if(x< 0.0 && y>=0.0) {q=3;}
	else if(x< 0.0 && y< 0.0) {q=4;}
	
	cut1[0]=1; //all	
	int edgeType;
	float distance=mFmsDbMaker->distanceFromEdge(points[i],edgeType); 
	//	if(distance<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType==4)){
	if(distance<-0.51 || edgeType==4){
	    cut1[1]=1;
	    if(e>=EThr){
		cut1[2]=1;
		if(pid2==1)      {cut1[5]=1;}
		else if(pid2==2) {cut1[6]=1;}
		else if(pid2==3) {cut1[7]=1;}
		else             {cut1[8]=1;}
	    }
	}
	if(edgeType==4 && distance>=-0.51){
	    cut1[3]=1;
	    if(e>=EThr) cut1[4]=1;
	}
	for(int c=0; c<NCUT1; c++){      
	    if(cut1[c]==0) continue;
	    n1[c]++;
	    mHene[c]->Fill(e);        
	    mHelo[c]->Fill(e);        
	    mHpt[c]->Fill(v1.perp());
	    mHept[c]->Fill(e,v1.perp());
	    mHeta[c]->Fill(v1.pseudoRapidity());    
	    mHphi[c]->Fill(v1.phi());
	    mHx[c]->Fill(x);
	    mHy[c]->Fill(y);
	    mHxy[c]->Fill(x,y);
	    mHpid[c]->Fill(float(pid));
	    mHpid2[c]->Fill(float(pid2));
	}//loop over cuts
	
	//Now FPS
	for(int l=1; l<=kFpsNLayer; l++) {
	    for(int s=1; s<=kFpsNSlat; s++) {
		int slatid = mFmsDbMaker->fpsSlatId(q,l,s);
		if(slatid<0) continue;
		float xyz[3],width[3];	  
		mFmsDbMaker->fpsPosition(q,l,s,xyz,width);
		//printf("Q%1dL%1dS%02d hit=%6.3f xyz=%6.2f %6.2f %6.2f\n",q,l,s,mHit[q-1][l-1][s-1],xyz[0],xyz[1],xyz[2]);
		if(slats[slatid]->mip()>0.5){
		    for(int c=0; c<NCUT1; c++){
			if(cut1[c]==0) continue;	    
			static const float zvertex=0.0;
			if(l==1){ 
			    float xx=project(x,z,xyz[2],zvertex);
			    mH2[q-1][l-1][c]->Fill(xx,float(s));
			    mHd[q-1][l-1][c]->Fill(xx-xyz[0]);
			    mHd2[q-1][l-1][c]->Fill(xx,xx-xyz[0]);
			}else{ 
			    float yy=project(y,z,xyz[2],zvertex);
			    mH2[q-1][l-1][c]->Fill(yy,float(s)); 
			    mHd[q-1][l-1][c]->Fill(yy-xyz[1]);
			    mHd2[q-1][l-1][c]->Fill(yy,y-xyz[1]);
			}
		    }//loop over cuts
		}//if FPS has mip
	    }//loop slat
	}//loop layer    
    }//loop over points
    for(int c=0; c<NCUT1; c++) mHn[c]->Fill(float(n1[c]));
    
    //Now pairs
    int cut2[NCUT2], n2[NCUT2];
    memset(n2,0,sizeof(n2));
    for(int i=0; i<npair; i++) {
	memset(cut2,0,sizeof(cut2));
	StFmsPointPair* pair=pairs[i];
	StFmsPoint* p0=pair->point(0);
	StFmsPoint* p1=pair->point(1);
	float e0=p0->energy();
	float e1=p1->energy();
	int pid=pair->fpsPid();
	cut2[0]=1; //all
	int edgeType0, edgeType1;
	float d0=mFmsDbMaker->distanceFromEdge(p0,edgeType0); 
	float d1=mFmsDbMaker->distanceFromEdge(p1,edgeType1); 
	//	if( (d0<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType0==4)) &&
	//          (d1<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType1==4)) ){
	if( (d0<-0.51 || edgeType0==4) &&
	    (d1<-0.51 || edgeType1==4) ){
	    cut2[1]=1;    
	    if(e0>=EThr && e1>=EThr && pair->energy()>ETotThr1){
		LOG_DEBUG << Form("EdgeType=%1d %1d, d=%20.15f %20.15f x=%20.15f %20.15f xx=%20.15f %20.15f\n",
				  edgeType0, edgeType1,d0,d1,
				  p0->x(),p1->x(),p0->XYZ().x(),p1->XYZ().x());
		cut2[2]=1;
		if(pair->energy()>ETotThr2) {cut2[9]=1;}
		if     (pid==11)   {cut2[5]=1;}
		else if(pid==22)   {cut2[6]=1;}
		else if(pid==33)   {cut2[7]=1;}
		else               {cut2[8]=1;}
	    }
	}
	if( (edgeType0==4 && d0>=-0.51) || (edgeType1==4 && d1>=-0.51) ){
	    cut2[3]=1;
	    if(e0>=EThr && e1>=EThr && pair->energy()>ETotThr1){
		cut2[4]=1;
		if(pair->energy()>ETotThr2) cut2[10]=1;
	    }
	}
	for(int c=0; c<NCUT2; c++){      
	    if(cut2[c]==0) continue;
	    n2[c]++;
	    mPene[c]->Fill(pair->energy());        
	    mPpt[c]->Fill(pair->pT());
	    mPept[c]->Fill(pair->energy(),pair->pT());
	    mPeta[c]->Fill(pair->eta());    
	    mPphi[c]->Fill(pair->phi());
	    mPpid[c]->Fill(float(pid/10),float(pid%10));
	    mPm1[c]->Fill(pair->mass());
	    mPm2[c]->Fill(pair->mass());
	    mPzgg[c]->Fill(pair->zgg());
	    mPdgg[c]->Fill(pair->dgg());
	    mPr30[c]->Fill(pair->coneEnergyFraction(2));
	    mPr100[c]->Fill(pair->coneEnergyFraction(0));
	    mPxy[c]->Fill(pair->x(),pair->y());
	}//loop over cuts
    }//loop over pairs
    for(int c=0; c<NCUT2; c++) mPn[c]->Fill(float(n2[c]));
}

//Read MuDST if available, and update FPS hits in StEvent using current DB values
void StFmsFpsMaker::readMuDST(){  
    int nh=mFmsColl->numberOfHits();
    StSPtrVecFmsHit& hits = mFmsColl->hits();
    int nfpshit=0;
    for(int j=0; j<nh; j++){ //count fps hits in StEvent->StFmsCollection->Hit
	if(hits[j]->detectorId()==kFpsDetId) nfpshit++; 
    }
    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
    if(!mudst){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no Mudst"<<endm; return;}
    StMuFmsCollection* muFmsColl = StMuDst::muFmsCollection();
    if(!muFmsColl){ LOG_INFO<<"StFmsFpsMaker::readMuDST found no StMuFmsCollection in MuDST"<<endm; return;}
    int nfps=0;
    int nhits = muFmsColl->numberOfHits();  
    for(int i=0; i<nhits; i++){
	StMuFmsHit* h = muFmsColl->getHit(i);
	if(h->detectorId()==15) h->setDetectorId(kFpsDetId); //hack to deal with wrong DetId
	if(h->detectorId()==kFpsDetId) { //only updating FPS hits... StFmsHitMaker deal with the rest
	    int flag=0;
	    int ch=h->channel();	  
	    float gain=mFmsDbMaker->fpsGain(ch);
	    float nmip=h->adc()/gain;
	    //printf("ch=%3d adc=%4d gain=%6.2f nmip=%6.2f\n",ch,h->adc(),gain,nmip);
	    if(nfpshit>0){ //only if there were FPS hits in StEnvent...
		for(int j=0; j<nh; j++){ //loop over fmsHits in StEvent and updating energy with new calibration
		    if(hits[j]->detectorId()==kFpsDetId && hits[j]->channel()==ch){
			if(h->adc()!= hits[j]->adc()) {
			    LOG_ERROR << "StFmsFpsMaker::readMuDst Found inconsistent FPS hit" <<endm;
			    h->print();
			    hits[j]->print();
			    break;
			}
			//LOG_INFO<<"StFmsFpsMaker::readMuDST found matching hits in StEvent->StFmsCollection, updating energy with new DB value "<<endm;
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
		//LOG_INFO<<"StFmsFpsMaker::readMuDST did not find matching hits in StEvent->StFmsCollection. Creating and adding"<<endm;
	    }
	    nfps++;
	}
    }
    LOG_INFO<<"StFmsFpsMaker::readMuDST Found "<<nhits<<" FMS hits in MuDst, updated "<<nfps<<" FPS hits"<<endm;
}

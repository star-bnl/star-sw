// \class StFmsOfflineQaMaker
// \author Akio Ogawa
//
//  $Id: StFmsOfflineQaMaker.cxx,v 1.2 2016/06/08 19:55:11 akio Exp $
//  $Log: StFmsOfflineQaMaker.cxx,v $
//  Revision 1.2  2016/06/08 19:55:11  akio
//  applying coverity report
//
//  Revision 1.1  2016/01/26 19:54:33  akio
//  Separated from StFmsFpsMaker... This is for FMS offline QA and also FMS-FPS alignments
//
//

#include "StFmsOfflineQaMaker.h"

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

inline float project(float x, float z, float zp, float vz){
    return x/(z-vz)*(zp-vz);
}

static const int NTRG=12;  //123=FMS-sm-bs123,456=FMS-lg-bs123,7=FMS-DiBS,8910=FMS-JP012,11=FMS-DiJP,13=LED
static const char *CTRG[NTRG] = {"SmBS1","SmBS2","SmBS3",
                                 "LgBS1","LgBS2","LgBS3","DiBS",
				 "JP2","JP1","JP0","DiJP","LED"};

int getFmsTrigId(const StTriggerId& trgid, int print=1){
    const int TIDBASE=480800;
    const int NBEAM=4;
    const int MAXVERSION=3;
    int trig=0;
    //    LOG_INFO << trgid <<endm;
    LOG_INFO << "TRGID = ";
    for(int k=0; k<NBEAM; k++){
	for(int j=0; j<MAXVERSION; j++){
	    for(int i=1; i<=NTRG; i++){
		int l=i;
		if(i==12) l=13;
		int id=TIDBASE + 10000*k + 20*j + l; 
		if(trgid.isTrigger(id)){
		    trig |= (1<<(i-1));
		    LOG_INFO << CTRG[i-1] << " ";
		}		
	    }      
	}
    }
    //include none-production id for now.... could be dangerous
    static int nonProdId[NTRG]={42,43,44,45,46,47,48,49,50,51,52,53};
    for(int i=0; i<NTRG; i++){
	if(trgid.isTrigger(nonProdId[i])){
	    trig |= (1<<i);
	    LOG_INFO << CTRG[i] << " ";
	}
    }
    LOG_INFO << endm;
    return trig;
}

ClassImp(StFmsOfflineQaMaker);

StFmsOfflineQaMaker::StFmsOfflineQaMaker(const Char_t* name):
    StMaker(name),mFilename((char *)"fmsqa.root")
{}

StFmsOfflineQaMaker::~StFmsOfflineQaMaker(){}

Int_t StFmsOfflineQaMaker::Init(){  
    mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
    if(!mFmsDbMaker){
	LOG_ERROR  << "StFmsOfflineQaMaker::InitRun Failed to get StFmsDbMaker" << endm;
	return kStFatal;
    }

    char c[100];
    mFile=new TFile(mFilename,"RECREATE");
    static const int pm[kFpsNQuad][kFpsNLayer]={{1,1,1},{1,-1,-1},{-1,1,1},{-1,-1,-1}}; //+or- measured coordnates
    
    mERatio[0]= new TH1F("ERatioCluster-Hit","ERatioCH",50.0,0.0,1.2);
    mERatio[1]= new TH1F("ERatioPoint-Cluster","ERatioPC",50.0,0.0,1.2);
    mBC       = new TH1F("BC","BC",120,0.0,120.0);
    mTrig[0]  = new TH1F("Trig","Trig",    13,0.0,13.0);
    mTrig[1]  = new TH1F("TrigAg","TrigAg",13,0.0,13.0);

    mFmsAdc      = new TH1F("FmsAdc","FmsAdc",4096,0.0,4096.0);
    mFmsHitLarge[0] = new TH2F("FmsHitLarge","FmsHitLarge",34,-17.0,17.0,34,0.0,34.0);
    mFmsHitSmall[0] = new TH2F("FmsHitSmall","FmsHitSmall",24,-12.0,12.0,24,0.0,24.0);
    mFmsHitLarge[1] = new TH2F("FmsHitLargeAg","FmsHitLargeAg",34,-17.0,17.0,34,0.0,34.0);
    mFmsHitSmall[1] = new TH2F("FmsHitSmallAg","FmsHitSmallAg",24,-12.0,12.0,24,0.0,24.0);
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
    mChi2[0]  = new TH1F("Chi2L","Chi2L",200.0,0.0,200.0);
    mChi2[1]  = new TH1F("Chi2S","Chi2S",200.0,0.0,200.0);
    mCluXY[0][0]= new TH2F("CluXYL","CluXYL",100.0,-17.0,17.0,100,0.0,34.0);
    mCluXY[1][0]= new TH2F("CluXYS","CluXYS",100.0,-12.0,12.0,100,0.0,24.0);
    mCluXY[0][1]= new TH2F("CluXYLAg","CluXYLAg",100.0,-17.0,17.0,100,0.0,34.0);
    mCluXY[1][1]= new TH2F("CluXYSAg","CluXYSAg",100.0,-12.0,12.0,100,0.0,24.0);
    mCluEta[0]=new TH1F("CluEta0","CluEta0",100,2.5,4.5);
    mCluEta[1]=new TH1F("CluEta1","CluEta1",100,2.5,4.5);
    mCluEta[2]=new TH1F("CluEta2","CluEta2",100,2.5,4.5);
    mCluEta[3]=new TH1F("CluEta3","CluEta3",100,2.5,4.5);
    mCluEta[4]=new TH1F("CluEta4","CluEta4",100,2.5,4.5);
    mCluEta[5]=new TH1F("CluEta5","CluEta5",100,2.5,4.5);

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
	sprintf(c,"NP_c%d",cut);   mHn  [cut]=new TH1F(c,c,100,0.0,100.0);
	sprintf(c,"e_c%d",cut);    mHene[cut]=new TH1F(c,c,100,0.0,100.0);
	sprintf(c,"elo_c%d",cut);  mHelo[cut]=new TH1F(c,c,100,0.0,4.0);
	sprintf(c,"pt_c%d",cut);   mHpt [cut]=new TH1F(c,c,100,0.0,10.0);
	sprintf(c,"ept_c%d",cut);  mHept[cut]=new TH2F(c,c,100,0.0,100.0,100,0.0,10.0);
	sprintf(c,"eta_c%d",cut);  mHeta[cut]=new TH1F(c,c,100,2.5,4.5);
	sprintf(c,"phi_c%d",cut);  mHphi[cut]=new TH1F(c,c,100,-3.2,3.2);
	sprintf(c,"x_c%d",cut);    mHx  [cut]=new TH1F(c,c,100,-100.0,100.0);
	sprintf(c,"y_c%d",cut);    mHy  [cut]=new TH1F(c,c,100,-100.0,100.0);
	sprintf(c,"dxl_c%d",cut);  mHdxL[cut]=new TH1F(c,c,120,-0.1,1.1);
	sprintf(c,"dxs_c%d",cut);  mHdxS[cut]=new TH1F(c,c,120,-0.1,1.1);
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
	sprintf(c,"p_phi_c%d",cut);  mPphi[cut]=new TH1F(c,c,100,-3.2,3.2);
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

Int_t StFmsOfflineQaMaker::Finish(){
    LOG_INFO << Form("Writing and closing %s",mFilename) << endm;
    mFile->Write();
    mFile->Close();
    return kStOK;
}

Int_t StFmsOfflineQaMaker::Make(){
    mBunch=-1;
    mTrigger=0;
    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFmsOfflineQaMaker::Make did not find StEvent"<<endm; return kStErr;}
    if(event->triggerData()) mBunch = event->triggerData()->bunchId7Bit();
    if(event->triggerIdCollection() && event->triggerIdCollection()->nominal()){
	mTrigger = getFmsTrigId(*(event->triggerIdCollection()->nominal()));
    }
    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
    if(mudst){ 
	LOG_INFO<<"StFmsFpsMaker::Make found Mudst, getting buunchId and triggerId from MuDst"<<endm; 
	mBunch = mudst->event()->triggerData()->bunchId7Bit(); 
	mTrigger = getFmsTrigId(mudst->event()->triggerIdCollection().nominal());
    }

    mFmsColl = event->fmsCollection();
    if(!mFmsColl) {LOG_ERROR << "StFmsOfflineQaMaker::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}

    if(mPrint) print();

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

    //Bunch counter and AG
    int ag=0;
    if     (mBunch>=30  && mBunch< 40) {ag=1;}
    else if(mBunch>=110 && mBunch<120) {ag=2;}
    mBC->Fill(float(mBunch));
    if(ag==0) mTrig[0]->Fill(0);
    if(ag==1) mTrig[1]->Fill(0);
    for(int i=0; i<NTRG; i++){
	if(mTrigger & (1<<i)){
	    if(ag==0) mTrig[0]->Fill(float(i+1));
	    if(ag==1) mTrig[1]->Fill(float(i+1));
	}
    }

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
		if(ag==0) mFmsHitLarge[0]->Fill(col,row,hit->energy());		
		if(ag==1) mFmsHitLarge[1]->Fill(col,row,hit->energy());		
	    }else{
		if(ag==0) mFmsHitSmall[0]->Fill(col,row,hit->energy());
		if(ag==1) mFmsHitSmall[1]->Fill(col,row,hit->energy());
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
	etot[1]+=clu->energy();
	if(clu->energy()<5.0) continue;
	int det=clu->detectorId();
	int ls=mFmsDbMaker->largeSmall(det);
	if(ls<0) continue;
	float nt=float(clu->nTowers());
	mNTow[ls]->Fill(nt);
	mNTowE[ls]->Fill(nt,clu->energy());
	mSigmax[ls]->Fill(clu->sigmaMax());
	mSigmin[ls]->Fill(clu->sigmaMin());
	mSigmaxE[ls]->Fill(clu->energy(),clu->sigmaMax());
	if(det%2==0){
	    if(ag==0) mCluXY[ls][0]->Fill(-clu->x(),clu->y());	
	    if(ag==1) mCluXY[ls][1]->Fill(-clu->x(),clu->y());	
	}else{
	    if(ag==0) mCluXY[ls][0]->Fill(clu->x(),clu->y());	
	    if(ag==1) mCluXY[ls][1]->Fill(clu->x(),clu->y());	
	}	   
	int nphoton=clu->nPhotons();
	float c2=0.0;
	if(nphoton==0)      {c2=0.0;}
	else if(nphoton==1) {c2=clu->chi2Ndf1Photon();}
	else if(nphoton==2) {c2=clu->chi2Ndf2Photon();}
	else {LOG_INFO << Form("NPHOTON=%d ???",nphoton) << endm;}
	mChi2[ls]->Fill(c2);
	int ctgy=clu->category();
	int type=(nphoton-1)*3 + ctgy;
	//cout << Form("NP=%1d Ctgry=%1d Type=%d\n",nphoton,ctgy,type);       
	mCluEta[type]->Fill(clu->fourMomentum().pseudoRapidity());	
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
	/*
	if(points[i]->detectorId()<=9)  cut1[1]=1;
	if(points[i]->detectorId()>=10) cut1[2]=1;
	if(points[i]->detectorId()<=9 && edgeType==4)  cut1[3]=1;
	if(points[i]->detectorId()>=10 && edgeType==4)  cut1[4]=1;
	*/
	if(e>=EThr){
	    cut1[1]=1;
	    if(distance<-0.51 || edgeType==4){
		cut1[2]=1;
		if(distance<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType==4)){
		    if(ag==0) {
			cut1[3]=1;
			if(pid2==1)      {cut1[6]=1;}
			else if(pid2==2) {cut1[7]=1;}
			else if(pid2==3) {cut1[8]=1;}
			else             {cut1[9]=1;}
		    }
		    if(ag==1) cut1[5]=1;
		}
	    }
	    if(edgeType==4 && distance>=-0.51) cut1[4]=1;
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
	    int det=points[i]->detectorId();
	    float localx=points[i]->x()/mFmsDbMaker->getXWidth(det);
	    float localy=points[i]->y()/mFmsDbMaker->getYWidth(det);
	    float dlx=localx-int(localx);
	    float dly=localy-int(localy);
	    if(points[i]->detectorId()<=9){		
		mHdxL[c]->Fill(dlx);
		mHdxL[c]->Fill(dly);
	    }else{
		mHdxS[c]->Fill(dlx);
		mHdxS[c]->Fill(dly);
	    }
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
	if(e0>=EThr && e1>=EThr && pair->energy()>ETotThr1){
	    cut2[1]=1;    
	    if( (d0<-0.51 || edgeType0==4) &&
		(d1<-0.51 || edgeType1==4) ){
		cut2[2]=1;
		if( (d0<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType0==4)) &&
		    (d1<-0.51 || (mFmsColl->isMergeSmallToLarge() && edgeType1==4)) ){
		    if(ag==0){
			cut2[3]=1;
			if     (pid==11)   {cut2[6]=1;}
			else if(pid==22)   {cut2[7]=1;}
			else if(pid==33)   {cut2[8]=1;}
			else               {cut2[9]=1;}
		    }
		    if(ag==1) cut2[5]=1;		    
		    if(n1[2]==2) cut2[10]=1;
		}
	    }
	    if( (edgeType0==4 && d0>=-0.51) || (edgeType1==4 && d1>=-0.51) ){
		cut2[4]=1;
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

    return kStOk;
}

void StFmsOfflineQaMaker::print(){
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

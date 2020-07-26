// $Id: StFcsPointMaker.cxx,v 1.5 2020/06/01 19:36:52 akio Exp $

#include "StFcsPointMaker.h"
#include "StLorentzVectorF.hh"

#include "StMessMgr.h"
#include "StEventTypes.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsPoint.h"
#include "StFcsDbMaker/StFcsDbMaker.h"

#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "tables/St_vertexSeed_Table.h"

#include <cmath>
#include "TMath.h"
#include "TVector2.h"

// global shower shape parameters 
static const int NPMAX=60;
std::array<double,NPMAX> mShowerShapeParameters;

// global tower data for current working cluster
static Int_t mNHit;    // # of hit in current working cluster
static Float_t mEtot;  // total energy of the cluster
static Float_t* mX;    // x of hit
static Float_t* mY;    // y of hit
static Float_t* mE;    // E of hit

StFcsPointMaker::StFcsPointMaker(const char* name) : StMaker(name) , mMinuit(7) {
    mMinuit.SetPrintLevel(-1);
}

StFcsPointMaker::~StFcsPointMaker() { }

void StFcsPointMaker::Clear(Option_t* option) {
    StMaker::Clear(option);
}

Int_t StFcsPointMaker::InitRun(Int_t runNumber) {
    // Ensure we can access database information
    LOG_DEBUG << "StFcsPointMaker initializing run" << endm;
    mDb = static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));
    if (!mDb) {
	LOG_ERROR << "StFcsPointMaker initializing failed due to no StFcsDbMaker" << endm;
	return kStErr;
    }
    return StMaker::InitRun(runNumber);
}

void StFcsPointMaker::setShowerShapeParameters(int det){
    double unused=0.0;
    double width = mDb->getXWidth(det);
    double scl=mShowerShapeScale; //scale shower shape to cell width and adjuistable scale factor
    if(mShowerShape==0){ 
	//Original single slice shower shap	
	scl *= 1.0/3.849; //optimum from FMS study, scale it to cell size
	mShowerShapeParameters= { width, 1.0708, 0.167773, -0.238578, 0.535845*scl, 0.850233*scl, 2.38264*scl, 1.0, unused, unused,
				 unused, unused, unused, unused, unused, unused, unused, unused, unused, unused,
		 		 unused, unused, unused, unused, unused, unused, unused, unused, unused, unused,
				 unused, unused, unused, unused, unused, unused, unused, unused, unused, unused,
				 unused, unused, unused, unused, unused, unused, unused, unused, unused, unused,
				 unused, unused, unused, unused, unused, unused, unused, unused, unused, unused};
    }else if(mShowerShape==1){ 
	//Yuxi's 6 slices * 3 gaus                                                                                                                   
	scl *= 0.8/3.849; //optimum from FMS study, scale it to cell size
	double a11=0.998438; double a12=0.222782;   double a13=-0.22122;   double b11=0.177028;double b12=0.000473222;double b13=0.178897;double w1 = 0.0372556;
	double a21=1.07711 ; double a22=-0.0281385; double a23=-0.0489747; double b21=0.199964;double b22=3.5021;     double b23=2.35246; double w2 = 0.202699;
	double a31=1.07901 ; double a32=0.0650143;  double a33=-0.144025;  double b31=0.446845;double b32=0.00544512; double b33=1.64565; double w3 = 0.293878;
	double a41=0.922174; double a42=0.0778254;  double a43=1.07474e-07;double b41=0.593804;double b42=0.6199;     double b43=3.49798; double w4 = 0.236854;
	double a51=0.999849; double a52=0.000151185;double a53=2.20244e-07;double b51=0.949953;double b52=1.84451;    double b53=3.40799; double w5 = 0.146041;
	double a61=0.997454; double a62=0.00254497; double a63=1.02127e-06;double b61=1.43387; double b62=2.91155;    double b63=3.4484;  double w6 = 0.0832717;
        mShowerShapeParameters= {width, a11*w1, a12*w1, a13*w1, b11*scl, b12*scl, b13*scl, unused ,unused, unused,
				 width, a21*w2, a22*w2, a23*w2, b21*scl, b22*scl, b23*scl, unused ,unused, unused,
				 width, a31*w3, a32*w3, a33*w3, b31*scl, b32*scl, b33*scl, unused ,unused, unused,
				 width, a41*w4, a42*w4, a43*w4, b41*scl, b42*scl, b43*scl, unused ,unused, unused,
				 width, a51*w5, a52*w5, a53*w5, b51*scl, b52*scl, b53*scl, unused ,unused, unused,
				 width, a61*w6, a62*w6, a63*w6, b61*scl, b62*scl, b63*scl, unused ,unused, unused};
    }else if(mShowerShape==2){
	//Zhanwen's small 45GeV new (6 slices * 2 gaus)
	scl *= 0.6/3.849; //optimum from FMS study, scale it to cell size
	double a1S[6]={0.0303644,0.212191,0.277429,0.0370035,0.0524404,0.00844062};
	double a2S[6]={0.00122867,0.105355,0.10538,0.152656,0.00664331,0.0108688};
	double b1S[6]={0.403493,0.514546,0.672826,1.82344,0.727991,1.48785};
	double b2S[6]={0.270492,0.514593,0.672655,0.644871,4.32003,0.25};
	mShowerShapeParameters= {width, a1S[0], a2S[0], 0.0, b1S[0]*scl, b2S[0]*scl, 0.0, unused, unused, unused,
				 width, a1S[1], a2S[1], 0.0, b1S[1]*scl, b2S[1]*scl, 0.0, unused, unused, unused,
				 width, a1S[2], a2S[2], 0.0, b1S[2]*scl, b2S[2]*scl, 0.0, unused, unused, unused,
				 width, a1S[3], a2S[3], 0.0, b1S[3]*scl, b2S[3]*scl, 0.0, unused, unused, unused,
				 width, a1S[4], a2S[4], 0.0, b1S[4]*scl, b2S[4]*scl, 0.0, unused, unused, unused,
				 width, a1S[5], a2S[5], 0.0, b1S[5]*scl, b2S[5]*scl, 0.0, unused, unused, unused};
    }else if(mShowerShape==3){ 
	//Zhanwen's large 45GeV new (6 slices & 2 gaus)
	scl *= 0.8/5.812; //optimum from FMS study, scale it to cell size
	double a1L[6]={0.0275364,0.200363,0.277157,0.0391611,0.0590757,0.0101089};
	double a2L[6]={0.000429808,0.0991777,0.104781,0.161916,0.00764026,0.012653};
	double b1L[6]={0.515974,0.661722,0.865167,2.35237,0.932038,1.87933};
	double b2L[6]={0.53531,0.661519,0.865226,0.828017,5.49041,0.321139};
	mShowerShapeParameters= {width, a1L[0], a2L[0], 0.0, b1L[0]*scl, b2L[0]*scl, 0.0, unused, unused, unused,
				 width, a1L[1], a2L[1], 0.0, b1L[1]*scl, b2L[1]*scl, 0.0, unused, unused, unused,
				 width, a1L[2], a2L[2], 0.0, b1L[2]*scl, b2L[2]*scl, 0.0, unused, unused, unused,
				 width, a1L[3], a2L[3], 0.0, b1L[3]*scl, b2L[3]*scl, 0.0, unused, unused, unused,
				 width, a1L[4], a2L[4], 0.0, b1L[4]*scl, b2L[4]*scl, 0.0, unused, unused, unused,
				 width, a1L[5], a2L[5], 0.0, b1L[5]*scl, b2L[5]*scl, 0.0, unused, unused, unused};
    }
    if(mShowerShape>0){ //get slope factor from vertex=(0,0,0) to z of 6 slices
	StThreeVectorD off = mDb->getDetectorOffset(det);
	double dz=mDb->getZDepth(det);
	double smax=mDb->getShowerMaxZ(det);
	double a = mDb->getDetectorAngle(det) / 180.0 * M_PI;
	double z0 = off.z()/cos(a);
	for(int i=0; i<6; i++){
	    double z = (i+0.5)*dz/6.0;
	    mShowerShapeParameters[i*10+7]=(z0 + z)/(z0+smax);
	}
    }
    if(mDebug>0) {
	for(int i=0; i<6; i++){
	    LOG_INFO << Form("Shower Shape Parameters det=%1d slice=%1d : ",det,i);
	    for(int j=0; j<10; j++) {LOG_INFO << Form(" %10.6f",mShowerShapeParameters[i*10+j]);}
	    LOG_INFO << endm;
	}
    }
}

Int_t StFcsPointMaker::Make() {
    LOG_DEBUG << "StFcsPointMaker Make!!!" << endm;
    
    //if(mReadMuDst) return readMuDst();
    
    StEvent* event = static_cast<StEvent*>(GetInputDS("StEvent"));
    mFcsCollection=0;
    if (event) mFcsCollection = event->fcsCollection();
    if(!mFcsCollection) {
	LOG_WARN << "StFcsPointMaker did not find fcsCollection in StEvent" << endm;
	return kStWarn;	
    }
    
    for(int det=0; det<=kFcsEcalSouthDetId; det++) {
      fitClusters(det);	
    }
    if(mDebug>0) mFcsCollection->print(3);
    return kStOk;
}

void StFcsPointMaker::fitClusters(int det) {
  StSPtrVecFcsCluster&  clusters = mFcsCollection->clusters(det);
  StSPtrVecFcsPoint&    points = mFcsCollection->points(det);

  points.clear(); //clear all points
  int nclu=clusters.size();
  for(int i=0; i<nclu; i++) clusters[i]->points().clear(); //reset point pointer from cluster

  setShowerShapeParameters(det);

  for(int i=0; i<nclu; i++){ //loop over all clusters
    StFcsCluster* c=clusters[i];
    StFcsPoint* p0=new StFcsPoint();
    StFcsPoint* p1=new StFcsPoint();
    StFcsPoint* p2=new StFcsPoint();
    mNHit=c->nTowers();
    mEtot=c->energy();
    mX=new float[mNHit];
    mY=new float[mNHit];
    mE=new float[mNHit];
    for(int j=0; j<mNHit; j++){
	StFcsHit* h=c->hits()[j];
	mE[j]=h->energy();
	mDb->getLocalXYinCell(h, mX[j], mY[j]);
    }
    double chi1=99999.0, chi2=99999.0;
    switch(c->category()){
    case 0:
	chi1 = fit1PhotonCluster(c,p0);
	chi2 = fit2PhotonCluster(c,p1,p2);
	break;
    case 1:
	chi1 = fit1PhotonCluster(c,p0);
	break;
    case 2:
	chi2 = fit2PhotonCluster(c,p1,p2);
	break;
    }	
    // sotre chi2 for both
    c->setChi2Ndf1Photon(chi1);
    c->setChi2Ndf2Photon(chi2);
    // pick better one
    if(chi1<chi2){	
	p0->setDetectorId(det);
	p0->setCluster(c);
	p0->setNParentClusterPhotons(1);
	StThreeVectorD xyz=mDb->getStarXYZfromColumnRow(det,p0->x(),p0->y());
	p0->setXYZ(xyz);
	p0->setFourMomentum(mDb->getLorentzVector(xyz,p0->energy()));
	mFcsCollection->addPoint(det,p0);
	c->addPoint(p0);
	delete p1;
	delete p2;
    }else{
	p1->setDetectorId(det);
	p1->setCluster(c);
	p1->setNParentClusterPhotons(2);
	StThreeVectorD xyz1=mDb->getStarXYZfromColumnRow(det,p1->x(),p1->y());
	p1->setXYZ(xyz1);
	p1->setFourMomentum(mDb->getLorentzVector(xyz1,p1->energy()));
	mFcsCollection->addPoint(det,p1);

	p2->setDetectorId(det);
	p2->setCluster(c);
	p2->setNParentClusterPhotons(2);
	StThreeVectorD xyz2=mDb->getStarXYZfromColumnRow(det,p2->x(),p2->y());
	p2->setXYZ(xyz2);
	p2->setFourMomentum(mDb->getLorentzVector(xyz2,p2->energy()));
	mFcsCollection->addPoint(det,p2);

	c->addPoint(p1,p2);
	delete p0;
    }
    // delete hit arrays
    delete [] mX;
    delete [] mY;
    delete [] mE;
  }
  
  //loop over all found points and fill fourMomentum
  int np = points.size();
  for(int i=0; i<np; i++){
      StFcsPoint* p=points[i];
      StThreeVectorF xyz = mDb->getStarXYZfromColumnRow(det,p->x(),p->y());
      p->setFourMomentum(mDb->getLorentzVector(xyz,p->energy(),0.0));
  }
}


Double_t StFcsPointMaker::fit1PhotonCluster(StFcsCluster* c, StFcsPoint* p){
    int err;
    Double_t chi2 = -1.0;
    mMinuit.SetFCN(minimizationFunctionNPhoton);
    mMinuit.mncler();
    if(mMinuit.GetNumFixedPars() > 0) {mMinuit.mnfree(0);}
    double x=c->x();
    double y=c->y();
    double e=c->energy();
    double dx=m_PH1_DELTA_X;  //0.5
    double de=m_PH1_DELTA_E;  //1.15
    const std::vector<TString> names = {"np", "x", "y", "e"};    
    double one=1.0;
    double zero=0.0;
    mMinuit.DefineParameter(0, "np", one, zero,        one,   one);
    mMinuit.DefineParameter(1, "x",  x,   dx/5,       x-dx,  x+dx);
    mMinuit.DefineParameter(2, "y",  y,   dx/5,       y-dx,  y+dx);
    mMinuit.DefineParameter(3, "e",  e,   e*(1-de)/5, e/de,  e*de);
    //double fval; mMinuit.mnprin(1,fval);
    //mMinuit.FixParameter(0);
    if(m_PH1_FixEnergy==1) mMinuit.FixParameter(3);

    //run minimization
    err = -1;
    double arguments[2]={1000.0,1.0}; //Max calls and tolerance 
    mMinuit.mnexcm("MIGRAD", arguments, 2, err);
    
    if(mMinuit.GetStatus() == 0){
	double par[4],perr[4],g[4];
	for(int i=0; i<4; i++){ mMinuit.GetParameter(i, par[i], perr[i]); }	 	
 	p->setX(par[1]);
 	p->setY(par[2]);
 	p->setEnergy(par[3]);
	mMinuit.Eval(4, g, chi2, par, err);
    }
    return chi2;
} 

Double_t StFcsPointMaker::fit2PhotonCluster(StFcsCluster* c, StFcsPoint* p1, StFcsPoint* p2){ 
    int err;
    Double_t chi2 = -1.0;
    mMinuit.SetFCN(minimizationFunction2Photon);
    mMinuit.mncler();
    if(mMinuit.GetNumFixedPars() > 0) {mMinuit.mnfree(0);}
    double x=c->x();
    double y=c->y();
    double smax= c->sigmaMax();
    //    double smin= c->sigmaMin();
    double d=std::max(m_PH2_StartDggFactor*smax,0.5); //1.1 = 2.2/2.0
    double t=c->theta();
    double z=0.0; 
    double e=c->energy();    
    double dx=m_PH2_DELTA_X;     //0.2
    double ddl=d*m_PH2_LOW_DGG;  //0.8
    double ddh=d*m_PH2_HIGH_DGG; //3.0
    double dt=m_PH2_MAXTHETA_F;  //TMath::PiOver2()
    double de=m_PH2_DELTA_E;     //1.05
    mMinuit.mnparm(0, "np",    2.0, 0.0,        2.0,    2.0,   err);
    mMinuit.mnparm(1, "x",     x,   dx/5,       x-dx,   x+dx,  err);
    mMinuit.mnparm(2, "y",     y,   dx/5,       y-dx,   y+dx,  err);
    mMinuit.mnparm(3, "dgg",   d,   d*0.05,     ddl,    ddh,   err);
    mMinuit.mnparm(4, "theta", t,   dt/5,       t-dt,   t+dt,  err);
    mMinuit.mnparm(5, "zgg",   z,   0.1,        -0.99,  0.99,  err);
    mMinuit.mnparm(6, "etot",  e,   e*(1-de)/5, e/de,   e*de,  err);
    mMinuit.FixParameter(0);
    // Fix E_total and theta, we don't want these to be free parameters                                       
    if(m_PH2_FixTheta==1)  mMinuit.FixParameter(4);
    if(m_PH2_FixEnergy==1) mMinuit.FixParameter(6);

    //run minimization
    err = -1;
    double arguments[2]={1000.0,1.0}; //Max calls and tolerance 
    mMinuit.mnexcm("MIGRAD", arguments, 2, err);

    if(mMinuit.GetStatus() == 0) {
	double par[7],perr[7],g[7];
	for(int i=0; i<7; i++){ mMinuit.GetParameter(i, par[i], perr[i]); }	 	
	double x=par[1];
	double y=par[2];
	double d=par[3];
	double t=par[4];
	double z=par[5];
	double e=par[6];
	double x1 = x + cos(t)*d*(1.0-z)/2.0;
	double y1 = y + sin(t)*d*(1.0-z)/2.0;
	double e1 = e*(1.0+z)/2.0;
	double x2 = x - cos(t)*d*(1.0+z)/2.0;
	double y2 = y - sin(t)*d*(1.0+z)/2.0;
	double e2 = e*(1.0-z)/2.0;
	p1->setX(x1);
	p1->setY(y1);
	p1->setEnergy(e1);
	p2->setX(x2);
	p2->setY(y2);
	p2->setEnergy(e2);
	mMinuit.Eval(7, g, chi2, par, 1);
    }
    return chi2;
}

// Minimization function to be called from TMinuit
void StFcsPointMaker::minimizationFunctionNPhoton(Int_t& npara, Double_t* grad, Double_t& fval, Double_t* para, Int_t){
    fval = 0.0;
    const int nPhotons = static_cast<int>(para[0]);
    for(int i=0; i<mNHit; i++){
	double expected = 0.0;
	for (int j=0; j<nPhotons; j++){
	    int k = 3*j;
	    expected += para[k+3] * energyDepositionInTower(mX[i], mY[i], para[k+1], para[k+2]);
	}
	const double measured = mE[i];
	const double deviation = measured - expected;
	const double ratio = measured / mEtot;
	const double err = 0.01 + 0.03 * pow(ratio, 1.0-0.001*mEtot) * pow(1.0-ratio, 1.0-0.007*mEtot)*mEtot;
	fval += deviation * deviation / err;
	//LOG_INFO << Form("i=%2d M=%6.2f  E=%6.2f  D=%6.2f R=%6.2f E=%6.2f F=%6.2f",i,measured,expected,deviation,ratio,err,deviation * deviation / err)<<endm;
    }
    fval = std::max(fval, 0.);
    //LOG_INFO << Form("FVAL=%6.2f",fval)<<endm;
}

// Minimization function to be called from TMinuit for 2 photon case only
void StFcsPointMaker::minimizationFunction2Photon(Int_t& npara, Double_t* grad, Double_t& fval, Double_t* para, Int_t){
    // Only need to translate into the old parameterization
    const double dgg   = para[3];
    const double zgg   = para[5];
    const double angle = para[4];
    double oldPara[7] = { 
	para[0],  // Number of photons, unchanged
	para[1] + cos(angle) * dgg * (1 - zgg) / 2.0,  // x 1
	para[2] + sin(angle) * dgg * (1 - zgg) / 2.0,  // y 1
	para[6] * (1 + zgg) / 2.0,                     // e 1 
	para[1] - cos(angle) * dgg * (1 + zgg) / 2.0,  // x 2
	para[2] - sin(angle) * dgg * (1 + zgg) / 2.0,  // y 2
	para[6] * (1 - zgg) / 2.0                      // e 2
    }; 
    // Now call the regular minimization function with the translated parameters
    minimizationFunctionNPhoton(npara, grad, fval, oldPara, 0);
}

// getting energy deposition in a tower by summing up 6 z slices 
//  x,y are cell center position 
//  xun,yun are photon position in cell coordinate at Z=shower max
//  xc,yc are photon position at z of the slice 
//     (scale factor = mShowerShapeParameters[istart+7] calculated in setShowerShapeParameters()
Double_t StFcsPointMaker::energyDepositionInTower(Double_t x, Double_t y,Double_t xun, Double_t yun){
    double sum = 0.0;
    for(Int_t i=0; i<6; i++){       
        Int_t istart = i*10;
	if(mShowerShapeParameters[istart]>0.0){
	    double xc = xun * mShowerShapeParameters[istart+7];
	    double yc = yun * mShowerShapeParameters[istart+7];
	    sum += energyDepositionInTowerSingleLayer(x-xc,y-yc,&mShowerShapeParameters[istart]);
	}
    }
    return sum;
}

/*
  Int_t StFcsPointMaker::readMuDst(){
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event){LOG_INFO<<"StFcsPointMaker::readMuDst found no StEvent"<<endm; return kStErr;}
  StFcsCollection* fcscol = event->fcsCollection();
  if(!fcscol){LOG_INFO<<"StFcsPointMaker::readMuDst found no FcsCollection"<<endm; return kStErr;}
  for (unsigned i(0); i < fcscol->numberOfClusters(); ++i) {
      StFcsCluster* c = fcscol->clusters()[i];
      if(c){
	  StThreeVectorF xyz = mDb->getStarXYZfromColumnRow(c->detectorId(),c->x(),c->y());
	  //c->setFourMomentum(compute4Momentum(xyz, c->energy()));
	  c->setFourMomentum(mDb->getLorentzVector(xyz,c->energy()));
      }
  }
  for (unsigned i(0); i < fcscol->numberOfPoints(); ++i) {
    StFcsCluster* p = fcscol->points()[i];
    if(p){
      StThreeVectorF xyz  = mDb->getStarXYZ(p->detectorId(),p->x(),p->y());
      p->setXYZ(xyz);
      //p->setFourMomentum(compute4Momentum(xyz, p->energy()));
      p->setFourMomentum(mDb->getLorentzVector(xyz,p->energy()));
    }
  }
  fcscol->sortPointsByET();
  return kStOk;
}
*/

/*!
 * \class  StFgtAlignmentMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 * $Id: StFgtAlignmentMaker.cxx,v 1.6 2013/04/04 17:09:36 akio Exp $
 *
 */

#include "StFgtAlignmentMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#include "TNtuple.h"
#include "TMinuit.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TH1F.h"
#include "TH2F.h"
#include "StThreeVectorF.hh"
#include "StHelix.hh"
#include "StDetectorName.h"
#include "fgtAlignment.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include <curses.h>

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#define __READ_AVTRACK__
#ifdef __READ_AVTRACK__
#include "StRoot/StFgtPool/StFgtClusterTools/StFgtGeneralBase.h"
#include "StRoot/StFgtPool/StFgtClusterTools/StFgtStraightTrackMaker.h"
#endif

static const int mDebug=0;      // debug mesasge level
static const int mEventDisp=0;  // event display

static const int MAXTRK=50000;  //max number of tracks
static const int MAXHIT=20;     //max number of hits
static const int NPAR=24*6;     //number of parameters=(6 discs)*(4 quads)*(3 angles + 3 offset)

static const int NAXIS=4;                                //  number of axis for hist
static TH1F *hist1[kFgtNumDiscs][kFgtNumQuads][NAXIS];   // 1d residual histos
static TH2F *hist2[kFgtNumDiscs][kFgtNumQuads][NAXIS*2]; // 2d residual histos

static const double PI = 4.0*atan(1);

struct TRKHIT{  
  int nhit, nhitUse, nhitTpc;
  float dca, chi2;
  int det[MAXHIT];                          //0-23=FGT(disc*4+quad), 24=vertex, 25=TPC
  float x[MAXHIT],y[MAXHIT],z[MAXHIT];      //x=r and y=phi for FGT hit if mFgtInputRPhi=1
  float ex[MAXHIT],ey[MAXHIT],ez[MAXHIT];   //xyz error
  bool use[MAXHIT];                         //false=do not use in fit true=use in fit
};
const char* ntpdef="nhit/I:nhitUse:nhitTpc:dca/F:chi2:det[20]/I:x[20]/F:y[20]:z[20]:ex[20]:ey[20]:ez[20]:use[20]/O";

static int mNtrk[kFgtNumQuads];            // number of tracks per quad
static int mNtrkUse[kFgtNumQuads];         // number of usable tracks per quad after hitmask
static TRKHIT mHit[kFgtNumQuads][MAXTRK];  // storage to keep hit for track
static TRKHIT mHitAlg;                     // current working hits to be modified with aliggnment parameters
static int mQuad;                          // quad currently working on
static int mHitMaskDisc;                   // disc mask to define hits to be used hits in track fits. bit0-5=fgt disc 0-5, bit6=vtx, bit7=tpc
static int mFgtInputRPhi;                  // 0=x/y/z or  1=r/phi/z for fgt hits in mHit
static int mFillHist;                      // 0=not filling histo 1=filling histo before alignments 2=filling histo after alignment
static int mReqHit=0;                      // required # of hits
static int mReqTpcHit=0;                   // required # of TPC hits
static float mReqDca=0;                    // max dca to accept track
static float mReqChi2=0;                   // max chi2 to accept track

static StFgtDb* mDb; 
static fgtAlignment_st* orig_algpar;

//Move fgt hits with alignment parameter
void getAlign(int itrk, fgtAlignment_st* algpar){
  memcpy(&mHitAlg,&mHit[mQuad][itrk],sizeof(mHitAlg));
  if(mDebug>3) printf("getAlign quad=%1d trk=%d mHitAlg.nhit=%d %d\n",mQuad,itrk,mHitAlg.nhit,mHit[mQuad][itrk].nhit);
  for(int i=0; i<mHitAlg.nhit; i++){
    if(mHitAlg.det[i]<24){      
      int disc=mHitAlg.det[i]/4;
      int quad=mHitAlg.det[i]%4;
      double r,phi;
      if(mFgtInputRPhi==0){
	r=sqrt(mHitAlg.x[i]*mHitAlg.x[i]+mHitAlg.y[i]*mHitAlg.y[i]);
	phi=atan2(mHitAlg.y[i],mHitAlg.x[i]);
      }else{
	r=mHitAlg.x[i];
	phi=mHitAlg.y[i];
      }
      TVector3 xyz;
      mDb->getStarXYZ(disc,quad,r,phi,xyz,2,algpar);
      mHitAlg.x[i]=xyz.X();
      mHitAlg.y[i]=xyz.Y();
      mHitAlg.z[i]=xyz.Z();
      if(mDebug>3) printf("mHit[%3d] x=%8.3f y=%8.3f z=%8.3f det=%2d\n",i,
			mHitAlg.x[i],mHitAlg.y[i],mHitAlg.z[i],mHitAlg.det[i]);
    }
  }
}

//fill residual histos
void fillHist(int det, float dx, float dy, float dr, float dp, float x, float y, float r, float p){
  int disc=det/4;
  int quad=det%4;
  hist1[disc][quad][0]->Fill(dx);
  hist1[disc][quad][1]->Fill(dy);
  hist1[disc][quad][2]->Fill(dr);
  hist1[disc][quad][3]->Fill(dp);
  hist2[disc][quad][0]->Fill(x,dx);
  hist2[disc][quad][1]->Fill(y,dx);
  hist2[disc][quad][2]->Fill(x,dy);
  hist2[disc][quad][3]->Fill(y,dy);
  hist2[disc][quad][4]->Fill(r,dr);
  hist2[disc][quad][5]->Fill(p,dr);
  hist2[disc][quad][6]->Fill(r,dp);
  hist2[disc][quad][7]->Fill(p,dp);
  if(mDebug>3) cout << Form("d=%1d q=%1d dx=%8.3f dy=%8.3f dr=%8.3f dp=%9.5f",
			    disc,quad,dx,dy,dr,dp) <<endl;
}

//event display
void eventDisplay(double *par){
  //setup helix track
  static StThreeVectorD n(0,0,1);
  StThreeVectorD o(par[0],par[1],0);
  int sign=int(par[2]/fabs(par[2]));
  double phase=par[4] - sign*PI/2.0;
  StHelix h(fabs(par[2]),par[3],phase,o,sign);
  //set up TGraphs
  TGraph *hitxz=new TGraph(mHitAlg.nhit); hitxz->SetMarkerStyle(21); hitxz->SetMarkerSize(0.4); hitxz->SetMarkerColor(kBlue);
  TGraph *hityz=new TGraph(mHitAlg.nhit); hityz->SetMarkerStyle(21); hityz->SetMarkerSize(0.4); hityz->SetMarkerColor(kBlue);
  TGraph *hitpr=new TGraph(mHitAlg.nhit); hitpr->SetMarkerStyle(21); hitpr->SetMarkerSize(0.4); hitpr->SetMarkerColor(kBlue);
  //Plot hits
  for(int i=0; i<mHitAlg.nhit; i++){
    StThreeVectorD z(0,0,mHitAlg.z[i]);
    double s=h.pathLength(z,n);
    double x=h.x(s);
    double y=h.y(s);
    double dx = x - mHitAlg.x[i];
    double dy = y - mHitAlg.y[i];
    double dr = sqrt(x*x+y*y) - sqrt(mHitAlg.x[i]*mHitAlg.x[i]+mHitAlg.y[i]*mHitAlg.y[i]);
    double dp = atan2(y,x) - atan2(mHitAlg.y[i],mHitAlg.x[i]);
    if(dp>PI)  dp-=2*PI;
    if(dp<-PI) dp+=2*PI;
    double hx=mHitAlg.x[i];
    double hy=mHitAlg.y[i];
    double hz=mHitAlg.z[i];
    double hr=sqrt(hx*hx+hy*hy);
    double hp=atan2(hy,hx);
    hitxz->SetPoint(i,hz,dx);
    hityz->SetPoint(i,hz,dy);
    if(abs(dp)<0.1) hitpr->SetPoint(i,hr,dp);
    //if(mDebug>3) printf("residHelix %3d dx=%12.8f f=%12.8\n",i,dx,dy,f);
  }
  TCanvas *c1=new TCanvas("EventDisp","Event Disp",0,0,800,800);
  c1->Divide(2,2);
  c1->cd(1); hitxz->Draw("AP");
  c1->cd(2); hityz->Draw("AP");
  c1->cd(3); hitpr->Draw("AP");
  c1->Update();
  cin.get();
}

//Helix fit, called from TMinuit
void fitHelix(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>4) printf("fitHelix nhit=%d\n",mHitAlg.nhit);
  f=0;
  static StThreeVectorD n(0,0,1);         
  StThreeVectorD o(par[0],par[1],0); 
  int sign=int(par[2]/fabs(par[2]));
  double phase=par[4] - sign*PI/2.0;
  StHelix h(fabs(par[2]),par[3],phase,o,sign);
  for(int i=0; i<mHitAlg.nhit; i++){
    if(mHitAlg.use[i]==false) continue;
    StThreeVectorD z(0,0,mHitAlg.z[i]);
    double s=h.pathLength(z,n);    
    double dx = h.x(s) - mHitAlg.x[i];
    double dy = h.y(s) - mHitAlg.y[i];
    f += dx*dx+dy*dy;
    if(mDebug>5) printf("dx=%8.4f dy=%8.4f f=%8.4f\n",dx,dy,f);
  }
  if(mDebug>4) printf("fitHelix x0=%12.8f y0=%12.8f c=%12.8f dip=%12.8f phase=%12.8f f=%12.8f\n",
		      par[0],par[1],par[2],par[3],par[4],f);
}

//Get residuals with helix fit
double residHelix(double *par){
  double f=0;
  static StThreeVectorD n(0,0,1);
  StThreeVectorD o(par[0],par[1],0);
  int sign=int(par[2]/fabs(par[2]));
  double phase=par[4] - sign*PI/2.0;
  StHelix h(fabs(par[2]),par[3],phase,o,sign);  
  for(int i=0; i<mHitAlg.nhit; i++){
    if(mHitAlg.det[i]>=24) continue;
    StThreeVectorD z(0,0,mHitAlg.z[i]);
    double s=h.pathLength(z,n);
    double x=h.x(s);
    double y=h.y(s);
    double dx = x - mHitAlg.x[i];
    double dy = y - mHitAlg.y[i];
    f += dx*dx+dy*dy;
    if(mDebug>3) printf("residHelix %3d dx=%12.8f dy=%12.8f f=%12.8f\n",i,dx,dy,f);
    if(mFillHist>0) {
      double hx=mHitAlg.x[i];
      double hy=mHitAlg.y[i];
      double hr=sqrt(hx*hx+hy*hy);
      double hp=atan2(hy,hx);
      while(hp>StFgtGeom::phiQuadXaxis(2)) hp-=2*PI;
      while(hp<StFgtGeom::phiQuadXaxis(1)) hp+=2*PI;
      double dr = hr-sqrt(x*x+y*y);
      double dp = hp-atan2(y,x);
      while(dp>PI)  dp-=2*PI;
      while(dp<-PI) dp+=2*PI;
      fillHist(mHitAlg.det[i],dx,dy,dr,dp,hx,hy,hr,hp);
    }
  }
  if(mDebug>2) printf("residHelix x0=%12.8f y1=%12.8f c=%12.8f dip=%12.8f phase=%12.8f f=%12.8f\n",
		      par[0],par[1],par[2],par[3],par[4],f);
  if(mFillHist>1 && mEventDisp) eventDisplay(par);
  return f;
}

//Minimize residuals with helix fit, called from TMinuit
void funcHelix(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>3) printf("funcHelix mQuad=%d mNtrk=%d\n",mQuad,mNtrk[mQuad]);
  f=0;
  fgtAlignment_st* algpar= (fgtAlignment_st*)par;
  int flg;
  double arg[10],p[5],r, e;
  TMinuit *m = new TMinuit(5);
  m->SetFCN(fitHelix);
  arg[0] =-1; m->mnexcm("SET PRI", arg, 1,flg);
  arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,flg);
  arg[0] = 1; m->mnexcm("SET ERR", arg, 1,flg);
  arg[0] = 500; arg[1] = 1.;
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    if(mHit[mQuad][itrk].nhitUse<mReqHit ||
       mHit[mQuad][itrk].nhitTpc<mReqTpcHit ||
       mHit[mQuad][itrk].dca>mReqDca ||
       mHit[mQuad][itrk].chi2>mReqChi2 ) continue;
    //make "aligned" hits in mHitAlg
    getAlign(itrk,algpar);
    //first guess of helix fit parameters and set up
    double dx = mHitAlg.x[mHitAlg.nhit-1]-mHitAlg.x[0];
    double dy = mHitAlg.y[mHitAlg.nhit-1]-mHitAlg.y[0];
    double dz = mHitAlg.z[mHitAlg.nhit-1]-mHitAlg.z[0];
    double dr = sqrt(dx*dx+dy*dy);
    double dip = atan2(dz,dr);
    double slp = atan2(dy,dx);
    double x0= mHitAlg.x[0]-dx/dz*mHitAlg.z[0];
    double y0= mHitAlg.y[0]-dy/dz*mHitAlg.z[0];      
    m->mnparm(0,"x0"  ,x0       ,0.1,  0, 0,iflag);
    m->mnparm(1,"y0"  ,y0       ,0.1,  0, 0,iflag);
    m->mnparm(2,"curv",0.0000001,1.0,  0, 0,iflag);
    m->mnparm(3,"dip" ,dip      ,0.1,  0,PI,iflag);
    m->mnparm(4,"slp" ,slp      ,0.1,-PI,PI,iflag);
    //do helix fit
    m->mnexcm("MIGRAD", arg ,2, flg);
    for(int j=0; j<6; j++){ m->GetParameter(j,r,e); p[j]=r; }
    //add up residuals
    f+=residHelix(p);
  }
  if(mDebug>3) printf("funcHelix f=%12.6f xoff=%12.8f yoff=%12.8f\n",f,algpar->xoff[8],algpar->xoff[8]);
}

//Straight line fit, called from TMinuit
void fitLine(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>4) printf("fitLine nhit=%d\n",mHitAlg.nhit);
  f=0;  
  for(int i=0; i<mHitAlg.nhit; i++){    
    if(mHitAlg.use[i]==false) continue;
    double dx = par[0] + par[1] * mHitAlg.z[i] - mHitAlg.x[i];
    double dy = par[2] + par[3] * mHitAlg.z[i] - mHitAlg.y[i];    
    f += dx*dx+dy*dy;
    if(mDebug>5) printf("dx=%8.4f dy=%8.4f dr=%8.4f\n",dx,dy,f);
  } 
  if(mDebug>4) printf("fitLine x0=%12.8f x1=%12.8f y0=%12.8f y1=%12.8f f=%12.8f\n",par[0],par[1],par[2],par[3],f);
}

//Get residuals with straight line fit
double residLine(double *par){
  double f=0;
  for(int i=0; i<mHitAlg.nhit; i++){    
    if(mHitAlg.det[i]<24){
      double x=par[0] + par[1] * mHitAlg.z[i];
      double y=par[2] + par[3] * mHitAlg.z[i];
      double dx = x - mHitAlg.x[i];
      double dy = y - mHitAlg.y[i];    
      f += dx*dx+dy*dy;
      if(mDebug>5) printf("residLine %3d dx=%12.8f dy=%12.8f dr=%8.2f\n",i,dx,dy,dx*dx+dy*dy);
      if(mFillHist>0) {
	double hx=mHitAlg.x[i];
	double hy=mHitAlg.y[i];
	double hr=sqrt(hx*hx+hy*hy);
	double hp=atan2(hy,hx);
	while(hp>StFgtGeom::phiQuadXaxis(2)) hp-=2*PI;
	while(hp<StFgtGeom::phiQuadXaxis(1)) hp+=2*PI;
	double dr = hr- sqrt(x*x+y*y);
	double dp = hp- atan2(y,x);
	while(dp>PI)  dp-=2*PI;
	while(dp<-PI) dp+=2*PI;
	fillHist(mHitAlg.det[i],dx,dy,dr,dp,hx,hy,hr,hp);
      }
    }
  }  
  if(mDebug>4) printf("residLine x0=%12.8f x1=%12.8f y0=%12.8f y1=%12.8f f=%12.8f\n",par[0],par[1],par[2],par[3],f);
  return f;
}

//Minimize residuals with line fit, called from TMinuit
void funcLine(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>3) printf("funcLine mQuad=%d mNtrk=%d\n",mQuad,mNtrk[mQuad]);
  f=0;
  fgtAlignment_st* algpar= (fgtAlignment_st*)par;
  int flg;  
  double arg[10],p[4], r, e;
  TMinuit *m = new TMinuit(4);
  m->SetFCN(fitLine);
  arg[0] =-1; m->mnexcm("SET PRI", arg, 1,flg);
  arg[0] = 1; m->mnexcm("SET ERR", arg, 1,flg);
  arg[0] = 500; arg[1] = 1.; 
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    if(mHit[mQuad][itrk].nhitUse<mReqHit || 
       mHit[mQuad][itrk].nhitTpc<mReqTpcHit ||
       mHit[mQuad][itrk].dca>mReqDca ||
       mHit[mQuad][itrk].chi2>mReqChi2 ) continue;
    //make "aligned" hits in mHitAlg
    getAlign(itrk,algpar);    
    //first guess of line fit parameter and set up
    double x1 = (mHitAlg.x[mHitAlg.nhit-1]-mHitAlg.x[0])/(mHitAlg.z[mHitAlg.nhit-1]-mHitAlg.z[0]);
    double y1 = (mHitAlg.y[mHitAlg.nhit-1]-mHitAlg.y[0])/(mHitAlg.z[mHitAlg.nhit-1]-mHitAlg.z[0]);
    double x0 = mHitAlg.x[0] - x1*mHitAlg.z[0];
    double y0 = mHitAlg.y[0] - y1*mHitAlg.z[0];
    m->mnparm(0,"x0",x0,0.1,0,0,iflag);
    m->mnparm(1,"x1",x1,0.1,0,0,iflag);
    m->mnparm(2,"y0",y0,0.1,0,0,iflag);
    m->mnparm(3,"y1",y1,0.1,0,0,iflag);
    //do straight line fit
    m->mnexcm("MIGRAD", arg ,2, flg);
    for(int j=0; j<4; j++){ m->GetParameter(j,r,e); p[j]=r; }
    //add up residuals
    f+=residLine(p);
  } 
  if(mDebug>3) printf("funcLine f=%12.6f xoff=%12.8f yoff=%12.8f\n",f,algpar->xoff[8],algpar->xoff[8]);
}

ClassImp(StFgtAlignmentMaker);

StFgtAlignmentMaker::StFgtAlignmentMaker(const Char_t *name) : StMaker(name),mEventCounter(0),mDataSource(0),
							       mOutTreeFile(0),mInTreeFile(0),
							       mFgtXerr(0.05), mFgtYerr(0.05), mFgtZerr(0.2),
							       mDcaCut(5.0),mChi2Cut(0.02),mRunNumber(0),mNStep(0) {
}

Int_t StFgtAlignmentMaker::Init(){
  memset(mNtrk,0,sizeof(mNtrk));
  memset(mHit,0,sizeof(mHit));
  bookHist();
  return kStOK; 
}

Int_t StFgtAlignmentMaker::InitRun(Int_t runnum){
  LOG_INFO << "StFgtAlignmentMaker::InitRun for "  << runnum << endm;
  StFgtDbMaker *fgtDbMkr = static_cast< StFgtDbMaker* >( GetMakerInheritsFrom( "StFgtDbMaker" ) );
  //StFgtDbMaker *fgtDbMkr = static_cast<StFgtDbMaker * >( GetMaker("fgtDb"));
  if( !fgtDbMkr ){
    LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
    return kStFatal;      
  } else {
    mDb = fgtDbMkr->getDbTables();
    if( !mDb ){
      LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
		<< fgtDbMkr->GetName() << endm;
      return kStFatal;
    }
  }
  return kStOK;
}

static const int mMaxStep=100;
int mNStep;

void StFgtAlignmentMaker::setStep(int discmask,int quadmask, int parmask, int hitmask_disc,
				  int trackType, int minHit, int minTpcHit){
  if(mNStep>=mMaxStep) {printf("Reached MaxStep\n"); return; }
  if(mNStep==0) { printf("Step0 is making before histo, and masks are set to 0\n"); discmask=0; quadmask=0; parmask=0; }
  mDiscMask[mNStep]=discmask;
  mQuadMask[mNStep]=quadmask;
  mParMask[mNStep]=parmask;
  mHitMask[mNStep]=hitmask_disc;
  mTrackType[mNStep]=trackType;
  mMinHit[mNStep]=minHit;
  mMinTpcHit[mNStep]=minTpcHit;
  printf("Adding step=%d with discMask=%x quadMask=%x parMask=%x hitMask=%x trkType=%d minHit=%d minTpcHit=%d\n",
	 mNStep,discmask,quadmask,parmask,hitmask_disc,trackType,minHit,minTpcHit);
  mNStep++;
}


Int_t StFgtAlignmentMaker::Make() {
  if(mDataSource==0){
    readFromStEvent();  
  }else if(mDataSource==2){
#ifdef __READ_AVTRACK__    
    readFromStraightTrackMaker();  
#endif
  }
  return kStOK;
}

Int_t StFgtAlignmentMaker::Finish() {
  gMessMgr->Info() << "StFgtAlignmentMaker::Finish()" << endm;

  if(mDataSource==1)      {fakeData();}
  else if(mDataSource==3) {readFromTree();}
  else if(mOutTreeFile)   {writeTree();}

  cout << "mDb="<<mDb<<endl;
  orig_algpar=mDb->getAlignment();
  fgtAlignment_st result;
  memcpy(&result,orig_algpar,sizeof(fgtAlignment_st));

  cout << "Creating Histo before alignment"<<endl;  
  mFillHist=1;
  for(int quad=0; quad<kFgtNumQuads; quad++){ 
    mQuad=quad; 
    doAlignment(&result,mDiscMask[0],mQuadMask[0],mParMask[0],mHitMask[0],mTrackType[0],mMinHit[0],mMinTpcHit[0],&result);
  } 
  saveHist();
  if(mNStep<=1) return kStOK; //exit if only step0 exist

  cout << "Doing Alignment with Number of steps = "<<mNStep<<endl;  
  mFillHist=0;  
  for(int quad=0; quad<kFgtNumQuads; quad++){
    mQuad=quad;
    int quadmask= 1<<quad;
    cout << Form("Doing alignment for quad=%1d with Ntrk=%4d",quad,mNtrk[quad])<<endl;
    if(mNtrk[mQuad]>0){
      for(int s=1; s<mNStep; s++){	
	if( quadmask && mQuadMask[s] ){
	  printf("Quad=%d Step=%d with discMask=%x quadMask=%x parMask=%x hitMask=%x trkType=%d minHit=%d minTpcHit=%d\n",
		 quad,s,mDiscMask[s],quadmask,mParMask[s],mHitMask[s],mTrackType[s],mMinHit[s],mMinTpcHit[s]);
	  doAlignment(&result,mDiscMask[s],quadmask,mParMask[s],mHitMask[s],mTrackType[s],mMinHit[s],mMinTpcHit[s],&result);
	}
      }
    }
  }
  writePar(&result);        
  cout << "Creating Histo after alignment using last step's parameters"<<endl;
  mFillHist=2;
  resetHist();
  int s=mNStep-1;
  for(int quad=0; quad<kFgtNumQuads; quad++){ 
    mQuad=quad; 
    doAlignment(&result,0,0,0,mHitMask[s],mTrackType[s],mMinHit[s],mMinTpcHit[s],&result);
  }
  saveHist();
  return kStOK;
}


void StFgtAlignmentMaker::fakeData() {
  mFgtInputRPhi=0;
  orig_algpar=new fgtAlignment_st; 
  memset(orig_algpar,0,sizeof(fgtAlignment_st));
  int quad=0;
  mNtrk[quad]=0;
  memset(mHit,0,sizeof(mHit));
  for(int itrk=0; itrk<1; itrk++){
    mHit[quad][itrk].nhit=0;
    for(int d=0; d<10; d++){
      double z;
      mHit[quad][itrk].nhit++;
      if     (d==0) {mHit[quad][itrk].det[d]=24; z=0;}
      else if(d>6)  {mHit[quad][itrk].det[d]=25; z=200.0+d;}
      else{         
	mHit[quad][itrk].det[d]=(d-1)*4+quad;
	z=StFgtGeom::getDiscZ(d-1);
      }
      mHit[quad][itrk].x[d]=20.0/67.399*(z-10); mHit[quad][itrk].ex[d]=0.1;
      mHit[quad][itrk].y[d]=0.0;                mHit[quad][itrk].ey[d]=0.1;
      mHit[quad][itrk].z[d]=z;                  mHit[quad][itrk].ez[d]=0.1;
      if(d==3) mHit[quad][itrk].x[d]+=0.2; // added mis-alignment
      if(mDebug>0){
	cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
		   itrk,d,quad,mHit[quad][itrk].det[d],
		   mHit[quad][itrk].x[d],mHit[quad][itrk].y[d],mHit[quad][itrk].z[d],
		   mHit[quad][itrk].ex[d],mHit[quad][itrk].ey[d],mHit[quad][itrk].ez[d])
	    <<endl;
      }
    }
    mNtrk[quad]++;
  }
}

void StFgtAlignmentMaker::readFromStraightTrackMaker(){
  mFgtInputRPhi=1;
  mEventCounter++;  // increase counter

  //mudst for vertex
  int flagvtx=0;
  StThreeVectorF vertex,vtxerr;
  const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");
  if(muDst){
    StMuEvent *event = static_cast<StMuEvent*>(muDst->event());
    if(event){
      int nPrimV=muDst->numberOfPrimaryVertices();
      if(nPrimV>0) { 
	StMuPrimaryVertex* V= muDst->primaryVertex(0); // select highest rank vertex
	vertex=V->position();
	vtxerr=V->posError();
	flagvtx=1;
      }
    }
  }

  //Getting track from Anselm's straight tracker
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (!fgtSTracker){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No SyFgtStraightTrackMaker" << endm;
    return;          // if no event, we're done
  }
  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
    if(mDebug>0) cout<<Form("Trk chi2=%8.3f dca=%8.3f",t->chi2,t->dca)<<endl;
    vector<AVPoint>* points=t->points;
    int quad=-1, nhit=0, ntrk=-1;    
    for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
      int disc=p->dID;
      int quad2=p->quadID;
      if(quad==-1) quad=quad2;
      ntrk=mNtrk[quad];
      mHit[quad][ntrk].det[nhit]=disc*4+quad2;
      mHit[quad][ntrk].x[nhit]=p->r;
      mHit[quad][ntrk].y[nhit]=p->phi;
      mHit[quad][ntrk].z[nhit]=StFgtGeom::getDiscZ(disc);
      mHit[quad][ntrk].ex[nhit]=mFgtXerr;
      mHit[quad][ntrk].ey[nhit]=mFgtYerr;
      mHit[quad][ntrk].ez[nhit]=mFgtZerr;
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			     ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
			     mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			     mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		      <<endl;
      nhit++;
    }
    if(flagvtx==1){
      mHit[quad][ntrk].det[nhit]=24;
      mHit[quad][ntrk].x[nhit]=vertex.x();
      mHit[quad][ntrk].y[nhit]=vertex.y();
      mHit[quad][ntrk].z[nhit]=vertex.z();
      mHit[quad][ntrk].ex[nhit]=vtxerr.x();
      mHit[quad][ntrk].ey[nhit]=vtxerr.y();
      mHit[quad][ntrk].ez[nhit]=vtxerr.z();
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
			      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		       <<endl;
      nhit++;
    }
    mHit[quad][ntrk].nhit=nhit;
    mHit[quad][ntrk].dca =t->dca;
    mHit[quad][ntrk].chi2=t->chi2;
    mNtrk[quad]++;
  }
  if(mEventCounter%100==0) cout << Form("StFgtAlignmentMaker::readFromStraightTrackMaker EVT=%d NTRK=%d %d %d %d",
				       mEventCounter,mNtrk[0],mNtrk[1],mNtrk[2],mNtrk[3])<<endl;
}

void StFgtAlignmentMaker::readFromStEvent() {
  mFgtInputRPhi=1;
  mEventCounter++;  // increase counter	
  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No StEvent" << endm;
    return;          // if no event, we're done
  }
  cout << "Event: Run "<< event->runId() << " Event No: " << event->id() << endl;
  
  if (event->fgtCollection()) {
    cout << "# of FGT hits:            " << event->fgtCollection()->getNumHits() << endl;
  }else{
    cout << "No FGT collection" << endl;
  }

  UInt_t NpVX = event->numberOfPrimaryVertices();
  cout << "Number of Primary vertex="<<NpVX<<endl;
  if (!NpVX) return;

  for (UInt_t i = 0; i < NpVX; i++) {
    const StPrimaryVertex *vx = event->primaryVertex(i);
    cout << Form("Vertex: %3i ",i) << *vx << endl;
    UInt_t nDaughters = vx->numberOfDaughters();
    for (UInt_t j = 0; j < nDaughters; j++) {
      StPrimaryTrack* pTrack = (StPrimaryTrack*) vx->daughter(j);
      if (! pTrack) continue;
      //if (pTrack->geometry()->momentum().pseudoRapidity()<1.0) continue;
      int nfgt=0, ntpc=0, quad=-1;
      StPtrVecHit &hits=pTrack->detectorInfo()->hits();
      for (StPtrVecHitConstIterator iter=hits.begin(); iter != hits.end(); iter++){
	StDetectorId det=(*iter)->detector();
	if (det == kTpcId) ntpc++;
	if (det == kFgtId) {	    
	  nfgt++;
	  StFgtPoint* point=(StFgtPoint*)(*iter);
	  quad=point->getQuad();
	}
      }
      cout << Form("Track: Tpc=%2d Fgt=%1d Quad=%1d",ntpc,nfgt,quad) << *pTrack << endl;
      if(nfgt==0) continue; 
      int itrk=mNtrk[quad];
      int ihit=0;
      //get vertex
      mHit[quad][itrk].det[ihit]=24;
      StThreeVectorF vxyz=vx->position();
      mHit[quad][itrk].x[ihit]=vxyz.x();
      mHit[quad][itrk].y[ihit]=vxyz.y();
      mHit[quad][itrk].z[ihit]=vxyz.z();
      StThreeVectorF evxyz=vx->positionError();
      mHit[quad][itrk].ex[ihit]=evxyz.x();
      mHit[quad][itrk].ey[ihit]=evxyz.y();
      mHit[quad][itrk].ez[ihit]=evxyz.z();
      ihit++;
      for (StPtrVecHitConstIterator iter=hits.begin(); iter != hits.end(); iter++){
	StDetectorId det=(*iter)->detector();
	if (det == kTpcId){
	  mHit[quad][itrk].det[ihit]=25;
	  StThreeVectorF xyz=(*iter)->position();
	  mHit[quad][itrk].x[ihit]=xyz.x();
	  mHit[quad][itrk].y[ihit]=xyz.y();
	  mHit[quad][itrk].z[ihit]=xyz.z(); 
	  StThreeVectorF exyz=(*iter)->positionError();
	  mHit[quad][itrk].ex[ihit]=exyz.x();
	  mHit[quad][itrk].ey[ihit]=exyz.y();
	  mHit[quad][itrk].ez[ihit]=exyz.z();	    
	}else if (det == kFgtId){
	  StFgtPoint* point=(StFgtPoint*)(*iter);	    
	  int disc=point->getDisc();
	  int quad2=point->getQuad();
	  mHit[quad][itrk].det[ihit]=disc*4+quad2;
	  StThreeVectorF xyz=(*iter)->position();
	  mHit[quad][itrk].x[ihit]=point->getPositionR();
	  mHit[quad][itrk].y[ihit]=point->getPositionPhi();
	  mHit[quad][itrk].z[ihit]=StFgtGeom::getDiscZ(disc);
	  StThreeVectorF exyz=(*iter)->positionError();
	  if(mFgtXerr>0.0) {mHit[quad][itrk].ex[ihit]=mFgtXerr;} else {mHit[quad][itrk].ex[ihit]=exyz.x();}
	  if(mFgtYerr>0.0) {mHit[quad][itrk].ey[ihit]=mFgtYerr;} else {mHit[quad][itrk].ey[ihit]=exyz.y();}
	  if(mFgtZerr>0.0) {mHit[quad][itrk].ez[ihit]=mFgtZerr;} else {mHit[quad][itrk].ez[ihit]=exyz.z();}
	}
	ihit++;
      }
      mHit[quad][itrk].nhit=ihit;
      for(int i=0; i<mHit[quad][itrk].nhit; i++){
	cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
		   itrk,i,quad,mHit[quad][itrk].det[i],
		   mHit[quad][itrk].x[i],mHit[quad][itrk].y[i],mHit[quad][itrk].z[i],
		   mHit[quad][itrk].ex[i],mHit[quad][itrk].ey[i],mHit[quad][itrk].ez[i])
	    <<endl;
      }
      mNtrk[quad]++;
    }//loop over tracks
  }//loop over vertex
}

void StFgtAlignmentMaker::writeTree(){
  cout << "Creating "<<mOutTreeFile<<endl;
  TFile *f=new TFile(mOutTreeFile,"RECREATE","tracks");
  for(int quad=0; quad<kFgtNumQuads; quad++){
    char c[10];
    sprintf(c,"q%1d",quad);
    TTree* t = new TTree(c,c);
    t->Branch("trk",&mHitAlg,ntpdef);
    int itrk;
    for(itrk=0; itrk<mNtrk[quad]; itrk++){
      memcpy(&mHitAlg,&mHit[quad][itrk],sizeof(mHitAlg));
      t->Fill();
    }
    cout<<Form(" Wrote %6d tracks for quad=%1d",itrk,quad)<<endl;
  }
  f->Write();
  f->Close();   
}

void StFgtAlignmentMaker::readFromTree(){
  cout <<"ReadFromTree mDb="<<mDb<<endl;
  mFgtInputRPhi=1;
  if(!mInTreeFile) {
    cout<<"StFgtAlignment: Please set input TTree file name using setReadTree()" << endl;
    return;
  }
  cout << "Reading "<<mInTreeFile<<endl;
  TFile *f=new TFile(mInTreeFile);
  for(int quad=0; quad<kFgtNumQuads; quad++){
    char c[10];
    sprintf(c,"q%1d",quad);
    TTree *t = (TTree*) f->Get(c);
    t->SetBranchAddress("trk",&mHitAlg);    
    mNtrk[quad]=t->GetEntries();
    int itrk;
    for(itrk=0; itrk<mNtrk[quad]; itrk++){
      t->GetEntry(itrk);
      memcpy(&mHit[quad][itrk],&mHitAlg,sizeof(mHitAlg));
      //cout << Form("q=%1d itrk=%6d ",quad,itrk)<<" mDb="<<mDb<<endl;
    }
    cout<<Form(" Read %6d tracks for quad=%1d",itrk,quad)<<endl;
  }
  f->Close();
}

void StFgtAlignmentMaker::setPar(TMinuit* m, fgtAlignment_st* algpar,int discmask,int quadmask, int parmask){
  char nam[20];
  int iflag;
  double *st,*mn,*mx;
  double stp[2]={ 0.01, 0.1};
  double min[2]={-0.10,-5.0};
  double max[2]={ 0.10, 5.0};
  double zero[2]={0.00, 0.0};
  char* name[6]={"phi  ","theta","psi  ","xoff ","yoff ","zoff "};
  double *par = &(algpar->phi[0]);
  for(int disc=0; disc<6; disc++){
    for(int quad=0; quad<4; quad++){
      int i=disc*4+quad;
      for(int p=0; p<6; p++){
	int j= i + p*24;
	if( (discmask & 1<<disc) && (quadmask & 1<<quad) && (parmask & 1<<p) ){st=stp; mn=min; mx=max;}
	else {st=zero; mn=zero; mx=zero;}
	sprintf(nam,"D%1dQ%1d-%5s"  ,disc,quad,name[p]); 
	int k=p/3;
	m->mnparm(j, nam, par[j], st[k], mn[k], mx[k], iflag);
      }
    }
  }
}

void StFgtAlignmentMaker::getPar(TMinuit* m, fgtAlignment_st* algpar){
  double r,e;
  double *par= &(algpar->phi[0]);
  for(int disc=0; disc<6; disc++){
    for(int quad=0; quad<4; quad++){
      int i=disc*4+quad;
      for(int p=0; p<6; p++){
	int j= i + p*24;
	m->GetParameter(j,r,e); 
	par[j]=r;
      }
    }
  }
}

void StFgtAlignmentMaker::writePar(fgtAlignment_st* algpar){
  char fname[50]="fgt_alignment.dat";
  int yearday=mRunNumber/1000;
  if(mRunNumber>0) sprintf(fname,"%d/fgt_alignment_%d.dat",yearday,mRunNumber);
  printf("Writing %s\n",fname);
  FILE *f=fopen(fname,"w");
  for(int disc=0; disc<6; disc++){
    for(int quad=0; quad<4; quad++){
      int i=disc*4+quad;
      printf("%1d %1d %3d %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
	     disc,quad,i,
	     algpar->phi[i], algpar->theta[i],algpar->psi[i],
	     algpar->xoff[i],algpar->yoff[i],algpar->zoff[i]);
      fprintf(f,"%1d %1d %3d %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
	     disc,quad,i,
	     algpar->phi[i], algpar->theta[i],algpar->psi[i],
	     algpar->xoff[i],algpar->yoff[i],algpar->zoff[i]);
    }
  }
}

void StFgtAlignmentMaker::bookHist(){
  const float maxdx=0.2;
  const float maxdp=0.005;
  //1d residuals
  const char* caxis1[NAXIS]={"dx","dy","dr","dphi"};
  const float ymax1[NAXIS]={maxdx,maxdx,maxdx,maxdp};
  //2d residuals
  const char* caxis2[NAXIS*2]={"dx.vs.x","dx.vs.y",  "dy.vs.x",  "dy.vs.y",
			       "dr.vs.r","dr.vs.phi","dphi.vs.r","dphi.vs.phi"};
  const float ymax2[NAXIS*2]={maxdx,maxdx,maxdx,maxdx,maxdx,maxdx,maxdp,maxdp};
  float xmin2,xmax2;
  float xmin[4]={  0,-10,-40,-40};
  float xmax[4]={ 40, 40,  0, 10};
  float ymin[4]={-10,-40,-40,  0};
  float ymax[4]={ 40,  0, 10, 40};
  float rmin=0.0, rmax=40.0;
  float phimin[4],phimax[4];
  phimin[0]=StFgtGeom::phiQuadXaxis(0); phimax[0]=StFgtGeom::phiQuadXaxis(3);
  phimin[1]=StFgtGeom::phiQuadXaxis(1); phimax[1]=StFgtGeom::phiQuadXaxis(0);
  phimin[2]=StFgtGeom::phiQuadXaxis(2); phimax[2]=StFgtGeom::phiQuadXaxis(1)+2*PI;
  phimin[3]=StFgtGeom::phiQuadXaxis(3); phimax[3]=StFgtGeom::phiQuadXaxis(2);

  const int nbin=50;
  const char* cquad[kFgtNumQuads]={"A","B","C","D"};
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int axis=0; axis<NAXIS; axis++){
	char c[50];
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],caxis1[axis]);
	hist1[disc][quad][axis]=new TH1F(c,c,nbin,-ymax1[axis],ymax1[axis]);

	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],caxis2[axis]);
	if(axis==0 || axis==2){
	  xmin2=xmin[quad]; xmax2=xmax[quad];
	}else{
	  xmin2=ymin[quad]; xmax2=ymax[quad];
	}
	hist2[disc][quad][axis]=new TH2F(c,c,nbin, xmin2,xmax2,nbin,-ymax2[axis],ymax2[axis]);

	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],caxis2[axis+NAXIS]);
	if(axis==0 || axis==2){
	  xmin2=rmin; xmax2=rmax;
	}else{
	  xmin2=phimin[quad]; xmax2=phimax[quad];
	}
	hist2[disc][quad][axis+NAXIS]=new TH2F(c,c,nbin, xmin2,xmax2,nbin,-ymax2[axis+NAXIS],ymax2[axis+NAXIS]);
      }
    }
  }
}

void StFgtAlignmentMaker::resetHist(){
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int axis=0; axis<NAXIS; axis++){
	hist1[disc][quad][axis]->Reset("ICES");
	hist2[disc][quad][axis]->Reset("ICES");
	hist2[disc][quad][axis+NAXIS]->Reset("ICES");
      }
    }
  }
}

void StFgtAlignmentMaker::saveHist(){
  char fname[3][50]={"alignment.root","alignment_before.root","alignment_after.root"};
  if(mRunNumber>0) {
    int yearday=mRunNumber/1000;
    sprintf(fname[0],"%d/alignment_%d.root",yearday,mRunNumber);
    sprintf(fname[1],"%d/alignment_before_%d.root",yearday,mRunNumber);
    sprintf(fname[2],"%d/alignment_after_%d.root",yearday,mRunNumber);
  }
  cout << "Writing " << fname[mFillHist] << endl;
  TFile *hfile = new TFile(fname[mFillHist],"update");  
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int axis=0; axis<NAXIS; axis++){
	hist1[disc][quad][axis]->Write();
	hist2[disc][quad][axis]->Write();
	hist2[disc][quad][axis+NAXIS]->Write();
      }
    }
  }
  hfile->Close();
}

void StFgtAlignmentMaker::setHitMask(int hitmask_disc){
  cout << Form("Alignment for Quad=%1d using Hits from=",mQuad);
  for(int i=0; i<6; i++) {if(hitmask_disc & (1<<i) ) {cout << Form("FgtD%1d ",i+1);}}
  if(hitmask_disc & 0x40) cout << "Vertex ";
  if(hitmask_disc & 0x80) cout << "TPC ";
  cout << Form("(hitmask_disc = %02x)",hitmask_disc);
  mHitMaskDisc=hitmask_disc;
  mNtrkUse[mQuad]=0;
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    mHit[mQuad][itrk].nhitUse=0;
    mHit[mQuad][itrk].nhitTpc=0;
    for(int ihit=0; ihit<mHit[mQuad][itrk].nhit; ihit++){
      mHit[mQuad][itrk].use[ihit]=false;
      int det=mHit[mQuad][itrk].det[ihit];      
      int disc=det/4;          //fgt disc0-5
      if     (det==24) disc=6; //vertex
      else if(det==25) {       //tpc
	disc=7; 
	mHit[mQuad][itrk].nhitTpc++;
      }
      if(hitmask_disc & (1<<disc)) {
	mHit[mQuad][itrk].use[ihit]=true; 
	mHit[mQuad][itrk].nhitUse++;
      }
    }
    if(mHit[mQuad][itrk].nhitUse>=mReqHit &&
       mHit[mQuad][itrk].nhitTpc>=mReqTpcHit &&
       mHit[mQuad][itrk].dca<=mReqDca &&
       mHit[mQuad][itrk].chi2<=mReqChi2 ) mNtrkUse[mQuad]++;
  }
  cout << Form(" usable ntrack=%d with nhit>=%d nTpcHit>=%d dca<%f chi2<%f",
	       mNtrkUse[mQuad],mReqHit,mReqTpcHit,mReqDca,mReqChi2)<<endl;
}

void StFgtAlignmentMaker::doAlignment(fgtAlignment_st* input, 
				      int discmask,int quadmask, int parmask, int hitmask_disc, 
				      int trackType, int minHit, int minTpcHit,
				      fgtAlignment_st* result){  
  double arg[10];
  int iflag, min=0;

  TMinuit *m = new TMinuit(NPAR);
  arg[0] =-1; m->mnexcm("SET PRI", arg ,1,iflag);   
  arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,iflag);
  arg[0] = 1; m->mnexcm("SET ERR", arg ,1,iflag);
  if(trackType==0)      { m->SetFCN(funcLine);  min=2;}
  else if(trackType==1) { m->SetFCN(funcHelix); min=3;}
  mReqHit=minHit;
  if(mReqHit<min) mReqHit=min;
  mReqTpcHit=minTpcHit;
  mReqDca=mDcaCut;
  mReqChi2=mChi2Cut;

  setHitMask(hitmask_disc);
  if(mNtrkUse[mQuad]==0) return;
  setPar(m,input,discmask,quadmask,parmask);

  if(parmask>0) {
    arg[0] = 0; m->mnexcm("SET PRI", arg ,1,iflag);
    //arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,iflag);
  }

  if(mFillHist>0){
    int np=NPAR;
    double *gin, f;
    double *par = (double *) input;
    if     (trackType==0) { funcLine (np,gin,f,par,iflag);}
    else if(trackType==1) { funcHelix(np,gin,f,par,iflag);}
  }else{    
    arg[0] = 20000; arg[1] = 1.; m->mnexcm("MIGRAD", arg ,2, iflag);
    if(mFillHist==0) getPar(m,result);
  }
}


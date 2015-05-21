/*!
 * \class  StFgtAlignmentMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 * $Id: StFgtAlignmentMaker.cxx,v 1.11 2015/05/21 19:46:15 akio Exp $
 *
 */

#include <stdio.h>
#include <math.h>
#include <curses.h>

#include "StFgtAlignmentMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#include "TNtuple.h"
#include "TMinuit.h"
#include "TRandom.h"
#include "TCanvas.h"
#include "TText.h"
#include "TGraph.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TArrayL64.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"
#include "THelixTrack.h"
#include "StDetectorName.h"
#include "fgtAlignment.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "StRoot/StFgtPool/StFgtClusterTools/StFgtGeneralBase.h"
#include "StRoot/StFgtPool/StFgtClusterTools/StFgtStraightTrackMaker.h"

#include "StRoot/StEEmcPool/StEEmcIUPi0/StEEmcIUPointMaker.h"
#include "StRoot/StEmcUtil/geometry/StEmcGeom.h"

static const int mDebug=0;      // debug mesasge level
static const int mEventDisp=0;  // event display

static const int MAXTRK=500000;   //max number of tracks
static const int MAXHIT=8;      //max number of hits
static const int NPAR=24*6;      //number of parameters=(6 discs)*(4 quads)*(3 angles + 3 offset)

static const int NAXIS=4;                                  //  number of axis for hist
static TH1F *hist1[kFgtNumDiscs+6][kFgtNumQuads][NAXIS];   // 1d residual histos
static TH2F *hist2[kFgtNumDiscs+6][kFgtNumQuads][NAXIS*2]; // 2d residual histos
static TH1F *histdz;

static const double PI = 4.0*atan(1);

const float ISOCUT=0.5;
const float maxdfgt=0.15,   maxdvtx=0.5,  maxdtpc=10.0, maxdemc=10.0;
const float maxpfgt=0.005,  maxpvtx=0.5, maxptpc= 0.3, maxpemc=0.3;

struct TRKHIT{  
  int run,evt,nhit,nhitUse,nhitFgt,nhitVtx,nhitTpc,nhitPrompt,nhitEemc,used;
  float dca, chi2, trkz, dz, eta, phi, opt;                 //track parameters (last one is 1/pT)
  int det[MAXHIT];                                          //0-23=FGT(disc*4+quad), 24-27=vertex, 28-31=TPC, 32-35=Prompt, 36-39=EEMC separated by quad
  float x[MAXHIT],y[MAXHIT],z[MAXHIT];                      //xyz
  float ex[MAXHIT],ey[MAXHIT],ez[MAXHIT];                   //xyz error  
  float lr[MAXHIT],lp[MAXHIT],r[MAXHIT],p[MAXHIT];          //local and globak r and phi
  float dx[MAXHIT],dy[MAXHIT],dr[MAXHIT],dp[MAXHIT];        //residuals
  float tw[MAXHIT],p1[MAXHIT],p2[MAXHIT],po[MAXHIT];        //EEMC energies (FGT: tw=ChgPhi, p1=ChgR, p2=PhiRAsy, po=EvenOddAsy)
  float su[MAXHIT],sv[MAXHIT];                              //ESMD energies
  int nrl[MAXHIT],nsu[MAXHIT],nsv[MAXHIT];                  //eemc number of relatives, strips in smd u & v
  float ele[4],trk[4],bemc[4],eemc[4],tot[4],rec[4],iso[4]; //4 momentums of ele/tracks/bemc/eemc/total(trk+bemc+eemc)/recoil(=tot-ele)/isolation cone
  float ptbal,riso;                                         //pt balance & energy ratio ele/total in isolation cone
  bool use[MAXHIT];                                         //false=do not use in fit true=use in fit
};

static int mNtrk[kFgtNumQuads];            // number of tracks per quad
static int mNtrkUse[kFgtNumQuads];         // number of usable tracks per quad after hitmask
static TRKHIT mHit[kFgtNumQuads][MAXTRK];  // storage to keep hit for track
static TRKHIT mHitAlg;                     // current working hits to be modified with aliggnment parameters
static int mQuad;                          // quad currently working on
static int mStep;                          // step number
static int mHitMaskDisc;                   // disc mask to define hits to be used hits in track fits. bit0-5=fgt, bit6=vtx, bit7=tpc, bit8=prompt, bit9=eemc
static int mResidMaskDisc;                 // disc mask to include residual sum of all tracks for alignments
static int mFgtInputRPhi;                  // 0=x/y/z or  1=r/phi/z for fgt hits in mHit
static int mFillHist;                      // 0=not filling histo 1=filling histo before alignments 2=filling histo after alignment
static int mReqHit=0;                      // required # of hits
static int mReqFgtHit=0;                   // required # of FGT hits
static int mReqVtx=0;                      // required vertex
static int mReqTpcHit=0;                   // required # of TPC hits
static int mReqPromptHit=0;                // required # of TPC prompt hits
static int mReqEemcHit=0;                  // required # of EEMC hits
static float mErrFGT=0;
static float mErrVTX=0;
static float mErrVTXZ=0;
static float mErrTPCI=0;
static float mErrTPCO=0;
static float mErrTPCZ=0;
static float mErrPPT=0;
static float mErrEMC=0;
static int mBeamLine=1;

static StFgtDb* mDb; 
static fgtAlignment_st* orig_algpar;

static StThreeVectorF ele,tracks,bemcs,eemcs,tot,rec,iso;
static float ptbal,riso;

#define SMALL_NUM   1e-4

class Vector{
public:
  Vector(double vx=0, double vy=0, double vz=0):x(vx),y(vy),z(vz){}
  Vector(const Vector& v):x(v.x),y(v.y),z(v.z){}
  Vector(const Vector& v1, const Vector& v2):x(v1.x-v2.x),y(v1.y-v2.y),z(v1.z-v2.z){}
  //  Vector operator=(const Vector& v) {return Vector(v.x,v.y,v.z);}
  Vector operator=(const Vector& v) {x=v.x; y=v.y; z=v.z;}
  Vector operator-() {return Vector(-x,-y,-z);}
  friend Vector operator+(const Vector& v1, const Vector& v2) {return Vector(v1.x+v2.x,v1.y+v2.y,v1.z+v2.z);}
  friend Vector operator-(const Vector& v1, const Vector& v2) {return Vector(v1.x-v2.x,v1.y-v2.y,v1.z-v2.z);}
  friend Vector operator*(const double d, const Vector& v) {return Vector(d*v.x,d*v.y,d*v.z);}
  friend Vector operator*(const Vector& v, const double d) {return Vector(d*v.x,d*v.y,d*v.z);}
  friend double operator*(const Vector& v1, const Vector& v2) {return v1.x*v2.x+v1.y*v2.y+v1.z*v2.z;}
  double length() {return sqrt((*this)*(*this));}
  void print(){printf("x=%8.3f y=%8.3f z=%8.3f\n",x,y,z);}
  double x,y,z;
};

class Line{
public:
  Line(Vector& u0, Vector& u1):v0(u0),v1(u1) {};
  Vector v0,v1;
};

double distance(Line l1, Line l2, Vector& p1, Vector& p2){
  Vector u(l1.v1,l1.v0);
  Vector v(l2.v1,l2.v0);
  Vector w(l1.v0,l2.v0);
  double a=u*u;
  double b=u*v;
  double c=v*v;
  double d=u*w;
  double e=v*w;
  double D=a*c - b*b;
  double sc,tc;
  if(D<SMALL_NUM) { // the lines are almost parallel
    sc = 0.0;
    tc = (b>c ? d/b : e/c);    // use the largest denominator
    printf("SMALL NUM!!!\n");
  }else{
    sc = (b*e - c*d) / D;
    tc = (a*e - b*d) / D;
  }
  // 2 closest point
  p1=l1.v0 + (sc * u);
  p2=l2.v0 + (tc * v);
  // get the difference of the two closest points
  Vector dP = p1 - p2;
  //Vector dP = w + (sc * u) - (tc * v);  // =  L1(sc) - L2(tc)
  return dP.length();   // return the closest distance
}

void getZvtxAndDca(double* p, double& dca, double& z, double zmin=-900, double zmax=900){
  double x0=0,y0=0,dxdz=0,dydz=0;
  if(mFgtInputRPhi>0){
    St_vertexSeedC* vSeed = St_vertexSeedC::instance();
    x0=vSeed->x0(); dxdz=vSeed->dxdz();
    y0=vSeed->y0(); dydz=vSeed->dydz();
  }
  Vector p1(p[0]+p[1]*zmin, p[2]+p[3]*zmin, zmin);
  Vector p2(p[0]+p[1]*zmax, p[2]+p[3]*zmax, zmax);
  Vector p3(x0+dxdz*zmin, y0+dydz*zmin, zmin);
  Vector p4(x0+dxdz*zmax, y0+dydz*zmax, zmax);
  Vector p5,p6;
  Line l1(p1,p2), l2(p3,p4);
  dca=distance(l1,l2,p5,p6); //3d DCA length
  z=p6.z;  //z of DCA point on beamline constrain
  //printf("BEAM=%8.2f %8.2f %8.2f FGT=%8.2f %8.2f %8.2f\n",p6.x,p6.y,p6.z,p5.x,p5.y,p5.z);
}

inline StPhysicalHelix getStPhysicalHelix(double x0, double y0, double curve, double dip, double slp){
  //printf("x0=%8.4f y0=%8.4f curv=%8.4f dip=%8.4f slp=%8.4f -> ",x0,y0,curve,dip,slp);
  StThreeVectorD o(x0,y0,0);
  int sign=int(curve/fabs(curve));
  double phase=slp - sign*PI/2.0;
  StPhysicalHelix h(fabs(curve),dip,phase,o,sign);
  //printf("GetHelix c=%10.7f  sign=%2d  h=%2d\n",curve,sign,h.h());
  //StThreeVectorD t1=h.at(0.0);
  //printf(" x1=%8.4f y1=%8.4f z1=%8.4f \n",t1.x(),t1.y(),t1.z());
  //StThreeVectorD t2=h.at(100.0);
  //printf(" x2=%8.4f y2=%8.4f z2=%8.4f \n",t2.x(),t2.y(),t2.z());
  //StThreeVectorD t3=h.at(200.0);
  //printf(" x3=%8.4f y3=%8.4f z3=%8.4f \n",t3.x(),t3.y(),t3.z());
  return h;
};

inline StPhysicalHelix getStPhysicalHelix(double *p) {return getStPhysicalHelix(p[0],p[1],p[2],p[3],p[4]);}

inline StPhysicalHelix getStPhysicalHelix(double x0, double y0, double dxdz, double dydz, double curve, int sign) {
  double dr=sqrt(dxdz*dxdz+dydz*dydz);
  double dip = atan2(1.0,dr);
  double slp = atan2(dydz,dxdz);
  return getStPhysicalHelix(x0,y0,curve*sign,dip,slp);
}

void getZvtxAndDcaFromHelix(double* par, double& dca, double& z, double &eta, double &phi, double& opt){
  St_vertexSeedC* vSeed = St_vertexSeedC::instance();

  //beamline
  //double xyz[3]={vSeed->x0(),vSeed->y0(), 0.0};
  //double dir[3]={vSeed->dxdz(),vSeed->dydz(),1.0};
  //if(mFgtInputRPhi==0){ xyz[0]=0.0; xyz[1]=0.0; xyz[2]=0.0; dir[0]=0.00001; dir[1]=0.00001; dir[2]=1;}
  //THelixTrack bb(xyz,dir,0.0000000001);
  //bb.Move(0.0);   printf(" BLT x1=%8.4f y1=%8.4f z1=%8.4f \n",bb.Pos()[0],bb.Pos()[1],bb.Pos()[2]);
  //bb.Move(100.0); printf(" BLT x2=%8.4f y2=%8.4f z2=%8.4f \n",bb.Pos()[0],bb.Pos()[1],bb.Pos()[2]); bb.Move(-100);
  //bb.Move(z);     printf(" BLT x3=%8.4f y3=%8.4f z3=%8.4f \n",bb.Pos()[0],bb.Pos()[1],bb.Pos()[2]); bb.Move(-z);

  //track
  double xyz2[3]={par[0], par[1], 0.0};
  double dir2[3]={cos(par[4])/tan(par[3]),sin(par[4])/tan(par[3]),1.0};
  THelixTrack hh(xyz2,dir2,par[2]);
  //hh.Move(0.0);   printf(" TRT x1=%8.4f y1=%8.4f z1=%8.4f \n",hh.Pos()[0],hh.Pos()[1],hh.Pos()[2]);
  //hh.Move(100.0); printf(" TRT x2=%8.4f y2=%8.4f z2=%8.4f \n",hh.Pos()[0],hh.Pos()[1],hh.Pos()[2]); hh.Move(-100);
  //hh.Move(z);     printf(" TRT x3=%8.4f y3=%8.4f z3=%8.4f \n",hh.Pos()[0],hh.Pos()[1],hh.Pos()[2]); hh.Move(-z);
  
  //get DCA
  double x0=0, y0=0, z0=0;
  if(mFgtInputRPhi>0){ x0=vSeed->x0(); y0=vSeed->y0();}
  double ss3=0;
  for(int i=0; i<5; i++){
    ss3=hh.Path(x0,y0); //closest point to x0,y0 line
    hh.Move(ss3);
    z0=hh.Pos()[2];
    if(mFgtInputRPhi>0){
      x0=vSeed->x0()+vSeed->dxdz()*z0;
      y0=vSeed->y0()+vSeed->dydz()*z0;
    }
    double x=hh.Pos()[0], y=hh.Pos()[1], z=hh.Pos()[2];
    double dx=x-x0, dy=y-y0, dz=z-z0;
    dca=sqrt(dx*dx+dy*dy+dz*dz);
    //printf("length=%8.4f x0=%8.4f y0=%8.4f z0=%8.4f : x=%8.4f y=%8.4f z=%8.4f dca=%8.4f\n",ss3,x0,y0,z0,hh.Pos()[0],hh.Pos()[1],hh.Pos()[2],dca);
    hh.Move(-ss3);
  }
  z=z0;     //z of DCA point on beamline constrain
  double bfield=0.5*tesla;
  StPhysicalHelix h = getStPhysicalHelix(par);
  StThreeVectorD p = h.momentumAt(ss3,bfield);
  //StThreeVectorD x = h.at(ss3);
  //printf("ST x=%8.4f y=%8.4f z=%8.4f\n",x.x(),x.y(),x.z());
  int sign=h.charge(bfield);
  eta=p.pseudoRapidity();
  phi=p.phi();
  opt=sign/p.mag();
  
  /*doesn't seems to work
  //make StPhysicalHelix from fitted parameters
  StPhysicalHelix h = getStPhysicalHelix(par);
  //make StPhysicalHelix from beamLine with very small curvature
  StPhysicalHelix b;
  if(mFgtInputRPhi==0){
    b = getStPhysicalHelix(0,0,0.000001,0.000001,0.0000000001,1);
  }else{
    b = getStPhysicalHelix(vSeed->x0(),vSeed->y0(),vSeed->dxdz(),vSeed->dydz(),0.0000000001,1);
  }
  
  StThreeVectorD t1=b.at(0.0);
  printf(" BL x1=%8.4f y1=%8.4f z1=%8.4f \n",t1.x(),t1.y(),t1.z());
  StThreeVectorD t2=b.at(100.0);
  printf(" BL x2=%8.4f y2=%8.4f z2=%8.4f \n",t2.x(),t2.y(),t2.z());
  StThreeVectorD t3=b.at(z);
  printf(" BL x3=%8.4f y3=%8.4f z3=%8.4f \n",t3.x(),t3.y(),t3.z());

  StThreeVectorD s1=h.at(0.0);
  printf(" TR x1=%8.4f y1=%8.4f z1=%8.4f \n",s1.x(),s1.y(),s1.z());
  StThreeVectorD s2=h.at(100.0);
  printf(" TR x2=%8.4f y2=%8.4f z2=%8.4f \n",s2.x(),s2.y(),s2.z());
  StThreeVectorD s3=h.at(z);
  printf(" TR x3=%8.4f y3=%8.4f z3=%8.4f d=%8.4f\n",s3.x(),s3.y(),s3.z(),abs(s3-t3));

  //get DCA 
  pair<double, double> d=b.pathLengths(h);
  printf("path %8.4f %8.4f\n",d.first,d.second);
  StThreeVectorD d1 = b.at(d.first);
  StThreeVectorD d2 = h.at(d.second);
  StThreeVectorD d3 = d1-d2;
  dca=d3.mag(); //3d dca
  z=d2.z();     //z of DCA point on beamline constrain
  double bfield=0.5*tesla;
  StThreeVectorD p = h.momentumAt(d.second,bfield);
  int sign=h.charge(bfield);
  eta=p.pseudoRapidity();
  phi=p.phi();
  opt=sign/p.mag();
  //printf("getZvtxAndDcaFromHelix c=%10.6f sign=%2d h=%2dcurv=%10.6f mag=%10.6f opt=%10.6f\n",par[2],sign,h.h(),h.curvature(),p.mag(), opt);
  */
}

//Move fgt hits with alignment parameter
void getAlign(int itrk, fgtAlignment_st* algpar){
  memcpy(&mHitAlg,&mHit[mQuad][itrk],sizeof(mHitAlg));
  if(mFgtInputRPhi==0) return; //skip alignment for fake data
  if(mDebug>3) printf("getAlign quad=%1d trk=%d mHitAlg.nhit=%d %d\n",mQuad,itrk,mHitAlg.nhit,mHit[mQuad][itrk].nhit);
  for(int i=0; i<mHitAlg.nhit; i++){
    if(mHitAlg.det[i]<24){
      double r=mHitAlg.lr[i];
      double phi=mHitAlg.lp[i];
      int disc=mHitAlg.det[i]/4;
      int quad=mHitAlg.det[i]%4;
      TVector3 xyz,gxyz;
      mDb->getStarXYZ(disc,quad,r,phi,xyz,2,algpar); //fgt internal alignment
      //printf("y local=%8.2f\n",xyz.Y());
      if(gStTpcDb){
	double local[3]={0,0,0}, global[3]={0,0,0};
	xyz.GetXYZ(local);
	TGeoHMatrix globalMatrix = gStTpcDb->Tpc2GlobalMatrix();  //TPC global offsets
	globalMatrix.LocalToMaster(local,global);
	gxyz.SetXYZ(global[0],global[1],global[2]);	
	//printf("FGT local  %9.6f %9.6f %9.6f\n",local[0],local[1],local[2]);
	//printf("FGT globl  %9.6f %9.6f %9.6f\n",global[0],global[1],global[2]);
	//globalMatrix.Print();
      }else{
	static int nmess=0;
	if(nmess<100){
	  printf("StFgtAlignmentMaker::Make could not get gStTpcDb... global xyz is same as fgt local xyz\n");
	  nmess++;
	}
	gxyz=xyz;
      }
      mHitAlg.x[i]=gxyz.X();
      mHitAlg.y[i]=gxyz.Y();
      mHitAlg.z[i]=gxyz.Z();
      mHitAlg.r[i]=sqrt(mHitAlg.x[i]*mHitAlg.x[i]+mHitAlg.y[i]*mHitAlg.y[i]);
      mHitAlg.p[i]=atan2(mHitAlg.y[i],mHitAlg.x[i]);
      //printf("after y=%8.2f\n",mHitAlg.y[i]);
    }
    if(mDebug>3) printf("mHit[%3d] x=%8.3f y=%8.3f z=%8.3f det=%2d\n",i,
			mHitAlg.x[i],mHitAlg.y[i],mHitAlg.z[i],mHitAlg.det[i]);
  }
}

//fill residual histos
void fillHist(int disc, int quad, float dx, float dy, float dr, float dp, float x, float y, float r, float p){
  hist1[disc][quad][0]->Fill(dx);
  hist1[disc][quad][1]->Fill(dy);
  hist1[disc][quad][2]->Fill(dr);
  hist1[disc][quad][3]->Fill(dp);
  hist2[disc][quad][0]->Fill(x,dx);
  hist2[disc][quad][1]->Fill(x,dy);
  hist2[disc][quad][2]->Fill(r,dr);
  hist2[disc][quad][3]->Fill(r,dp);
  hist2[disc][quad][4]->Fill(y,dx);
  hist2[disc][quad][5]->Fill(y,dy);
  hist2[disc][quad][6]->Fill(p,dr);
  hist2[disc][quad][7]->Fill(p,dp);
  if(mDebug>3) cout << Form("FillHist d=%1d q=%1d dx=%8.3f dy=%8.3f dr=%8.3f dp=%9.5f",
			    disc,quad,dx,dy,dr,dp) <<endl;
  //if(disc>6) cout << Form("FillHist d=%1d q=%1d dx=%8.3f dy=%8.3f dr=%8.3f dp=%9.5f",
  //			  disc,quad,dx,dy,dr,dp) <<endl;
}

//event display
void eventDisplay(double *par){
  //setup helix track
  StPhysicalHelix h = getStPhysicalHelix(par);
  static StThreeVectorD n(0,0,1);
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
    //double dr = sqrt(x*x+y*y) - sqrt(mHitAlg.x[i]*mHitAlg.x[i]+mHitAlg.y[i]*mHitAlg.y[i]);
    double dp = atan2(y,x) - atan2(mHitAlg.y[i],mHitAlg.x[i]);
    if(dp>PI)  dp-=2*PI;
    if(dp<-PI) dp+=2*PI;
    double hx=mHitAlg.x[i];
    double hy=mHitAlg.y[i];
    double hz=mHitAlg.z[i];
    double hr=sqrt(hx*hx+hy*hy);
    //double hp=atan2(hy,hx);
    hitxz->SetPoint(i,hz,dx);
    hityz->SetPoint(i,hz,dy);
    if(fabs(dp)<0.1) hitpr->SetPoint(i,hr,dp);
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
  StPhysicalHelix h = getStPhysicalHelix(par);
  static StThreeVectorD n(0,0,1);
  int ifvtx=0;
  int ndf=0;
  for(int i=0; i<mHitAlg.nhit; i++){
    if(mHitAlg.use[i]==false) continue;
    if(mHitAlg.det[i]/4==6) ifvtx=1;
    StThreeVectorD z(0,0,mHitAlg.z[i]);
    double s=h.pathLength(z,n);    
    double dx = h.x(s) - mHitAlg.x[i];
    double dy = h.y(s) - mHitAlg.y[i];
    f += (dx*dx+dy*dy)/(mHitAlg.ex[i]*mHitAlg.ex[i]+mHitAlg.ey[i]*mHitAlg.ey[i]);
    ndf+=2;
    if(mDebug>5) printf("Helix1 Curve=%12.9f x=%8.3f y=%8.3f dx=%8.4f dy=%8.4f f=%8.4fe\n",par[2],h.x(s),h.y(s),dx,dy,f);
  }
  if(ifvtx==0 && mBeamLine==1){
    double z,dca,eta,phi,opt;
    getZvtxAndDcaFromHelix(par,dca,z,eta,phi,opt);
    f += dca*dca/mErrVTX/mErrVTX;
    ndf++;
  }
  ndf-=5;
  if(ndf<=0) printf("ERROR fitHelix ndf=%d\n",ndf);
  f/=double(ndf);
  if(mDebug>4) printf("fitHelix x0=%12.8f y0=%12.8f c=%12.8f dip=%12.8f phase=%12.8f f=%12.8f\n",
		      par[0],par[1],par[2],par[3],par[4],f);
}

//Get residuals with helix fit
double residHelix(double *par){
  double f=0, chi2=0;
  StPhysicalHelix h = getStPhysicalHelix(par);
  static StThreeVectorD n(0,0,1);
  int ifvtx=0, ifvtx2=0;
  int ndf=0,ndf2=0;
  for(int i=0; i<mHitAlg.nhit; i++){
    StThreeVectorD z(0,0,mHitAlg.z[i]);
    double s=h.pathLength(z,n);
    double x=h.x(s);
    double y=h.y(s);
    double dx = mHitAlg.x[i] - x;
    double dy = mHitAlg.y[i] - y;
    int disc=mHitAlg.det[i]/4;
    double ff=(dx*dx+dy*dy)/(mHitAlg.ex[i]*mHitAlg.ex[i]+mHitAlg.ey[i]*mHitAlg.ey[i]);
    if(mResidMaskDisc & (1<<disc)) {
      if(disc==6) ifvtx=1;
      f += ff;
      ndf+=2;
    }
    if(mHitMaskDisc & (1<<disc)) {
      if(disc==6) ifvtx2=1;
      chi2 += ff;
      ndf2+=2;
    }
    if(mDebug>3) printf("residHelix %3d dx=%12.8f dy=%12.8f f=%12.8f\n",i,dx,dy,f);
    if(mFillHist>0) {
      double r=sqrt(x*x+y*y);
      double phi=atan2(y,x);
      double hx=mHitAlg.x[i];
      double hy=mHitAlg.y[i];
      double hr=mHitAlg.r[i];
      double hp=mHitAlg.p[i];
      double dr = hr-r;
      double dp = hp-phi;
      //if(disc>8) printf("EEMCDR n=%2d used=%d %8.4f ->%8.4f\n",mHitAlg.nhitEemc,mHitAlg.used,mHitAlg.dr[i],dr);
      while(dp>PI)  dp-=2*PI;
      while(dp<-PI) dp+=2*PI;
      mHitAlg.dx[i]=dx;
      mHitAlg.dy[i]=dy;
      mHitAlg.dr[i]=dr;
      mHitAlg.dp[i]=dp;
      int quad=mHitAlg.det[i]%4;
      if(disc<kFgtNumDiscs){
	while(phi>StFgtGeom::phiQuadXaxis(2)) phi-=2*PI;
	while(phi<StFgtGeom::phiQuadXaxis(1)) phi+=2*PI;
      }
      fillHist(disc,quad,dx,dy,dr,dp,x,y,r,phi);
    }
  }
  if(ifvtx+ifvtx2<2 && mBeamLine==1){
    double z,dca,eta,phi,opt;
    getZvtxAndDcaFromHelix(par,dca,z,eta,phi,opt);
    if(ifvtx==0)  {f    += dca*dca/mErrVTX/mErrVTX; ndf++; }
    if(ifvtx2==0) {chi2 += dca*dca/mErrVTX/mErrVTX; ndf2++;}
  }
  ndf-=5;
  if(ndf<=0) printf("ERROR resudHelix ndf=%d\n",ndf);
  f/=double(ndf);
  if(mFillHist>0) {
    ndf2-=5;
    if(ndf2<=0) {printf("ERROR residHelix ndf2=%d\n",ndf2);}
    else {chi2/=double(ndf2);}
    double z,dca,eta,phi,opt;
    getZvtxAndDcaFromHelix(par,dca,z,eta,phi,opt);
    if(mDebug>0) printf("UPDATE Dca=%8.4f->%8.4f  trkz=%8.4f->%8.4f chi2=%8.4f->%8.4f ndf=%d\n",mHitAlg.dca,dca,mHitAlg.trkz,z,mHitAlg.chi2,chi2,ndf2); 
    mHitAlg.dca=dca;
    mHitAlg.dz=mHitAlg.dz-mHitAlg.trkz+z;
    mHitAlg.trkz=z;
    mHitAlg.chi2=chi2;
    mHitAlg.eta=eta;
    mHitAlg.phi=phi;
    mHitAlg.opt=opt;
  }
  if(mFillHist>1 && mEventDisp) eventDisplay(par);
  if(mDebug>2) 
    printf("residHelix x0=%12.8f y0=%12.8f c=%12.8f dip=%12.8f phase=%12.8f f=%12.8f\n",
		      par[0],par[1],par[2],par[3],par[4],f);
  return f;
}

//a track fit to helix
void fitTrackHelix(fgtAlignment_st* algpar, int itrk, double *par){
  int flag;
  static double arg[10];
  static TMinuit *m = 0;
  if(m==0){
    m=new TMinuit(5);
    m->SetFCN(fitHelix);
    arg[0] =-1; m->mnexcm("SET PRI", arg, 1,flag);
    arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,flag);  
    arg[0] = 1; m->mnexcm("SET ERR", arg, 1,flag);
    arg[0] = 500; arg[1] = 1.;
  }
  //make "aligned" hits in mHitAlg
  getAlign(itrk,algpar);
  //first guess of helix fit parameters and set up
  double dx = mHitAlg.x[1]-mHitAlg.x[0];
  double dy = mHitAlg.y[1]-mHitAlg.y[0];
  double dz = mHitAlg.z[1]-mHitAlg.z[0];
  double dr = sqrt(dx*dx+dy*dy);
  double dip = atan2(dz,dr);
  double slp = atan2(dy,dx);
  double x0= mHitAlg.x[0]-dx/dz*mHitAlg.z[0];
  double y0= mHitAlg.y[0]-dy/dz*mHitAlg.z[0];
  m->mnparm(0,"x0"  ,x0       ,0.1,  0, 0,flag);
  m->mnparm(1,"y0"  ,y0       ,0.1,  0, 0,flag);
  m->mnparm(2,"curv",0.0001   ,1.0,  0, 0,flag);
  m->mnparm(3,"dip" ,dip      ,0.1,  0, 0,flag);
  m->mnparm(4,"slp" ,slp      ,0.1,  0, 0,flag);
  //do helix fit
  m->mnexcm("MIGRAD", arg ,2, flag);
  double r,e;
  for(int j=0; j<5; j++){ m->GetParameter(j,r,e); par[j]=r;}
}

//Minimize residuals with helix fit, called from TMinuit
void funcHelix(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>3) printf("funcHelix mQuad=%d mNtrk=%d\n",mQuad,mNtrk[mQuad]);
  f=0;
  fgtAlignment_st* algpar= (fgtAlignment_st*)par;
  double p[5];
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    if(mHit[mQuad][itrk].used==0) continue;    
    fitTrackHelix(algpar,itrk,p);
    f+=residHelix(p);     //add up residuals
    if(mFillHist>0) {
      memcpy(&mHit[mQuad][itrk],&mHitAlg,sizeof(mHitAlg));
      //printf("mHitAlg.z[0]=%8.2f\n",mHitAlg.z[0]);
      //printf("mHit[mQuad][itrk].z[0]=%8.2f\n",mHit[mQuad][itrk].z[0]);
    }
  }
  if(mDebug>3) printf("funcHelix f=%12.6f xoff=%12.8f yoff=%12.8f\n",f,algpar->xoff[8],algpar->xoff[8]);
  static int ncall[4]={0,0,0,0};
  ncall[mQuad]++;
  if(ncall[mQuad]%100==0) printf("funcHelix quad=%d ncall=%d f=%12.6f\n",mQuad,ncall[mQuad],f);
}

//Straight line fit, called from TMinuit
void fitLine(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  if(mDebug>4) printf("fitLine nhit=%d\n",mHitAlg.nhit);
  f=0;  
  int ifvtx=0;
  int ndf=0;
  for(int i=0; i<mHitAlg.nhit; i++){    
    if(mHitAlg.use[i]==false) continue;
    if(mHitAlg.det[i]/4==6) ifvtx=1;
    double dx = par[0] + par[1] * mHitAlg.z[i] - mHitAlg.x[i];
    double dy = par[2] + par[3] * mHitAlg.z[i] - mHitAlg.y[i];    
    f += (dx*dx+dy*dy)/(mHitAlg.ex[i]*mHitAlg.ex[i]+mHitAlg.ey[i]*mHitAlg.ey[i]);
    ndf+=2;
    if(mDebug>5) printf("dx=%8.4f dy=%8.4f dr=%8.4f\n",dx,dy,f);
  } 
  if(ifvtx==0 && mBeamLine==1){
    double z,dca;
    getZvtxAndDca(par,dca,z);
    f += dca*dca/mErrVTX/mErrVTX;
    ndf++;
  }
  ndf-=4;
  if(ndf<=0) {printf("ERROR fitLine ndf=%d\n",ndf);}
  else {f/=double(ndf);}
  if(mDebug>4) printf("fitLine x0=%12.8f x1=%12.8f y0=%12.8f y1=%12.8f f=%12.8f\n",par[0],par[1],par[2],par[3],f);
}

//Get residuals with straight line fit
double residLine(double *par){
  //printf("residLine mQuad=%d mNtrk=%d mFillHist=%d\n",mQuad,mNtrk[mQuad],mFillHist);
  double f=0, chi2=0;
  int ifvtx=0, ifvtx2=0;
  int ndf=0, ndf2=0;
  for(int i=0; i<mHitAlg.nhit; i++){    
    double x=par[0] + par[1] * mHitAlg.z[i];
    double y=par[2] + par[3] * mHitAlg.z[i];
    double dx = mHitAlg.x[i]-x;
    double dy = mHitAlg.y[i]-y;    
    int disc=mHitAlg.det[i]/4;
    if(mResidMaskDisc & (1<<disc)) {
      f += (dx*dx+dy*dy)/(mHitAlg.ex[i]*mHitAlg.ex[i]+mHitAlg.ey[i]*mHitAlg.ey[i]);
      ndf+=2;
    }
    if(mHitMaskDisc & (1<<disc)) {
      chi2 += (dx*dx+dy*dy)/(mHitAlg.ex[i]*mHitAlg.ex[i]+mHitAlg.ey[i]*mHitAlg.ey[i]);
      ndf2+=2;
    }
    if(mDebug>5) printf("residLine %3d dx=%12.8f dy=%12.8f dr=%8.2f\n",i,dx,dy,dx*dx+dy*dy);
    if(mFillHist>0) {
      double r=sqrt(x*x+y*y);
      double phi=atan2(y,x);
      double hx=mHitAlg.x[i];
      double hy=mHitAlg.y[i];
      double hr=mHitAlg.r[i];
      double hp=mHitAlg.p[i];
      double dr = hr-r;
      double dp = hp-phi;
      while(dp>PI)  dp-=2*PI;
      while(dp<-PI) dp+=2*PI;
      mHitAlg.dx[i]=dx;
      mHitAlg.dy[i]=dy;
      mHitAlg.dr[i]=dr;
      mHitAlg.dp[i]=dp;
      int quad=mHitAlg.det[i]%4;
      if(disc<kFgtNumDiscs){
	while(phi>StFgtGeom::phiQuadXaxis(2)) phi-=2*PI;
	while(phi<StFgtGeom::phiQuadXaxis(1)) phi+=2*PI;
      }
      fillHist(disc,quad,dx,dy,dr,dp,x,y,r,phi);
      //if(disc==6 && fabs(dx)<0.01 && fabs(dy)<0.01){
      //	printf("VTX %3d q=%1d d=%1d trk=%6.4f %6.4f %6.4f %6.4f fgt=%6.4f %6.4f %6.4f %6.4f xyrp=%6.4f %6.4f %6.4f %6.4f dxyrp=%6.4f %6.4f %6.4f %6.4f\n",
      //	       i,quad,disc,par[0],par[1],par[2],par[3],x,y,r,phi,hx,hy,hr,hp,dx,dy,dr,dp);
      //}
      //if(disc>6)
	//printf("EEMC2 %3d q=%1d d=%1d trk=%6.2f %6.2f %6.2f %6.2f fgt=%6.1f %6.1f %6.1f %6.2f xyrp=%6.1f %6.1f %6.1f %6.2f dxyrp=%6.1f %6.1f %6.1f %6.2f\n",
	//      i,quad,disc,par[0],par[1],par[2],par[3],x,y,r,phi,hx,hy,hr,hp,dx,dy,dr,dp);
    }
  }
  if(ifvtx+ifvtx2<2 && mBeamLine==1){
    double z,dca;
    getZvtxAndDca(par,dca,z);
    if(ifvtx==0)  {f    += dca*dca/mErrVTX/mErrVTX; ndf++; }
    if(ifvtx2==0) {chi2 += dca*dca/mErrVTX/mErrVTX; ndf2++;}
  }
  ndf-=4;
  if(ndf<=0) printf("ERROR residLine ndf=%d\n",ndf);
  f/=double(ndf);
  if(mFillHist>0) {
    ndf2-=4;
    if(ndf2<=0) printf("ERROR residLine ndf2=%d\n",ndf2);
    chi2/=double(ndf2);
    //update DCA and Trkz
    double dca,z;
    getZvtxAndDca(par,dca,z);
    if(mDebug>0) printf("UPDATE Dca=%8.4f->%8.4f  trkz=%8.4f->%8.4f chi2=%8.4f->%8.4f ndf=%d\n",mHitAlg.dca,dca,mHitAlg.trkz,z,mHitAlg.chi2,chi2,ndf2);
    mHitAlg.dca=dca;
    mHitAlg.dz=mHitAlg.dz-mHitAlg.trkz+z;
    mHitAlg.trkz=z;
    mHitAlg.chi2=chi2;
    double theta=atan2(sqrt(par[1]*par[1]+par[3]*par[3]),1.0);
    mHitAlg.eta=-log(tan(theta/2.0)) ;
    mHitAlg.phi=atan2(par[3],par[1]);
    mHitAlg.opt=0.0;
  }
  if(mFillHist>1 && mEventDisp) eventDisplay(par);
  if(mDebug>4) printf("residLine x0=%12.8f x1=%12.8f y0=%12.8f y1=%12.8f f=%12.8f\n",par[0],par[1],par[2],par[3],f);
  return f;
}

//a track fit to line
void fitTrackLine(fgtAlignment_st* algpar, int itrk, double *par){
  int flag;
  static double arg[10];
  static TMinuit *m = 0;
  if(m==0){
    m=new TMinuit(4);
    m->SetFCN(fitLine);
    arg[0] =-1; m->mnexcm("SET PRI", arg, 1,flag);
    arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,flag);
    arg[0] = 1; m->mnexcm("SET ERR", arg, 1,flag);
    arg[0] = 500; arg[1] = 1.;
  }
  //make "aligned" hits in mHitAlg
  getAlign(itrk,algpar);
  //first guess of helix fit parameters and set up
  double x1 = (mHitAlg.x[1]-mHitAlg.x[0])/(mHitAlg.z[1]-mHitAlg.z[0]);
  double y1 = (mHitAlg.y[1]-mHitAlg.y[0])/(mHitAlg.z[1]-mHitAlg.z[0]);
  double x0 = mHitAlg.x[0] - x1*mHitAlg.z[0];
  double y0 = mHitAlg.y[0] - y1*mHitAlg.z[0];
  m->mnparm(0,"x0",x0,0.1,0,0,flag);
  m->mnparm(1,"x1",x1,0.1,0,0,flag);
  m->mnparm(2,"y0",y0,0.1,0,0,flag);
  m->mnparm(3,"y1",y1,0.1,0,0,flag);
  //do Line fit
  m->mnexcm("MIGRAD", arg ,2, flag);
  double r,e;
  for(int j=0; j<4; j++){ m->GetParameter(j,r,e); par[j]=r;}
}

//Minimize residuals with line fit, called from TMinuit
void funcLine(Int_t &npar, Double_t* gin, Double_t &f, Double_t *par, Int_t iflag){
  //printf("funcLine mQuad=%d mNtrk=%d mFillHist=%d\n",mQuad,mNtrk[mQuad],mFillHist);
  if(mDebug>3) printf("funcLine mQuad=%d mNtrk=%d\n",mQuad,mNtrk[mQuad]);
  f=0;
  fgtAlignment_st* algpar= (fgtAlignment_st*)par;
  double p[4];
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    if(mHit[mQuad][itrk].used==0) continue;
    fitTrackLine(algpar,itrk,p); //track fit
    f+=residLine(p);   //add up residuals
    if(mFillHist>0) {
      memcpy(&mHit[mQuad][itrk],&mHitAlg,sizeof(mHitAlg));
      //printf("mHitAlg.z[0]=%8.2f\n",mHitAlg.z[0]);
      //printf("mHit[mQuad][itrk].z[0]=%8.2f\n",mHit[mQuad][itrk].z[0]);
    }
  } 
  static int ncall[4]={0,0,0,0};
  ncall[mQuad]++;
  if(ncall[mQuad]%100==0) printf("funcLine quad=%d ncall=%d f=%12.6f\n",mQuad,ncall[mQuad],f);
}

ClassImp(StFgtAlignmentMaker);

StFgtAlignmentMaker::StFgtAlignmentMaker(const Char_t *name) : StMaker(name),mEventCounter(0),
							       mErrFgt(0.02), mErrVtx(0.02), mErrVtxZ(1.0), 
							       mErrTpcI(0.6), mErrTpcO(0.12),mErrTpcZ(0.12),mErrPpt(0.1),mErrEmc(0.3),
							       mFakeNtrk(2000),mFakeEmin(40),mFakeEmax(40),mFakeEtamin(1.6),mFakeEtamax(1.6),
							       mFakePhimin(0),mFakePhimax(0),mFakeVtxSig(0),
							       mDataSource(0),
							       mOutTreeFile(0),mInTreeFile(0),mReadParFile(0),
							       mRunNumber(0),mSeqNumber(0),mDay(0),mNStep(0){
  memset(mDzCut  ,0,sizeof(mDzCut  ));
  memset(mDcaCut ,0,sizeof(mDcaCut ));
  memset(mFgtRCut,0,sizeof(mFgtRCut));
  memset(mFgtPCut,0,sizeof(mFgtPCut));
  memset(mTpcRCut,0,sizeof(mTpcRCut));
  memset(mTpcPCut,0,sizeof(mTpcPCut));
  memset(mEmcRCut,0,sizeof(mEmcRCut));
  memset(mEmcPCut,0,sizeof(mEmcPCut));  
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
  if(mReadParFile==0){
    cout << "mDb="<<mDb<<endl;
    orig_algpar=mDb->getAlignment();
  }else{
    readPar(orig_algpar);
  }
  return kStOK;
}

static const int mMaxStep=100;

void StFgtAlignmentMaker::setStep(int discmask,int quadmask, int parmask, int hitmask_disc, int residmask,
				  int trackType, int minHit, int minFgtHit, int minVtx, int minTpcHit, int minPromptHit, int minEemcHit,
				  float dzcut, float dcacut, float fgtrcut, float fgtpcut, float tpcrcut, float tpcpcut, float emcrcut, float emcpcut){
  if(mNStep>=mMaxStep) {printf("Reached MaxStep\n"); return; }
  if(mNStep==0) { printf("Step0 is making before histo, and masks are set to 0\n"); discmask=0; quadmask=0; parmask=0; }
  mDiscMask[mNStep]=discmask;
  mQuadMask[mNStep]=quadmask;
  mParMask[mNStep]=parmask;
  mHitMask[mNStep]=hitmask_disc;
  mResidMask[mNStep]=residmask;
  mTrackType[mNStep]=trackType;
  mMinHit[mNStep]=minHit;
  mMinFgtHit[mNStep]=minFgtHit;
  mMinVtx[mNStep]=minVtx;
  mMinTpcHit[mNStep]=minTpcHit;
  mMinPromptHit[mNStep]=minPromptHit;
  mMinEemcHit[mNStep]=minEemcHit;
  mDzCut[mNStep]=dzcut;
  mDcaCut[mNStep]=dcacut;
  mFgtRCut[mNStep]=fgtrcut;
  mFgtPCut[mNStep]=fgtpcut;
  mTpcRCut[mNStep]=tpcrcut;
  mTpcPCut[mNStep]=tpcpcut;
  mEmcRCut[mNStep]=emcrcut;
  mEmcPCut[mNStep]=emcpcut;
  printf("Adding step=%d with discMask=%x quadMask=%x parMask=%x hitMask=%x trkType=%d minHit=%d minFgt=%d minTpc=%d minPrompt=%d minEemc=%d\n",
	 mNStep,discmask,quadmask,parmask,hitmask_disc,trackType,minHit,minFgtHit,minTpcHit,minPromptHit,minEemcHit);
  printf("                    Cuts dz=%8.3f dca=%8.3f fgtdr=%8.3f fgtdp=%8.3f tpcdr=%8.3f tpcdp=%8.3f emcdr=%8.3f emcdp=%8.3f\n",
	 mDzCut[mNStep],mDcaCut[mNStep],mFgtRCut[mNStep],mFgtPCut[mNStep],mTpcRCut[mNStep],mTpcPCut[mNStep],mEmcRCut[mNStep],mEmcPCut[mNStep]);
  mNStep++;
}


Int_t StFgtAlignmentMaker::Make() {
  orig_algpar=mDb->getAlignment();
  mErrFGT=mErrFgt;
  mErrVTX=mErrVtx;
  mErrVTXZ=mErrVtxZ;
  mErrTPCI=mErrTpcI;
  mErrTPCO=mErrTpcO;
  mErrTPCZ=mErrTpcZ;
  mErrPPT=mErrPpt;
  mErrEMC=mErrEmc;
  if(mDataSource==0){
    readFromStEvent();  
  }else if(mDataSource==1){
    readFromStEventGlobal();  
  }else if(mDataSource==2){
    readFromStraightTrackMaker();  
    DispFromStraightTrackMaker();
  }else if(mDataSource==5){
    calcETBalanceFromStEvent();
    readFromStraightTrackAndStEvent();  
  }else if(mDataSource==6){
    readFromStraightTrackAndMudst();  
  }
  return kStOK;
}

Int_t StFgtAlignmentMaker::Finish() {
  gMessMgr->Info() << "StFgtAlignmentMaker::Finish()" << endm;
  
  if(mDataSource==4)      {fakeData();}
  else if(mDataSource==3) {readFromTree();}
  overWriteError();
  fgtAlignment_st result;  
  memcpy(&result,orig_algpar,sizeof(fgtAlignment_st));

  cout << "Doing Alignment with Number of steps = "<<mNStep<<endl;
  for(int s=0; s<mNStep; s++){
    mStep=s;
    if(mDiscMask[s]+mParMask[s]>0) {mFillHist=0;}
    else {mFillHist=1; resetHist(); printf("Saving hist for step=%d\n",mStep);}
    for(int quad=0; quad<kFgtNumQuads; quad++){ 
      mQuad=quad; 
      int quadmask= 1<<quad;
      if( (mFillHist>0) || (quadmask & mQuadMask[s]) ){
	cout << Form("Doing alignment for quad=%1d with Ntrk=%4d",quad,mNtrk[quad])<<endl;
	if(mNtrk[mQuad]>0){
	  printf("Quad=%d Step=%d with discMask=%x quadMask=%x parMask=%x hitMask=%x residMask=%x trkType=%d minHit=%d minFgt=%d vtx=%d minTpc=%d minPrompt=%d minEEmc=%d\n",
		 quad,s,mDiscMask[s],quadmask,mParMask[s],mHitMask[s],mResidMask[s],
		 mTrackType[s],mMinHit[s],mMinFgtHit[s],mMinVtx[s],mMinTpcHit[s],mMinPromptHit[s],mMinEemcHit[s]);
	  printf(" Cuts dz=%8.3f dca=%8.3f fgtdr=%8.3f fgtdp=%8.3f tpcdr=%8.3f tpcdp=%8.3f emcdr=%8.3f emcdp=%8.3f\n",
		 mDzCut[s],mDcaCut[s],mFgtRCut[s],mFgtPCut[s],mTpcRCut[s],mTpcPCut[s],mEmcRCut[s],mEmcPCut[s]);
	  doAlignment(&result,mDiscMask[s],quadmask,mParMask[s],mHitMask[s],mResidMask[s],
		      mTrackType[s],mMinHit[s],mMinFgtHit[s],mMinVtx[s],mMinTpcHit[s],mMinPromptHit[s],mMinEemcHit[s],&result);
	} 
      }
    }
    if(mFillHist>0){ 
      saveHist();   
      if(mOutTreeFile) writeTree();
    }    
  }
  writePar(&result);        
  return kStOK;
}

void StFgtAlignmentMaker::fakeData() {
  mFgtInputRPhi=0;
  orig_algpar=new fgtAlignment_st; 
  memset(orig_algpar,0,sizeof(fgtAlignment_st));
  int quad=0;
  mNtrk[quad]=0;
  memset(mHit,0,sizeof(mHit));
  int opt=1;
  if(opt==0){
    for(int itrk=0; itrk<1; itrk++){
      mHit[quad][itrk].nhit=0;
      for(int d=0; d<10; d++){
	double z;
	mHit[quad][itrk].nhit++;
	if     (d==0) {mHit[quad][itrk].det[d]=24+quad; z=0;}
	else if(d>6)  {mHit[quad][itrk].det[d]=28+quad; z=200.0+d;}
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
  }else{
    //fake track inputs
    mQuad=0;
    TRandom rand;
    int mFgt=1,mVtx=1,mTpc=0,mPpt=1,mEmc=1;
    const double B = 0.5*tesla;  
    StThreeVectorD zvec(0,0,1);
    //TPC and EEMC z positions
    static const int NTPC=45;
    double tpcr[NTPC];
    for(int ipad= 0;ipad< 8; ipad++){tpcr[ipad]=60.0 + 4.8*ipad;}
    for(int ipad= 8;ipad<13; ipad++){tpcr[ipad]=60.0 + 4.8*7 + 5.2*(ipad-8);}
    for(int ipad=13;ipad<45; ipad++){tpcr[ipad]=127.950 + 2.0*(ipad-13);}
    double zppt=209;
    double zemc=280;
    
    for(int itrk=0; itrk<mFakeNtrk; itrk++){      
      double ene=mFakeEmin;
      double eta=mFakeEtamin;
      double phi=mFakePhimin;
      StThreeVectorD v(0,0,0);
      if(mFakeEmax>mFakeEmin)     ene=rand.Uniform(mFakeEmin,mFakeEmax);
      if(mFakeEtamax>mFakeEtamin) eta=rand.Uniform(mFakeEtamin,mFakeEtamax);
      if(mFakePhimax>mFakePhimin) phi=rand.Uniform(mFakePhimin,mFakePhimax);
      if(mFakeVtxSig>0) {
	double zz=-999;
	while (zz>60 || zz<-120) {zz=rand.Gaus(0,mFakeVtxSig);}
	v.setZ(zz);
      }
      //Making helix
      double theta=2*atan(exp(-eta));  
      double pt=ene*sin(theta);
      double pz=ene*cos(theta);
      double px=ene*sin(theta)*cos(phi);
      double py=ene*sin(theta)*sin(phi);
      StThreeVectorD p(px, py, pz);
      StPhysicalHelixD pos(p,v,B,1);
      StPhysicalHelixD neg(p,v,B,-1);
      if(itrk<3){
	printf("FAKE pt=%8.3f eta=%8.3f phi=%8.3f ene=%8.3f\n",pt,eta,phi,ene);
	printf("FAKE pxyz=%8.3f %8.3f %8.3f\n",px,py,pz);
	printf("FAKE vxyz=%8.3f %8.3f %8.3f\n",v.x(),v.y(),v.z());
	//print rough trajectory
	for (double z=0; z<=300; z+=20) { 
	  StThreeVectorD zplane(0,0,z);
	  double sp=pos.pathLength(zplane,zvec);
	  double sn=pos.pathLength(zplane,zvec);
	  StThreeVectorD pxyz = pos.at(sp);    
	  StThreeVectorD nxyz = neg.at(sn);    
	  printf("FAKE z=%8.3f pos=%8.3f %8.3f neg=%8.3f %8.3f\n",z,pxyz.x(),pxyz.y(),nxyz.x(),nxyz.y());
	}
      }

      //make hits
      StPhysicalHelixD trk;
      if(itrk%2==0) {trk=pos;}
      else          {trk=neg;}
      int nhit=0;
      if(mVtx){
	StThreeVectorD zplane(0,0,v.z());
	double s=trk.pathLength(zplane,zvec);
	StThreeVectorD xyz = trk.at(s);    
	mHit[mQuad][itrk].x[nhit]=xyz.x(); mHit[mQuad][itrk].y[nhit]=xyz.y(); mHit[mQuad][itrk].z[nhit]=xyz.z();
	mHit[mQuad][itrk].ex[nhit]=mErrVtx; mHit[mQuad][itrk].ey[nhit]=mErrVtx; mHit[mQuad][itrk].ez[nhit]=mErrVtxZ;
	mHit[mQuad][itrk].det[nhit]=4*6+mQuad;
	nhit++;
	if(itrk<5) printf("VTX  %8.3f %8.3f %8.3f %8.4f %8.4f\n",xyz.x(),xyz.y(),xyz.z(),mErrVtx,mErrVtxZ);      
      }
      mHit[mQuad][itrk].dz=0;
      mHit[mQuad][itrk].trkz=v.z();
      if(mFgt){
	int nfgt=0;
	for(int idisc=0; idisc<6; idisc++){
	  if(idisc==3) continue;
	  StThreeVectorD zplane(0,0,StFgtGeom::getDiscZ(idisc));
	  double s=trk.pathLength(zplane,zvec);
	  StThreeVectorD xyz = trk.at(s);    
	  if(xyz.perp()>10.0 && xyz.perp()<38.3){
	    mHit[mQuad][itrk].x[nhit]=xyz.x(); mHit[mQuad][itrk].y[nhit]=xyz.y(); mHit[mQuad][itrk].z[nhit]=xyz.z();
	    mHit[mQuad][itrk].ex[nhit]=mErrFgt; mHit[mQuad][itrk].ey[nhit]=mErrFgt; mHit[mQuad][itrk].ez[nhit]=0.0;
	    mHit[mQuad][itrk].det[nhit]=4*idisc+mQuad;
	    nhit++;
	    nfgt++;
	    if(itrk<5) printf("FGT%1d %8.3f %8.3f %8.3f %8.4f\n",idisc+1,xyz.x(),xyz.y(),xyz.z(),mErrFgt);      
	  }
	}
	if(nfgt<3) {continue;}
      }
      if(mTpc){
	for(int ipad=0; ipad<NTPC; ipad++){
	  double r=tpcr[ipad];
	  pair <double,double> ss=trk.pathLength(r);
	  double s=ss.first;
	  if(s<0) s=ss.second;
	  StThreeVectorD xyz = trk.at(s);
	  //double z=r/tan(theta);
	  if(xyz.z()<198){      
	    mHit[mQuad][itrk].x[nhit]=xyz.x(); mHit[mQuad][itrk].y[nhit]=xyz.y(); mHit[mQuad][itrk].z[nhit]=xyz.z();
	    if(ipad<13) {mHit[mQuad][itrk].ex[nhit]=mErrTpcI; mHit[mQuad][itrk].ey[nhit]=mErrTpcI;}
	    else        {mHit[mQuad][itrk].ex[nhit]=mErrTpcO; mHit[mQuad][itrk].ey[nhit]=mErrTpcO;}
	    mHit[mQuad][itrk].ez[nhit]=mErrTpcZ;
	    mHit[mQuad][itrk].det[nhit]=4*7+mQuad;
	    nhit++;
	    if(itrk<5) printf("TPC  %8.3f %8.3f %8.3f %8.4f %8.4f\n",xyz.x(),xyz.y(),xyz.z(),mHit[mQuad][itrk].ex[nhit],mErrTpcZ);
	  }
	}
      }
      if(mPpt){
	StThreeVectorD zplane(0,0,zppt);
	double s=trk.pathLength(zplane,zvec);
	StThreeVectorD xyz = trk.at(s);    
	if(xyz.perp()>tpcr[0] && xyz.perp()<tpcr[NTPC-1]){
	  mHit[mQuad][itrk].x[nhit]=xyz.x(); mHit[mQuad][itrk].y[nhit]=xyz.y(); mHit[mQuad][itrk].z[nhit]=xyz.z();
	  mHit[mQuad][itrk].ex[nhit]=mErrPpt; mHit[mQuad][itrk].ey[nhit]=mErrPpt; mHit[mQuad][itrk].ez[nhit]=0.0;
	  mHit[mQuad][itrk].det[nhit]=4*8+mQuad;
	  nhit++;
	  if(itrk<5) printf("PPT  %8.3f %8.3f %8.3f %8.4f\n",xyz.x(),xyz.y(),xyz.z(),mErrPpt);      
	}
      }
      if(mEmc){
	StThreeVectorD zplane(0,0,zemc);
	double s=trk.pathLength(zplane,zvec);
	StThreeVectorD xyz = trk.at(s);    
	if(xyz.perp()>tpcr[0] && xyz.perp()<tpcr[NTPC-1]){
	  mHit[mQuad][itrk].x[nhit]=xyz.x(); mHit[mQuad][itrk].y[nhit]=xyz.y(); mHit[mQuad][itrk].z[nhit]=xyz.z();
	  mHit[mQuad][itrk].ex[nhit]=mErrEmc; mHit[mQuad][itrk].ey[nhit]=mErrEmc; mHit[mQuad][itrk].ez[nhit]=0.0;
	  mHit[mQuad][itrk].det[nhit]=4*11+mQuad;
	  mHit[mQuad][itrk].tw[nhit]=float(ene);
	  mHit[mQuad][itrk].nrl[nhit]=0;
	  mHit[mQuad][itrk].su[nhit]=100.0;
	  mHit[mQuad][itrk].sv[nhit]=100.0;
	  nhit++;
	  if(itrk<5) printf("EMC  %8.3f %8.3f %8.3f %8.4f\n",xyz.x(),xyz.y(),xyz.z(),mErrEmc);      
	}
      }
      mHit[mQuad][itrk].nhit=nhit;
      mHit[mQuad][itrk].dca=0.0;
      mHit[mQuad][itrk].chi2=0.0;
      mHit[mQuad][itrk].riso=1.0;
      //adding gauusian error for each track
      for(int ii=0; ii<nhit; ii++){
	mHit[mQuad][itrk].x[ii]+= mHit[mQuad][itrk].ex[ii]*rand.Gaus();
	mHit[mQuad][itrk].y[ii]+= mHit[mQuad][itrk].ey[ii]*rand.Gaus();
	mHit[mQuad][itrk].z[ii]+= mHit[mQuad][itrk].ez[ii]*rand.Gaus();
	mHit[mQuad][itrk].r[ii]=sqrt(mHit[mQuad][itrk].x[ii]*mHit[mQuad][itrk].x[ii]+mHit[mQuad][itrk].y[ii]*mHit[mQuad][itrk].y[ii]);
	mHit[mQuad][itrk].p[ii]=atan2(mHit[mQuad][itrk].y[ii],mHit[mQuad][itrk].x[ii]);
      }      
    }
    mNtrk[mQuad]=mFakeNtrk;
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
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No StFgtStraightTrackMaker" << endm;
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
      mHit[quad][ntrk].lr[nhit]=p->r;
      mHit[quad][ntrk].lp[nhit]=p->phi;
      mHit[quad][ntrk].z[nhit]=StFgtGeom::getDiscZ(disc);
      mHit[quad][ntrk].ex[nhit]=mErrFgt;
      mHit[quad][ntrk].ey[nhit]=mErrFgt;
      mHit[quad][ntrk].ez[nhit]=0;
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			     ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
			     mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			     mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		      <<endl;
      nhit++;
    }
    if(flagvtx==1){
      mHit[quad][ntrk].det[nhit]=24+quad;
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

void StFgtAlignmentMaker::readFromStraightTrackAndStEvent(){
  float MaxDrTpc=maxdtpc, MaxDpTpc=maxptpc;
  float MaxDrEemc=maxdemc, MaxDpEemc=maxpemc;
  float MaxDz=5.0, MaxDca=3.0, MaxChi2=50;

  mFgtInputRPhi=1;
  mEventCounter++;  // increase counter

  //Get StEvent
  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No StEvent" << endm;
    return;          // if no event, we're done
  }
  cout << "Event: Run "<< event->runId() << " Event No: " << event->id() << endl;

  //vertex
  int flagvtx=0;
  StThreeVectorF vertex,vtxerr;
  UInt_t NpVX = event->numberOfPrimaryVertices();
  cout << "Number of Primary vertex="<<NpVX<<endl;
  if(NpVX>0){
    const StPrimaryVertex *vx = event->primaryVertex(0); // select highest rank vertex
    cout << "Verrex :" << *vx << endl;
    vertex=vx->position();
    vtxerr=vx->positionError();
    flagvtx=1;
  }else{
    vertex.set(0.0,0.0,-999.0);
    vtxerr.set(999.0,999.0,999.0);
    //return;
  }

  //Getting track from Anselm's straight tracker
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (!fgtSTracker){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No SyFgtStraightTrackMaker" << endm;
    return;          // if no event, we're done
  }
  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
    float dz=-999.0;
    if(flagvtx){dz=t->trkZ - vertex.z();}
    if(mDebug>0) cout<<Form("Trk chi2=%8.3f dca=%8.3f zvtx=%8.3f dz=%8.3f",t->chi2,t->dca,t->trkZ,dz)<<endl;
    if(t->dca>MaxDca) continue;
    if(t->chi2>MaxChi2) continue;
    vector<AVPoint>* points=t->points;
    int quad=-1, nhit=0, ntrk=-1;    
    //FGT hits on track
    for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
      int disc=p->dID;
      int quad2=p->quadID;
      if(quad==-1) {quad=quad2; mQuad=quad;}
      ntrk=mNtrk[quad];
      mHit[quad][ntrk].det[nhit]=disc*4+quad2;
      mHit[quad][ntrk].lr[nhit]=p->r;
      mHit[quad][ntrk].lp[nhit]=p->phi;
      mHit[quad][ntrk].z[nhit]=StFgtGeom::getDiscZ(disc);
      mHit[quad][ntrk].ex[nhit]=mErrFgt;
      mHit[quad][ntrk].ey[nhit]=mErrFgt;
      mHit[quad][ntrk].ez[nhit]=0;
      mHit[quad][ntrk].tw[nhit]=p->phiCharge;
      mHit[quad][ntrk].p1[nhit]=p->rCharge;
      mHit[quad][ntrk].p2[nhit]=(p->phiCharge-p->rCharge)/(p->phiCharge+p->rCharge);
      mHit[quad][ntrk].po[nhit]=p->fgtHitPhi->getEvenOddChargeAsy();      
      mHit[quad][ntrk].use[nhit]=1;
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
			      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		       <<endl;
      nhit++;
    }    
    //vertex from StEvent
    if(flagvtx==1 && fabs(dz)<MaxDz){
      mHit[quad][ntrk].det[nhit]=24+quad;
      mHit[quad][ntrk].x[nhit]=vertex.x();
      mHit[quad][ntrk].y[nhit]=vertex.y();
      mHit[quad][ntrk].z[nhit]=vertex.z();
      mHit[quad][ntrk].r[nhit]=sqrt(vertex.x()*vertex.x()+vertex.y()*vertex.y());
      mHit[quad][ntrk].p[nhit]=atan2(vertex.y(),vertex.x());
      mHit[quad][ntrk].ex[nhit]=vtxerr.x();
      mHit[quad][ntrk].ey[nhit]=vtxerr.y();
      mHit[quad][ntrk].ez[nhit]=vtxerr.z();
      mHit[quad][ntrk].use[nhit]=1;
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
			      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		       <<endl;
      nhit++;
    }
    mHit[quad][ntrk].run=event->runId();
    mHit[quad][ntrk].evt=event->id();
    mHit[quad][ntrk].dca =t->dca;
    mHit[quad][ntrk].trkz =t->trkZ;
    mHit[quad][ntrk].dz =dz;
    mHit[quad][ntrk].chi2=t->chi2;
    mHit[quad][ntrk].nhit=nhit;
 
    //Refit with straight line with FGT and vertex
    int refit=1;
    double p[4];
    if(refit){
      fitTrackLine(orig_algpar,ntrk,p);
      if(mDebug>0){
	printf("FGT ONLY TRA CK : %8.4f %8.4f %8.4f %8.4f\n",t->ax,t->mx,t->ay,t->my);
	printf("REFIT with VTX : %8.4f %8.4f %8.4f %8.4f\n",p[0],p[1],p[2],p[3]);
      }
    }else{ p[0]=t->ax; p[1]=t->mx; p[2]=t->ay; p[3]=t->my;}
    
    //EEMC near FGT Track
    double ddr2=99999999;
    mHit[quad][ntrk].nhitEemc=0;
    StEEmcIUPointMaker *mEEpoints = (StEEmcIUPointMaker *)GetMaker("mEEpoints");
    if(mEEpoints){
      StEEmcIUPointVec_t mPoints = mEEpoints->points();
      for ( Int_t ipoint=0; ipoint<mEEpoints->numberOfPoints(); ipoint++){
        StEEmcIUPoint point=mEEpoints->point(ipoint);
        float x=point.position().x();
        float y=point.position().y();
        float z=point.position().z();
        float r=sqrt(x*x + y*y);
        float phi=atan2(y,x);
	float e[6]; int w[3];
        e[0]=point.energy(0);
        e[1]=point.energy(1)*1000;
        e[2]=point.energy(2)*1000;
        e[3]=point.energy(3)*1000;
        e[4]=point.cluster(0).energy()*1000;
        e[5]=point.cluster(1).energy()*1000;
	w[0]=point.numberOfRelatives();
        w[1]=point.cluster(0).numberOfStrips();
        w[2]=point.cluster(1).numberOfStrips();
        //printf("EEMC %d xyz=%6.2f %6.2f %6.2f e=%6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
        //       ipoint,x,y,z,e[neemc][0],e[neemc][1],e[neemc][2],e[neemc][3],e[neemc][4],e[neemc][5]);
	//double xx = t->ax + t->mx*z;
	//double yy = t->ay + t->my*z;
        double xx = p[0] + p[1]*z;
        double yy = p[2] + p[3]*z;
        double rr = sqrt(xx*xx+yy*yy);
        double pp = atan2(yy,xx);
        double dx = x-xx;
        double dy = y-yy;
        double dr = r-rr;
        double dp = phi-pp;
	while(dp>PI)  dp-=2*PI;
        while(dp<-PI) dp+=2*PI;
        if(mDebug>3) printf("EEMC %3d xyrp=%6.1f %6.1f %6.1f %6.2f e=%6.1f %6.1f %6.1f %6.1f %6.1f %6.1f dxyrp=%6.1f %6.1f %6.1f %6.2f\n",
			    ipoint,x,y,r,phi,e[0],e[1],e[2],e[3],e[4],e[5],dx,dy,dr,dp);
        if(dr < 0.5*rr-25.0 &&  //cut out high eta track
           fabs(dr)<MaxDrEemc && fabs(dp)<MaxDpEemc && e[1]>0.05 && e[2]>0.05){
          mHit[quad][ntrk].det[nhit]=36+quad;
          if(w[0]==0 && e[0]<1.5 && e[1]<4 && e[2]<4 && e[3]>0.05){ //MIP selection
            mHit[quad][ntrk].det[nhit]=40+quad;
          }
          if(e[0]>1 && e[1]>2 && e[2]>3 && e[2]>e[1] && (e[4]+e[5])/e[0]>10){ //Electron selection minus post
            mHit[quad][ntrk].det[nhit]=44+quad;
          }
	  if(mHit[quad][ntrk].nhitEemc>0 && (dx*dx+dy*dy)>ddr2) continue;
	  ddr2=dx*dx+dy*dy;
	  mHit[quad][ntrk].nhitEemc=1;
	  mHit[quad][ntrk].x[nhit]=x;
	  mHit[quad][ntrk].y[nhit]=y;
	  mHit[quad][ntrk].z[nhit]=z;
          mHit[quad][ntrk].r[nhit]=sqrt(x*x+y*y);
          mHit[quad][ntrk].p[nhit]=atan2(y,x);
	  mHit[quad][ntrk].ex[nhit]=0.3;
	  mHit[quad][ntrk].ey[nhit]=0.3;
	  mHit[quad][ntrk].ez[nhit]=0.3;
          mHit[quad][ntrk].tw[nhit]=e[0];
          mHit[quad][ntrk].p1[nhit]=e[1];
          mHit[quad][ntrk].p2[nhit]=e[2];
          mHit[quad][ntrk].po[nhit]=e[3];
          mHit[quad][ntrk].su[nhit]=e[4];
          mHit[quad][ntrk].sv[nhit]=e[5];
          mHit[quad][ntrk].nrl[nhit]=w[0];
          mHit[quad][ntrk].nsu[nhit]=w[1];
          mHit[quad][ntrk].nsv[nhit]=w[2];
	  if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
				  ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
				  mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
				  mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
			   <<endl;
	}
      }
    }
    if(mHit[quad][ntrk].nhitEemc>0) nhit++;

    //TPC near FGT track
    Long64_t ntot=0;
    ddr2=99999999;
    mHit[quad][ntrk].nhitPrompt=0;
    StTpcHitCollection* TpcHitCollection = event->tpcHitCollection();
    if(TpcHitCollection){
      //printf("got TpcHitCollection\n");
      UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
      for (UInt_t i = 0; i< numberOfSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (!sectorCollection) continue;
	//printf("got tpc sector collection\n");
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (int j = 0; j< numberOfPadrows; j++) {
	  StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	  if (!rowCollection) continue;
	  //printf("got tpc padrow collection\n");
	  StSPtrVecTpcHit &hits = rowCollection->hits();
	  Long64_t NoHits = hits.size();
	  ntot += NoHits;
	  //cout << "got nhit = "<<NoHits<<" / "<<ntot<<endl;
	  for (Long64_t k = 0; k < NoHits; k++) {
	    const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
	    const StThreeVectorF& xyz = tpcHit->position();
	    if(xyz.z()<0) continue;
	    if(xyz.z()<200.0) continue;  //hack only prompt for now
	    double xx = p[0] + p[1]*xyz.z();
	    double yy = p[2] + p[3]*xyz.z();
	    double rr = sqrt(xx*xx+yy*yy);
	    double pp = atan2(yy,xx);
	    double dx = xyz.x()-xx;
	    double dy = xyz.y()-yy;
	    double dr = xyz.perp()-rr;
	    double dp = atan2(xyz.y(),xyz.x())-pp;
	    while(dp> PI) dp-=2*PI;
	    while(dp<-PI) dp+=2*PI;
	    if(nhit>=MAXHIT){ printf("NOT ENOUGH SPACE FOR HIT!!!\n"); continue;}
	    if(fabs(dr)<MaxDrTpc && fabs(dp)<MaxDpTpc && nhit<MAXHIT){
	      mHit[quad][ntrk].det[nhit]=28+quad;
	      if(xyz.z()>200) mHit[quad][ntrk].det[nhit]=32+quad; //prompt hit
	      if(mHit[quad][ntrk].nhitPrompt>0 && (dx*dx+dy*dy)>ddr2) continue;
	      ddr2=dx*dx+dy*dy;
              mHit[quad][ntrk].nhitPrompt=1;
	      mHit[quad][ntrk].x[nhit]=xyz.x();
	      mHit[quad][ntrk].y[nhit]=xyz.y();
	      mHit[quad][ntrk].z[nhit]=xyz.z();
	      mHit[quad][ntrk].r[nhit]=xyz.perp();
	      mHit[quad][ntrk].p[nhit]=atan2(xyz.y(),xyz.x());
	      mHit[quad][ntrk].ex[nhit]=0.2;
	      mHit[quad][ntrk].ey[nhit]=0.2;
	      mHit[quad][ntrk].ez[nhit]=0.2;
	      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Det=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
				      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit],
				      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
				      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
			       <<endl;
	    }
	  }
	}
      }
    }
    if(mHit[quad][ntrk].nhitPrompt>0) nhit++;
    mHit[quad][ntrk].nhit=nhit;
    fillETBalance(ntrk);
    mNtrk[quad]++;
  }
  if(mEventCounter%100==0) cout << Form("StFgtAlignmentMaker::readFromStraightTrackMaker EVT=%d NTRK=%d %d %d %d",
				       mEventCounter,mNtrk[0],mNtrk[1],mNtrk[2],mNtrk[3])<<endl;
}

void StFgtAlignmentMaker::readFromStraightTrackAndMudst(){
  float MaxDrEemc=maxdemc, MaxDpEemc=maxpemc;
  float MaxDz=10.0, MaxDca=5.0, MaxChi2=50;

  mFgtInputRPhi=1;
  mEventCounter++;  // increase counter

  //Get MuDST
  const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");  
  if (!muDst){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No MuDST" << endm;
    return;          // if no mudst, we're done
  }
  StMuEvent *event = static_cast<StMuEvent*>(muDst->event());
  if (!event){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No MuDST event" << endm;
    return;          // if no mudst event, we're done
  }
  StEventInfo &info=event->eventInfo();

  if(mDebug>0) cout << "Event: Run "<< event->runId() << " Event No: " << event->eventId() << endl;

  //vertex
  int flagvtx=0;
  StThreeVectorF vertex,vtxerr;
  int nPrimV=muDst->numberOfPrimaryVertices();
  if(mDebug>2) cout << "Number of Primary vertex="<<nPrimV<<endl;
  if(nPrimV>0){
    StMuPrimaryVertex* vx= muDst->primaryVertex(0);
    vertex=vx->position();
    vtxerr=vx->posError();
    if(mDebug>2) cout << "Vertex = " << vertex.z() << endl;
    flagvtx=1;
  }else{
    return;
  }

  //Getting track from Anselm's straight tracker
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (!fgtSTracker){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No SyFgtStraightTrackMaker" << endm;
    return;          // if no event, we're done
  }
  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
    float dz=t->trkZ - vertex.z();
    if(mDebug>0) cout<<Form("Trk chi2=%8.3f dca=%8.3f zvtx=%8.3f dz=%8.3f",t->chi2,t->dca,t->trkZ,dz)<<endl;
    if(t->dca>MaxDca) continue;
    if(t->chi2>MaxChi2) continue;
    histdz->Fill(dz);
    if(fabs(dz)>MaxDz) continue;
    vector<AVPoint>* points=t->points;
    int quad=-1, nhit=0, ntrk=-1;    
    //FGT hits on track
    for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
      int disc=p->dID;
      int quad2=p->quadID;
      if(quad==-1) {quad=quad2; mQuad=quad;}
      ntrk=mNtrk[quad];
      mHit[quad][ntrk].run=event->runId();
      mHit[quad][ntrk].evt=event->eventId();
      mHit[quad][ntrk].det[nhit]=disc*4+quad2;
      mHit[quad][ntrk].lr[nhit]=p->r;
      mHit[quad][ntrk].lp[nhit]=p->phi;
      mHit[quad][ntrk].z[nhit]=StFgtGeom::getDiscZ(disc);
      mHit[quad][ntrk].ex[nhit]=mErrFgt;
      mHit[quad][ntrk].ey[nhit]=mErrFgt;
      mHit[quad][ntrk].ez[nhit]=0;
      mHit[quad][ntrk].tw[nhit]=p->phiCharge;
      mHit[quad][ntrk].p1[nhit]=p->rCharge;
      mHit[quad][ntrk].p2[nhit]=(p->phiCharge-p->rCharge)/(p->phiCharge+p->rCharge);
      mHit[quad][ntrk].po[nhit]=p->fgtHitPhi->getEvenOddChargeAsy();
      mHit[quad][ntrk].use[nhit]=1;
      if(mDebug>0) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Disc=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit]/4,
			      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		       <<endl;
      nhit++;
    }    
    //vertex from mudst
    if(flagvtx==1){
      mHit[quad][ntrk].det[nhit]=24+quad;
      mHit[quad][ntrk].x[nhit]=vertex.x();
      mHit[quad][ntrk].y[nhit]=vertex.y();
      mHit[quad][ntrk].z[nhit]=vertex.z();
      mHit[quad][ntrk].r[nhit]=sqrt(vertex.x()*vertex.x()+vertex.y()*vertex.y());
      mHit[quad][ntrk].p[nhit]=atan2(vertex.y(),vertex.x());
      mHit[quad][ntrk].ex[nhit]=vtxerr.x();
      mHit[quad][ntrk].ey[nhit]=vtxerr.y();
      mHit[quad][ntrk].ez[nhit]=vtxerr.z();
      mHit[quad][ntrk].use[nhit]=0;
      if(mDebug>-1) cout<<Form("Trk=%3d Hit=%3d Quad=%1d Disc=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
			      ntrk,nhit,quad,mHit[quad][ntrk].det[nhit]/4,
			      mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
			      mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
		       <<endl;
      nhit++;
    }
    mHit[quad][ntrk].dca =t->dca;
    mHit[quad][ntrk].trkz =t->trkZ;
    mHit[quad][ntrk].dz =dz;
    mHit[quad][ntrk].chi2=t->chi2;
    mHit[quad][ntrk].nhit=nhit;
 
    //mudst Refit with straight line with FGT and vertex
    int refit=0;
    double p[4];
    if(refit){
      int flg;
      double arg[10],r, e;
      TMinuit *m = new TMinuit(4);
      m->SetFCN(fitLine);
      arg[0] =-1; m->mnexcm("SET PRI", arg, 1,flg);
      arg[0] = 1; m->mnexcm("SET ERR", arg, 1,flg);
      arg[0] = 500; arg[1] = 1.;
      //make "aligned" hits in mHitAlg
      getAlign(ntrk,orig_algpar);
      //first guess of line fit parameter and set up
      double x1 = (mHitAlg.x[mHitAlg.nhit-1]-mHitAlg.x[0])/(mHitAlg.z[mHitAlg.nhit-1]-mHitAlg.z[0]);
      double y1 = (mHitAlg.y[mHitAlg.nhit-1]-mHitAlg.y[0])/(mHitAlg.z[mHitAlg.nhit-1]-mHitAlg.z[0]);
      double x0 = mHitAlg.x[0] - x1*mHitAlg.z[0];
      double y0 = mHitAlg.y[0] - y1*mHitAlg.z[0];
      m->mnparm(0,"x0",x0,0.1,0,0,flg);
      m->mnparm(1,"x1",x1,0.1,0,0,flg);
      m->mnparm(2,"y0",y0,0.1,0,0,flg);
      m->mnparm(3,"y1",y1,0.1,0,0,flg);
      //do straight line fit
      m->mnexcm("MIGRAD", arg ,2, flg);
      for(int j=0; j<4; j++){ m->GetParameter(j,r,e); p[j]=r; }
      if(mDebug>0){
	printf("FGT ONLY TRACK : %8.4f %8.4f %8.4f %8.4f\n",t->ax,t->mx,t->ay,t->my);
	printf("REFIT with VTX : %8.4f %8.4f %8.4f %8.4f\n",p[0],p[1],p[2],p[3]);
      }
    }else{
      p[0]=t->ax; p[1]=t->mx; p[2]=t->ay; p[3]=t->mx;
    }    

    //EEMC near FGT Track
    mHit[quad][ntrk].nhitEemc=0;
    StEEmcIUPointMaker *mEEpoints = (StEEmcIUPointMaker *)GetMaker("mEEpoints");
    if(!mEEpoints) {printf("No EEMC Point\n");}
    else{
      StEEmcIUPointVec_t mPoints = mEEpoints->points();
      for ( Int_t ipoint=0; ipoint<mEEpoints->numberOfPoints(); ipoint++){
        StEEmcIUPoint point=mEEpoints->point(ipoint);
        float x=point.position().x();
        float y=point.position().y();
        float z=point.position().z();
	float r=sqrt(x*x + y*y);
	float phi=atan2(y,x);
	float e[6]; int w[3];
        e[0]=point.energy(0);
        e[1]=point.energy(1)*1000;
        e[2]=point.energy(2)*1000;
        e[3]=point.energy(3)*1000;
        e[4]=point.cluster(0).energy()*1000;
        e[5]=point.cluster(1).energy()*1000;
        w[0]=point.numberOfRelatives();
        w[1]=point.cluster(0).numberOfStrips();
	w[2]=point.cluster(1).numberOfStrips();
	//double xx = t->ax + t->mx*z;
	//double yy = t->ay + t->my*z;
        double xx = p[0] + p[1]*z;
        double yy = p[2] + p[3]*z;
        double rr = sqrt(xx*xx + yy*yy);
        double pp = atan2(yy,xx);
        double dx = x-xx;
        double dy = y-yy;
        double dr = r-rr;
        double dp = phi-pp;
	while(dp>PI)  dp-=2*PI;
        while(dp<-PI) dp+=2*PI;
	if(mDebug>3) printf("EEMC %3d xyrp=%6.1f %6.1f %6.1f %6.2f e=%6.1f %6.1f %6.1f %6.1f %6.1f %6.1f dxyrp=%6.1f %6.1f %6.1f %6.2f\n",
			    ipoint,x,y,r,phi,e[0],e[1],e[2],e[3],e[4],e[5],dx,dy,dr,dp);
	if(dr < 0.5*rr-25.0 &&  //cut out low  _track
	   fabs(dr)<MaxDrEemc && fabs(dp)<MaxDpEemc && e[1]>0.05 && e[2]>0.05){
	  mHit[quad][ntrk].det[nhit]=36+quad;
	  if(w[0]==0 && e[0]<1.5 && e[1]<4 && e[2]<4 && e[3]>0.05){ //MIP selection
	    mHit[quad][ntrk].det[nhit]=40+quad;
	  }
	  if(e[0]>1 && e[1]>2 && e[2]>3 && e[2]>e[1] && (e[4]+e[5])/e[0]>10){ //Electron selection minus post
	    mHit[quad][ntrk].det[nhit]=44+quad;
	  }
	  mHit[quad][ntrk].nhitEemc++;
	  mHit[quad][ntrk].x[nhit]=x;
	  mHit[quad][ntrk].y[nhit]=y;
	  mHit[quad][ntrk].z[nhit]=z;
	  mHit[quad][ntrk].r[nhit]=sqrt(x*x+y*y);
	  mHit[quad][ntrk].p[nhit]=atan2(y,x);
	  mHit[quad][ntrk].ex[nhit]=0.5;
	  mHit[quad][ntrk].ey[nhit]=0.5;
	  mHit[quad][ntrk].ez[nhit]=0.5;	  
	  mHit[quad][ntrk].tw[nhit]=e[0];
	  mHit[quad][ntrk].p1[nhit]=e[1];
	  mHit[quad][ntrk].p2[nhit]=e[2];
	  mHit[quad][ntrk].po[nhit]=e[3];
	  mHit[quad][ntrk].su[nhit]=e[4];
	  mHit[quad][ntrk].sv[nhit]=e[5];
	  mHit[quad][ntrk].nrl[nhit]=w[0];
	  mHit[quad][ntrk].nsu[nhit]=w[1];
	  mHit[quad][ntrk].nsv[nhit]=w[2];
	  if(mDebug>0) cout<<Form("Trk=%3d EHit=%3d Quad=%1d Disc=%2d XYZ=%8.4f %8.4f %8.4f err=%8.4f %8.4f %8.4f",
				  ntrk,nhit,quad,mHit[quad][ntrk].det[nhit]/4,
				  mHit[quad][ntrk].x[nhit],mHit[quad][ntrk].y[nhit],mHit[quad][ntrk].z[nhit],
				  mHit[quad][ntrk].ex[nhit],mHit[quad][ntrk].ey[nhit],mHit[quad][ntrk].ez[nhit])
			   <<endl;
	  if(mDebug>0) printf("EEMC %3d q=%1d d=%1d trk=%6.2f %6.2f %6.2f %6.2f fgt=%6.1f %6.1f %6.1f %6.2f xyrp=%6.1f %6.1f %6.1f %6.2f dxyrp=%6.1f %6.1f %6.1f %6.2f\n",
			      ntrk,quad,mHit[quad][ntrk].det[nhit]/4,p[0],p[1],p[2],p[3],xx,yy,rr,pp,x,y,r,phi,dx,dy,dr,dp);
	  nhit++;
	}
      }
    }
    mHit[quad][ntrk].nhit=nhit;
    //save only eemc correlated!!!
    //if(mHit[quad][ntrk].nhitEemc==0) continue;
    mNtrk[quad]++;
  }
  if(mEventCounter%100==0) cout << Form("StFgtAlignmentMaker::readFromStraightTrackMaker EVT=%d NTRK=%d %d %d %d",
				       mEventCounter,mNtrk[0],mNtrk[1],mNtrk[2],mNtrk[3])<<endl;
}

void StFgtAlignmentMaker::DispFromStraightTrackMaker(){
  static int nplot=0;
  float zmin=-50, zmin2=100;
  float zmax=300;
  float maxDist=10.0;
  float maxDCA=5.0;
  //Getting track from Anselm's straight tracker
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (!fgtSTracker){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No SyFgtStraightTrackMaker" << endm;
    return;          // if no event, we're done
  }
  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
    //fgt track
    int quad=-1;
    if(t->dca>maxDCA) continue;
    if(t->trkZ<-200 || t->trkZ>200) continue;
    cout<<Form("Trk chi2=%9.6f dca=%7.2f z=%7.2f mx=%7.2f ax=%7.2f my=%7.2f ay=%7.2f",
	       t->chi2,t->dca,t->trkZ,t->mx,t->ax,t->my,t->ay)<<endl;
    TGraph *hitxz=new TGraph(1); hitxz->SetMarkerStyle(21); hitxz->SetMarkerSize(0.8); hitxz->SetMarkerColor(kRed);
    TGraph *hityz=new TGraph(1); hityz->SetMarkerStyle(21); hityz->SetMarkerSize(0.8); hityz->SetMarkerColor(kRed);
    TGraph *hitrz=new TGraph(1); hitrz->SetMarkerStyle(21); hitrz->SetMarkerSize(0.8); hitrz->SetMarkerColor(kRed);
    TGraph *hitpz=new TGraph(1); hitpz->SetMarkerStyle(21); hitpz->SetMarkerSize(0.8); hitpz->SetMarkerColor(kRed);
    TF1 *fxz = new TF1("fxz","[0]+[1]*x",zmin,zmax); fxz->SetParameter(0,t->ax); fxz->SetParameter(1,t->mx); 
    TF1 *fyz = new TF1("fyz","[0]+[1]*x",zmin,zmax); fyz->SetParameter(0,t->ay); fyz->SetParameter(1,t->my);     
    TF1 *frz = new TF1("frz","sqrt(([0]+[1]*x)*([0]+[1]*x)+([2]+[3]*x)*([2]+[3]*x))",zmin,zmax); 
    frz->SetParameter(0,t->ax); frz->SetParameter(1,t->mx); frz->SetParameter(2,t->ay); frz->SetParameter(3,t->my);     
    TF1 *fpz = new TF1("fpz","atan2([2]+[3]*x,[0]+[1]*x)",zmin,zmax); 
    fpz->SetParameter(0,t->ax); fpz->SetParameter(1,t->mx); fpz->SetParameter(2,t->ay); fpz->SetParameter(3,t->my);         
    vector<AVPoint>* points=t->points;    
    int i=0;
    //fgt hits
    for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){      
      quad=p->quadID;
      hitxz->SetPoint(i,p->z,p->x);
      hityz->SetPoint(i,p->z,p->y);
      hitrz->SetPoint(i,p->z,p->r);
      hitpz->SetPoint(i,p->z,p->phi);
      cout << Form(" %2d quad=%d xyz=%8.3f %8.3f %8.3f",i,quad,p->x,p->y,p->z)<<endl;
      i++;      
    }
    //TPC
    Long64_t ntot=0;
    int ndisp=0, nprompt=0, neemc=0;
    TGraph *tpcxz=new TGraph(1); tpcxz->SetMarkerStyle(21); tpcxz->SetMarkerSize(0.8); tpcxz->SetMarkerColor(kBlue);
    TGraph *tpcyz=new TGraph(1); tpcyz->SetMarkerStyle(21); tpcyz->SetMarkerSize(0.8); tpcyz->SetMarkerColor(kBlue);
    TGraph *tpcrz=new TGraph(1); tpcrz->SetMarkerStyle(21); tpcrz->SetMarkerSize(0.8); tpcrz->SetMarkerColor(kBlue);
    TGraph *tpcpz=new TGraph(1); tpcpz->SetMarkerStyle(21); tpcpz->SetMarkerSize(0.8); tpcpz->SetMarkerColor(kBlue);
    TGraph *tpcdxz=new TGraph(1); tpcdxz->SetMarkerStyle(21); tpcdxz->SetMarkerSize(0.8); tpcdxz->SetMarkerColor(kBlue);
    TGraph *tpcdyz=new TGraph(1); tpcdyz->SetMarkerStyle(21); tpcdyz->SetMarkerSize(0.8); tpcdyz->SetMarkerColor(kBlue);
    TGraph *tpcdrz=new TGraph(1); tpcdrz->SetMarkerStyle(21); tpcdrz->SetMarkerSize(0.8); tpcdrz->SetMarkerColor(kBlue);
    TGraph *tpcdpz=new TGraph(1); tpcdpz->SetMarkerStyle(21); tpcdpz->SetMarkerSize(0.8); tpcdpz->SetMarkerColor(kBlue);
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if(event){
      //printf("got StEvent\n");
      StTpcHitCollection* TpcHitCollection = event->tpcHitCollection();
      if(TpcHitCollection){
	//printf("got TpcHitCollection\n");
	UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
	for (UInt_t i = 0; i< numberOfSectors; i++) {
	  StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);      
	  if (!sectorCollection) continue;
	  //printf("got tpc sector collection\n");
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (!rowCollection) continue;
	    //printf("got tpc padrow collection\n");
	    StSPtrVecTpcHit &hits = rowCollection->hits();	
	    Long64_t NoHits = hits.size();
	    ntot += NoHits;
	    //cout << "got nhit = "<<NoHits<<" / "<<ntot<<endl;
	    for (Long64_t k = 0; k < NoHits; k++) {
	      const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
	      const StThreeVectorF& xyz = tpcHit->position();
	      if(xyz.z()<0) continue;
	      double xx = t->ax + t->mx*xyz.z();
	      double yy = t->ay + t->my*xyz.z();
	      double rr = sqrt(xx*xx+yy*yy);
	      double pp = atan2(yy,xx);
	      double dx = xyz.x()-xx;
	      double dy = xyz.y()-yy;
	      double dr = xyz.perp()-rr;
	      double dp = atan2(xyz.y(),xyz.x())-pp; 
	      static const double PI=3.141592654;
	      if(dp>PI) dp-=2*PI;
	      if(dp<-PI) dp+=2*PI;
	      double dist = sqrt(dx*dx+dy*dy);
	      if(dist<maxDist){
		tpcxz->SetPoint(ndisp,xyz.z(),xyz.x());
		tpcyz->SetPoint(ndisp,xyz.z(),xyz.y());
		tpcrz->SetPoint(ndisp,xyz.z(),xyz.perp());
		tpcpz->SetPoint(ndisp,xyz.z(),atan2(xyz.y(),xyz.x()));
		tpcdxz->SetPoint(ndisp,xyz.z(),dx);
		tpcdyz->SetPoint(ndisp,xyz.z(),dy);
		tpcdrz->SetPoint(ndisp,xyz.z(),dr);
		tpcdpz->SetPoint(ndisp,xyz.z(),dp);
		ndisp++;
		if(xyz.z()>200) nprompt++;
		fillHist(6,quad,dx,dy,dr,dp,xx,yy,rr,pp);
	      }
	    }
	  }
	}
      }
    }
    cout << "Got TPC hits near fgt track="<<ndisp<<" out of total="<<ntot<<endl;
    //EEMC
    ndisp=0;
    float e[10][6];
    TGraph *emcxz=new TGraph(1); emcxz->SetMarkerStyle(21); emcxz->SetMarkerSize(0.8); emcxz->SetMarkerColor(kRed);
    TGraph *emcyz=new TGraph(1); emcyz->SetMarkerStyle(21); emcyz->SetMarkerSize(0.8); emcyz->SetMarkerColor(kRed);
    TGraph *emcrz=new TGraph(1); emcrz->SetMarkerStyle(21); emcrz->SetMarkerSize(0.8); emcrz->SetMarkerColor(kRed);
    TGraph *emcpz=new TGraph(1); emcpz->SetMarkerStyle(21); emcpz->SetMarkerSize(0.8); emcpz->SetMarkerColor(kRed);
    TGraph *emcdxz=new TGraph(1); emcdxz->SetMarkerStyle(21); emcdxz->SetMarkerSize(0.8); emcdxz->SetMarkerColor(kRed);
    TGraph *emcdyz=new TGraph(1); emcdyz->SetMarkerStyle(21); emcdyz->SetMarkerSize(0.8); emcdyz->SetMarkerColor(kRed);
    TGraph *emcdrz=new TGraph(1); emcdrz->SetMarkerStyle(21); emcdrz->SetMarkerSize(0.8); emcdrz->SetMarkerColor(kRed);
    TGraph *emcdpz=new TGraph(1); emcdpz->SetMarkerStyle(21); emcdpz->SetMarkerSize(0.8); emcdpz->SetMarkerColor(kRed);
    StEEmcIUPointMaker *mEEpoints = (StEEmcIUPointMaker *)GetMaker("mEEpoints");
    if(mEEpoints){
      StEEmcIUPointVec_t mPoints = mEEpoints->points();
      for ( Int_t ipoint=0; ipoint<mEEpoints->numberOfPoints(); ipoint++){
	StEEmcIUPoint point=mEEpoints->point(ipoint);
	float x=point.position().x();
	float y=point.position().y();
	float z=point.position().z();
	e[neemc][0]=point.energy(0);
	e[neemc][1]=point.energy(1)*1000;
	e[neemc][2]=point.energy(2)*1000;
	e[neemc][3]=point.energy(3)*1000;
	e[neemc][4]=point.cluster(0).energy()*1000;
	e[neemc][5]=point.cluster(1).energy()*1000;	
	printf("EEMC %d xyz=%6.2f %6.2f %6.2f e=%6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n",
	       ipoint,x,y,z,e[neemc][0],e[neemc][1],e[neemc][2],e[neemc][3],e[neemc][4],e[neemc][5]);
	double xx = t->ax + t->mx*z;
	double yy = t->ay + t->my*z;
	double rr = sqrt(xx*xx+yy*yy);
	double pp = atan2(yy,xx);
	double dx = x-xx;
	double dy = y-yy;
	double dr = sqrt(x*x+y*y)-rr;
	double dp = atan2(y,x)-pp;
	static const double PI=3.141592654;
	if(dp>PI) dp-=2*PI;
	if(dp<-PI) dp+=2*PI;
	double dist = sqrt(dx*dx+dy*dy);
	if(dist<maxDist && e[neemc][1]>0.3 && e[neemc][2]>0.3){
	  emcxz->SetPoint(ndisp,z,x);
	  emcyz->SetPoint(ndisp,z,y);
	  emcrz->SetPoint(ndisp,z,sqrt(x*x+y*y));
	  emcpz->SetPoint(ndisp,z,atan2(y,x));
	  emcdxz->SetPoint(ndisp,z,dx);
	  emcdyz->SetPoint(ndisp,z,dy);
	  emcdrz->SetPoint(ndisp,z,dr);
	  emcdpz->SetPoint(ndisp,z,dp);
	  ndisp++;
	  neemc++;
	  fillHist(7,quad,dx,dy,dr,dp,xx,yy,rr,pp);
	}
      }
    }
    //Draw
    if(ndisp==0) continue;
    //if(nprompt==0) continue;
    if(neemc==0) continue;
    //if(nprompt==0 && neemc==0) continue;
    //getting idea where we should plot
    double xmin = (t->ax + t->mx*zmin2);
    double ymin = (t->ay + t->my*zmin2);
    double xmax = (t->ax + t->mx*zmax);
    double ymax = (t->ay + t->my*zmax);
    double rmin = sqrt(xmin*xmin+ymin*ymin);
    double rmax = sqrt(xmax*xmax+ymax*ymax);
    double pmax = atan2(ymax,xmax) + 0.3;
    double pmin = atan2(ymax,xmax) - 0.3;
    if(rmax<70) continue;
    //Draw 
    static TCanvas *c1=0, *c2=0; 
    if(!c1) c1=new TCanvas("EventDisp","Event Disp",50,50,800,800);
    if(!c2) c2=new TCanvas("EventDisp2","Event Disp2",100,100,800,800);
    gStyle->SetOptStat(0);
    //far view
    TH2F *xz = new TH2F("xz","xz",1,zmin,zmax,1,-200,200); 
    TH2F *yz = new TH2F("yz","yz",1,zmin,zmax,1,-200,200);
    TH2F *rz = new TH2F("rz","rz",1,zmin,zmax,1, -10,200);
    TH2F *pz = new TH2F("pz","pz",1,zmin,zmax,1,-3.2,3.2);
    c1->Clear();
    c1->Divide(2,2);
    c1->cd(1); xz->Draw(); hitxz->Draw("P"); fxz->Draw("LSAME"); tpcxz->Draw("P"); emcxz->Draw("P");
    c1->cd(2); yz->Draw(); hityz->Draw("P"); fyz->Draw("LSAME"); tpcyz->Draw("P"); emcyz->Draw("P");
    c1->cd(3); rz->Draw(); hitrz->Draw("P"); frz->Draw("LSAME"); tpcrz->Draw("P"); emcrz->Draw("P");
    c1->cd(4); pz->Draw(); hitpz->Draw("P"); fpz->Draw("LSAME"); tpcpz->Draw("P"); emcpz->Draw("P");
    c1->Update();
    //close view for TPC
    //if(xmax<xmin) {double t=xmin; xmin=xmax; xmax=t;}
    //if(ymax<ymin) {double t=ymin; ymin=ymax; ymax=t;}
    //float ddx=fabs(xmax-xmin), avx=(xmax+xmin)/2.0;
    //float ddy=fabs(ymax-ymin), avy=(ymax+ymin)/2.0;
    //if(ddx<20) {xmin=avx-10.0; xmax=avx+10.0;}
    //if(ddy<20) {ymin=avy-10.0; ymax=avy+10.0;}
    //TH2F *xz2 = new TH2F("xz","xz",1,zmin2,zmax,1,xmin,xmax); 
    //TH2F *yz2 = new TH2F("yz","yz",1,zmin2,zmax,1,ymin,ymax);
    //TH2F *rz2 = new TH2F("rz","rz",1,zmin2,zmax,1,rmin,rmax);
    //TH2F *pz2 = new TH2F("pz","pz",1,zmin2,zmax,1,pmin,pmax);
    TH2F *xz2 = new TH2F("dxz","dxz",1,zmin2,zmax,1,-12,12);
    TH2F *yz2 = new TH2F("dyz","dyz",1,zmin2,zmax,1,-12,12);
    TH2F *rz2 = new TH2F("drz","drz",1,zmin2,zmax,1,-12,12);
    TH2F *pz2 = new TH2F("dpz","dpz",1,zmin2,zmax,1,-0.3,0.3);
    c2->Clear();
    c2->Divide(2,2);
    c2->cd(1); xz2->Draw(); /* hitxz->Draw("P");  fxz->Draw("LSAME"); */  tpcdxz->Draw("P"); emcdxz->Draw("P"); 
    c2->cd(2); yz2->Draw(); /* hityz->Draw("P");  fyz->Draw("LSAME"); */  tpcdyz->Draw("P"); emcdyz->Draw("P");
    c2->cd(3); rz2->Draw(); /* hitrz->Draw("P");  frz->Draw("LSAME"); */  tpcdrz->Draw("P"); emcdrz->Draw("P");
    c2->cd(4); pz2->Draw(); /* hitpz->Draw("P");  fpz->Draw("LSAME"); */  tpcdpz->Draw("P"); emcdpz->Draw("P");
    for(int i=0; i<neemc; i++){
      char t[100]; 
      sprintf(t,"E=%4.1f P1=%4.2f P2=%4.2f T=%4.2f U=%4.2f V=%4.2f",e[i][0],e[i][1],e[i][2],e[i][3],e[i][4],e[i][5]);
      printf("%s\n",t);
      TText *tt = new TText(0.1,0.9-0.07*i,t); tt->SetNDC(); tt->SetTextSize(0.04); tt->Draw();
    }
    c2->Update();
    if(nplot<50){
      char tmp[100];
      sprintf(tmp,"disp/evdisp1_%d.png",nplot); c1->SaveAs(tmp);
      sprintf(tmp,"disp/evdisp2_%d.png",nplot); c2->SaveAs(tmp);      
      nplot++;
    }
    //printf("Hit something -->");
    //cin.get();
    //printf(" Contineing...\n");
  }
}

void StFgtAlignmentMaker::calcETBalanceFromStEvent() {
  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    gMessMgr->Warning() << "StFgtAlignmentMaker::Make : No StEvent" << endm;
    return;          // if no event, we're done
  }
  ele.set(0,0,0); tracks.set(0,0,0); bemcs.set(0,0,0); eemcs.set(0,0,0); tot.set(0,0,0); rec.set(0,0,0); iso.set(0,0,0);
  int nele=0,ntrack=0, nbemc=0, neemc=0, ntot=0, nrec=0, niso=0;
  UInt_t NpVX = event->numberOfPrimaryVertices();
  if (!NpVX) return;
  NpVX=1; //only 1st vertex for now
  for (UInt_t i = 0; i < NpVX; i++) {
    const StPrimaryVertex *vtx = event->primaryVertex(i);
    if(mDebug>2) cout << Form("Vertex: %3i ",i) << *vtx << endl;
    //EEMC find W candidate
    StEEmcIUPointMaker *mEEpoints = (StEEmcIUPointMaker *)GetMaker("mEEpoints");
    if(mEEpoints){
      StEEmcIUPointVec_t mPoints = mEEpoints->points();
      for ( Int_t ipoint=0; ipoint<mEEpoints->numberOfPoints(); ipoint++){	
	StEEmcIUPoint point=mEEpoints->point(ipoint);
	float e=point.energy(0);
	StThreeVectorF pp(point.position().x(),point.position().y(),point.position().z());
	StThreeVectorF d=pp - vtx->position();
	d=d/d.mag();
	StThreeVectorF ee=e*d;
	eemcs+=ee;
	if(e>10.0 && 
	   // point.numberOfRelatives()==0 && 
	   point.cluster(0).energy()>0.02 && 
	   point.cluster(1).energy()>0.02){	  
	  if(ee.perp() > ele.perp()) ele=ee;
	  nele++;
	}
      }
    }
    //EEMC isolation cone around W candidate
    if(mEEpoints && nele>0){
      StEEmcIUPointVec_t mPoints = mEEpoints->points();
      for ( Int_t ipoint=0; ipoint<mEEpoints->numberOfPoints(); ipoint++){
	StEEmcIUPoint point=mEEpoints->point(ipoint);
        float e=point.energy(0);
        StThreeVectorF pp(point.position().x(),point.position().y(),point.position().z());
        StThreeVectorF d=pp - vtx->position();
        d=d/d.mag();
	neemc++;
	double deta = ele.pseudoRapidity() - d.pseudoRapidity();
	double dphi = ele.phi() - d.phi(); if(dphi>PI) {dphi-=2*PI;} if(dphi<-PI) {dphi+=2*PI;}
	if(fabs(deta)<ISOCUT && fabs(dphi)<ISOCUT) { StThreeVectorF ee=e*d; iso+=ee; niso++;}
      }
    }
    //Tracks
    UInt_t nDaughters = vtx->numberOfDaughters();
    for (UInt_t j = 0; j < nDaughters; j++) {
      StPrimaryTrack* pTrack = (StPrimaryTrack*) vtx->daughter(j);
      if (! pTrack) continue;
      tracks+=pTrack->geometry()->momentum();
      ntrack++;
      double deta = ele.pseudoRapidity() - pTrack->geometry()->momentum().pseudoRapidity();
      double dphi = ele.phi() - pTrack->geometry()->momentum().phi(); if(dphi>PI) {dphi-=2*PI;} if(dphi<-PI) {dphi+=2*PI;}
      if(fabs(deta)<ISOCUT && fabs(dphi)<ISOCUT) { iso+=pTrack->geometry()->momentum(); }
    }
    //BEMC    
    StEmcDetector *btow=event->emcCollection()->detector(kBarrelEmcTowerId);    
    if(btow){
      for(int i=0; i<btow->numberOfModules(); i++){
	StEmcModule* mod=btow->module(i+1);
	StSPtrVecEmcRawHit& hits = mod->hits();
	for(UInt_t k=0;k<hits.size();k++) {
	  float e=hits[k]->energy();
	  if(e<0.2) continue;
	  int mod=hits[k]->module();
	  int eta=hits[k]->eta();
	  int sub=hits[k]->sub();
	  float x,y,z;	    
	  StEmcGeom::instance("bemc")->getXYZ(mod,eta,sub,x,y,z);
	  //printf("mod=%3d eta=%3d sub=%3d ene=%8.2f xyz=%8.2f %8.2f %8.2f\n",mod,eta,sub,e,x,y,z);
	  StThreeVectorF d = StThreeVectorF(x,y,z) - vtx->position();
	  d=d/d.mag();	    
	  bemcs+=e*d;
	  nbemc++;
	  double deta = ele.pseudoRapidity() - d.pseudoRapidity();
	  double dphi = ele.phi() - d.phi(); if(dphi>PI) {dphi-=2*PI;} if(dphi<-PI) {dphi+=2*PI;}
	  if(fabs(deta)<ISOCUT && fabs(dphi)<ISOCUT) { StThreeVectorF ee=e*d; iso+=ee; }
	}
      }
    }      
  } 
  tot=tracks+bemcs+eemcs; ntot=ntrack+nbemc+neemc;
  rec=tot-ele; if(nele>0) {nrec=ntot-1;} else {nrec=ntot;}
  float dphi=ele.phi() - tot.phi();
  ptbal = tot.perp()*cos(dphi);
  riso = ele.mag()/iso.mag();
  printf("NELE=%4d NTRK=%4d NBEMC=%4d NEEMC=%4d NTOT=%4d NREC=%4d NISO=%4d\n",nele,ntrack,nbemc,neemc,ntot,nrec,niso);
  printf("EELE=%6.1f ETRK=%6.1f EBEMC=%6.1f EEEMC=%6.1f ETOT=%6.1f EREC=%6.1f EISO=%6.1f\n",
	 ele.mag(),tracks.mag(),bemcs.mag(),eemcs.mag(),tot.mag(),rec.mag(),iso.mag());
  printf("PELE=%6.1f PTRK=%6.1f PBEMC=%6.1f PEEMC=%6.1f PTOT=%6.1f PREC=%6.1f PISO=%6.1f\n",
	 ele.perp(),tracks.perp(),bemcs.perp(),eemcs.perp(),tot.perp(),rec.perp(),iso.perp());
  printf("Dphi=%6.1f ETBal=%6.1f R(ele/iso)=%6.3f\n",dphi,ptbal,riso);
}

void StFgtAlignmentMaker::fillETBalance(int itrk){
  mHit[mQuad][itrk].ele[0]=ele.x();
  mHit[mQuad][itrk].ele[1]=ele.y();
  mHit[mQuad][itrk].ele[2]=ele.z();
  mHit[mQuad][itrk].ele[3]=ele.mag();
  mHit[mQuad][itrk].trk[0]=tracks.x();
  mHit[mQuad][itrk].trk[1]=tracks.y();
  mHit[mQuad][itrk].trk[2]=tracks.z();
  mHit[mQuad][itrk].trk[3]=tracks.mag();
  mHit[mQuad][itrk].bemc[0]=bemcs.x();
  mHit[mQuad][itrk].bemc[1]=bemcs.y();
  mHit[mQuad][itrk].bemc[2]=bemcs.z();
  mHit[mQuad][itrk].bemc[3]=bemcs.mag();
  mHit[mQuad][itrk].eemc[0]=eemcs.x();
  mHit[mQuad][itrk].eemc[1]=eemcs.y();
  mHit[mQuad][itrk].eemc[2]=eemcs.z();
  mHit[mQuad][itrk].eemc[3]=eemcs.mag();
  mHit[mQuad][itrk].tot[0]=tot.x();
  mHit[mQuad][itrk].tot[1]=tot.y();
  mHit[mQuad][itrk].tot[2]=tot.z();
  mHit[mQuad][itrk].tot[3]=tot.mag();
  mHit[mQuad][itrk].rec[0]=rec.x();
  mHit[mQuad][itrk].rec[1]=rec.y();
  mHit[mQuad][itrk].rec[2]=rec.z();
  mHit[mQuad][itrk].rec[3]=rec.mag();
  mHit[mQuad][itrk].iso[0]=iso.x();
  mHit[mQuad][itrk].iso[1]=iso.y();
  mHit[mQuad][itrk].iso[2]=iso.z();
  mHit[mQuad][itrk].iso[3]=iso.mag();
  mHit[mQuad][itrk].ptbal=ptbal;
  mHit[mQuad][itrk].riso=riso;
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
  if(mDebug>2) cout << "Number of Primary vertex="<<NpVX<<endl;
  if (!NpVX) return;

  for (UInt_t i = 0; i < NpVX; i++) {
    const StPrimaryVertex *vx = event->primaryVertex(i);
    if(mDebug>2) cout << Form("Vertex: %3i ",i) << *vx << endl;
    UInt_t nDaughters = vx->numberOfDaughters();
    for (UInt_t j = 0; j < nDaughters; j++) {
      StPrimaryTrack* pTrack = (StPrimaryTrack*) vx->daughter(j);
      if (! pTrack) continue;
      //if (pTrack->geometry()->momentum().pseudoRapidity()<1.0) continue;
      int nfgt=0, ntpc=0, nprompt=0, quad=-1;
      StPtrVecHit &hits=pTrack->detectorInfo()->hits();
      for (StPtrVecHitConstIterator iter=hits.begin(); iter != hits.end(); iter++){
	StDetectorId det=(*iter)->detector();
	if (det == kTpcId) {
	  ntpc++;
	  StThreeVectorF xyz=(*iter)->position();
	  if(xyz.z()>200.0) {nprompt++;}
	}else if (det == kFgtId) {	    
	  nfgt++;
	  StFgtPoint* point=(StFgtPoint*)(*iter);
	  quad=point->getQuad();
	}
      }
      cout << Form("Track: Tpc=%2d Pmp=%1d Fgt=%1d Quad=%1d ",ntpc,nprompt,nfgt,quad) << *pTrack << endl;
      if(nfgt==0) continue; 
      int itrk=mNtrk[quad];
      int ihit=0;
      //get vertex
      mHit[quad][itrk].det[ihit]=24+quad;
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
	  mHit[quad][itrk].det[ihit]=28+quad;
	  StThreeVectorF xyz=(*iter)->position();
	  mHit[quad][itrk].x[ihit]=xyz.x();
	  mHit[quad][itrk].y[ihit]=xyz.y();
	  mHit[quad][itrk].z[ihit]=xyz.z(); 
	  if(mHit[quad][itrk].z[ihit]>200.0){mHit[quad][itrk].det[ihit]=32+quad;}
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
	  mHit[quad][itrk].lr[ihit]=point->getPositionR();
	  mHit[quad][itrk].lp[ihit]=point->getPositionPhi();
	  mHit[quad][itrk].z[ihit]=StFgtGeom::getDiscZ(disc);
	  StThreeVectorF exyz=(*iter)->positionError();
	  if(mErrFgt>=0.0) {mHit[quad][itrk].ex[ihit]=mErrFgt;} else {mHit[quad][itrk].ex[ihit]=exyz.x();}
	  if(mErrFgt>=0.0) {mHit[quad][itrk].ey[ihit]=mErrFgt;} else {mHit[quad][itrk].ey[ihit]=exyz.y();}
	  if(mErrFgt>=0.0) {mHit[quad][itrk].ez[ihit]=0.0;}     else {mHit[quad][itrk].ez[ihit]=exyz.z();}
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

void StFgtAlignmentMaker::readFromStEventGlobal() {
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
    cout << "# of FGT point:            " << event->fgtCollection()->getNumPoints() << endl;
  }else{
    cout << "No FGT collection" << endl;
  }

  StSPtrVecTrackNode& trackNode = event->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  cout << "# of tracks "<<nTracks<<endl;
  for (UInt_t  i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    int nfgt=0, ntpc=0, nprompt=0, quad=-1;
    StPtrVecHit &hits=gTrack->detectorInfo()->hits();
    for (StPtrVecHitConstIterator iter=hits.begin(); iter != hits.end(); iter++){
      StDetectorId det=(*iter)->detector();
      if (det == kTpcId) {
	ntpc++;
	StThreeVectorF xyz=(*iter)->position();
	if(xyz.z()>200.0) {nprompt++;}
      }else if (det == kFgtId) {	    
	nfgt++;
	StFgtPoint* point=(StFgtPoint*)(*iter);
	quad=point->getQuad();
      }
    }
    cout << Form("Track: Tpc=%2d Pmp=%1d Fgt=%1d Quad=%1d ",ntpc,nprompt,nfgt,quad) << *gTrack << endl;
    if(nfgt==0) continue; 
    int itrk=mNtrk[quad];
    int ihit=0;
    for (StPtrVecHitConstIterator iter=hits.begin(); iter != hits.end(); iter++){
      StDetectorId det=(*iter)->detector();
      if (det == kTpcId){
	mHit[quad][itrk].det[ihit]=28+quad;
	StThreeVectorF xyz=(*iter)->position();
	mHit[quad][itrk].x[ihit]=xyz.x();
	mHit[quad][itrk].y[ihit]=xyz.y();
	mHit[quad][itrk].z[ihit]=xyz.z(); 
	if(mHit[quad][itrk].z[ihit]>200.0){mHit[quad][itrk].det[ihit]=32+quad;}
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
	mHit[quad][itrk].lr[ihit]=point->getPositionR();
	mHit[quad][itrk].lp[ihit]=point->getPositionPhi();
	mHit[quad][itrk].z[ihit]=StFgtGeom::getDiscZ(disc);
	StThreeVectorF exyz=(*iter)->positionError();
	if(mErrFgt>0.0) {mHit[quad][itrk].ex[ihit]=mErrFgt;} else {mHit[quad][itrk].ex[ihit]=exyz.x();}
	if(mErrFgt>0.0) {mHit[quad][itrk].ey[ihit]=mErrFgt;} else {mHit[quad][itrk].ey[ihit]=exyz.y();}
	if(mErrFgt>0.0) {mHit[quad][itrk].ez[ihit]=0;}       else {mHit[quad][itrk].ez[ihit]=exyz.z();}
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
}

void StFgtAlignmentMaker::writeTree(){
  char fname[200];
  sprintf(fname,"alignment_trkout_step%d.root",mStep+1);
  if(mDay>0){
    sprintf(fname,"%d/alignment_trkout_step%d_day%d.root",mDay,mStep+1,mDay);
  } else if(mRunNumber>0) {
    int yearday=mRunNumber/1000;
    sprintf(fname,"%d/alignment_trkout_step%d.%d.root",yearday,mStep+1,mRunNumber);
    if(mSeqNumber>0){
      sprintf(fname,"%d/alignment_trkout_step%d.%d.%d.root",yearday,mStep+1,mRunNumber,mSeqNumber);
    }
  }
  cout << "Creating "<<fname<<endl;
  TFile *f=new TFile(fname,"RECREATE","tracks");
  for(int quad=0; quad<kFgtNumQuads; quad++){
    char c[10];
    sprintf(c,"q%1d",quad);
    TTree* t = new TTree(c,c);
    t->Branch("run", &(mHitAlg.run),       "run/I");
    t->Branch("evt", &(mHitAlg.evt),       "evt/I");
    t->Branch("nhit",&(mHitAlg.nhit),      "nhit/I");
    t->Branch("nuse",&(mHitAlg.nhitUse),   "nuse/I");
    t->Branch("nfgt",&(mHitAlg.nhitFgt),   "nfgt/I");
    t->Branch("nvtx",&(mHitAlg.nhitVtx),   "nvtx/I");
    t->Branch("ntpc",&(mHitAlg.nhitTpc),   "ntpc/I");
    t->Branch("nppt",&(mHitAlg.nhitPrompt),"nppt/I");
    t->Branch("nemc",&(mHitAlg.nhitEemc),  "nemc/I");
    t->Branch("used",&(mHitAlg.used),      "used/I");
    t->Branch("dca" ,&(mHitAlg.dca),       "dca/F");
    t->Branch("chi2",&(mHitAlg.chi2),      "chi2/F");
    t->Branch("trkz",&(mHitAlg.trkz),      "trkz/F");
    t->Branch("dz" , &(mHitAlg.dz),        "dz/F");
    t->Branch("eta", &(mHitAlg.eta),       "eta/F");
    t->Branch("phi", &(mHitAlg.phi),       "phi/F");
    t->Branch("opt", &(mHitAlg.opt),       "opt/F");
    t->Branch("det", mHitAlg.det,          "det[nhit]/I");
    t->Branch("x",   mHitAlg.x,            "x[nhit]/F");
    t->Branch("y",   mHitAlg.y,            "y[nhit]/F");
    t->Branch("z",   mHitAlg.z,            "z[nhit]/F");
    t->Branch("ex",  mHitAlg.ex,           "ex[nhit]/F");
    t->Branch("ey",  mHitAlg.ey,           "ey[nhit]/F");
    t->Branch("ez",  mHitAlg.ez,           "ez[nhit]/F");
    t->Branch("lr",  mHitAlg.lr,           "lr[nhit]/F");
    t->Branch("lp",  mHitAlg.lp,           "lp[nhit]/F");
    t->Branch("r",   mHitAlg.r,            "r[nhit]/F");
    t->Branch("p",   mHitAlg.p,            "p[nhit]/F");
    t->Branch("dx",  mHitAlg.dx,           "dx[nhit]/F");
    t->Branch("dy",  mHitAlg.dy,           "dy[nhit]/F");
    t->Branch("dr",  mHitAlg.dr,           "dr[nhit]/F");
    t->Branch("dp",  mHitAlg.dp,           "dp[nhit]/F");
    t->Branch("tw",  mHitAlg.tw,           "tw[nhit]/F");
    t->Branch("p1",  mHitAlg.p1,           "p1[nhit]/F");
    t->Branch("p2",  mHitAlg.p2,           "p2[nhit]/F");
    t->Branch("po",  mHitAlg.po,           "po[nhit]/F");
    t->Branch("su",  mHitAlg.su,           "su[nhit]/F");
    t->Branch("sv",  mHitAlg.sv,           "sv[nhit]/F");
    t->Branch("nrl", mHitAlg.nrl,          "nrl[nhit]/I");
    t->Branch("nsu", mHitAlg.nsu,          "nsu[nhit]/I");
    t->Branch("nsv", mHitAlg.nsv,          "nsv[nhit]/I");
    t->Branch("ele", mHitAlg.ele,          "ele[4]/F");
    t->Branch("trk", mHitAlg.trk,          "trk[4]/F");
    t->Branch("bemc",mHitAlg.bemc,         "bemc[4]/F");
    t->Branch("eemc",mHitAlg.eemc,         "eemc[4]/F");
    t->Branch("tot", mHitAlg.tot,          "tot[4]/F");
    t->Branch("rec", mHitAlg.rec,          "rec[4]/F");
    t->Branch("iso", mHitAlg.iso,          "iso[4]/F");
    t->Branch("ptbal",&(mHitAlg.ptbal),    "ptbal/F");
    t->Branch("riso", &(mHitAlg.riso),     "riso/F");
    t->Branch("use", mHitAlg.use,          "use[nhit]/O");

    int itrk=0,ntrk=0;
    for(itrk=0; itrk<mNtrk[quad]; itrk++){
      if(mHit[quad][itrk].used==0) continue;
      memcpy(&mHitAlg,&mHit[quad][itrk],sizeof(mHitAlg));
      //printf("WTREE %1d %8d %2d %6.2f %6.2f %6.2f\n",quad,itrk,mHitAlg.nhit,mHitAlg.z[0],mHitAlg.z[1],mHitAlg.z[2]);
      t->Fill();
      ntrk++;
    }
    cout<<Form(" Wrote %6d/%6d tracks for quad=%1d",ntrk,itrk,quad)<<endl;
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
    t->SetBranchAddress("run", &(mHitAlg.run));
    t->SetBranchAddress("evt", &(mHitAlg.evt));
    t->SetBranchAddress("nhit",&(mHitAlg.nhit));
    t->SetBranchAddress("nuse",&(mHitAlg.nhitUse));
    t->SetBranchAddress("nfgt",&(mHitAlg.nhitFgt));
    t->SetBranchAddress("ntpc",&(mHitAlg.nhitTpc));
    t->SetBranchAddress("nppt",&(mHitAlg.nhitPrompt));
    t->SetBranchAddress("nemc",&(mHitAlg.nhitEemc));
    t->SetBranchAddress("used",&(mHitAlg.used));
    t->SetBranchAddress("dca" ,&(mHitAlg.dca));
    t->SetBranchAddress("chi2",&(mHitAlg.chi2));
    t->SetBranchAddress("trkz",&(mHitAlg.trkz));
    t->SetBranchAddress("dz" , &(mHitAlg.dz));
    t->SetBranchAddress("eta", &(mHitAlg.eta));
    t->SetBranchAddress("phi", &(mHitAlg.phi));
    t->SetBranchAddress("opt", &(mHitAlg.opt));
    t->SetBranchAddress("det", mHitAlg.det);
    t->SetBranchAddress("x",   mHitAlg.x);
    t->SetBranchAddress("y",   mHitAlg.y);
    t->SetBranchAddress("z",   mHitAlg.z);
    t->SetBranchAddress("ex",  mHitAlg.ex);
    t->SetBranchAddress("ey",  mHitAlg.ey);
    t->SetBranchAddress("ez",  mHitAlg.ez);
    t->SetBranchAddress("lr",  mHitAlg.lr);
    t->SetBranchAddress("lp",  mHitAlg.lp);
    t->SetBranchAddress("r",   mHitAlg.r);
    t->SetBranchAddress("p",   mHitAlg.p);
    t->SetBranchAddress("dx",  mHitAlg.dx);
    t->SetBranchAddress("dy",  mHitAlg.dy);
    t->SetBranchAddress("dr",  mHitAlg.dr);
    t->SetBranchAddress("dp",  mHitAlg.dp);
    t->SetBranchAddress("tw",  mHitAlg.tw);
    t->SetBranchAddress("p1",  mHitAlg.p1);
    t->SetBranchAddress("p2",  mHitAlg.p2);
    t->SetBranchAddress("po",  mHitAlg.po);
    t->SetBranchAddress("su",  mHitAlg.su);
    t->SetBranchAddress("sv",  mHitAlg.sv);
    t->SetBranchAddress("nrl", mHitAlg.nrl);
    t->SetBranchAddress("nsu", mHitAlg.nsu);
    t->SetBranchAddress("nsv", mHitAlg.nsv);
    t->SetBranchAddress("ele", mHitAlg.ele);
    t->SetBranchAddress("trk", mHitAlg.trk);
    t->SetBranchAddress("bemc",mHitAlg.bemc);
    t->SetBranchAddress("eemc",mHitAlg.eemc);
    t->SetBranchAddress("tot", mHitAlg.tot);
    t->SetBranchAddress("rec", mHitAlg.rec);
    t->SetBranchAddress("iso", mHitAlg.iso);
    t->SetBranchAddress("ptbal",&(mHitAlg.ptbal));
    t->SetBranchAddress("riso", &(mHitAlg.riso));
    t->SetBranchAddress("use", mHitAlg.use);
    mNtrk[quad]=t->GetEntries();
    int itrk;
    for(itrk=0; itrk<mNtrk[quad]; itrk++){
      if(itrk>=MAXTRK){
	cout<<Form(" MAXTRK=%d reached\n",MAXTRK)<<endl;
	mNtrk[quad]=MAXTRK;
	break;
      }
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
  const char* name[6]={"phi  ","theta","psi  ","xoff ","yoff ","zoff "};
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
  char fname[100]="fgt_alignment.dat";
  int yearday=mRunNumber/1000;
  if(mRunNumber>0) {
    sprintf(fname,"%d/fgt_alignment_%d.dat",yearday,mRunNumber);
    if(mSeqNumber>0) sprintf(fname,"%d/fgt_alignment_%d.%07d.dat",yearday,mRunNumber,mSeqNumber);
  }
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
  fclose(f);
}

void StFgtAlignmentMaker::readPar(fgtAlignment_st* algpar){
  algpar=orig_algpar=new fgtAlignment_st;
  printf("Reading %s\n",mReadParFile);
  FILE *f=fopen(mReadParFile,"r");
  if(f==0) {printf("Could not open %s\n",mReadParFile); return;}
  for(int disc=0; disc<6; disc++){
    for(int quad=0; quad<4; quad++){
      int ii, i=disc*4+quad;
      fscanf(f,"%d %d %d %lf %lf %lf %lf %lf %lf\n",
	     &disc,&quad,&ii,
	     &(algpar->phi[i]), &(algpar->theta[i]),&(algpar->psi[i]),
	     &(algpar->xoff[i]),&(algpar->yoff[i]),&(algpar->zoff[i]));
      printf("%1d %1d %3d %12.8f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
	     disc,quad,i,
	     algpar->phi[i], algpar->theta[i],algpar->psi[i],
	     algpar->xoff[i],algpar->yoff[i],algpar->zoff[i]);
    }
  }
  fclose(f);
}

void StFgtAlignmentMaker::bookHist(){
  const float maxd[NAXIS][kFgtNumDiscs+6]={ {maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdvtx,maxdtpc,maxdtpc,maxdemc,maxdemc,maxdemc},
					    {maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdvtx,maxdtpc,maxdtpc,maxdemc,maxdemc,maxdemc},
					    {maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdfgt,maxdvtx,maxdtpc,maxdtpc,maxdemc,maxdemc,maxdemc},
					    {maxpfgt,maxpfgt,maxpfgt,maxpfgt,maxpfgt,maxpfgt,maxpvtx,maxptpc,maxptpc,maxpemc,maxpemc,maxpemc}};
  float xmin[4]={  0,-10,-40,-40};
  float xmax[4]={ 40, 40,  0, 10};
  float ymin[4]={-10,-40,-40,  0};
  float ymax[4]={ 40,  0, 10, 40};
  float rmin=10.0, rmax=40.0;
  float phimin[4],phimax[4];
  phimin[0]=StFgtGeom::phiQuadXaxis(0); phimax[0]=StFgtGeom::phiQuadXaxis(3);
  phimin[1]=StFgtGeom::phiQuadXaxis(1); phimax[1]=StFgtGeom::phiQuadXaxis(0);
  phimin[2]=StFgtGeom::phiQuadXaxis(2); phimax[2]=StFgtGeom::phiQuadXaxis(1)+2*PI;
  phimin[3]=StFgtGeom::phiQuadXaxis(3); phimax[3]=StFgtGeom::phiQuadXaxis(2);
  const int nbin=50;
  //1d residuals
  const char* caxis1[NAXIS]={"dx","dy","dr","dphi"};
  //2d residuals
  const char* caxis2[NAXIS*2]={"dx.vs.x","dy.vs.x", "dr.vs.r",  "dphi.vs.r",
			       "dx.vs.y","dy.vs.y", "dr.vs.phi","dphi.vs.phi"};
  const char* cquad[kFgtNumQuads]={"A","B","C","D"};
  const char* cdisc[kFgtNumDiscs+6]={"1","2","3","4","5","6","V","T","P","E","m","e"};

  for(int disc=0; disc<kFgtNumDiscs+6; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int axis=0; axis<NAXIS; axis++){
	char c[50];
	//1d hist
	sprintf(c,"%1s%1s-%s",cdisc[disc],cquad[quad],caxis1[axis]);
	hist1[disc][quad][axis]=new TH1F(c,c,nbin,-maxd[axis][disc],maxd[axis][disc]);

	//2d hist
	float xmin1,xmax1,xmin2,xmax2;
	if(axis<2){ 
	  xmin1=xmin[quad]; xmax1=xmax[quad]; xmin2=ymin[quad]; xmax2=ymax[quad]; 
	  if(disc==kFgtNumDiscs){ xmin1=-10;xmax1=10; xmin2=-10; xmax2=10;}
	  if(disc>kFgtNumDiscs) { xmin1=-160; xmax1=160; xmin2=-160; xmax2=160; }
	}else{ 
	  xmin1=rmin; xmax1=rmax; xmin2=phimin[quad]; xmax2=phimax[quad]; 	  
	  if(disc==kFgtNumDiscs) { xmin1=0;  xmax1=10; xmin2=-PI; xmax2=PI;}
	  if(disc>kFgtNumDiscs)  { xmin1*=4;  xmax1*=4; xmin2=-PI; xmax2=PI;}
	}
	sprintf(c,"%1s%1s-%s",cdisc[disc],cquad[quad],caxis2[axis]);
	hist2[disc][quad][axis]=new TH2F(c,c,nbin, xmin1,xmax1,nbin,-maxd[axis][disc],maxd[axis][disc]); 
	sprintf(c,"%1s%1s-%s",cdisc[disc],cquad[quad],caxis2[axis+NAXIS]);
	hist2[disc][quad][axis+NAXIS]=new TH2F(c,c,nbin, xmin2,xmax2,nbin,-maxd[axis][disc],maxd[axis][disc]); 
      }
    }
  }  
  histdz=new TH1F("Zfgt-Ztpc","Zfgt-Ztpc",60,-30,30);
}

void StFgtAlignmentMaker::resetHist(){
  for(int disc=0; disc<kFgtNumDiscs+6; disc++){
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
  char fname[100];
  sprintf(fname,"alignment_step%d.root",mStep+1);
  if(mDay>0){
    sprintf(fname,"%d/alignment_step%d_day%d.root",mDay,mStep+1,mDay);
  } else if(mRunNumber>0) {
    int yearday=mRunNumber/1000;
    sprintf(fname,"%d/alignment_step%d.%d.root",yearday,mStep+1,mRunNumber);
    if(mSeqNumber>0){
      sprintf(fname,"%d/alignment_step%d.%d.%d.root",yearday,mStep+1,mRunNumber,mSeqNumber);
    }
  }
  cout << "Writing " << fname << endl;
  TFile *hfile = new TFile(fname,"RECREATE");  
  for(int disc=0; disc<kFgtNumDiscs+6; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int axis=0; axis<NAXIS; axis++){
	hist1[disc][quad][axis]->Write();
	hist2[disc][quad][axis]->Write();
	hist2[disc][quad][axis+NAXIS]->Write();
      }
    }
  }
  histdz->Write();
  hfile->Close();
}

void StFgtAlignmentMaker::setHitMask(int hitmask_disc){
  cout << Form("Alignment Step %d for Quad=%1d using Hits from=",mStep,mQuad);
  for(int i=0; i<6; i++) {if(hitmask_disc & (1<<i) ) {cout << Form("FgtD%1d ",i+1);}}
  if(hitmask_disc & 0x40) cout << "Vertex ";
  if(hitmask_disc & 0x80) cout << "TPC ";
  if(hitmask_disc & 0x100) cout << "Prompt ";
  if(hitmask_disc & 0x200) cout << "Eemc ";
  if(hitmask_disc & 0x400) cout << "Mip ";
  if(hitmask_disc & 0x800) cout << "Ele ";
  cout << Form("(hitmask_disc = %03x)",hitmask_disc);
  mHitMaskDisc=hitmask_disc;
  mNtrkUse[mQuad]=0;
  for(int itrk=0; itrk<mNtrk[mQuad]; itrk++){
    mHit[mQuad][itrk].nhitUse=0;
    mHit[mQuad][itrk].nhitFgt=0;
    mHit[mQuad][itrk].nhitVtx=0;
    mHit[mQuad][itrk].nhitTpc=0;
    mHit[mQuad][itrk].nhitPrompt=0;
    mHit[mQuad][itrk].nhitEemc=0;
    mHit[mQuad][itrk].used=0;
    for(int ihit=0; ihit<mHit[mQuad][itrk].nhit; ihit++){
      mHit[mQuad][itrk].use[ihit]=false;
      int det=mHit[mQuad][itrk].det[ihit];      
      int disc=det/4;          //fgt disc0-5
      if((hitmask_disc & (1<<disc)) ==0) continue;
      if     (disc<6)  {
	if(mFgtRCut[mStep]>0 && fabs(mHit[mQuad][itrk].dr[ihit])>mFgtRCut[mStep]) continue;
	if(mFgtPCut[mStep]>0 && fabs(mHit[mQuad][itrk].dp[ihit])>mFgtPCut[mStep]) continue;
	mHit[mQuad][itrk].nhitFgt++;
      }else if(disc==6) { //vertex
	if(mDzCut[mStep]>0 &&  fabs(mHit[mQuad][itrk].dz )>mDzCut[mStep] ) continue;
	if(mDcaCut[mStep]>0 && fabs(mHit[mQuad][itrk].dca)>mDcaCut[mStep]) continue;       
	mHit[mQuad][itrk].nhitVtx++;
      }else if(disc==7) { //tpc
	if(mTpcRCut[mStep]>0 && fabs(mHit[mQuad][itrk].dr[ihit])>mTpcRCut[mStep]) continue;
	if(mTpcPCut[mStep]>0 && fabs(mHit[mQuad][itrk].dp[ihit])>mTpcPCut[mStep]) continue;
	mHit[mQuad][itrk].nhitTpc++;
      }else if(disc==8) { //tpc prompt
	if(mTpcRCut[mStep]>0 && fabs(mHit[mQuad][itrk].dr[ihit])>mTpcRCut[mStep]) continue;
	if(mTpcPCut[mStep]>0 && fabs(mHit[mQuad][itrk].dp[ihit])>mTpcPCut[mStep]) continue;
	mHit[mQuad][itrk].nhitTpc++;
	mHit[mQuad][itrk].nhitPrompt++;
      }else if(disc==9 || disc==10 || disc==11) { //eemc, mip, electron
	if(mEmcRCut[mStep]>0 && fabs(mHit[mQuad][itrk].dr[ihit])>mEmcRCut[mStep]) continue;
	if(mEmcPCut[mStep]>0 && fabs(mHit[mQuad][itrk].dp[ihit])>mEmcPCut[mStep]) continue;
	mHit[mQuad][itrk].nhitEemc++;
	//if(mHit[mQuad][itrk].dr[itrk]!=0)
	//  printf("EEMCCUT %10.6f %10.6f %d\n",mHit[mQuad][itrk].dr[ihit],mEmcRCut[mStep],mHit[mQuad][itrk].nhitEemc);	
      }
      mHit[mQuad][itrk].use[ihit]=true; 
      mHit[mQuad][itrk].nhitUse++;
    }
    //printf("SetHitMask %d n=%d %d %d %d %d %f %f\n",itrk,mHit[mQuad][itrk].nhitUse,mHit[mQuad][itrk].nhitFgt,mHit[mQuad][itrk].nhitTpc,mHit[mQuad][itrk].nhitPrompt,mHit[mQuad][itrk].nhitEemc,mHit[mQuad][itrk].dca,mHit[mQuad][itrk].chi2);
    if(mHit[mQuad][itrk].nhitUse>=mReqHit &&
       mHit[mQuad][itrk].nhitFgt>=mReqFgtHit &&
       mHit[mQuad][itrk].nhitVtx>=mReqVtx &&
       mHit[mQuad][itrk].nhitTpc>=mReqTpcHit &&
       mHit[mQuad][itrk].nhitPrompt>=mReqPromptHit &&
       mHit[mQuad][itrk].nhitEemc>=mReqEemcHit){
      mHit[mQuad][itrk].used=1;
      mNtrkUse[mQuad]++;
    }
  }
  cout << Form(" usable ntrack=%d with nhit>=%d nFgt>=%d vVtx>=%d nTpc>=%d nPrompt>=%d nEemc>=%d",
	       mNtrkUse[mQuad],mReqHit,mReqFgtHit,mReqVtx,mReqTpcHit,mReqPromptHit,mReqEemcHit)<<endl;
}

void StFgtAlignmentMaker::overWriteError(){
  printf("Over Writing Hit Errors\n");
  for(int iquad=0; iquad<kFgtNumQuads; iquad++){
    printf("  quad=%d ntrk=%d\n",iquad,mNtrk[iquad]);
    for(int itrk=0; itrk<mNtrk[iquad]; itrk++){
      for(int ihit=0; ihit<mHit[iquad][itrk].nhit; ihit++){
	//	printf("BBB  quad=%d itrk=%d nhit=%d ihit=%d 1928=%d\n",iquad,itrk,mHit[iquad][itrk].nhit,ihit,mHit[iquad][1928].nhit);
	if(ihit>100) return; //hack
	int det=mHit[iquad][itrk].det[ihit];      
	int disc=det/4;          
	if     (disc<6)  { //fgt disc0-5
	  mHit[iquad][itrk].ex[ihit]=mErrFgt;
	  mHit[iquad][itrk].ey[ihit]=mErrFgt;
	  mHit[iquad][itrk].ez[ihit]=0.0;
	}else if(disc==6) { //vertex
	  mHit[iquad][itrk].ex[ihit]=mErrVtx;
	  mHit[iquad][itrk].ey[ihit]=mErrVtx;
	  mHit[iquad][itrk].ez[ihit]=mErrVtxZ;
	}else if(disc==7) { //tpc
	  float x= mHit[iquad][itrk].x[ihit];
	  float y= mHit[iquad][itrk].y[ihit];
	  float r=sqrt(x*x+y*y);
	  if(r<127.950){ //inner
	    mHit[iquad][itrk].ex[ihit]=mErrTpcI;
	    mHit[iquad][itrk].ey[ihit]=mErrTpcI;
	    mHit[iquad][itrk].ez[ihit]=mErrTpcZ;
	  }else{         //outer
	    mHit[iquad][itrk].ex[ihit]=mErrTpcO;
	    mHit[iquad][itrk].ey[ihit]=mErrTpcO;
	    mHit[iquad][itrk].ez[ihit]=mErrTpcZ;
	  }
	}else if(disc==8) { //tpc prompt
	  mHit[iquad][itrk].ex[ihit]=mErrPpt;
	  mHit[iquad][itrk].ey[ihit]=mErrPpt;
	  mHit[iquad][itrk].ez[ihit]=0;
	}else if(disc==9 || disc==10 || disc==11) { //eemc, mip, electron
	  mHit[iquad][itrk].ex[ihit]=mErrEmc;
	  mHit[iquad][itrk].ey[ihit]=mErrEmc;
	  mHit[iquad][itrk].ez[ihit]=0;
	}
      }
    }
  }
}

void StFgtAlignmentMaker::doAlignment(fgtAlignment_st* input, 
				      int discmask,int quadmask, int parmask, int hitmask_disc, int residmask,
				      int trackType, int minHit, int minFgtHit, int minVtx, int minTpcHit, int minPromptHit, int minEemcHit,
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
  mReqFgtHit=minFgtHit;
  mReqVtx=minVtx;
  mReqTpcHit=minTpcHit;
  mReqPromptHit=minPromptHit;
  mReqEemcHit=minEemcHit;
  mResidMaskDisc=residmask;

  setHitMask(hitmask_disc);
  if(mNtrkUse[mQuad]==0) return;
  setPar(m,input,discmask,quadmask,parmask);

  if(parmask>0) {
    arg[0] = 0; m->mnexcm("SET PRI", arg ,1,iflag);
    //arg[0] =-1; m->mnexcm("SET NOWarning", arg, 0,iflag);
  }

  if(mFillHist>0){
    int np=NPAR;
    double *gin=0, f=0;
    double *par = (double *) input;
    if     (trackType==0) { funcLine (np,gin,f,par,iflag);}
    else if(trackType==1) { funcHelix(np,gin,f,par,iflag);}
  }else{    
    arg[0] = 20000; arg[1] = 1.; m->mnexcm("MIGRAD", arg ,2, iflag);
    if(mFillHist==0) getPar(m,result);
  }
}


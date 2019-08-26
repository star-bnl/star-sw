#include "TMath.h"
#include "StvGrappa.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TVector3.h"

#include "Stv/StvHit.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

ClassImp(StvGrappa)
enum StvGrappa2_e {kX,kY,kZ,kR};

class gemini 
{
public:
  gemini(){}
  void clear() { mHitsOn.clear();mHitsOff.clear();}
  void add( const float *xyz,int onTrak);
   int pri();
public:
std::vector<double> mHitsOn;
std::vector<double> mHitsOff;
};

static gemini gem;



//______________________________________________________________________________
StvGrappa::StvGrappa(const char* name) 
{
static int iGrappa=0;
static int nCall=0; nCall++;
  if (!name) name = "";
  TString tsName(name);
  if (!*name) { tsName = "Grappa_";}
  tsName += iGrappa++;
  SetName(tsName);
  memset(mBeg,0,mEnd-mBeg+1);
  mActive = 1;
}
//______________________________________________________________________________
void StvGrappa::Add(double x,double y,double z,int iObj)
{
  if (!mActive) return;
  mNTotal++;
  float r = sqrt(x*x+y*y);
  mPts[iObj][kX].push_back(x);
  mPts[iObj][kY].push_back(y);
  mPts[iObj][kZ].push_back(z);
  mPts[iObj][kR].push_back(r);
}
//______________________________________________________________________________
void StvGrappa::Clear(const char *)
{
  if (!mActive) return;
  delete mCanvas;mCanvas=0;
  for (int i=0;i<(int)mKeep.size();i++) { delete mKeep[i];}
  mKeep.clear();
  MyClear();
  mState = 0;
}

//______________________________________________________________________________
void StvGrappa::MyClear()
{
  for (int i=0;i<kNObj;i++) {
    for (int j=0;j<kNVal;j++) {
      mPts[i][j].clear();
  } } 
}
//______________________________________________________________________________
int StvGrappa::NObj() const
{
  int n = 0;
  for (int i=0;i<kNObj;i++) {
    for (int j=0;j<kNVal;j++) {
      n+= mPts[i][j].size();
  } } 
 return n;

}
//______________________________________________________________________________
void StvGrappa::MakeCanvas()
{
  if (!mCanvas) {mCanvas = new TCanvas(GetName(),GetName(),600,800);
                 mCanvas->Divide(1,kNVal);}
  else          {mCanvas->Clear();};
}
//______________________________________________________________________________
void StvGrappa::MySize (int iPad, int iX,int iY)
{

   if (iPad==0) {
     for (int i=0;i<kNVal;i++) { mMiMax[i][0]=1e11; mMiMax[i][1]=-1e11;}
     for (int iObj=0;iObj<kNObj;iObj++) {
       int nSize = mPts[iObj][iX].size();
       for (int i=0;i<nSize;i++) {
	 for (int iVal=0;iVal<kNVal;iVal++) {
           float val = mPts[iObj][iVal][i];
           if (mMiMax[iVal][0]>val) mMiMax[iVal][0]=val;
           if (mMiMax[iVal][1]<val) mMiMax[iVal][1]=val;
   } } } }
   TGraph *gra = new TGraph(2,mMiMax[iX],mMiMax[iY]);
   mCanvas->cd(iPad+1);
   gra->Draw("AP");
   mKeep.push_back(gra);
}
//______________________________________________________________________________
void StvGrappa::Show()
{
static const int iXY[kNPad][2] = {{0,1},{2,0},{2,1},{2,3}};
  if (!NObj()) return;
  if (!mActive) return;
  MySort();
  MakeCanvas();
  
  for (int iPad=0;iPad<kNPad;iPad++) {
    int iX = iXY[iPad][0],iY = iXY[iPad][1];
    MySize(iPad,  iX,iY);
//		Nodes
    mOpt = "PL";
//    mOpt = "PC";
    mMarkerColor = kGreen;	//
    mMarkerStyle = kFullDotMedium;
    mMarkerSize = 1;
    MyShow(iPad,kNode,iX,iY);
//		Unused not true hits
    mOpt = "P";
    mMarkerColor = kBlue;	//
    mMarkerStyle = kFullDotMedium; 
    mMarkerSize = 1;
    MyShow(iPad,kHit,iX,iY);
//		Unused true hits
    mOpt = "P";
    mMarkerColor = kRed;	//
    mMarkerStyle = kFullDotMedium; 
    mMarkerSize = 1;
    MyShow(iPad,kThit,iX,iY);
//		Used untrue hits
    mOpt = "P";
    mMarkerColor = kBlue;	//Blue
    mMarkerStyle = kStar; mMarkerSize = 1;
    MyShow(iPad,kHIT,iX,iY);
//		Used True hits
    mOpt = "P";
    mMarkerColor = kRed;	//Red
    mMarkerStyle = kStar; mMarkerSize = 1;
    MyShow(iPad,kTHIT,iX,iY);
//		Helix nodes
    mOpt = "PL";
    mMarkerColor = kYellow;	//
    mMarkerStyle = kFullDotSmall;
    mMarkerSize = 1;
    MyShow(iPad,kHelx,iX,iY);
//		Seed hits
    mOpt = "P";
    mMarkerColor = kBlack;	//
    mMarkerStyle = kMultiply; 
    mMarkerSize = 1;
    MyShow(iPad,kPont,iX,iY);
  }
  MyClear();
  mCanvas->Modified();
  mCanvas->Update();
  while(!gSystem->ProcessEvents()){}; 
  MyClear();
}

//______________________________________________________________________________
void StvGrappa::MyShow(int iPad,int iObj, int iX,int iY)
{

  int n = mPts[iObj][iX].size();
  if (!n) return;
  auto *myGra = new TGraph(n,&mPts[iObj][iX][0],&mPts[iObj][iY][0]);

  myGra->SetMarkerStyle(mMarkerStyle);
  myGra->SetMarkerSize (mMarkerSize );
  myGra->SetMarkerColor(mMarkerColor);
  mCanvas->cd(iPad+1);
  myGra->Draw(mOpt);
  mKeep.push_back(myGra);
}  
//______________________________________________________________________________
void StvGrappa::MySort()
{
//static const double kMicron = 1e-4;
std::vector<int> Idx;
std::vector<float> Buf;
#if 0
//		cleanup same hits
  for (int jObj:{kHIT,kTHIT}) {
    int nj = mPts[jObj][0].size();
    for (int j=0; j<nj;j++) {
//??      int ii = 0;
      for (int iObj:{kHit,kThit}) {
	int ii = 0;
	int ni = mPts[iObj][0].size();
	for (int i = 0; i<ni; i++) {
          int k = 0;
          for (k=0;k<3; k++) {
            if (fabs(mPts[iObj][k][i]-mPts[jObj][k][j])>kMicron) break;
          };
          if (k==3 || ii==i) continue;
          for (int k=0;k<4; k++) {mPts[iObj][k][ii] = mPts[iObj][k][i];}
          ii++;
	}//end i
	for (int k=0;k<4; k++) {mPts[iObj][k].resize(ii);}
      }//end iObj
    }//end j
  }//end jObj
#endif     
#if 0
//	Sorting
  for (int iObj=0;iObj<kNObj;iObj++) {
    int nSize = mPts[iObj][2].size();
    if (!nSize) continue;
    Idx.resize(nSize);
    Buf.resize(nSize);
    TMath::Sort(nSize,&mPts[iObj][2][0],&Idx[0],kTRUE);
    for (int iX=0;iX<4;iX++) {
      auto &arr = mPts[iObj][iX];
      arr.swap(Buf);
      for (int i=0;i<nSize;i++) {arr[i]=Buf[Idx[i]];}
  } }
#endif     
}

//______________________________________________________________________________
void StvGrappa::Show(const StvTrack *tk)
{
  if (mState==0 || mState == 2) Clear();
  mState = 2;
  int qua = 0;
  mTkTruth = tk->GetIdTru(&qua);
  int nSelHits=0;
  for (StvNodeConstIter it=tk->begin();it!=tk->end();++it) {
    const StvNode *node = (*it);
    const StvHit *hit = node->GetHit();
    const double *Xd  = node->GetFP().pos();
    Add(Xd[0],Xd[1],Xd[2],kNode);
    if (!hit) continue;
    if (node->GetXi2()>33) continue;
    nSelHits++;
    const float *Xf  = hit->x();
    StvGrappa_e ee = (mTkTruth && hit->idTru()==mTkTruth)? kTHIT:kHIT;
    Add(Xf[0],Xf[1],Xf[2],ee);
    gem.add(Xf,1);
  }
  printf("Show:nTrkHits=%d\n",nSelHits);
  Show();
}
//______________________________________________________________________________
void StvGrappa::Show(const StvHits *tk,int objType)
{
  if (mState==0 || mState == 2) Clear();
  mState = 2;
  mTkTruth = 0;
  for (int it=0;it<(int)tk->size();++it) {
    const StvHit *hit = (*tk)[it];
    const float *Xf  = hit->x();
    Add(Xf[0],Xf[1],Xf[2],objType);
  }
  Show();
}
//______________________________________________________________________________
double StvGrappa::Dist2(const StvTrack *tk,const float *xhit) const
{
  auto* node = tk->GetNode(StvTrack::kFirstPoint);
  if (!node) return 1e11;
  auto &par = node->GetFP(0);
  int iq = par.getCharge();
  const double *pos = par.pos();
  double mom[3],xyz[3],hit[3]={xhit[0],xhit[1],xhit[2]};
  par.getMom(mom);
  const double *mag = par.mag();
  
  THelix3d helx(iq,pos,mom,mag);
  double s = helx.Path(hit,xyz);
  if (fabs(s)>400) return 400;
//  if (fabs(s)>1e3) return 1e11;
  double dist2 = (TVector3(xyz)-TVector3(hit)).Mag2();
  return dist2;
}
#if 0
//______________________________________________________________________________
double StvGrappa::Dist2(const StvTrack *tk,const float *xhit) const
{
static const double kKEEP = 0.9;

  TVector3 A,B,X(xhit);
  double a=1e11,b=1e11;
  for (StvNodeConstIter it=tk->begin();it!=tk->end();++it) {
    const StvNode *node = (*it);
    TVector3 N(node->GetFP().pos());
    double dis = (X-N).Mag2();
    if (dis < a) {
      b = a; B = A; a = dis; A = N;
    } else if(dis < b) {
      b = dis; B = N;
    }
  }
  double aMbaMb = (A-B).Dot(A-B);
  double aMbbMx = (A-B).Dot(B-X);
  double bMxbMx = (B-X).Dot(B-X);
  double tau = -aMbbMx/aMbaMb;
  if (tau<  -kKEEP) tau =-kKEEP;
  if (tau> 1+kKEEP) tau = 1+kKEEP;
  double dis = aMbaMb*tau*tau +2*aMbbMx*tau + bMxbMx;
  return dis;
}
#endif
//______________________________________________________________________________
double StvGrappa::Dist2(const StvHits *tk,const float *xhit) const
{
enum { kKEEP = 3};
  TVector3 A,B,X(xhit);
  double a=1e11,b=1e11;
  for (int it=0;it<(int)tk->size();++it) {
    const StvHit *hit= (*tk)[it];
    TVector3 N(hit->x());
    double dis = (X-N).Mag2();
    if (dis < a) {
      b = a; B = A; a = dis; A = N;
    } else if(dis < b) {
      b = dis; B = N;
    }
  }
  double aMbaMb = (A-B).Dot(A-B);
  double aMbbMx = (A-B).Dot(B-X);
  double bMxbMx = (B-X).Dot(B-X);
  double tau = -aMbbMx/aMbaMb;
  if (tau<  -kKEEP) tau =-kKEEP;
  if (tau> 1+kKEEP) tau = 1+kKEEP;
  double dis = aMbaMb*tau*tau +2*aMbbMx*tau + bMxbMx;
  return dis;
}
//______________________________________________________________________________
void StvGrappa::MakeLims(const StvTrack *tk,double xMiMax[2][3]) const
{
  enum {kCorrida=20};
//  double Rmax=9,Rmin=1e11;
  for (int i=0;i<3;i++) {
    xMiMax[0][i] =  1e11;    
    xMiMax[1][i] = -1e11;
  }
  
  auto* node = tk->GetNode(StvTrack::kFirstPoint);
  assert(node);
  auto &par = node->GetFP(0);
  int iq = par.getCharge();
  const double *pos = par.pos();
  double mom[3];
  par.getMom(mom);
  const double *mag = par.mag();
  
  THelix3d helx(iq,pos,mom,mag);
  double len00 = helx.Path(0.,0.);
  double x00[3];
  for (double s=len00; 1; s++) {
    helx.Eval(s,x00);
    if (fabs(x00[2]) > 210) 			break;
    if (x00[0]*x00[0]+x00[1]*x00[1]>200*200) 	break;
      for (int i=0;i<3;i++) {
      if (xMiMax[0][i]>x00[i]-kCorrida)  xMiMax[0][i]=x00[i]-kCorrida;  
      if (xMiMax[1][i]<x00[i]+kCorrida)  xMiMax[1][i]=x00[i]+kCorrida;
    }
  }
}
//______________________________________________________________________________
void StvGrappa::MakeLims(const StvHits *tk,double xMiMax[2][3]) const
{

  for (int i=0;i<3;i++) {
    xMiMax[0][i] = 1e11;    
    xMiMax[1][i] =-1e11;
  }
  for (int it=0;it<(int)tk->size();++it) {
    const StvHit *hit = (*tk)[it];
    const float *x = hit->x();
    for (int i=0;i<3;i++) {
      if (xMiMax[0][i]>x[i])  xMiMax[0][i]=x[i];  
      if (xMiMax[1][i]<x[i])  xMiMax[1][i]=x[i];
    }
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
void StvGrappa::Zhow(const StvTrack *tk)
{
enum {kCorrida=20};
  Clear();
  gem.clear();
  mState = 1;
  int qua=0;
  mTkTruth = tk->GetIdTru(&qua);
  const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetAllHits();
  double lims[2][3];
  MakeLims(tk,lims);
  int nTotHits=0,nAccHits=0,nDi2Hits=0;;

  for (int ihit=0;ihit<(int)hitArr->size(); ihit++) {
    const StvHit *hit = (StvHit*)(*hitArr)[ihit];
    nTotHits++;
    TestHit(hit);
    const float *f = hit->x();
    int rej = 0;
    for (int i=0;i<3;i++) {
      if (f[i] <lims[0][i]) {rej = 13; break;};    
      if (f[i] >lims[1][i]) {rej = 13; break;};     
    }
    if (rej) continue;
    nAccHits++;
    double dist2 = Dist2(tk,f);
    if (dist2 > kCorrida*kCorrida) continue;
    nDi2Hits++;
    StvGrappa_e ee = (mTkTruth && hit->idTru()==mTkTruth)? kThit:kHit;
    Add(f[0],f[1],f[2],ee);
    gem.add(f,0);
  }
  printf("ZHOW: nTotHits,nAccHit,nDi2Hits=%d %d %d\n",nTotHits,nAccHits,nDi2Hits);
  Show(tk);
  gem.pri();
//      Show();
}
//______________________________________________________________________________
void StvGrappa::Zhow(const StvHits *tk, int objType)
{
enum {kCorrida=20};
  Clear();
  mState = 2;
  const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetAllHits();
  double lims[2][3];
  MakeLims(tk,lims);
  for (int ihit=0;ihit<(int)hitArr->size(); ihit++) {
    const StvHit *hit = (StvHit*)(*hitArr)[ihit];
    const float *f = hit->x();
    int rej = 0;
    for (int i=0;i<3;i++) {
      if (f[i] <lims[0][i]) {rej = 13; break;};    
      if (f[i] >lims[1][i]) {rej = 13; break;};     
    }
    if (rej) continue;
    double dist2 = Dist2(tk,f);
    if (dist2 > kCorrida*kCorrida) continue;
    
    Add(f[0],f[1],f[2],kHit);
  }
  Show(tk,objType);
}

#include "StEvent/StHit.h"
//______________________________________________________________________________
void StvGrappa::TestHit(const StvHit *hit) const
{
    const float *f = hit->x();
    const StHit *stHit = (const StHit*)hit->stHit();
      
    const StThreeVectorF& pos = stHit->position();
    assert(fabs(f[2]-pos[2])<=0);
}
//______________________________________________________________________________
void gemini::add(const float *xyz,int onTrak) 
{
  auto * v = (onTrak)? &mHitsOn:&mHitsOff;
  for (int i=0;i<3;i++) v->push_back(xyz[i]);
}
//______________________________________________________________________________
int gemini::pri() 
{
static const double delta = 1.;
  std::vector<double> sel;
  int num=0;
  for (int i=0; i<(int)mHitsOff.size();i+=3) {
    const double *a = &mHitsOff[i];
    for (int j=0; j<(int)mHitsOn.size();j+=3) {
      const double *b = &mHitsOn[j];
      double s = 0;
      for (int k=0;k<3;k++) { s+=pow(a[k]-b[k],2); }
      if (s<=0.) continue;
      if (s>delta*delta) continue;

      printf("%d - (%g %g %g) (%g %g %g)\n",num++
            ,a[0],a[1],a[2] ,b[0],b[1],b[2]);
  } }
  return num;
}

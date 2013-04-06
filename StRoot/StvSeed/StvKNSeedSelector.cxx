#include <string.h>
#include <math.h>
#include <assert.h>
#include <map>
#include "StvUtil/StvDebug.h"
#include "StvKNSeedSelector.h"
#include "StvSeedConst.h"

static const float kMaxAng =  9*3.14/180;	//???Maximal angle allowed for connected hits
static const float kMinAng =  1*3.14/180;;	//KN angle allowed
static const float kMaxRatio=   3;		//ratio 
static const float kErrFact=  1./3;		//bigErr/kErrFact/len is angle error

#define Sq(x) ((x)*(x))


//_____________________________________________________________________________
static inline double Ang( const float A[3],const float B[3]) 
{
  double ang = (A[0]-B[0])*(A[0]-B[0])+(A[1]-B[1])*(A[1]-B[1])+(A[2]-B[2])*(A[2]-B[2]);
  ang = sqrt(ang);
  if (ang>0.5) 		{ ang  = 2.*asin(ang/2);}
  else if (ang>0.1) 	{ ang *= (1+ang*ang/24);}
  return ang;
}
//_____________________________________________________________________________
static inline void Ave( float A[3],const float B[3]) 
{
  float nor = 0;
  for (int i=0;i<3;i++) { A[i] += 0.2*B[i]; nor += A[i]*A[i];}
  nor = sqrt(nor);
  for (int i=0;i<3;i++) { A[i]/=nor;}
}
//_____________________________________________________________________________
static inline void Add( float A[3],const float B[3],float len) 
{
float wt=len*len;
 A[0] += B[0]*wt; A[1] += B[1]*wt; A[2] += B[2]*wt;
}
//_____________________________________________________________________________
static inline void Zer( float A[3]) 
{
 A[0] =0; A[1] = 0; A[2] =0;
}
//_____________________________________________________________________________
static inline double Nor(float A[3]) 
{
   double sum = Sq(A[0])+Sq(A[1])+Sq(A[2]);
   sum = sqrt(sum);
   A[0]/=sum;A[1]/=sum;A[2]/=sum;
   return sum;
}
//_____________________________________________________________________________
static inline float D0t(const  float A[3],const float B[3]) 
{
 return A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}

//_____________________________________________________________________________
void StvKNSeedSelector::Insert( int iA,int iB,float dis) 
{
   float *a = mAux[iA].mDist;
   int   *n = mAux[iA].mNbor;
   if (dis >= a[kKNumber-1]) return;
   int jk=0;
   for (;jk<kKNumber;jk++) 			{if (dis < a[jk]) break;}
   for (int jj=kKNumber-2; jj>=jk;jj--) 	{a[jj+1] = a[jj]; n[jj+1]=n[jj];}       
   a[jk]=dis; n[jk] = iB;
}
//_____________________________________________________________________________
StvKNSeedSelector::StvKNSeedSelector()
{ 
  mStartHit = 0;
}
//_____________________________________________________________________________
/// Reset first hit position
/// @param float startPos[3] - position of starting hit
/// @param void *startHit    - addres of hit. Format of hit is not used there

void StvKNSeedSelector::Reset(const float startPos[3], void *startHit)		
{
  mState = 0;
  mAux.clear();
  mSel.clear();
  mStartHit = startHit;
  memcpy(mStartPos,startPos,sizeof(mStartPos));
  mKNNDist = 1e11;
  mMinIdx = -1;
  mStartRad = sqrt(Sq(mStartPos[0])+Sq(mStartPos[1]));
  mErr = SEED_ERR(mStartRad)*kErrFact;
  
}
//_____________________________________________________________________________
/// Add new hit to selector
/// @param float pos[3] - position of starting hit
/// @param void *voidHit    - addres of hit. Format of hit is not used there

void  StvKNSeedSelector::Add(const float pos[3],void *voidHit)
{
  int last = mAux.size();
  mAux.resize(last+1);
  StvKNAux &aux = mAux.back(); 
  aux.mHit = voidHit; 
  float *myDir=aux.mDir,nor=0;
  for (int i=0;i<3;i++) {myDir[i] = pos[i]-mStartPos[i];nor+= myDir[i]*myDir[i];}
  nor = sqrt(nor);
  for (int i=0;i<3;i++) {myDir[i]/=nor;}
  aux.mLen = nor;aux.mSel = 0;
}  
//_____________________________________________________________________________
void  StvKNSeedSelector::Relink()
{

  for (int i1=0;i1<(int)mAux.size();i1++) {
    if (!mAux[i1].mHit) continue;
    if ( mAux[i1].mSel) continue;
    mAux[i1].Reset(); mAux[i1].mSel=0; 
    for (int i2=0;i2<i1;i2++) {
      if (!mAux[i2].mHit) continue;
      if ( mAux[i2].mSel) continue;
      Update(i1,i2);
  } }

}  
//_____________________________________________________________________________
int StvKNSeedSelector::Select()
{
static int nCall=0; nCall++;
  while (1) {
    mSel.clear();
    mMapLen.clear();
    Relink();
    mKNNDist = kMaxAng;
///		Find most dense place
    mMinIdx  = -1;
    for (int i=0;i<(int)mAux.size();i++) 
    { 
      if (!mAux[i].mHit) continue;		//ignore discarded hit
      if ( mAux[i].mSel) continue;		//ignore used hit
      float qwe = mAux[i].mDist[kKNumber-1];
      if (qwe>=mKNNDist) continue;
      mKNNDist=qwe; mMinIdx = i;
    }
    if (mMinIdx<0) return 0;
    if (mKNNDist > kMinAng*kMaxRatio) return 0;	
    memcpy(mAveDir,mAux[mMinIdx].mDir,sizeof(mAveDir));



  ///		define the best direction
    std::map<float,int>::iterator myIt;
    mNHits=0; Zer(mAveDir);
    Pass(mMinIdx,mKNNDist);
    double wid = Width();
    if (mKNNDist*wid > kMinAng) return 0;	

double ei0 = sqrt(mEigen[0])*57;
double ei1 = sqrt(mEigen[1])*57;
StvDebug::Count("KNNDis",mKNNDist *57);
StvDebug::Count("MinEig",sqrt(mEigen[0])*57);
StvDebug::Count("KNNDis:MinEig" ,ei0,mKNNDist*57);
StvDebug::Count("MaxEig:MinEig",ei0,ei1);

    return mSel.size();
  }//end while
  return 0;
}
//_____________________________________________________________________________
void StvKNSeedSelector::Pass(int iux, double accuAng)
{
  StvKNAux &aux = mAux[iux];
  aux.mSel=1; mNHits++;
  mMapLen[aux.mLen]=iux;
  for (int ifan=0;ifan<kKNumber;ifan++) {
    if (aux.mDist[ifan]>accuAng)	continue;
    int idx = aux.mNbor[ifan];
    if (mAux[idx].mSel) 		continue; 
    Pass(idx,accuAng);
  }
}
//_____________________________________________________________________________
void StvKNSeedSelector::Update(int ia,int ib)
{

  float *aDir = mAux[ia].mDir;
  float *bDir = mAux[ib].mDir;

  double dis = Ang(aDir,bDir);
  if (dis>kMaxAng) return;
  Insert(ia,ib,dis);
  Insert(ib,ia,dis);
}  
#include "TVector3.h"
//_____________________________________________________________________________
double StvKNSeedSelector::Width() 
{
  if (fabs(mAveDir[2])>=0.99)	return 101;
  if (mMapLen.size()<3) 	return 102;

  TVector3 myX(mAveDir),myZ(myX.Orthogonal()),myY(myZ.Cross(myX));		
  double G[3]={0},uv[2]={0};
  std::map<float,int>::iterator myIt;

  for( myIt=mMapLen.begin(); myIt!=mMapLen.end();++myIt)
  {
    int i = (*myIt).second;
    TVector3 dir(mAux[i].mDir);
    double u = myY.Dot(dir);
    double v = myZ.Dot(dir);
    uv[0]+=u;uv[1]+=v;
    G[0]+=u*u;G[1]+=u*v;G[2]+=v*v;
  }
  double dN = mMapLen.size();
  G[0]/=dN;G[1]/=dN;G[2]/=dN; uv[0]/=dN;uv[1]/=dN;
  G[0]-=uv[0]*uv[0];G[1]-=uv[0]*uv[1];G[0]-=uv[1]*uv[1];

  double bb = 0.5*(G[0]+G[2]);
  double cc = G[0]*G[2]-G[1]*G[1];
  double dis = bb*bb - cc*cc; if (dis<0) dis = 0;
  mEigen[0] = cc/(bb+sqrt(dis));
  mEigen[1] = G[0]+G[2]-mEigen[0];
  return sqrt(mEigen[0]/(mEigen[1]+1e-11));

}
#if 1
#include "TSystem.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "Stv/StvDraw.h"
class StvHit;
//______________________________________________________________________________
void StvKNSeedSelector::Show() const
{
static TCanvas *myCanvas = 0;
static TGraph  *szGraph  = 0;
static TGraph  *ptGraph  = 0;
static TGraph  *slGraph  = 0;
  std::vector<double> x,y,X,Y;
  double reg[2]={999,-999};

  TVector3 mainDir(mStartPos);mainDir*=(-1.);
  mainDir = mainDir.Unit();
  TVector3 myX(mainDir),myY(myX.Orthogonal()),myZ(myX.Cross(myY));		
  
  int nPts = mAux.size();
  for (int ia =0;ia<nPts;ia++) {
    if (!mAux[ia].mHit) continue;
    TVector3 hit(mAux[ia].mDir);
    double uu = myY.Dot(hit);
    double vv = myZ.Dot(hit);
    double v = asin(vv);      
    double u = asin(uu/cos(v));
    u=u/M_PI*180;
    v=v/M_PI*180;
    if (reg[0]>u) reg[0]=u;if (reg[0]>v) reg[0]=v;
    if (reg[1]<u) reg[1]=u;if (reg[1]<v) reg[1]=v;
    if (mAux[ia].mSel) {X.push_back(u);Y.push_back(v);}
    else               {x.push_back(u);y.push_back(v);}
  }

  if(!myCanvas) myCanvas = new TCanvas("KNSelector_Show","",600,800);
  myCanvas->Clear();
  delete szGraph; szGraph=0;
  delete ptGraph; ptGraph=0; 
  delete slGraph; slGraph=0;

//		Define the scene
  szGraph = new TGraph(2, reg, reg);
  szGraph->SetMarkerColor(kYellow);
  szGraph->Draw("AP");

  if (x.size()) {
    ptGraph  = new TGraph(x.size(), &x[0], &y[0]);
    ptGraph->SetMarkerColor(kGreen);
    ptGraph->Draw("Same *");}
  if (X.size()) {
    slGraph  = new TGraph(X.size(), &X[0], &Y[0]);
    slGraph->SetMarkerColor(kRed);
    slGraph->Draw("Same *");}

static StvDraw *myDraw = new StvDraw;
  myDraw->Clear();
  myDraw->Hits((const std::vector<StvHit*>&)Get(),kUsedHit);

  std::vector<StvHit*> myUnused;
  for (int iux=0;iux<(int)mAux.size();iux++) {
    if (!mAux[iux].mHit) continue;
    if ( mAux[iux].mSel) continue;
    myUnused.push_back((StvHit*)mAux[iux].mHit);
  }
  myDraw->Hits(myUnused,kUnusedHit);


  myDraw->UpdateModified();

  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
#endif //Show

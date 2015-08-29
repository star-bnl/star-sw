#include <string.h>
#include <math.h>
#include <assert.h>
#include <map>
#include "StvUtil/StvDebug.h"
#include "StvKNSeedSelector.h"
#include "Stv/StvHit.h"

#include "StvSeedConst.h"
//static const float kMaxAng =  9*3.14/180;	//???Maximal angle allowed for connected hits
static const float kMaxAng =  15*3.14/180;	//???Maximal angle allowed for connected hits
static const float kSinHalfAng = sin(kMaxAng/2);

//static const float kMinAng =  1*3.14/180;;	//KN angle allowed
//static const float kMinAng =  2*3.14/180;;	//KN angle allowed
//static const float kMinAng =  9*3.14/180;;	//KN angle allowed
static const float kMinAng =  5*3.14/180;;	//KN angle allowed
//static const float kMinAng =  3*3.14/180;;	//KN angle allowed
//static const float kMinAng =  12*3.14/180;;	//KN angle allowed

//static const float kDisRatio=   1.0;		//ratio for KNN distance
//static const float kDisRatio=   0.7;		//ratio for KNN distance
//static const float kDisRatio=   0.6;		//ratio for KNN distance
//static const float kDisRatio=   0.8;		//ratio for KNN distance
//static const float kDisRatio=   0.9;		//ratio for KNN distance
static const float kDisRatio=   0.8;		//ratio for KNN distance

static const float kErrFact=  1./3;		//bigErr/kErrFact/len is angle error

enum { kNumTheDiv = 20 };			//number of divisions in theta
static float kStpTheDiv = M_PI/kNumTheDiv;	//step in theta map

#define Sq(x) ((x)*(x))


//_____________________________________________________________________________
static inline double Ang( const float A[3],const float B[3]) 
{
  double cang = ((A[0]-B[0])*A[0]+(A[1]-B[1])*A[1]+(A[2]-B[2])*A[2]);
assert(cang>-0.01);
  if (cang<0) cang = 0;
  double ang = 2*sqrt(cang/2);
       if (ang>1.99) { ang = M_PI;}
  else if (ang>0.1 ) { ang = 2.*asin(ang/2); }
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
   if (mKNNDist < a[kKNumber-1]) return;
   mKNNDist = a[kKNumber-1];
   mMinIdx = iA;
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
  mTheDiv.clear();
  mStartHit = startHit;
  memcpy(mStartPos,startPos,sizeof(mStartPos));
  mKNNDist = 1e11;
  mMinIdx = -1;
  mStartRad = sqrt(Sq(mStartPos[0])+Sq(mStartPos[1]));
  mErr = SEED_ERR(mStartRad)*kErrFact;
  Zer(mAveDir);  
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
  aux.mPhi = atan2(myDir[1],myDir[0]);
  aux.mCosThe = sqrt(myDir[0]*myDir[0]+myDir[1]*myDir[1]);
  aux.mThe = asin(myDir[2]);
  aux.mLen = nor;aux.mSel = 0;
  float iThe = floor(aux.mThe/kStpTheDiv)*kStpTheDiv;
  mTheDiv[iThe].insert(std::pair<float,int>(aux.mPhi,last));
  float myMaxPhi = kSinHalfAng/aux.mCosThe;
        myMaxPhi = (myMaxPhi<0.99)? 2*asin(myMaxPhi): M_PI;
  if (aux.mPhi-myMaxPhi<-M_PI) {
    last = mAux.size(); mAux.resize(last+1); mAux.back() = aux;
    mAux.back().mPhi = aux.mPhi+2*M_PI;
    mTheDiv[iThe].insert(std::pair<float,int>(mAux.back().mPhi,last));
  }
  if (aux.mPhi+myMaxPhi> M_PI) {
    last = mAux.size();  mAux.resize(last+1); mAux.back() = aux;
    mAux.back().mPhi = aux.mPhi-2*M_PI;
    mTheDiv[iThe].insert(std::pair<float,int>(mAux.back().mPhi,last));
  }
}  
#ifdef KNNMAP
//_____________________________________________________________________________
void  StvKNSeedSelector::Relink()
{
  mKNNDist = kMaxAng;
  mMinIdx = -1;
  MyTheDiv::iterator it2The = mTheDiv.begin();
  for (MyTheDiv::iterator it1The = mTheDiv.begin();
       it1The != mTheDiv.end();++it1The) 	//Main loop over theta	
  {
    float the1 = (*it1The).first;
    for (;it2The!=mTheDiv.end();++it2The) {
      float the2 = (*it2The).first;
      if (the2>the1-kMaxAng) break;
    }
    if (it2The==mTheDiv.end()) continue;


    MyPhiDiv &my1PhiDiv = (*it1The).second;	//Main phi loop
    for (MyPhiDiv::iterator it1Phi=my1PhiDiv.begin();
         it1Phi !=  my1PhiDiv.end();  
         ++it1Phi) 					{//Main loop over phi
      int i1 = (*it1Phi).second;
      StvKNAux &aux1 = mAux[i1];
      float myMaxPhi = kSinHalfAng/aux1.mCosThe;
      myMaxPhi = (myMaxPhi<0.99)? 2*asin(myMaxPhi): M_PI;
		// Secondary theta loop
      for (MyTheDiv::iterator it3The=it2The;it3The!=mTheDiv.end();++it3The) 
      {
	if ((*it3The).first>aux1.mThe+kStpTheDiv+kMaxAng) break;
	MyPhiDiv &my3PhiDiv = (*it3The).second;

//	for (MyPhiDiv::iterator it3Phi = my3PhiDiv.begin();
	for (MyPhiDiv::iterator it3Phi = my3PhiDiv.lower_bound(aux1.mPhi-myMaxPhi);
             it3Phi !=my3PhiDiv.end();
	     ++it3Phi) { 		//loop over Phi partner
          if ((*it3Phi).first > aux1.mPhi+myMaxPhi) break;
          int i2 = (*it3Phi).second;
          if (i2>=i1) continue;
          StvKNAux & aux2 = mAux[i2];
          if (!aux2.mHit) 	continue;
          if ( aux2.mSel) 	continue;
          if (fabs(aux1.mThe-aux2.mThe) > kMaxAng) continue;
          if ( Ang(aux1.mDir,aux2.mDir) > kMaxAng) continue;
          Update(i1,i2);
	}//end of it3Phi loop
      }// end of it3The loop
    }//End of main Phi loop

  }//End of main theta loop
}  
#endif
#ifndef KNNMAP
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
#endif

//_____________________________________________________________________________
int StvKNSeedSelector::Select()
{
static int nCall=0; nCall++;
  while (1) {
    mSel.clear();
    mMapLen.clear();
    Relink();
//     mKNNDist = kMaxAng;
// ///		Find most dense place
//     mMinIdx  = -1;
//     for (int i=0;i<(int)mAux.size();i++) 
//     { 
//       if (!mAux[i].mHit) continue;		//ignore discarded hit
//       if ( mAux[i].mSel) continue;		//ignore used hit
//       float qwe = mAux[i].mDist[kKNumber-1];
//       if (qwe>=mKNNDist) continue;
//       mKNNDist=qwe; mMinIdx = i;
//     }
    if (mMinIdx<0) return 0;


  ///		define the best direction
    memcpy(mAveDir,mAux[mMinIdx].mDir,sizeof(mAveDir));
    assert(fabs(mAveDir[0])+fabs(mAveDir[1])+fabs(mAveDir[2])>0.1);


    mNHits=0; 
    Pass(mMinIdx,mKNNDist*kDisRatio);
    double wid = Width();
    if (mKNNDist*wid > kMinAng) return 0;	
 	 
    const void *hpPre = 0;
    mSel.push_back(mStartHit); 	 
    std::map<float,int>::iterator myIt;
    int iPre = 0;

    for (myIt = mMapLen.begin(); myIt !=mMapLen.end(); ++myIt) 
    { 	 
      int i = (*myIt).second; 	 
      const void *hp = ((StvHit*)(mAux[i].mHit))->detector();
      if (hpPre != hp ) {
         mSel.push_back(mAux[i].mHit);
	 iPre = i;hpPre = hp;
	 continue;
      }
      //Hit from the same detector
      float ang0 =  Ang(mAveDir, mAux[iPre].mDir);   
      float ang1 =  Ang(mAveDir, mAux[i   ].mDir);   
      if (ang0<ang1) continue;  //Old friend is better
      mSel.back() = mAux[i].mHit;
      iPre = i; hpPre = hp;
    } 	 
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
static int nCall = 0; nCall++;
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
  G[0]-=uv[0]*uv[0];G[1]-=uv[0]*uv[1];G[2]-=uv[1]*uv[1];

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

#if 0
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
#endif
  myCanvas->Modified();
  myCanvas->Update();
  while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 

}
#endif //Show

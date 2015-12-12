#include <string.h>
#include <math.h>
#include <assert.h>
#include <map>
#include "StvUtil/StvDebug.h"
#include "StvKNSeedSelector.h"
#include "Stv/StvHit.h"

#include "TStopwatch.h"


#include "StvSeedConst.h"
//static const float kMaxDis =  9*3.14/180;	//???Maximal angle allowed for connected hits
//static const float kMaxDis =  15*3.14/180;	//???Maximal angle allowed for connected hits
static const float kMaxDis =  30*3.14/180;	//???Maximal angle allowed for connected hits
static const float kMaxLam = (M_PI-kMaxDis)/2;
static const float kSinDis = sin(kMaxDis/2);

//static const float kMinDis =  1*3.14/180;;	//KN angle allowed
//static const float kMinDis =  2*3.14/180;;	//KN angle allowed
//static const float kMinDis =  9*3.14/180;;	//KN angle allowed
static const float kMinDis =  5*3.14/180;;	//KN angle allowed
//static const float kMinDis =  3*3.14/180;;	//KN angle allowed
//static const float kMinDis =  12*3.14/180;;	//KN angle allowed

//static const float kDisRatio=   1.0;		//ratio for KNN distance
//static const float kDisRatio=   0.7;		//ratio for KNN distance
//static const float kDisRatio=   0.6;		//ratio for KNN distance
//static const float kDisRatio=   0.8;		//ratio for KNN distance
//static const float kDisRatio=   0.9;		//ratio for KNN distance
//static const float kDisRatio=   1.5;		//ratio for KNN distance
static const float kDisRatio=   3.;		//ratio for KNN distance

static const float kErrFact=  1./3;		//bigErr/kErrFact/len is angle error

//enum { kNumTheDiv = 20 };			//number of divisions in theta
enum { kNumTheDiv = 40 };			//number of divisions in theta
static float kStpTheDiv = M_PI/kNumTheDiv;	//step in theta map

TStopwatch SW;


#define Sq(x) ((x)*(x))
//_____________________________________________________________________________
//_____________________________________________________________________________
static void Eigen2(const double err[3], float lam[2], float eig[2][2])
{

  double spur = err[0]+err[2];
  double det  = err[0]*err[2]-err[1]*err[1];
  double dis  = spur*spur-4*det;
  if (dis<0) dis = 0;
  dis = sqrt(dis);
  lam[0] = 0.5*(spur+dis);
  lam[1] = 0.5*(spur-dis);
  if (!eig) return;
  eig[0][0] = 1; eig[0][1]=0;
  if (dis>1e-6*spur) {// eigenvalues are different
    if (fabs(err[0]-lam[0])>fabs(err[2]-lam[0])) {
     eig[0][1] = 1; eig[0][0]= -err[1]/(err[0]-lam[0]);
    } else {
     eig[0][0] = 1; eig[0][1]= -err[1]/(err[2]-lam[0]);
    }
    double tmp = sqrt(eig[0][0]*eig[0][0]+eig[0][1]*eig[0][1]);
    eig[0][0]/=tmp; eig[0][1]/=tmp;
  }
  eig[1][0]=-eig[0][1];  eig[1][1]= eig[0][0];
}


//_____________________________________________________________________________
static inline double Ang( const float A[3],const float B[3]) 
{
  double cang = ((A[0]-B[0])*A[0]+(A[1]-B[1])*A[1]+(A[2]-B[2])*A[2]);
assert(cang>-0.01);
  if (cang<0) cang = 0;
  double ang = 2*sqrt(cang/2);
       if (ang>1.99) { ang = M_PI;}
  else if (ang>1.0 ) { ang = 2.*asin(ang/2); }
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
   auto *detB = mAux[iB].mDet;
   int jk = 0,jj=0;
   for (jk=0;jk<kKNumber;jk++) {
     if (n[jk]<0) 	break;
     auto *detJ = mAux[n[jk]].mDet;
     if (detB != detJ) 	continue;
     if (dis>a[jk]) 	return;
     for (jj=jk+1;jj<kKNumber;jj++) {
       if (n[jj]<0) 	break;
       n[jj-1]=n[jj]; a[jj-1] = a[jj];
     }
     n[jj]=-99; a[jj]=1e11;
     break;
   }
   for (jk=0;jk<kKNumber;jk++) {
     if (n[jk]<0) 	break;
     if (dis < a[jk])	break;
   }
   if (n[jk]>=0) {
     for (jj=kKNumber-2; jj>=jk;jj--) 	{a[jj+1] = a[jj]; n[jj+1]=n[jj];}       
   }
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
#ifdef KNNMAP2
  mTheMap.clear();
#endif
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

void  StvKNSeedSelector::Add(const float pos[3],void *voidHit,const void *voidDet)
{
  int last = mAux.size();
  mAux.resize(last+1);
  auto &aux = mAux.back(); 
  aux.mHit = voidHit; 
  aux.mDet = voidDet; 
  float *myDir=aux.mDir,nor=0;
  for (int i=0;i<3;i++) {myDir[i] = pos[i]-mStartPos[i];nor+= myDir[i]*myDir[i];}
  nor = sqrt(nor);
  for (int i=0;i<3;i++) {myDir[i]/=nor;}
  aux.mPhi = atan2(myDir[1],myDir[0]);
  aux.mCosThe = sqrt((1-myDir[2])*(1+myDir[2]));
  if (aux.mCosThe<=1e-6) { mAux.resize(last); return;}
  aux.mThe = asin(myDir[2]);
  aux.mLen = nor;aux.mSel = 0;
#ifdef KNNMAP1
  float iThe = floor(aux.mThe/kStpTheDiv)*kStpTheDiv;

  mTheDiv[iThe].insert(std::pair<float,int>(aux.mPhi,last));

  float  myMaxPhi = 2*kSinDis/aux.mCosThe;
  if      (myMaxPhi>=2*0.99) 	{myMaxPhi = M_PI;}
  else if (myMaxPhi> 2*0.50) 	{myMaxPhi = 2*asin(myMaxPhi/2);}

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
#endif
#ifdef KNNMAP2
  mTheMap.insert(std::pair<float,int>(aux.mThe,last));
#endif
}  
#ifdef KNNMAP0
//_____________________________________________________________________________
void  StvKNSeedSelector::Relink()
{
  mMinIdx = -1;
  mKNNDist = kMaxDis;
  for (int i1=0;i1<(int)mAux.size();i1++) {
//    if ( mAux[i1].mSel) continue;
    mAux[i1].Reset();  
    for (int i2=0;i2<i1;i2++) {
//      if ( mAux[i2].mSel) continue;
      if (fabs(mAux[i1].mThe-mAux[i2].mThe)>mKNNDist)	continue;
      float dang = fabs(mAux[i1].mPhi-mAux[i2].mPhi);
      if (dang > M_PI) dang -= 2*M_PI;
      if (fabs(dang)*mAux[i1].mCosThe>mKNNDist) 		continue;
      Update(i1,i2);
  } }
}
#endif
#ifdef KNNMAP1
//_____________________________________________________________________________
void  StvKNSeedSelector::Relink()
{
  mMinIdx = -1;
  mKNNDist = kMaxDis;
  MyTheDiv::iterator it2The = mTheDiv.begin();
  for (MyTheDiv::iterator it1The = mTheDiv.begin();
       it1The != mTheDiv.end();++it1The) 	//Main loop over theta	
  {
    float the1 = (*it1The).first;
    for (;it2The!=mTheDiv.end();++it2The) {
      float the2 = (*it2The).first;
      if (the2>=the1-mKNNDist) break;
    }
    if (it2The==mTheDiv.end()) continue;


    MyPhiDiv &my1PhiDiv = (*it1The).second;	//Main phi loop
    for (MyPhiDiv::iterator it1Phi=my1PhiDiv.begin();
         it1Phi !=  my1PhiDiv.end();  
         ++it1Phi) 					{//Main loop over phi
      int i1 = (*it1Phi).second;
      StvKNAux &aux1 = mAux[i1];
      void* hit1 = aux1.mHit;
//      if (aux1.mSel) 	continue;
		// Secondary theta loop
      for (MyTheDiv::iterator it3The=it2The;it3The!=mTheDiv.end();++it3The) 
      {
        float the3 = (*it3The).first;
	if (the3>aux1.mThe) break;
	MyPhiDiv &my3PhiDiv = (*it3The).second;
  
        float  myMaxPhi = 2*kSinDis/aux1.mCosThe;
        if (myMaxPhi>=2) 		{myMaxPhi = M_PI;}
        else if (myMaxPhi> 0.5) 	{myMaxPhi = 2*asin(myMaxPhi/2);}
        int n = my3PhiDiv.size();
        MyPhiDiv::iterator it3Phi = (myMaxPhi<M_PI && n>5)? my3PhiDiv.lower_bound(aux1.mPhi-myMaxPhi)
	                                                  : my3PhiDiv.begin();
	
	for (;it3Phi !=my3PhiDiv.end(); ++it3Phi) { 		//loop over Phi partner
          if ((*it3Phi).first > aux1.mPhi) break;
          int i3 = (*it3Phi).second;
          if (i3==i1) continue;
          StvKNAux & aux3 = mAux[i3];
//          if ( aux3.mSel) 	continue;
          if ( aux3.mHit==hit1) continue;
          if (fabs(aux1.mThe-aux3.mThe) > mKNNDist) 	continue;
          float dang = fabs(aux1.mPhi-aux3.mPhi);
          if (dang > M_PI) dang -= 2*M_PI;
          if (fabs(dang)*aux1.mCosThe>mKNNDist) 		continue;
          Update(i1,i3);
	}//end of it3Phi loop
      }// end of it3The loop
    }//End of main Phi loop

  }//End of main theta loop
}  
#endif
#ifdef KNNMAP2
//_____________________________________________________________________________
void  StvKNSeedSelector::Relink()
{
  mMinIdx = -1;
  mKNNDist = kMaxDis;
  auto  it2The = mTheMap.begin();
  for (auto it1The = mTheMap.begin();
       it1The != mTheMap.end();++it1The) 	//Main loop over theta	
  {
    float the1 = (*it1The).first;
    int     i1 = (*it1The).second;
    auto &aux1 = mAux[i1];
//    if ( aux1.mSel) 	continue;
    for (;it2The!=mTheMap.end();++it2The) {
      float the2 = (*it2The).first;
//    if (the2>=the1-kMaxDis) break;
      if (the2>=the1-mKNNDist) break;
    }
    if (it2The==mTheMap.end()) continue;
    for (auto it3The=it2The;it3The!=mTheMap.end();++it3The) 
    {
      float the3 = (*it3The).first;
      if (the3>the1) 	break;
      int i3 = (*it3The).second;
      if (i1 == i3) 			continue;
//    if (fabs(the3-the1)>kMaxDis) 	continue;
      if (fabs(the3-the1)>mKNNDist) 	continue;
      auto &aux3 = mAux[i3];
//      if ( aux3.mSel) 	continue;
      float dang = fabs(aux1.mPhi-aux3.mPhi);
      if (dang > M_PI) dang -= 2*M_PI;
//    if (fabs(dang)*aux1.mCosThe>kMaxDis) 	continue;
      if (fabs(dang)*aux1.mCosThe>mKNNDist) 	continue;
      Update(i1,i3);
    }// end of it3The loop

  }//End of main theta loop
}  
#endif
    
    
//_____________________________________________________________________________
int StvKNSeedSelector::Select()
{
static int nCall=0; nCall++;
  while (1) {
    mSel.clear();
    mMapLen.clear();
//    int n = mAux.size();
// #ifdef KNNMAP0
// const char *tit = "RelinkTimeMap0";
// #endif
// #ifdef KNNMAP1
// const char *tit = "RelinkTimeMap1";
// #endif
// 
// #ifdef KNNMAP2
// const char *tit = "RelinkTimeMap2";
// #endif
// 
// 
// SW.Start();
// for (int jk=0;jk<100;jk++) {
// for (int i=0;i<n;i++) {
//     mAux[i].mDist[0]=1e11; mAux[i].mDist[1]=1e11; mAux[i].mDist[2]=1e11; mAux[i].mDist[3]=1e11;
//     mAux[i].mNbor[0]=  -1; mAux[i].mNbor[1]=  -1; mAux[i].mNbor[2]=  -1; mAux[i].mNbor[3]=  -1;
// }
// 
     Relink();
// }
// SW.Stop();
// 
// StvDebug::Count(tit,mAux.size(),SW.CpuTime());
    if (mMinIdx<0) return 0;

  ///		define the best direction
    memcpy(mAveDir,mAux[mMinIdx].mDir,sizeof(mAveDir));
    assert(fabs(mAveDir[0])+fabs(mAveDir[1])+fabs(mAveDir[2])>0.1);


    mNHits=0; 
    Pass(mMinIdx,mKNNDist*kDisRatio);
    double wid = Width();
    if (mKNNDist*wid > kMinDis) return 0;	
 	 
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
    if (aux.mDist[ifan]>accuAng)	break;	// the rest even bigger
    int idx = aux.mNbor[ifan];
    if (mAux[idx].mSel) 		continue; 
    Pass(idx,accuAng);
  }
}
//_____________________________________________________________________________
void StvKNSeedSelector::Update(int ia,int ib)
{

  if (mAux[ia].mDet == mAux[ib].mDet) return;
  float *aDir = mAux[ia].mDir;
  float *bDir = mAux[ib].mDir;

  double dis = Ang(aDir,bDir);
//if (dis>kMaxDis) return;
  if (dis>mKNNDist) return;
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
  Eigen2(G,mEigen,0);
  return sqrt(mEigen[1]/(mEigen[0]+1e-11));

}

//______________________________________________________________________________
int StvKNSeedSelector::Zelect()
{


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
  std::vector<double> X[3],Y[3];
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
    int k = (mAux[ia].mSel&3); if (k>2) k=2;
    X[k].push_back(u);Y[k].push_back(v);
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
  int color[3]={kGreen,kRed,kBlue};
  for (int k=0;k<3;k++) {
    if (!X[k].size()) continue;
    ptGraph  = new TGraph(X[k].size(), &X[k][0], &Y[k][0]);
    ptGraph->SetMarkerColor(color[k]);
    ptGraph->Draw("Same *");
  }

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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TCernLib.h"
#include "StiHitErrCalculator.h"
#include "StiNodePars.h"
#include "StiHit.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

ClassImp(StiHitErrCalculator)
//______________________________________________________________________________
StiHitErrCalculator::StiHitErrCalculator(const char *name):TNamed(name,"")
{
  mNPar = 0;
  memset(mPar,0,sizeof(mPar));
}
//______________________________________________________________________________
void StiHitErrCalculator::SetPars(const double *par,int nPar)
{
  assert(nPar<=kMaxPars);
  mNPar = nPar;
  memcpy(mPar,par, mNPar*sizeof(*mPar));
}
//______________________________________________________________________________
void StiHitErrCalculator::CalcDetErrs(const StiHit *hit, const StiNodePars *node
                                     ,StiHitErrs *hrr)
{
/// Calculate hit error matrix in local detector system. In this system
/// detector plane is x = const 
  const Mtx33F_t *hD=0;
  if (hit) {
    const StHitPlane *hp = hit->detector(); hD = &hp->GetDir(hit->x_g());
  } else   {
    const static float myDir[3][3]={{1,0,0},{0,1,0},{0,0,1}};hD = &myDir;
  }

  float Nt[3],nor;
  nor = sqrt(1.+node->_tanl*node->_tanl);
//		Nt == track direction in Hit frame
  for (int j=0;j<3;j++) {
    Nt[j] = ((*hD)[j][0]*node->_cosCA+(*hD)[j][1]*node->_sinCA+(*hD)[j][2]*node->_tanl)/nor;
  }
//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = Nt[2],mCl = sqrt((1-mSl)*(1+mSl));
  mSp = Nt[1]/mCl, mCp = Nt[0]/mCl;
  hrr->hYY = (mPar[kThkDet]*mSp*mSp   + mPar[kWidTrk])
           / (mCp*mCp);
  hrr->hZZ = (mPar[kThkDet]*(mSl*mSl) + mPar[kWidTrk]*((mSp*mSl)*(mSp*mSl)+mCp*mCp))
           / ((mCp*mCl)*(mCp*mCl));
  hrr->hYZ = (mPar[kThkDet]           + mPar[kWidTrk])*(mSp*mSl)/(mCp*mCp*mCl);

  hrr->hYY+= mPar[kYErr];
  hrr->hZZ+= mPar[kZErr];
}  
//______________________________________________________________________________
void StiHitErrCalculator::CalcDCAErrs(const StiHit *hit, const StiNodePars *node
                                     ,StiHitErrs *hrr)
{
/// Calculate hit error matrix in DCA  system. In this system
/// track is along x axis, Y axis comes thru hit point 
  const Mtx33F_t *hD=0;
  if (hit) {// real case
    const StHitPlane *hp = hit->detector();
    hD = &hp->GetDir(hit->x_g());
  } else   { //test case hit==0
    const static float myDir[3][3]={{1,0,0},{0,1,0},{0,0,1}};
    hD = &myDir;
  }

//const double *trkPosG = &node->_x;
  float cosP = node->_cosCA,sinP = node->_sinCA,tanL = node->_tanl;
  float cosL = 1./sqrt(1.+tanL*tanL),sinL=tanL*cosL;
  float NG[3][3] = {{ cosL*cosP, cosL*sinP, sinL}
                   ,{     -sinP,      cosP,    0}
		   ,{-sinL*cosP,-sinL*sinP, cosL}};
  float *NtG = NG[0];		   
  float NL[3][3], *Nt=NL[0],*Np=NL[1],*Nl=NL[2];
  for (int j=0;j<3;j++) {
    Nt[j] = ((*hD)[j][0]*NtG[0]+(*hD)[j][1]*NtG[1]+(*hD)[j][2]*NtG[2]);}

//		Nt = (cos(Lam)*cos(Phi),cos(Lam)*sin(Phi),sin(Lam))
  mSl = Nt[2],mCl = sqrt((1-mSl)*(1+mSl));
  mSp = Nt[1]/mCl, mCp = Nt[0]/mCl;
  Np[0]=-mSp;     Np[1]=mCp;     Np[2]=0;
  Nl[0]=-mSl*mCp; Nl[1]=-mSl*mSp;Nl[2]=mCl;

  float tmp[2][3],T[2][2];
  TCL::mxmpy1((*hD)[0],NL[1] ,tmp[0],3,3,2);
  TCL::mxmpy (NG[1],tmp[0],T[0]  ,2,3,2);
  float g[3],G[3];

  g[0] = mPar[kThkDet]*mSp*mSp 	       + mPar[kWidTrk] + mPar[kYErr]*mCp*mCp;
  g[2] = mPar[kThkDet]*mCp*mCp*mSl*mSl + mPar[kWidTrk] + mPar[kYErr]*(mSl*mSp)*(mSl*mSp)+ mPar[kZErr]*mCl*mCl;
  g[1] = mPar[kThkDet]*mCp*mSp*mSl;
  TCL::trasat(T[0], g, G, 2,2);

  hrr->hYY = G[0];
  hrr->hZZ = G[2];
  hrr->hYZ = G[1];
}  
//______________________________________________________________________________
//______________________________________________________________________________
void StiTpcHitErrCalculator::CalcDetErrs(const StiHit *hit, const StiNodePars *node
                                        ,StiHitErrs *hrr)
{
    float zSpan = fabs(hit->x_g()[2]-210)/100;
    hrr->hYY+=mPar[kYDiff]*zSpan;
    hrr->hZZ+=mPar[kZDiff]*zSpan;
}
//______________________________________________________________________________
void StiTpcHitErrCalculator::CalcDCAErrs(const StiHit *hit, const StiNodePars *node
                                     ,StiHitErrs *hrr)
{
    float zSpan = fabs(hit->x_g()[2]-210)/100;
    double save1 = mPar[kYErr],save2 = mPar[kZErr];
    mPar[kYErr]+=mPar[kYDiff]*zSpan;
    mPar[kZErr]+=mPar[kZDiff]*zSpan;
    StiHitErrCalculator::CalcDCAErrs(hit,node,hrr);
    mPar[kYErr]=save1;mPar[kZErr]=save2;
}
#if 1
#include "TRandom.h"
#include "TVector3.h"
//______________________________________________________________________________
void StiHitErrCalculator::Test(double phiG,double lamG)
{
  double par[6]={0};
  par[kYErr]=0.03*0.03;
  par[kZErr]=0.07*0.07;
  par[kThkDet]=3*3;
  par[kWidTrk]=0.1*0.1;

//   double Lam = 3.14*(gRandom->Rndm()-0.5)/2;
//   double Phi = 3.14*(gRandom->Rndm()-0.5);
  double Lam = lamG/180*M_PI;
  double Phi = phiG/180*M_PI;
  double W = sqrt(par[kWidTrk]); 	//Width of the track
  double D = sqrt(par[kThkDet]);  	//Thickness of detector plane 
  par[kThkDet]/=12;
  double cL = cos(Lam);
  double sL = sin(Lam);
  double tL = tan(Lam);
  double cP = cos(Phi);
  double sP = sin(Phi);
//double tP = tan(Phi);
  TVector3 Nt(cL*cP,cL*sP,sL);
  TVector3 Np(-sP, cP, 0);
  TVector3 Nl(-sL*cP,-sL*sP,cL);
  TVector3 V;
//   printf("Nt="); Nt.Print();
//   printf("Np="); Np.Print();
//   printf("Nl="); Nl.Print();
  double YZ[3]={0},BG[3]={0};
  int nEl=10000,iEl=0;
  while (1) {
    double alfa = D/Nt[0]*(gRandom->Rndm()-0.5)*1.9;
    double beta = gRandom->Gaus()*W;
    double gama = gRandom->Gaus()*W;
    V = Nt*alfa + Np*beta + Nl*gama;
    if (fabs(V[0])>0.5*D) continue;
    V[1]+=  gRandom->Gaus()*par[kYErr];
    V[2]+=  gRandom->Gaus()*par[kZErr];
  par[kZErr]=0;


    if(++iEl>=nEl) break;
    YZ[0] += V[1]*V[1]; YZ[1] += V[1]*V[2];YZ[2] += V[2]*V[2];

//    Project along X to X=0
    V[0]=0;
    beta = (Np*V);
    gama = (Nl*V);
    BG[0]+=beta*beta; BG[1]+=beta*gama;BG[2]+=gama*gama;
  }
  for (int j=0;j<3;j++){YZ[j]/=nEl; BG[j]/=nEl;} 
  
  printf("Phi=%d Lam=%d: YY=%g YZ=%g ZZ=%g\n"
        , int(Phi/3.1415*180),int(Lam/3.1415*180),YZ[0],YZ[1],YZ[2]);

  StiHitErrCalculator calc;
  calc.SetPars(par,4);
  StiNodePars np; np._cosCA = cP;  np._sinCA = sP; np._tanl=tL;
  StiHitErrs hitErr;
  calc.CalcDetErrs(0, &np, &hitErr);
  printf("Calculator:  YY=%g YZ=%g ZZ=%g\n"
        ,hitErr.hYY,hitErr.hYZ,hitErr.hZZ);

  printf("DCA       : BB=%g BG=%g GG=%g\n",BG[0],BG[1],BG[2]);
  calc.CalcDCAErrs(0, &np, &hitErr);
  printf("DCA Calc  : BB=%g BG=%g GG=%g\n"
        ,hitErr.hYY,hitErr.hYZ,hitErr.hZZ);


}
#endif //0

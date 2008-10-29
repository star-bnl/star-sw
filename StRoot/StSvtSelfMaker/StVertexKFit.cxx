#include "StVertexKFit.h"
#include "TCL.h"
#include "TArrayD.h"
#include "TArrayI.h"
#include "TMath.h"
#include "THelixTrack.h"
#include "StThreeVectorD.hh"
static const double DEF_ERR = 0.1*0.1;
static const double DIST_MAX = 1.;
static const double CHI2_MAX = 33.;

static double joinTwo(int nP1,const double *P1,const double *E1
                     ,int nP2,const double *P2,const double *E2
	             ,              double *PJ,      double *EJ);

class StVertexKFitAux {
public:
double x[3];
double d[3];
double rho;
double erk[9];  
double dca2;
};               


ClassImp(StVertexKFit)
//______________________________________________________________________________
StVertexKFit::StVertexKFit()
{
  memset(mBeg,0,mEnd-mBeg+1);
}

//______________________________________________________________________________
void StVertexKFit::SetVtx(const double *vtx,const double *etx)
{
  memset(mBeg,0,mEnd-mBeg+1);
  TCL::ucopy(vtx,mVtx,3);
  TCL::vzero(mEtx,6);
  mEtx[0] = DEF_ERR*9; mEtx[2]= DEF_ERR*9; mEtx[5]= DEF_ERR*9;
  if (etx) TCL::ucopy(etx,mEtx,6);
}
//______________________________________________________________________________
void StVertexKFit::SetVtx(const float *vtx,const float *etx)
{
 double t[9];
 TCL::ucopy(vtx,t,3);
 double *et = (etx)? t+3:0;
 if (et) TCL::ucopy(etx,et,6);
 SetVtx(t,et);
}
//______________________________________________________________________________
void StVertexKFit::AddTrk(const double *xyz,const double *dir,double curv
                         ,const double *erk)
{ 
  enum {szAux = sizeof(StVertexKFitAux)/sizeof(double)};
  mNAux++;
  int n = mNAux*szAux;
  if (mArr.GetSize()<n) {mArr.Set(n*2);mAux=0;}
  mAux = (StVertexKFitAux*)mArr.GetArray();
  n = mNAux-1;
  TCL::ucopy(xyz,mAux[n].x,3);
  TCL::ucopy(dir,mAux[n].d,3);
  mAux[n].rho = curv;
  mAux[n].erk[0]=0;
  if (erk) TCL::ucopy(erk,mAux[n].erk,9);
}  
//______________________________________________________________________________
void StVertexKFit::AddTrk(const float *xyz,const float *dir,float curv
                         ,const float *erk)
{ 
  double d[20];
  TCL::ucopy(xyz,d+0,3);
  TCL::ucopy(dir,d+3,3);
  d[6]= curv;
  d[7]=0;
  if (erk) TCL::ucopy(erk,d+7,9);
  AddTrk(d,d+3,d[6],d+7);
} 
//______________________________________________________________________________
double StVertexKFit::Fit()
{
  TArrayD aDca(mNAux); double *dca2 = aDca.GetArray();
  TArrayI iDca(mNAux); int    *idx  = iDca.GetArray();
  for (int i=0;i<mNAux;i++) {
    THelixTrack ht(mAux[i].x,mAux[i].d,mAux[i].rho);
    ht.Backward();
    dca2[i] = fabs(ht.Dca(0.,0.));
  }
  TMath::Sort(mNAux,dca2,idx,1);
  mNTk = 0;
  for (int j=0;j<mNAux;j++) {
    int i = idx[j];
    if (dca2[i] >DIST_MAX) 	break;
    THelixTrack ht(mAux[i].x,mAux[i].d,mAux[i].rho);
    ht.Backward();
    double xNear[3],dNear[3],s;
    s = ht.Step(mVtx,xNear,dNear);
    if (fabs(s)>100) 		continue;

    StThreeVectorD nZZ(dNear);
    StThreeVectorD vNear(xNear);
    StThreeVectorD vVert(mVtx );
    StThreeVectorD nXX= vNear-vVert;
    double dist2 = nXX.mag2();
    double dist = sqrt(dist2);
    if (dist<1e-4) {//almost coinside
      int imin=0;
      if (fabs(dNear[imin])>fabs(dNear[1])) imin=1;
      if (fabs(dNear[imin])>fabs(dNear[2])) imin=2;
      double tmp[3]={0,0,0}; tmp[imin]=1;
      nXX = StThreeVectorD(tmp);
    }
    nXX -= nZZ*nZZ.dot(nXX); //nXX and nZZ exactly orthogonal
    nXX = nXX.unit();

    StThreeVectorD nYY = nZZ.cross(nXX);

    double T[3][3];
    TCL::ucopy(&(nXX.x()),T[0],3);
    TCL::ucopy(&(nYY.x()),T[1],3);
    TCL::ucopy(&(nZZ.x()),T[2],3);

    double etxL[6],vtxL[3];
    TCL::vzero(vtxL,3);
    TCL::trasat(T[0],mEtx,etxL,3,3);
    assert((etxL[0]>0) && (etxL[2]>0) && (etxL[5]>0));

    double etkL[3],vtkL[2];
    etkL[0] = DEF_ERR;etkL[1] = 0;etkL[2] = DEF_ERR;
    vtkL[0] = dist; vtkL[1]=0;

    double ejL[6],vjL[3];
    mChi2[1] = joinTwo(2,vtkL,etkL, 3,vtxL,etxL,  vjL, ejL );
    if (mChi2[1]>CHI2_MAX) 	continue;
    assert((ejL[0]>0) && (ejL[2]>0) && (ejL[5]>0));


  // 	back to global
    double vtxG[3],etxG[6];
    TCL::tratsa(T[0],ejL,etxG,3,3);
    assert((etxG[0]>0) && (etxG[2]>0) && (etxG[5]>0));
    TCL::mxmpy2(T[0], vjL, vtxG, 3, 3, 1);
    TCL::vadd(mVtx,vtxG,vtxG,3);
    if (fabs(vtxG[0]) >1) 	continue;
    if (fabs(vtxG[1]) >1) 	continue;
    if (fabs(vtxG[2]) >100) 	continue;
    TCL::ucopy(vtxG,mVtx,3);
    TCL::ucopy(etxG,mEtx,6);

    mChi2[0]  = (mChi2[0]*mNTk + mChi2[1])/(mNTk+1);
    mNTk++;
  }

  return mChi2[1];
  
}
//______________________________________________________________________________
void StVertexKFit::Print(const char *txt) const
{
  if (!txt) txt="";
  printf("===== StVertexKFit::Print(%s)\n",txt);
  double vxRad = sqrt(TCL::vdot(mVtx,mVtx,3));
  printf("===== Vtx(%g, %g, %g)\n",mVtx[0],mVtx[1],mVtx[2]);
  printf("===== nTk = %d Rad=%g Chi2 = %g %g\n\n",mNTk,vxRad,mChi2[0],mChi2[1]);
}  
  
//______________________________________________________________________________
double joinTwo(int nP1,const double *P1,const double *E1
              ,int nP2,const double *P2,const double *E2
	      ,              double *PJ,      double *EJ)
{

  assert(nP1<=nP2);
  int nE1 = nP1*(nP1+1)/2;
  int nE2 = nP2*(nP2+1)/2;
  TArrayD ard(nE2*6);
  double *a = ard.GetArray();  
  double *sumE 		= (a);
  double *sumEI 	= (a+=nE2);
  double *e1sumEIe1 	= (a+=nE2);
  double *subP 		= (a+=nE2);
  double *sumEIsubP	= (a+=nE2);
  double chi2=3e33,p,q;

// Choose the smalest errors
  const double *p1 = P1, *p2 = P2, *e1 = E1, *e2 = E2, *t;
  double choice = (nP1==nP2)? 0:1;
  if (!choice   ) {
    for (int i=0,n=1;i<nE2;i+=(++n)) {
    p=fabs(e1[i]);q=fabs(e2[i]);choice += (p-q)/(p+q+1e-10);
  }}
  if ( choice >0) {t = p2; p2 = p1; p1 = t; t = e2; e2 = e1; e1 = t;}

  do {//empty loop
//  	Join errors
    TCL::vadd(e1,e2,sumE,nE1);
    int negati = sumE[2]<0;
    if (negati) TCL::vcopyn(sumE,sumE,nE1);
    int ign0re = sumE[0]<=0;
    if (ign0re) sumE[0] = 1;
    TCL::trsinv(sumE,sumEI,nP1);
    if (ign0re) {sumE[0]  = 0; sumEI[0] = 0;}
    if (negati) {TCL::vcopyn(sumE,sumE,nE1);TCL::vcopyn(sumEI,sumEI,nE1);}
    TCL::vsub(p2       ,p1   ,subP       ,nP1);
    TCL::trasat(subP,sumEI,&chi2,1,nP1); 
    if (!EJ) break;
    TCL::trqsq (e1  ,sumEI,e1sumEIe1,nP2); 
    TCL::vsub(e1,e1sumEIe1,EJ,nE2);
  } while(0);
//  	Join params
  if (PJ) {
    TCL::tras(subP     ,sumEI,sumEIsubP,1,nP1);
    TCL::tras(sumEIsubP,e1   ,PJ       ,1,nP2);
    TCL::vadd(PJ       ,p1   ,PJ         ,nP2);
  }
  return chi2;
}
  
   

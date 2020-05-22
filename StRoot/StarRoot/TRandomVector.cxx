/***************************************************************************
 *
 * $Id: TRandomVector.cxx,v 1.9 2020/05/22 23:41:28 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "TRandomVector.h"
#include "TCernLib.h"
ClassImp(TRandomVector)
//______________________________________________________________________________
TRandomVector::TRandomVector()
{
   fDim=0;
}
//_____________________________________________________________________________
TRandomVector::TRandomVector(const TMatrixDSym& errMtx,UInt_t seed)
{ assert(! Set(errMtx,seed)); }
//_____________________________________________________________________________
TRandomVector::TRandomVector(const TVectorD& errDia,UInt_t seed)
{ 
  fDim = errDia.GetNrows();
  TMatrixDSym mtx(fDim);
  fRandom.SetSeed(seed);
  for (int i=0;i<fDim;i++){mtx[i][i] = errDia[i];}
  RandRotate(mtx);
  assert(! Set(mtx,seed)); 
}
//_____________________________________________________________________________
TRandomVector::TRandomVector(int nSide,const double *G,UInt_t seed)
{ 
  TMatrixDSym errMtx(nSide);
  for (int i=0,li=0;i< nSide;li+=++i) {
  for (int j=0;j<=i; j++)             {
    errMtx[i][j] = G[li+j];
    errMtx[j][i] = G[li+j];
  } }
  assert(!Set(errMtx,seed));
}
//_____________________________________________________________________________
int TRandomVector::Set(const TMatrixDSym& errMtx,UInt_t  seed)
{
  if (seed) fRandom.SetSeed(seed);
  fDim = errMtx.GetNcols();
  fErrMtx.ResizeTo(fDim,fDim);
  fEigMtx.ResizeTo(fDim,fDim);
  fEigVal.ResizeTo(fDim);
  fResult.ResizeTo(fDim);
  
  fErrMtx = errMtx;
  if (fDim<1) {	Error("Set","Size too small %d",fDim);
    		fDim=0; return 1;}
  fEigMtx= fErrMtx.EigenVectors(fEigVal);

  for (int i=0;i<fDim;i++) { 
  for (int j=0;j<fDim;j++) { 
    double sum=0;
    for (int k=0;k<fDim;k++) { 
      sum += fEigMtx[i][k]*fEigMtx[j][k]*fEigVal[k];}
    double dif = fErrMtx[i][j]-sum;
    if (fabs(dif)>1e-6)
      printf("*** %2i %2i %g = %g %g\n",i,j,fErrMtx[i][j],sum,dif);
  } }
//  fEigMtx.T();
  for (int i=0;i<fDim;i++) { 
    if (fEigVal[i]<0) {
      Error("Set","Non positive error matrix: eigen(%d)=%g",i,fEigVal[i]);
      fDim=0; return 2;}
    fEigVal[i] = sqrt(fEigVal[i]);
  } 

  return 0;
}
//_____________________________________________________________________________
const TVectorD& TRandomVector::Gaus()
{
  if (!fDim) {Error("Gaus","Not initialised properly");}
  TVectorD rnd(fDim);
  for (int i=0;i<fDim;i++){ rnd[i]= fRandom.Gaus()*fEigVal[i];};
  fResult = fEigMtx*rnd;
  return fResult;
}
//_____________________________________________________________________________
void TRandomVector::RandRotate(TMatrixDSym& errMtx)
{
  int nDim = errMtx.GetNrows();
  TMatrixD A(nDim,nDim),B(nDim,nDim),R(nDim,nDim);
  A = errMtx;
  for (int L=0;L<nDim;L++) {
    R.UnitMatrix();
    for (int M=(L&1);M+1<nDim;M+=2) {
      double S = (gRandom->Rndm()-0.5)+0.01;
      double C = sqrt((1-S)*(1+S)); 
      R[M+0][M] = C; R[M+0][M+1] = S; 
      R[M+1][M] =-S; R[M+1][M+1] = C; 
    }
    B.Mult(A,R);
    R.Transpose(R);
    A.Mult(R,B);
  }
//  errMtx = A;
  for (int i=0;i<nDim;i++) {
  for (int j=0;j<=i  ;j++) {
    double a = A[i][j];
    double b = A[j][i];
    assert(fabs(a-b)<1e-6);
    errMtx[i][j] = 0.5*(a+b);
    errMtx[j][i] = 0.5*(a+b);
  } }
}
//_____________________________________________________________________________
double TRandomVector::Sign(const TMatrixDSym &Si)
{
  int n = Si.GetNrows();
   TMatrixDSym S(Si);
  TVectorD coe(n);
  for (int i=0;i< n;++i) {
    double qwe = S[i][i];
    if(qwe<=0) return qwe;
    qwe = pow(2.,-int(log(qwe)/(2*log(2))));
    coe[i]=qwe;
  }

  for (int i=0;i< n;++i) {
    for (int j=0;j< n;j++) {S[i][j]*=coe[i]*coe[j];}}

  TVectorD EigVal(n);  
  S.EigenVectors(EigVal);

  double ans = 3e33;
  for (int i=0;i<n;i++) {if (EigVal[i]<ans) ans = EigVal[i];}
  return ans;
} 

//_____________________________________________________________________________
void TRandomVector::Test(int nevt)
{
enum {kMySize=10};
  TRandomVector *RV=0;
  TMatrixDSym S(kMySize);

  TVectorD V(kMySize);
  for (int i=0;i<kMySize;i++) {
    V[i] = (i+1)*(1+0.1*gRandom->Rndm());
  }

  RV = new TRandomVector(V);
  S = RV->GetMtx();
  for (int i=0;i<kMySize;i++) 	{
    for (int j=0;j<i;j++) 	{
      double t = S[i][j]/sqrt(S[i][i]*S[j][j]);
      printf("%g \t",t);
    }
    printf("%g \n",S[i][i]);
  }   

  
//  S.Print("");
  TVectorD res(kMySize);
  TMatrixD SS(kMySize,kMySize);
for (int evt=0;evt<nevt;evt++) {
  const TVectorD &res = RV->Gaus();
   for (int ii=0;ii<kMySize;ii++) 	{
   for (int jj=0;jj<=ii;jj++) 		{SS[ii][jj]+=res[ii]*res[jj];}}
}
  SS*=(1./nevt);
  
  double Qa = 0,maxQa=0,maxCorr=0;
for (int i=0;i<kMySize;i++) {
for (int k=0;k<=i;k++) {
  double nor = sqrt(S[i][i]*S[k][k]);
  double dif = (S[i][k]-SS[i][k])/nor;
  if ( i!=k && fabs(S[i][k]/nor)>fabs(maxCorr)) maxCorr = S[i][k]/nor;
  if (fabs(dif)> 0.1) 
    printf("(%d %d) \t%g = \t%g \t%g\n",i,k,S[i][k]/nor,SS[i][k]/nor,dif);
  dif = fabs(dif);
  Qa+= (dif); if (dif>maxQa) maxQa=dif;
}}
int n = ((kMySize*kMySize+kMySize)/2);
Qa/=(n);
printf("Quality %g < %g < 1  maxCorr=%g\n",Qa,maxQa,maxCorr);
}
//_____________________________________________________________________________
void TRandomVector::TestXi2()
{
  enum {kNDim = 5, nEv=10000};
  TVectorD dia(kNDim); 
  for (int i=0;i<kNDim;i++) { dia[i] = i+gRandom->Rndm();}
  
  TRandomVector RV(dia);
  
  auto &G = RV.GetMtx();
  
  auto GI = G; GI.Invert();


  double Xi2 = 0;
  for (int iEv=0;iEv<nEv;iEv++) {
    auto &v = RV.Gaus();  
    Xi2 += GI.Similarity(v);
  }
  Xi2/=nEv*kNDim;
  printf ("TRandomVector::TestXi2(): <Xi2>/Ndf = %g\n",Xi2);
}















/***************************************************************************
 *
 * $Id: TRandomVector.cxx,v 1.2 2010/12/16 21:22:53 perev Exp $
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
static const double kSMALL = 0.1;

  fDim = errDia.GetNrows();
  TMatrixDSym mtx(fDim);
  fRandom.SetSeed(seed);
  for (int i=0;i<fDim;i++){assert(errDia[i]>0); mtx[i][i] = errDia[i];}
  TMatrixD T(2,2),Tt(2,2),ext(2,2);
  for (int i=0;  i<fDim-1;i++) {
  for (int j=i+1;j<fDim  ;j++) {
    double phiMax = kSMALL; 
    double dDia = fabs(errDia[i]-errDia[j])+1e-10;
    double myMax = 2*errDia[i]*kSMALL/dDia;
    if (phiMax*phiMax> myMax) phiMax = sqrt(myMax);
    myMax = 2*errDia[j]*kSMALL/dDia;
    if (phiMax*phiMax> myMax) phiMax = sqrt(myMax);
    myMax = kSMALL*kSMALL*errDia[i]*errDia[j]/(dDia*dDia);
    if (phiMax*phiMax> myMax) phiMax = sqrt(myMax);
    double phi = (fRandom.Rndm()<0.5)? -phiMax:phiMax;
    double c = cos(phi),s=sin(phi); 
    T[0][0]= c; T[0][1]= s;
    T[1][0]=-s; T[1][1]= c;
    Tt = T; Tt.T();
    ext[0][0] = mtx[i][i];ext[0][1] = mtx[i][j];
    ext[1][0] = mtx[j][i];ext[1][1] = mtx[j][j];

    ext = T*(ext*Tt);
    assert(ext[0][0]*ext[1][1]>ext[0][1]*ext[1][0]);
    mtx[i][i] = ext[0][0]; mtx[i][j] = ext[0][1];
    mtx[j][i] = ext[0][1]; mtx[j][j] = ext[1][1];
  } }
    assert(! Set(mtx,0)); 
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
  for (int i=0;i<fDim;i++){ rnd[i]= gRandom->Gaus()*fEigVal[i];};
  fResult = fEigMtx*rnd;
  return fResult;
}
//_____________________________________________________________________________
void TRandomVector::Test(int nevt)
{
enum {kMySize=15};
TRandomVector *RV=0;
TMatrixDSym S(kMySize);

if (nevt>0 ) {
  for (int i=0;i<kMySize;i++) {
   S[i][i] = (i+1)*(1+0.1*gRandom->Rndm());
   for (int j=0;j<i;j++) {
     double corr = sqrt(S[i][i]*S[j][j])*(0.5-gRandom->Rndm())*0.1;
     S[i][j]=corr;  
     S[j][i]=corr;  
  } }
  RV = new TRandomVector(S);
} else { //vector case
  TVectorD V(kMySize);
  for (int i=0;i<kMySize;i++) {
    V[i] = (i+1)*(1+0.1*gRandom->Rndm());
  }
  RV = new TRandomVector(V);
  S = RV->GetMtx();
}  
  nevt = abs(nevt);
//  S.Print("");
  TVectorD res(kMySize);
  TMatrixD SS(kMySize,kMySize);
for (int evt=0;evt<nevt;evt++) {
  const TVectorD &res = RV->Gaus();
   for (int ii=0;ii<kMySize;ii++) 	{
   for (int jj=0;jj<=ii;jj++) 		{SS[ii][jj]+=res[ii]*res[jj];}}
}
  SS*=(1./nevt);
  
  double Qa = 0,maxQa=0;;
for (int i=0;i<kMySize;i++) {
for (int k=0;k<=i;k++) {
  double nor = sqrt(S[i][i]*S[k][k]);
  double dif = (S[i][k]-SS[i][k])/nor;
  if (fabs(dif)> 0.1) 
    printf("(%d %d) \t%g = \t%g \t%g\n",i,k,S[i][k]/nor,SS[i][k]/nor,dif);
  dif = fabs(dif);
  Qa+= (dif); if (dif>maxQa) maxQa=dif;
}}
int n = ((kMySize*kMySize+kMySize)/2);
Qa/=(n);
printf("Quality %g < %g < 1\n",Qa,maxQa);
}

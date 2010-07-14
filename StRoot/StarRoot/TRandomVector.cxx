/***************************************************************************
 *
 * $Id: TRandomVector.cxx,v 1.1 2010/07/14 18:21:57 perev Exp $
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
TRandomVector::TRandomVector(const TMatrixTSym<double>& errMtx,UInt_t seed)
{ assert(! Set(errMtx,seed)); }
//_____________________________________________________________________________
int TRandomVector::Set(const TMatrixTSym<double>& errMtx,UInt_t  seed)
{
  fRandom.SetSeed(seed);
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
    if (fEigVal[i]<0) {
      Error("Set","Non positive error matrix: eigen(%d)=%g",i,fEigVal[i]);
      fDim=0; return 2;}
    fEigVal[i] = sqrt(fEigVal[i]);
  } 

  return 0;
}
//_____________________________________________________________________________
const TVectorT<double>& TRandomVector::Gaus()
{
  if (!fDim) {Error("Gaus","Not initialised properly");}
  TVectorT<double> rnd(fDim);
  for (int i=0;i<fDim;i++){ rnd[i]= gRandom->Gaus()*fEigVal[i];};
  fResult = fEigMtx*rnd;
  return fResult;
}
//_____________________________________________________________________________
void TRandomVector::Test(int nevt)
{
enum {kMySize=15};
TMatrixTSym<double> S(kMySize);
for (int i=0;i<kMySize;i++) {
 S[i][i] = (i+1)*(1+0.1*gRandom->Rndm());
 for (int j=0;j<i;j++) {
   double corr = sqrt(S[i][i]*S[j][j])*(0.5-gRandom->Rndm())*0.1;
   S[i][j]=corr;  
   S[j][i]=corr;  
} }
TRandomVector RV(S);


  TVectorT<double> res(kMySize);
  TMatrixT<double> SS(kMySize,kMySize);
  SS*=0.;
for (int evt=0;evt<nevt;evt++) {
  const TVectorT<double> &res = RV.Gaus();
   for (int ii=0;ii<kMySize;ii++) {
   for (int jj=0;jj<kMySize;jj++) {SS[ii][jj]+=res[ii]*res[jj];}}

}
//  SS.Print();
  SS*=(1./nevt);
  
  double Qa = 0,maxQa=0;;
for (int i=0;i<kMySize;i++) {
for (int k=0;k<=i;k++) {
  double dif = (S[i][k]-SS[i][k])/sqrt(S[i][i]*S[k][k]);
  printf("(%d %d) \t%g = \t%g \t%g\n",i,k,S[i][k],SS[i][k],dif);
  dif = fabs(dif);
  Qa+= (dif); if (dif>maxQa) maxQa=dif;
}}
int n = ((kMySize*kMySize+kMySize)/2);
Qa/=(n);
printf("Quality %g < %g < 1\n",Qa,maxQa);
}

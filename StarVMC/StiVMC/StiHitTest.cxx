#include "StiHitTest.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TVectorD.h"
#include "TMatrixDSym.h"
#include "TMath.h"
//______________________________________________________________________________
void StiHitTest::Reset()
{
  memset(fBeg,   0,fEnd-fBeg+1);
  fW[0]=-1;
}

//______________________________________________________________________________
void StiHitTest::Add(double x,double y,double z)
{
  double xx[3]; xx[0]=x;xx[1]=y;xx[2]=z;
  Add(xx);
}
//______________________________________________________________________________
void StiHitTest::Add(double x[3])
{
  fN++;
  for (int i=0;i<3;i++) {
    fX[i] += x[i];
    for (int j=0;j<3;j++) {
      fM[i][j] += x[i]*x[j];
  }}
}
//______________________________________________________________________________
void StiHitTest::doIt()
{
  if (!fN) 		return;
  if (fW[0]>=0.) 	return;
  for (int i=0;i<3*4;i++) {fX[i]/=fN;}
  for (int i=0;i<3;i++) {
    for (int j=0;j<3;j++) {
      fM[i][j] -= fX[i]*fX[j];
  }}
  TMatrixDSym Sym(3,fM[0],"");
  TVectorD vals(3);
  TMatrixD vecs = Sym.EigenVectors(vals);
//  vals.Print();
  memcpy(fW,   vals.GetMatrixArray(),sizeof(fW));
  vecs.Transpose(vecs);
  memcpy(fV[0],vecs.GetMatrixArray(),sizeof(fV));
}
//______________________________________________________________________________
double StiHitTest::width(int idx)
{
  doIt();
  return fW[idx];
}

//______________________________________________________________________________
const double *StiHitTest::vector(int idx)
{
  doIt();
  return fV[idx];
}
//______________________________________________________________________________
double StiHitTest::yAngle() const
{
   double dy = fV[2][1]; double dx = fV[2][0];
   if (dx<0) {dx=-dx;dy=-dy;}
   return TMath::ATan2(dy,dx);
}
//______________________________________________________________________________
double StiHitTest::zAngle() const
{
   double dz = fV[2][2]; double dx = fV[2][0];
   if (dx<0) {dx=-dx;dz=-dz;}
   return TMath::ATan2(dz,dx);
}




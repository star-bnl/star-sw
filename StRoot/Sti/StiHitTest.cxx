#include "StiHitTest.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TVectorD.h"
#include "TMatrixDSym.h"
//______________________________________________________________________________
void StiHitTest::reset()
{
  fN=0; fW = -1.;
  memset(fX,   0,sizeof(fX));
  memset(fM[0],0,sizeof(fM));
}

//______________________________________________________________________________
void StiHitTest::add(double x,double y,double z)
{
  double xx[3]; xx[0]=x;xx[1]=y;xx[2]=z;
  add(xx);
}
//______________________________________________________________________________
void StiHitTest::add(double x[3])
{
  fN++;
  for (int i=0;i<3;i++) {
    fX[i] += x[i];
    for (int j=0;j<3;j++) {
      fM[i][j] += x[i]*x[j];
  }}
}
//______________________________________________________________________________
double StiHitTest::width()
{
  if (!fN) return 0.;
  if (fW>=0.) return fW;
  for (int i=0;i<3*4;i++) {fX[i]/=fN;}
  for (int i=0;i<3;i++) {
    for (int j=0;j<3;j++) {
      fM[i][j] -= fX[i]*fX[j];
  }}
  TMatrixDSym Sym(3,fM[0],"");
  TVectorD vals(3);
  Sym.EigenVectors(vals);
//  vals.Print();
  fW =  sqrt(fabs(vals.Min()));
  return fW;
}


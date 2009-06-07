// $Id: StiELossTrk.cxx,v 1.1 2009/06/07 02:28:36 perev Exp $
//
//
// Class StiELossTrk
// ------------------
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "StiELossTrk.h"
//_____________________________________________________________________________
void StiELossTrk::Reset()
{
  memset(fBeg,0,fEnd-fBeg+1);
 Set(1.,PiMASS);
}
//_____________________________________________________________________________
void StiELossTrk::Set(double p2,double mass)
{
  fP2=p2; fMas2 = mass*mass; 
  fFak = (14.1*14.1*(fP2+fMas2))/(fP2*fP2*1e6);
}
//_____________________________________________________________________________
void StiELossTrk::Add(double len,double x0)
{
  fMCS[0] += len/x0;
  fMCS[1] -= len*len/x0;
  fMCS[2] += len*len*len/(3*x0);
  fTotLen += len;
}
//_____________________________________________________________________________
double StiELossTrk::GetTheta2() const 
{
  assert(fFak>0);
  return fFak*fMCS[0];
}
//_____________________________________________________________________________
double StiELossTrk::GetOrt2() const 
{
  assert(fFak>0);
  return fFak*(fMCS[2]+fTotLen*(fMCS[1]+fTotLen*fMCS[0]));
}
//_____________________________________________________________________________
void StiELossTrk::GetCoef(double coe[3]) const 
{
  for (int i=0;i<3;i++) {coe[i]=fFak*fMCS[i];}
}
//_____________________________________________________________________________
double StiELossTrk::GetOrt2(double coe[3],double len) 
{
  return (coe[2]+len*(coe[1]+len*coe[0]));
}

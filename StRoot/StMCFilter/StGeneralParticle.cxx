// @(#)root/eg:$Id: StGeneralParticle.cxx,v 1.2 2009/08/25 20:49:16 fine Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "math.h"
#include "string.h"

#include "StGeneralParticle.h"

//______________________________________________________________________________
StGeneralParticle::StGeneralParticle(int idx) : StGenParticle(idx),fDaughter(0)
{
  Clear();
}
//______________________________________________________________________________
StGeneralParticle::~StGeneralParticle() 
{}
//______________________________________________________________________________
const StGenParticle *StGeneralParticle::GetDaughter(int i)  const  
{
  if (i>=(int)fDaughter.size()) return 0;
  return fDaughter[i];
}
//______________________________________________________________________________
void  StGeneralParticle::Momentum(double p4[4])   const
{
  memcpy(p4,fP,4*sizeof(*fP));
}
//______________________________________________________________________________
void  StGeneralParticle::Vertex(double v3[3])   const
{
  memcpy(v3,fV,3*sizeof(*fV));
}
//______________________________________________________________________________
void StGeneralParticle::Clear()
{
  memset(fBeg,0,fEnd-fBeg+1);
  fWeight = 1; fStatusCode=1;
  fDaughter.clear();
}
//______________________________________________________________________________
void StGeneralParticle::SetVert(float v[4])
{
  for (int i=0;i<4;i++) { fV[i]=v[i];}
}
//______________________________________________________________________________
void StGeneralParticle::SetMom(float p[5])
{
  for (int i=0;i<5;i++) { fP[i]=p[i];}
}
//______________________________________________________________________________
void StGeneralParticle::AddDaughter(const StGeneralParticle *m)
{
   fDaughter.push_back(m);   
}

 

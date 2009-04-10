// @(#)root/eg:$Id: StGENParticle.cxx,v 1.1 2009/04/10 19:59:20 perev Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "math.h"

#include "StGENParticle.h"

//______________________________________________________________________________
StGENParticle::StGENParticle(int idx) : StGenParticle(idx),fDaughter(0)
{
  Clear();
}
//______________________________________________________________________________
StGENParticle::~StGENParticle() 
{}
//______________________________________________________________________________
const StGenParticle *StGENParticle::GetDaughter(int i)  const  
{
  if (i>=(int)fDaughter.size()) return 0;
  return fDaughter[i];
}
//______________________________________________________________________________
void  StGENParticle::Momentum(double p4[4])   const
{
  memcpy(p4,fP,4*sizeof(*fP));
}
//______________________________________________________________________________
void  StGENParticle::Vertex(double v3[3])   const
{
  memcpy(v3,fV,3*sizeof(*fV));
}
//______________________________________________________________________________
void StGENParticle::Clear()
{
  memset(fBeg,0,fEnd-fBeg+1);
  fWeight = 1; fStatusCode=1;
  fDaughter.clear();
}
//______________________________________________________________________________
void StGENParticle::SetVert(float v[4])
{
  for (int i=0;i<4;i++) { fV[i]=v[i];}
}
//______________________________________________________________________________
void StGENParticle::SetMom(float p[5])
{
  for (int i=0;i<5;i++) { fP[i]=p[i];}
}
//______________________________________________________________________________
void StGENParticle::AddDaughter(const StGENParticle *m)
{
   fDaughter.push_back(m);   
}

 

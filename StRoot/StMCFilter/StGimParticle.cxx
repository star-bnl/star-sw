// @(#)root/eg:$Id: StGimParticle.cxx,v 1.3 2016/06/21 20:28:51 jwebb Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "math.h"
#include "string.h"

#include "StGimParticle.h"

//______________________________________________________________________________
StGimParticle::StGimParticle(int idx) : 
  StGenParticle(idx),
  fBeg{0},
  fPdgCode{0},
  fGeaCode{0},
  fStatusCode{0},
  fMother{0,0},
  fWeight{0},
  fP{0,0,0,0,0},
  fV{0,0,0,0},
  fDaughter(0)
{
  Clear();
}
//______________________________________________________________________________
StGimParticle::~StGimParticle() 
{}
//______________________________________________________________________________
const StGenParticle *StGimParticle::GetDaughter(int i)  const  
{
  if (i>=(int)fDaughter.size()) return 0;
  return fDaughter[i];
}
//______________________________________________________________________________
void  StGimParticle::Momentum(double p4[4])   const
{
  memcpy(p4,fP,4*sizeof(*fP));
}
//______________________________________________________________________________
void  StGimParticle::Vertex(double v3[3])   const
{
  memcpy(v3,fV,3*sizeof(*fV));
}
//______________________________________________________________________________
void StGimParticle::Clear()
{
  memset(fBeg,0,fEnd-fBeg+1);
  fWeight = 1; fStatusCode=1;
  fDaughter.clear();
}
//______________________________________________________________________________
void StGimParticle::SetVert(float v[4])
{
  for (int i=0;i<4;i++) { fV[i]=v[i];}
}
//______________________________________________________________________________
void StGimParticle::SetMom(float p[5])
{
  for (int i=0;i<5;i++) { fP[i]=p[i];}
}
//______________________________________________________________________________
void StGimParticle::AddDaughter(const StGimParticle *m)
{
   fDaughter.push_back(m);   
}

 

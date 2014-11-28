// @(#)root/eg:$Id: StHepParticle.cxx,v 1.5 2009/08/25 20:49:16 fine Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include <stdlib.h>
#include <math.h>
#include <vector>
#include <cassert>
#include <cstring>

#include "StHepParticle.h"
my_hepevt*      StHepParticleMaster::mgMyHepevt=0;
StHepParticleMaster *StHepParticleMaster::mgInst    =0;
static std::vector<StHepParticle*> myVec;

//______________________________________________________________________________

class my_hepevt {
public:
 enum {NMXHEP=4000};
 int  nevhep;             	/* The event number */
 int  nhep;               	/* The number of entries in this event */
 int  isthep[NMXHEP];     	/* The Particle status code*/
 int  idhep [NMXHEP];      	/* The particle id */
 int  jmohep[NMXHEP][2];    	/* The position of the mother particle */
 int  jdahep[NMXHEP][2];    	/* Position of the first daughter... */
 double phep[NMXHEP][5];    	/* 4-Momentum, mass */
 double vhep[NMXHEP][4];    	/* Vertex information */
};

//______________________________________________________________________________
StHepParticleMaster::StHepParticleMaster(void *addr)
{
  assert(!mgInst && "StHepParticleMaster created twice. NEVER,JAMAIS");
  mgInst = this;
  mgMyHepevt = (my_hepevt*)addr;
}
//______________________________________________________________________________
StHepParticleMaster::~StHepParticleMaster()
{
  mgInst = 0;
  mgMyHepevt = 0;
  for (int i=0;i<(int)myVec.size();i++) { delete myVec[i]; }
  myVec.resize(0);
}
//______________________________________________________________________________
void StHepParticleMaster::Update() 
{
  mNTk = mgMyHepevt->nhep;
  if (mNTk <= (int)myVec.size()) return;
  for (int i = (int)myVec.size(); i < mNTk;i++) 
  {
    myVec.push_back(new StHepParticle(i));
  } 
}
//______________________________________________________________________________
const StHepParticleMaster *StHepParticleMaster::Instance() 
{
  assert(mgInst && "No new for StHepParticleMaster. Never,Jamais");
  return mgInst;
}  
//______________________________________________________________________________
const StHepParticle *StHepParticleMaster::operator()(int idx) const
{
  ((StHepParticleMaster*)this)->Update();
  assert(idx>=0 && "Never,Jamais");
  if (idx >= (int)myVec.size()) 	return 0;
  if (!myVec[idx]->GetStatusCode()) 	return 0;
  return myVec[idx];
}

//______________________________________________________________________________
//______________________________________________________________________________
   StHepParticle::StHepParticle(int idx):StGenParticle(idx) {}
//______________________________________________________________________________
int  StHepParticle::GetStatusCode()  const
{
  return StHepParticleMaster::mgMyHepevt->isthep[mIdx];
}
//______________________________________________________________________________
int  StHepParticle::GetPdgCode()     const
{
  return StHepParticleMaster::mgMyHepevt->idhep[mIdx];
}
//______________________________________________________________________________
const StHepParticle *StHepParticle::GetMother(int i)  const
{
  int j = StHepParticleMaster::mgMyHepevt->jmohep[mIdx][i];
  
  return (j) ? myVec[j-1]:0;
}
//______________________________________________________________________________
const StHepParticle *StHepParticle::GetDaughter(int i)  const
{
  int j0 = StHepParticleMaster::mgMyHepevt->jdahep[mIdx][0];
  if (!j0) 		return 0;
  int j1 = StHepParticleMaster::mgMyHepevt->jdahep[mIdx][1];
  j0+=i; if (j0>j1) 	return 0;
  return myVec[j0-1];
}

//______________________________________________________________________________
double  StHepParticle::GetMass()  const
{
  return StHepParticleMaster::mgMyHepevt->phep[mIdx][4];
}
//______________________________________________________________________________
int StHepParticle::GetNDaughters()  const
{
  int j0 = StHepParticleMaster::mgMyHepevt->jdahep[mIdx][0];
  if (!j0) 		return 0;
  int j1 = StHepParticleMaster::mgMyHepevt->jdahep[mIdx][1];
  return j1-j0+1;
}
//______________________________________________________________________________
void StHepParticle::Momentum(double p4[4]) const
{
  memcpy(p4,StHepParticleMaster::mgMyHepevt->phep[mIdx],4*sizeof(double));
}
//______________________________________________________________________________
void  StHepParticle::Vertex(double v[3]) const
{
  double *V = StHepParticleMaster::mgMyHepevt->vhep[mIdx];
  for (int i=0;i<3;i++) {v[i] = V[i]*10;}
}
//______________________________________________________________________________
double  StHepParticle::Time()  const
{
   return StHepParticleMaster::mgMyHepevt->vhep[mIdx][3]*10;
}

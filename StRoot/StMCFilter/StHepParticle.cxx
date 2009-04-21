// @(#)root/eg:$Id: StHepParticle.cxx,v 1.3 2009/04/21 19:10:51 perev Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include <stdlib.h>
#include <math.h>
#include <vector>

#include "StHepParticle.h"
my_hepevt*      StHepParticleMaster::mgMyHepevt=0;
StHepParticleMaster *StHepParticleMaster::mgInst    =0;
static std::vector<StHepParticle*> myVec;

//______________________________________________________________________________
/*
NMXHEP:
    maximum numbers of entries (particles) that can be stored in the common block.
    The default value of 4000 can be changed via the parameter construction. 
    In the translation, it is checked that this value is not exceeded.

NEVHEP:
    is normally the event number, but may have special meanings, according to the 
    description below:

    > 0 :
        event number, sequentially increased by 1 for each call to the main event 
	generation routine, starting with 1 for the first event generated. 
    = 0 :
        for a program which does not keep track of event numbers, as some of the 
	PYTHIA routines. 
    = -1 :
        special initialization record; not used by PYTHIA. 
    = -2 :
        special final record; not used by PYTHIA. 

NHEP:
    the actual number of entries stored in the current event. These are found in 
    the first NHEP positions of the respective arrays below. Index IHEP, 
    1$\leq$IHEP$\leq$NHEP, is used below to denote a given entry.

ISTHEP(IHEP):
    status code for entry IHEP, with the following meanings:

    = 0 :
        null entry. 
    = 1 :
        an existing entry, which has not decayed or fragmented. This is the main 
	class of entries, which represents the `final state' given by the generator. 
    = 2 :
        an entry which has decayed or fragmented and is therefore not appearing 
	in the final state, but is retained for event history information. 
    = 3 :
        a documentation line, defined separately from the event history. 
	This could include the two incoming reacting particles, etc. 
    = 4 - 10 :
        undefined, but reserved for future standards. 
    = 11 - 200 :
        at the disposal of each model builder for constructs specific to his program, 
	but equivalent to a null line in the context of any other program. 
    = 201 - :
        at the disposal of users, in particular for event tracking in the detector. 

IDHEP(IHEP) :
    particle identity, according to the PDG standard. The four additional codes 91-94 
    have been introduced to make the event history more legible, see section [*] and 
    the MSTU(16) description of how daughters can point back to them.

JMOHEP(1,IHEP) :
    pointer to the position where the mother is stored. The value is 0 for initial entries.

JMOHEP(2,IHEP) :
    pointer to position of second mother. Normally only one mother exists, 
    in which case the value 0 is to be used. In PYTHIA, entries with codes 91-94 
    are the only ones to have two mothers. The flavour contents of these objects, 
    as well as details of momentum sharing, have to be found by looking at the 
    mother partons, i.e. the two partons in positions JMOHEP(1,IHEP) and JMOHEP(2,IHEP) 
    for a cluster or a shower system, and the range JMOHEP(1,IHEP)-JMOHEP(2,IHEP) 
    for a string or an independent fragmentation parton system.

JDAHEP(1,IHEP) :
    pointer to the position of the first daughter. If an entry has not decayed, this is 0.

JDAHEP(2,IHEP) :
    pointer to the position of the last daughter. If an entry has not decayed, this is 0. 
    It is assumed that daughters are stored sequentially, so that the whole range 
    JDAHEP(1,IHEP)-JDAHEP(2,IHEP) contains daughters. 
    This variable should be set also when only one daughter is present, as in 
    $\mathrm{K}^0 \to \mathrm{K}_{\mathrm{S}}^0$ decays, so that looping from the first 
    daughter to the last one works transparently. Normally daughters are stored after mothers, 
    but in backwards evolution of initial-state radiation the opposite may appear, 
    i.e. that mothers are found below the daughters they branch into. 
    Also, the two daughters then need not appear one after the other, but may be 
    separated in the event record.

PHEP(1,IHEP) :
    momentum in the $x$ direction, in GeV/$c$.

PHEP(2,IHEP) :
    momentum in the $y$ direction, in GeV/$c$.

PHEP(3,IHEP) :
    momentum in the $z$ direction, in GeV/$c$.

PHEP(4,IHEP) :
    energy, in GeV.

PHEP(5,IHEP) :
    mass, in GeV/$c^2$. For space-like partons, it is allowed to use a negative mass, 
    according to PHEP(5,IHEP) $ = -\sqrt{-m^2}$.

VHEP(1,IHEP) :
    production vertex $x$ position, in mm.

VHEP(2,IHEP) :
    production vertex $y$ position, in mm.

VHEP(3,IHEP) :
    production vertex $z$ position, in mm.

VHEP(4,IHEP) :
    production time, in mm/$c$ ( $\approx 3.33 \times 10^{-12}$ s). 

*/

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
  assert(!mgInst);
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
  assert(mgInst);
  return mgInst;
}  
//______________________________________________________________________________
const StHepParticle *StHepParticleMaster::operator()(int idx) const
{
  ((StHepParticleMaster*)this)->Update();
  assert(idx>=0);
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

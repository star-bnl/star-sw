// @(#)root/eg:$Id: StGenParticle.cxx,v 1.5 2009/05/09 00:44:58 perev Exp $
// Author: Victor Perev  17/03/2009

/*!
\class  StGenParticle 
and derived StHepParticle,StGENParticle are based on HEPEVT standard
(http://cepa.fnal.gov/psm/simulation/mcgen/lund/pythia_manual/pythia6.3/pythia6301/node39.html)
Description of HEPEVT is followed

<p>
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

<p>
NHEP:
    the actual number of entries stored in the current event. These are found in 
    the first NHEP positions of the respective arrays below. Index IHEP, 
    1$\leq$IHEP$\leq$NHEP, is used below to denote a given entry.

<p>
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

<p>
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

<p>
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

<p>
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
//______________________________________________________________________________
#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include <map>

#include "StGenParticle.h"
//______________________________________________________________________________
void StGenParticle::Print(const char *opt) const
{
static int nCall=0; nCall++;
  printf("%4d - ",GetIdx());
  printf("Ist=%1d",GetStatusCode());
  printf(" Pdg=%5d",GetPdgCode());
  printf(" Gea=%3d",GetGeaCode());

  double V[3];  Vertex(V); printf(" Z=%5.1f ",V[2]);

  if (GetNDaughters()) printf("\tKids=%d",GetNDaughters());

  int moth1 = -1,moth2=-1;
  const StGenParticle *m = GetMother(0);
  if (m) moth1 = m->GetIdx();
  m = GetMother(1);
  if (m) moth2 = m->GetIdx();

  if (moth1>=0 || moth2>=0) {
    printf("\tMoth=(");
    if (moth1>=0) {printf("%d ",moth1);} else {printf("_ ");}
    if (moth2>=0) {printf("%d ",moth2);} else {printf("_" );}
    printf(")");
  }


  printf("\n");
}
//______________________________________________________________________________
double  StGenParticle::GetCalcMass()  const
{
  double p[4];
  Momentum(p);
  double m =p[3]*p[3]-(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  return (m>0) ? sqrt(m) : -sqrt(-m);
}
//______________________________________________________________________________
double StGenParticle::R()  const 
{ 
  double x[3]; Vertex(x); return sqrt(x[0]*x[0]+x[1]*x[1]);

}
//______________________________________________________________________________
double StGenParticle::Rho()  const 
{ 
  double x[3]; Vertex(x); return sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);                   

}
//______________________________________________________________________________
double StGenParticle::P()  const 
{ 
  double p[4]; Momentum(p); return sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);

}
//______________________________________________________________________________
double StGenParticle::Pt()  const 
{ 
  double p[4]; Momentum(p); return sqrt(p[0]*p[0]+p[1]*p[1]);      

}
//______________________________________________________________________________
double StGenParticle::Energy()  const 
{ 
  double p[4]; Momentum(p); return p[3];                  

}
//______________________________________________________________________________
double StGenParticle::Eta()  const
{
  double p[4]; Momentum(p);                 
  double pmom = P();
  if (pmom > fabs(p[2])) return 0.5*log((pmom+p[2])/(pmom-p[2]));
  else                   return 1.e30;
}
//______________________________________________________________________________
double StGenParticle::Phi()  const
{
  double p[4]; Momentum(p);                 
  return atan2(p[1],p[0]);
}  
//______________________________________________________________________________
double StGenParticle::Theta()  const
{
  double p[4]; Momentum(p);                 
  return acos(p[2]/P());
}  
int  StGenParticle::GetPdgCode() const { return StGenParticleMaster::Gea2Pdg(GetGeaCode());}
int  StGenParticle::GetGeaCode() const { return StGenParticleMaster::Pdg2Gea(GetPdgCode());}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
void StGenParticleMaster::Print(const char *tit) const
{
  if (!tit) tit = "";
   printf("StGenParticleMaster::Print(%s)\n",tit);
   const StGenParticle *p=0;
   for (int i=0;p=(*this)(i);i++) {
     p->Print();
   }
   printf("\n");

}

// *
// * In rough order of multiplicity: gamma,pi+,pi-,pi0,etc
// *                -   gamma  pi+   pi-   pi0   K0L   K+   K-    K0S   Eta
// *               prot  neut  ~p    ~n    e+    e-   nu_e ~nu_e  mu+   mu-
// *              lamda sigm+ sigm0 sigm-  xi0   xi- omeg-
// *           ~: lamda sigm- sigm0 sigm+  xi0   xi+ omeg+
// *              tau+  tau-    D+    D-    D0  ~D0   DS+   DS-  LamC+
// *                W+    W-    Z0   nu_m ~nu_m nu_t ~nu_t  geant geant geant
// *              Deutron Triton Alpha HE3
enum {kNGEA = 50};
static const int IDGEA[] = {
                0,    1,    8,    9,    7,   10,   11,  12,   16,   17,
                14,   13,   15,   25,    2,    3,   4,    4,    5,    6,
                18,   19,   20,   21,   22,   23,  24,
                26,   27,   28,   29,   30,   31,  32,
                33,   34,   35,   36,   37,   38,  39,   40,   41,
                42,   43,   44,    4,    4,    4,   4,   48,   48,   48,
                45,    46,    47,    49};

static const int IDPDG[] = {
                 0,   22,  211, -211,  111,  130,  321, -321, 310,  221,
              2212, 2112,-2212,-2112,  -11,   11,  -12,  12,  -13,   13,
              3122, 3222, 3212, 3112, 3322, 3312, 3334,
             -3122,-3222,-3212,-3112,-3322,-3312,-3334,
               -15,   15,  411, -411,  421, -421,  431, -431, 4122,
                24,  -24,   23,  -14,   14,  -16,   16,   71,   72,  75,
            700201,700301,700202,700302};
//______________________________________________________________________________
int StGenParticleMaster::Gea2Pdg(int igea)  
{

static int PdgOfGea[kNGEA]={0};
  
  if (! PdgOfGea[1]) {
    for (int i = 0;i<kNGEA; i++) {  PdgOfGea[IDGEA[i]] = IDPDG[i]; }
  }
  if (igea >=kNGEA)	return 0;
  return PdgOfGea[igea];
}
//______________________________________________________________________________
double StGenParticleMaster::Gea2Mas(int igea)  
{
static double mass[kNGEA]={
	 0		,0		,0.00051	,0.00051	,0
	,0.10566	,0.10566	,0.13498	,0.13957	,0.13957
	,0.49767	,0.4936		,0.4936		,0.93957	,0.93827
	,0.93827	,0.49767	,0.54745	,1.11568	,1.18937
	,1.19255	,1.19744	,1.3149		,1.3213		,1.67245
	,0.93957	,1.11568	,1.18937	,1.19255	,1.19744
	,1.3149		,1.3213		,1.67245	,1.777		,1.777
	,1.8693		,1.8693		,1.8645		,1.8645		,1.9685
	,1.9685		,2.2849		,80.33		,80.33		,91.187
	,0		,0		,0		,0		,0};
	
  if (igea>=kNGEA) return 0.;
  return mass[igea];
}

//______________________________________________________________________________
int StGenParticleMaster::Pdg2Gea(int ipdg)  
{
typedef std::map<int, int > pdgMap_t;
static  pdgMap_t pdgMap;
static  int once = 2009;

  if (once) { once=0; for (int i=0;i<kNGEA;i++) {pdgMap[IDPDG[i]] = IDGEA[i];}}  
  
  pdgMap_t::iterator it = pdgMap.find(ipdg);
  if (it == pdgMap.end()) return 0;
  return (*it).second;
}  

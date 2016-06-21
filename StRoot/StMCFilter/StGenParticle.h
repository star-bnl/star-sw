// @(#)STAR/eg:$Id: StGenParticle.h,v 1.6 2016/06/21 20:28:51 jwebb Exp $

/*!
  \class  StGenParticle 
  \author Victor Perev 
  \date   17/03/2009

  StGenParticle and derived StHepParticle,StGENParticle are based on HEPEVT 
  standard (http://cepa.fnal.gov/psm/simulation/mcgen/lund/pythia_manual/pythia6.3/pythia6301/node39.html)
  Description of HEPEVT follows

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


#ifndef ROOT_StGenParticle
#define ROOT_StGenParticle


/// Abstract base class for particles related to common /HEPEVT/
class StGenParticle  {

protected:
                                // ****** constructors and destructor
   StGenParticle(int idx=0){mIdx=idx;}

   virtual ~StGenParticle(){;}

void SetIdx(int idx) { mIdx =  idx;}
int  GetIdx() const  { return mIdx;}

public:

virtual void                 Print(const char *opt="") const;
virtual int          	     GetStatusCode()     const=0;
virtual int          	     GetPdgCode()        const;
virtual int          	     GetGeaCode()        const;
virtual const StGenParticle *GetMother(int i=0)  const=0; 
virtual const StGenParticle *GetDaughter(int i)  const=0; 
virtual       double         GetCalcMass     ()  const;
virtual       double         GetMass         ()  const=0;
virtual       int            GetNDaughters   ()  const=0;
virtual       double         GetWeight       ()  const { return 1.;}
virtual       void           Momentum(double p4[4])   const=0 ;
virtual       void           Vertex(double v[3]) const=0;
virtual       double         Time() const=0;
              int            IsPrimary       ()  const { return !GetMother(0);} //Is this particle primary one?
              int            IsFinal         ()  const { return !GetStatusCode()==1;} //Is this particle final one?

double        R     () const;
double        Rho   () const;
double        P     () const;
double        Pt    () const;
double        Energy() const;
double        Eta   () const;
double        Phi   () const;
double        Theta () const;

protected:

  int mIdx;
};

class StGenParticleMaster  {


protected:
                                // ****** constructors and destructor
  StGenParticleMaster() : mNTk(0) {;}

public:
int Size() const { return mNTk;}

virtual ~StGenParticleMaster(){;}
virtual void Update()=0;
virtual void Print(const char *tit) const;

virtual const StGenParticle *operator()(int idx) const=0;
static int    Gea2Pdg(int igea);
static int    Pdg2Gea(int ipdg); 
static double Gea2Mas(int igea);  


protected:
int mNTk;


};

#endif


// @(#)STAR/eg:$Id: StHepParticle.h,v 1.1 2009/04/10 19:59:20 perev Exp $
// Author: V.Perev  Mar/2009
////////////////////////////////
//                                                                      //
// StHepParticle: defines  equivalent of HEPEVT particle                //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_StHepParticle
#define ROOT_StHepParticle
#include "StGenParticle.h"

class StHepParticle :public StGenParticle {
friend class StHepParticles;

public:
                                // ****** constructors and destructor
   StHepParticle(int idx=0);

   virtual ~StHepParticle(){;}

public:

virtual int          	     GetStatusCode()     const;
virtual int          	     GetPdgCode()        const;
virtual const StHepParticle *GetMother(int i=0)  const; 
virtual const StHepParticle *GetDaughter(int i)  const; 
virtual       double         GetMass         ()  const;
virtual       int            GetNDaughters   ()  const;
virtual       void           Momentum(double p4[4])   const ;
virtual       void           Vertex(double v[3]) const;
virtual       double         Time() 		 const;
protected:

};

class my_hepevt;
class StHepParticles :public StGenParticles {
friend class StHepParticle;
public:
StHepParticles(void *addr);
virtual ~StHepParticles();
static const StHepParticles *Instance();
virtual const StHepParticle *operator()(int idx) const;
void Update() ;

private:

static my_hepevt *mgMyHepevt;
static StHepParticles* mgInst;
};

#endif


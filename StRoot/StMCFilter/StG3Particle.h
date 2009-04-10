// @(#)STAR/eg:$Id: StG3Particle.h,v 1.1 2009/04/10 19:59:20 perev Exp $
// Author: V.Perev  Mar/2009
////////////////////////////////
//                                                                      //
// StG3Particle: defines  equivalent of HEPEVT particle                //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_StG3Particle
#define ROOT_StG3Particle
#include "StGENParticle.h"

//      SUBROUTINE GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
//      SUBROUTINE GFVERT(NVTX,V,NTBEAM,NTTARG,TOFG,UBUF,NWBUF)
typedef void (*GFKINE_t) (int &,float*,float*,int &, int   &,int *,int &);
typedef void (*GFVERT_t) (int &,float*,int  &,int &, float &,int *,int &);

class StG3Particles :public StGenParticles {
public:
StG3Particles(GFKINE_t fk,GFVERT_t fv);
virtual ~StG3Particles();
static const StG3Particles *Instance();
virtual const StGenParticle *operator()(int idx) const;
void Update() ;

private:
static StG3Particles* mgInst;
static GFKINE_t mgFK;
static GFVERT_t mgFV;

};

#endif


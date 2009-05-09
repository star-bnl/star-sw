// @(#)STAR/eg:$Id: StG3Particle.h,v 1.4 2009/05/09 00:44:58 perev Exp $
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

/// Master class for StGENParticle filled from GEANT3 internal structures
class StG3ParticleMaster :public StGenParticleMaster {
public:
         StG3ParticleMaster(GFKINE_t fk,GFVERT_t fv);
virtual ~StG3ParticleMaster();
static  const StG3ParticleMaster *Instance();
virtual const StGenParticle *operator()(int idx) const;
   void Update() ;

private:
static StG3ParticleMaster* mgInst;
static GFKINE_t mgFK;
static GFVERT_t mgFV;

};

#endif


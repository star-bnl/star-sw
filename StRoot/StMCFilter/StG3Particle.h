/*!
 * \class StG3ParticleMaster 
 * \author V.Perev  Mar/2009
 * 
 * Defines  equivalent of HEPEVT particle  
 */
// @(#)STAR/eg:$Id: StG3Particle.h,v 1.6 2009/06/23 19:52:05 jeromel Exp $


#ifndef ROOT_StG3Particle
#define ROOT_StG3Particle
#include "StGENParticle.h"

/// class StG3ParticleMaster is a implementation of container of tracks 
/// in HEPEVT style:
/// http://cepa.fnal.gov/psm/simulation/mcgen/lund/pythia_manual/pythia6.3/pythia6301/node39.html
/// It is interface to Geant3 tracks and vertices 
///

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


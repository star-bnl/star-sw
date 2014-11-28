//////////////////////////////////////////////////////////////////////
//
// $Id: StppParticle.cxx,v 1.3 2003/09/11 18:14:18 thenry Exp $ 
// $Log: StppParticle.cxx,v $
// Revision 1.3  2003/09/11 18:14:18  thenry
// *** empty log message ***
//
// Revision 1.2  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2002/01/16 20:22:53  akio
// First version
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
// First Version of StppParticle
//
//////////////////////////////////////////////////////////////////////
//
// StppParticle
//
// geant track class for Spin pp uDst
//
//////////////////////////////////////////////////////////////////////
#include <iostream>
#include <math.h>
#include "StppParticle.h"
#include "tables/St_g2t_track_Table.h"

ClassImp(StppParticle)

StppParticle::StppParticle(){}
StppParticle::~StppParticle(){}

#ifndef __CINT__
StppParticle::StppParticle(g2t_track_st* trk){
   set(trk);
}

void StppParticle::set(g2t_track_st* trk) {
   pid    = trk->ge_pid ;
   key    = trk->id  ;
   pt     = trk->pt  ;
   eta    = trk->eta ;
   e      = trk->e ;
   psi    = atan2(trk->p[1],trk->p[0]);
   if(psi<0) psi += 2. * M_PI ;
   nTpcHits = trk->n_tpc_hit ;
   nFtpHits = trk->n_ftp_hit ;
   nSvtHits = trk->n_svt_hit ;
   charge = (Short_t)trk->charge ;
}
#endif /*__CINT__*/

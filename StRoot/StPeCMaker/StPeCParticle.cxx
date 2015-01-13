//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/15           Pablo Yepes: yepes@rice.edu
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <math.h>
#include "StPeCParticle.h"
#include "tables/St_g2t_track_Table.h"


ClassImp(StPeCParticle)

StPeCParticle::StPeCParticle() {
}

StPeCParticle::~StPeCParticle() {
}

#ifndef __CINT__
StPeCParticle::StPeCParticle ( g2t_track_st* trk) {
   set ( trk ) ;
}
void StPeCParticle::set ( g2t_track_st* trk) {

   pid     = trk->ge_pid ;
   key    = trk->id  ;
   pt     = trk->pt  ;
   eta    = trk->eta ;
   e      = trk->e ;
   psi    = atan2(trk->p[1],trk->p[0]);
   if ( psi < 0 ) psi += 2. * M_PI ;
   vertexId = trk->start_vertex_p ;
   
   nTpcHits = trk->n_tpc_hit ;
   nFtpHits = trk->n_ftp_hit ;
   nSvtHits = trk->n_svt_hit ;

   charge = (Short_t)trk->charge ;
}

StPeCParticle::StPeCParticle ( StMuMcTrack* trk) {
   set ( trk ) ;
}
void StPeCParticle::set ( StMuMcTrack* trk) {

  pid     = trk->GePid() ;
  key    = trk->Id()  ;
  pt     = trk->pT()  ;
  eta    = trk->Eta() ;
  e      = trk->E() ;
  psi    = atan2(trk->Pxyz().x(),trk->Pxyz().y());
   if ( psi < 0 ) psi += 2. * M_PI ;
   vertexId = trk->IdVx() ;
   
   nTpcHits = trk->No_tpc_hit() ;
   nFtpHits = trk->No_ftp_hit() ;
   nSvtHits = trk->No_svt_hit() ;

   charge = (Short_t)trk->Charge() ;
}
#endif /*__CINT__*/





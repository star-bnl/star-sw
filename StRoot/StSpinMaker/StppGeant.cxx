//////////////////////////////////////////////////////////////////////
//
// $Id: StppGeant.cxx,v 1.5 2007/07/12 19:59:43 fisyak Exp $
// $Log: StppGeant.cxx,v $
// Revision 1.5  2007/07/12 19:59:43  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.4  2006/11/06 20:39:18  perev
// using std added
//
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
//
//////////////////////////////////////////////////////////////////////
#include "Stiostream.h"
using namespace std;
#include <math.h>
#include "StppGeant.h"
#include "StppParticle.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

ClassImp(StppGeant)

StppGeant::StppGeant(){
  gParticles = new TClonesArray ("StppParticle",400);  //warning hardcoded # of track limits!
  infoLevel = 0;
  clear();
}

StppGeant::~StppGeant() {
  clear();
  delete gParticles;
}

void StppGeant::clear ( ) {
  gXVertex = 0.0;
  gYVertex = 0.0;
  gZVertex = 0.0;
  gNParticle = 0;
  gLCP = 0;
  gParticles->Clear();
}

Int_t StppGeant::fill(TDataSet* geant){
  //get g2t tracks
  St_g2t_track*  trk = 0 ;
  trk = (St_g2t_track *)geant->Find("g2t_track") ;
  if(!trk){
    cout << "StppGeant::fill: tracks not found" << endl;
    return 1 ;
  }
  int nTracks = trk->GetNRows() ; 
  g2t_track_st* trkT = trk->GetTable() ;

  // fill tracks
  gNParticle = 0 ;
  float maxpt = 0.0;
  for (int i=0; i<nTracks; i++ ) {
    if(trkT[i].start_vertex_p != 1) continue; //Take only triggerd event tracks. Is this OK?
    StppParticle *p = new((*gParticles)[gNParticle]) StppParticle(&(trkT[i])) ;   
    if(p->pt > maxpt && p->charge != 0){
      maxpt = p->pt;
      gLCP = p;
    }
    gNParticle++; 
  }  
  
  //get vertex
  St_g2t_vertex*  vtx = 0 ;
  vtx = (St_g2t_vertex *)geant->Find("g2t_vertex") ;
  if (!vtx){
    cout << "StppGeant::fill: vertex not found" << endl;
    return 1 ;
  }
  g2t_vertex_st* vtxT = vtx->GetTable() ;
  gXVertex = vtxT[0].ge_x[0] ;
  gYVertex = vtxT[0].ge_x[1] ;
  gZVertex = vtxT[0].ge_x[2] ;  

  return 0 ;
}

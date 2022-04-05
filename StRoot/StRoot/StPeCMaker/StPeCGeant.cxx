//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/18 yepes 
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <math.h>
#include "StPeCGeant.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"


ClassImp(StPeCGeant)

StPeCGeant::StPeCGeant() {
  pPart  = new TClonesArray ("StPeCParticle", 10);
  nPart  = 0 ;
}

StPeCGeant::~StPeCGeant() {
  delete pPart ;
}

void StPeCGeant::clear() {
   nPart = 0 ;
   gPt   = 0 ;
   gPz   = 0 ;
   gEta  = 0 ;
   gMass = 0 ;
   gY    = 0 ;
   gPsi   = 0 ;
   gZVertex = 0 ;

   pPart->Clear();
}

Int_t StPeCGeant::fill ( TDataSet* geant ) {
//
//  fill our local Track class
//
   St_g2t_track*  trk = 0 ;
   trk = (St_g2t_track *)geant->Find("g2t_track") ;

// TDataSet* jaja ;
// Bool_t a = 1 ;
// TDataSetIter tI(jaja,a);
//   St_g2t_track   *g2t_trackTablePointer   =  (St_g2t_track *)   geantDstI("g2t_track");
// geantDstI("g2t_tpc_hit");


   if ( !trk ) {
      printf ( "StPeCGeant::fill: tracks not found \n" ) ; 
      return 1 ;
   }

   int nTracks = trk->GetNRows() ; 

   g2t_track_st* trkT = trk->GetTable() ;


   float px = 0 ;
   float py = 0 ;
        gPz = 0 ;
   float e  = 0 ;
   nPart    = 0 ;
   TClonesArray &pParticle = *pPart ;

   int vert ;
   printf ( "StPeCGeant::fill: %i tracks found \n", nTracks ) ; 

   for ( int i = 0 ; i < nTracks ; i++ ) {
      new(pParticle[nPart++]) StPeCParticle(&(trkT[i])) ;
      
      vert = trkT[i].start_vertex_p ;
      if ( vert != 1 ) continue ;
      px  += trkT[i].p[0];
      py  += trkT[i].p[1];
      gPz += trkT[i].p[2];
      e   += trkT[i].e   ;
   }
   
   gPt  = ::sqrt(px*px+py*py);
   gPsi = atan2(py,px);
   if ( gPsi < 0 ) gPsi += M_PI ;
   gMass = ::sqrt(e*e-gPt*gPt-gPz*gPz);

   float theta = atan2(gPt,gPz);
   gEta = -::log(tan(theta/2.)) ;
   gY   = 0.5*::log((e+gPz)/(e-gPz));

   St_g2t_vertex*  vtx = 0 ;
   vtx = (St_g2t_vertex *)geant->Find("g2t_vertex") ;
   if ( !vtx ) {
      printf ( "StPeCGeant::fill: vertex not found \n" ) ; 
      return 1 ;
   }
   g2t_vertex_st* vtxT = vtx->GetTable() ;

   gZVertex = vtxT->ge_x[2] ;
   
   return 0 ;
}

Int_t StPeCGeant::fill ( StMuDst * mu  ) {
//
//  fill  McEvent class to access StMuMcTracks
//
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
     LOG_INFO << "StPeCGeant::fill: using MuDst ++++++++++++++++++++++++++++++++++++++++++++++++++++---------- "  << endm;
   if ( !muEvent ) {
      printf ( "StPeCGeant::fill: MC event not found \n" ) ; 
      return 1 ;
   }

    TClonesArray *MuMcTracks     = mu->mcArray(1); 
    TClonesArray *MuMcVertices   = mu->mcArray(0);

   int nTracks = MuMcTracks->GetEntriesFast(); 
   LOG_INFO << "StPeCGeant::fill: "<<nTracks<<"  tracks found "<<endm;

   float px = 0 ;
   float py = 0 ;
        gPz = 0 ;
   float e  = 0 ;
   nPart    = 0 ;
   TClonesArray &pParticle = *pPart ;

   int vert ;

for (Int_t kg = 0; kg < nTracks; kg++) {
  StMuMcTrack *mcTrack = (StMuMcTrack *) MuMcTracks->UncheckedAt(kg);

  new(pParticle[nPart++]) StPeCParticle(mcTrack) ;

  LOG_INFO << "StPeCGeant::fill Geant pid: "<<mcTrack->GePid()<<endm;  
  //       vert = trkT[i].start_vertex_p ;
  //       if ( vert != 1 ) continue ;
  if(mcTrack->GePid()==8 || mcTrack->GePid()==9){
    px  += mcTrack->Pxyz().x();
    py  += mcTrack->Pxyz().y();
    gPz += mcTrack->Pxyz().z();
    e   += mcTrack->E();
  }
 }
   
   gPt  = ::sqrt(px*px+py*py);
   gPsi = atan2(py,px);
   if ( gPsi < 0 ) gPsi += M_PI ;
   gMass = ::sqrt(e*e-gPt*gPt-gPz*gPz);

   float theta = atan2(gPt,gPz);
   gEta = -::log(tan(theta/2.)) ;
   gY   = 0.5*::log((e+gPz)/(e-gPz));

//    St_g2t_vertex*  vtx = 0 ;
//    vtx = (St_g2t_vertex *)geant->Find("g2t_vertex") ;
//    if ( !vtx ) {
//       printf ( "StPeCGeant::fill: vertex not found \n" ) ; 
//       return 1 ;
//    }
//    g2t_vertex_st* vtxT = vtx->GetTable() ;

    StMuMcVertex *mcVertex = (StMuMcVertex *) MuMcVertices->UncheckedAt(0);

   gZVertex = mcVertex->XyzV().z() ;
   
   return 0 ;
}




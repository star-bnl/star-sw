// $Id: St_l3t_Maker.cxx,v 1.20 2000/02/23 21:55:40 yepes Exp $
// $Log: St_l3t_Maker.cxx,v $
// Revision 1.20  2000/02/23 21:55:40  yepes
// fix problem with null data set
//
// Revision 1.19  2000/02/23 21:39:21  yepes
// fix MakeOnline for case when no Online data
//
// Revision 1.18  2000/02/09 20:05:31  yepes
// modifications to accomodate new gl3 including analysis modules
//
// Revision 1.17  2000/02/01 18:37:54  yepes
// tphit table filled now with l3 clusters including track associated with hit
//
// Revision 1.16  2000/01/26 18:59:18  yepes
//
// Revision 1.15  1999/12/23 18:09:07  yepes
// Double interface to read DAQ format or tpchit_st from tpc
//
// Revision 1.14  1999/12/16 21:01:39  flierl
// feed tracker with banks instead of tcl_tphit structs
//
// Revision 1.13  1999/11/18 17:08:54  fisyak
// Add protection against absense of tphit
//
// Revision 1.12  1999/08/13 17:15:59  yepes
// *** empty log message ***
//
// Revision 1.11  1999/08/12 15:42:42  yepes
// Change printing
//
// Revision 1.10  1999/08/11 20:02:14  yepes
// *** empty log message ***
//
// Revision 1.10 1999/08/10 yepes
// set some new parameters for ftfTpc
//
// Revision 1.9  1999/07/15 13:58:14  perev
// cleanup
//
// Revision 1.8  1999/05/21 21:15:59  yepes
// Fixint problem with no Hits
//
// Revision 1.7  1999/05/05 18:37:21  yepes
// *** empty log message ***
//
// Revision 1.6  1999/05/05 18:30:16  yepes
// define maximum number of tracks as NHits/20
//
// Revision 1.5  1999/04/17 22:37:34  fisyak
// remove annoying printf
//
// Revision 1.4  1999/03/12 15:27:32  perev
// New maker schema
//
// Revision 1.3  1999/02/26 17:25:11  kathy
// fix histograms
//
// Revision 1.2  1999/02/19 14:39:31  fisyak
// New version from Pablo, tpc safe
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_l3t_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_l3t_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_XDFFile.h"
#include "tpc/St_tpt_Module.h"
#include "FtfSl3.h"
#include "gl3Conductor.h"
#include "gl3GeneralHistos.h"
#include "gl3JPsi.h"
#include "gl3GammaGamma.h"
#include "gl3dEdx.h"
#include "gl3HighPt.h"
#include "TH1.h"
#include "tables/St_hitarray_Table.h"
ClassImp(St_l3t_Maker)
  
  //_____________________________________________________________________________
St_l3t_Maker::St_l3t_Maker(const char *name):
    StMaker(name)
{
}
//_____________________________________________________________________________
St_l3t_Maker::~St_l3t_Maker(){
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Init(){
  // Create tables
  St_DataSetIter       local(GetInputDB("params/l3"));
//
  m_l3_hits_on_track = new TH1F("L3tL3trackNumHits","Number of hits on reconstructed tracks",50,.5,50.5);
  m_l3_azimuth       = new TH1F("L3tL3trackPhi","Azimuthal distribution of tracks",60,0.,360.0);
  m_l3_tan_dip       = new TH1F("L3tL3trackTanDip","Distribution of the dip angle",100,-1.5,1.5);
  m_l3_r0            = new TH1F("L3tL3trackR0","Radius for track parameters",100,0.0,200.);
  m_l3_z0            = new TH1F("L3tL3trackZ0","Z      for track parameters",100,-200.,200.);
  m_l3_pt            = new TH1F("L3tL3trackPt","Track pt                   ",50,0.,2.5);
  m_l3_XyChi2        = new TH1F("L3tL3XyChi2 ","Track xy Chi2              ",50,0.,50.);
  m_l3_SzChi2        = new TH1F("L3tL3SzChi2 ","Track sz Chi2              ",50,0.,50.);
  m_l3_nHitsSector   = new TH1F("L3tL3HitsSector","# hits per sector       ",50,0.,10000.);
  m_l3_nTracksSector = new TH1F("L3tL3NTracksSector","# Tracks per sector   ",50,0.,1000.);
  m_l3_cpuTimeSector = new TH1F("L3tL3CpuTimeSector","CPU Time per sector(ms)",100,0.,200.);
  m_l3_realTimeSector = new TH1F("L3tL3RealTimeSector","Real Time per sector(ms)",100,0.,200.);
//
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Make(){
//
//   Depending on m_Mode the online or offline makers are used.
//
   if ( m_Mode == 0 ) {
      if ( MakeOnLine() == kStWarn ) return MakeOffLine();
      return kStOk ;
   }
   else if ( m_Mode == 1 ) {
      if ( MakeOnLine() == kStWarn ) return kStErr ;
   }
   else if ( m_Mode == 2 ) {
      if ( MakeOffLine() == kStWarn ) return kStErr ;
   }
   else {
      fprintf ( stderr, "St_l3t_Maker:Make: m_Mode %d invalid option \n", m_Mode ) ;
      fprintf ( stderr, "                   Nothing done!!!" ) ;
   }
   return kStOk ;
}
 //_____________________________________________________________________________
Int_t St_l3t_Maker::MakeOnLine(){


// get l3 dataset
   St_DataSet* sec_bank_set = 0 ;
   sec_bank_set = GetInputDS("l3Clufi");
   if ( !sec_bank_set ) {
      fprintf ( stderr, "St_l3t_Maker:MakeOnLine: no L3 data \n" ) ;
      return kStWarn;
   }

   if ( !sec_bank_set->GetListSize() ) {
      fprintf ( stderr, "St_l3t_Maker:MakeOnLine: no L3 data \n" ) ;
      return kStWarn;
   }
   fprintf ( stderr, "St_l3t_Maker:MakeOnLine: Online space points as input for L3T \n" ) ;
//
//    Create tracker and gl3 objects
//
   FtfSl3   tracker ;
   gl3Conductor gl3 ;
   gl3GeneralHistos  fillHistoModule ;
   gl3JPsi           jPsiM ;
   gl3GammaGamma     gammaGammaM ;
   gl3dEdx           dEdxM ;
   gl3HighPt         highPtM ;

   gl3.add ( &fillHistoModule ) ;
   gl3.add ( &jPsiM   ) ;
   gl3.add ( &gammaGammaM ) ;
   gl3.add ( &dEdxM ) ;
   gl3.add ( &highPtM ) ;

   gl3.init();

   int const maxBytes = 500000 ;
   char   buffer[maxBytes] ;
   L3_P* gl3Header  = (L3_P *)buffer ;
   memset ( buffer, 0, sizeof(L3_P) ) ;
   int   nBytesUsed  = sizeof(L3_P);
   char* trackDataPointer = buffer + nBytesUsed  ;
//
//    Set parameters
//
   tracker.setup (  ) ;
// tracker.para.infoLevel = 10 ;
   for ( int ie = 0 ; ie < gl3.nEvents ; ie++ ) gl3.event[ie].bField = 0.5 ;
   tracker.reset();
//
//    Create hit table to store L3 clusters in offline format
//
   int maxHits = 300000 ;
   int nHits = 0 ;
   int token ;
   St_tcl_tphit *hitS = new St_tcl_tphit("l3Hit",maxHits); 
   m_DataSet->Add(hitS);
   tcl_tphit_st*  hit  = (tcl_tphit_st  *)hitS->GetTable();
   table_head_st* hitH = (table_head_st *)hitS->GetHeader(); 

// create iterator

   St_DataSetIter sec_bank_iter(sec_bank_set);
//
//    Loop over hits_in_sec_xx 
//
   Char_t secname[15] = "hits_in_sec_00";
   for(Int_t secIndex=1;secIndex<=12; secIndex++) {
      if ( secIndex < 10 ) sprintf ( &(secname[13]), "%1d", secIndex ) ;
      else sprintf ( &(secname[12]), "%2d", secIndex ) ;
//
//    Get hit array (=bank) and check it is there
//
      St_hitarray* bankEntries = 0 ;
      bankEntries = (St_hitarray*)sec_bank_iter(secname);
      if ( !bankEntries ) {
         fprintf ( stderr, "St_l3t_Maker:MakeOnLine: no L3 data for Supersector %d\n",
                   secIndex ) ;
         continue ; 
      }
//
//   Get table and check it is there
//
      hitarray_st* bankEntriesSt = 0 ;
      bankEntriesSt = (hitarray_st*) bankEntries->GetTable();
      if ( !bankEntriesSt ){ 
         fprintf ( stderr, 
                   "St_l3t_Maker:MakeOnLine: no L3 data table for Supersector %d\n",
                   secIndex ) ;
         continue ; 
      }
//
// Read clusters in DAQ format
//
      tracker.readSector((TPCSECLP *)bankEntriesSt) ;
      if ( tracker.nHits < 1 ) continue ;
//
//    Call tracker
//
      tracker.processSector();
//
//      Write online track buffer
//
      token = ((TPCSECLP *)bankEntriesSt)->bh.token ;
      int nBytes = tracker.fillTracks ( maxBytes-nBytesUsed, trackDataPointer,
	                                token ) ;
      if ( nBytes < 0 ) continue ;
      //
      //   Update gl3 header
      //
      nBytesUsed += nBytes ;
      gl3Header->bh.token = token ;
      gl3Header->sector[secIndex].len = nBytes ;
      gl3Header->sector[secIndex].off = trackDataPointer - buffer ;
      trackDataPointer += nBytes ;
      //
      //   Fill histos
      //
      m_l3_nHitsSector->Fill    ( tracker.nHits ) ;
      m_l3_nTracksSector->Fill  ( tracker.nTracks ) ;
      m_l3_cpuTimeSector->Fill  ( 1000.*tracker.cpuTime ) ;
      m_l3_realTimeSector->Fill ( 1000.*tracker.realTime ) ;
      printf ( "St_sl3Maker: %d tracks \n", tracker.nTracks ) ;
      //
      //     Fill offline hit table
      //
      for ( int j = 0 ; j < tracker.nHits ; j++ ) {
	 hit[nHits].id    = nHits + 1 ; 
	 hit[nHits].row   = secIndex * 100 + tracker.hit[j].row ; 
	 hit[nHits].x     = tracker.hit[j].x ;
	 hit[nHits].y     = tracker.hit[j].y ;
	 hit[nHits].z     = tracker.hit[j].z ;
	 hit[nHits].dx    = tracker.hit[j].dx ;
	 hit[nHits].dy    = tracker.hit[j].dy ;
	 hit[nHits].dz    = tracker.hit[j].dz ;
	 hit[nHits].q     = tracker.hit[j].q  ;
	 hit[nHits].track = 0  ;
	 if ( tracker.hit[j].track != NULL ) {
	    hit[nHits].track = tracker.hit[j].track->id + 10000 * secIndex  ;
	 }
	 nHits++ ;
	 if ( nHits >= maxHits ) {
	      fprintf ( stderr, " St_l3t_Maker:MakeOnLine: %d larger than max in hit table \n", nHits ) ;
	      break ;
	 }
      }
   }

   //
   //  Read event in gl3Event
   //

   gl3.processEvent ( nBytesUsed, buffer ) ;
   gl3Event* eventP = 0 ;
   eventP = gl3.getEvent(token);

   //
   //   Generate output table
   //
   int nTracks = 1;
   if ( eventP ) nTracks = max(1,eventP->getNTracks());   
   St_tpt_track *trackS = new St_tpt_track("l3Track",nTracks); 
   m_DataSet->Add(trackS);
   //
   tpt_track_st*  track  = (tpt_track_st *)trackS->GetTable(); 
   table_head_st* trackH = (table_head_st *)trackS->GetHeader(); 
   //
   //   Copy gl3 to tpt_track_st table
   //
   if ( eventP ) {
      gl3Track*     gTrk ;
      tpt_track_st* tTrk ;
      for ( int i = 0 ; i < (int)eventP->getNTracks() ; i++ ) {
         gTrk = eventP->getTrack(i);
         if ( !gTrk ) continue ;
         tTrk = &(track[i]);
         tTrk->id       = gTrk->id ;
         tTrk->flag     = 1 ;
         tTrk->invp     = 1./fabs(gTrk->pt);
         tTrk->nfit     = gTrk->nHits ;
         tTrk->nrec     = gTrk->nHits ;
         tTrk->q        = (long)(gTrk->pt/fabs(gTrk->pt));
         tTrk->chisq[0] = gTrk->chisq[0] ;
         tTrk->chisq[1] = gTrk->chisq[1] ;
         tTrk->length   = gTrk->trackLength ;
         tTrk->phi0     = gTrk->phi0 * toDeg ;
         tTrk->psi      = gTrk->psi  * toDeg ;
         tTrk->r0       = gTrk->r0   ;
         tTrk->tanl     = gTrk->tanl ;
         tTrk->z0       = gTrk->z0   ;
      }
   }
   trackH->nok = nTracks ;
   hitH->nok   = nHits ;
   MakeHistograms();
  
   return kStOk ;
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::MakeOffLine(){

   FtfFinder tracker ; 
   printf ( "St_l3t_Maker: start offline processing \n" ) ;

   St_DataSet *tpc_data =  GetDataSet("tpc_hits");
   St_tcl_tphit   *tphit = (St_tcl_tphit     *) tpc_data->Find("tphit");
   if (!tphit) {
      fprintf ( stderr, " St_l3t_Maker::MakeOffLine: No TPC space points \n" ) ;
      return kStWarn;
   }
   fprintf ( stderr, "St_l3t_Maker:MakeOffLine: Offline space points as input for L3T \n" ) ;

   Int_t nHits = tphit->GetNRows();
   St_tcl_tphit   *l3Hit = new St_tcl_tphit("l3Hit",nHits);
   m_DataSet->Add(l3Hit);
   *l3Hit = *tphit ;

   tcl_tphit_st*  hit  = (tcl_tphit_st  *)l3Hit->GetTable();
  
   int maxTracks      = nHits / 10 ; 
   if ( maxTracks < 1 ) maxTracks = 1 ;
   tracker.maxHits    = nHits ;
   tracker.maxTracks  = maxTracks ;
   tracker.hit        = new FtfHit[nHits] ;
   tracker.track      = new FtfTrack[maxTracks] ;
  
   tracker.para.phiMin =   0.F / toDeg ;
   tracker.para.phiMax = 360.F / toDeg ;
   tracker.para.etaMin = -2.2F ;
   tracker.para.etaMax =  2.2F ;
   tracker.para.nPhi   = 60 ;
   tracker.para.nEta   = 60 ;
   tracker.reset() ;
//
//   Fill ftf hit table
//
   for ( int i = 0 ; i < nHits ; i++ ) {
      tracker.hit[i].row = hit[i].row%100 ;
      tracker.hit[i].x   = hit[i].x ;
      tracker.hit[i].y   = hit[i].y ;
      tracker.hit[i].z   = hit[i].z ;
      tracker.hit[i].dx  = hit[i].dx ;
      tracker.hit[i].dy  = hit[i].dy ;
      tracker.hit[i].dz  = hit[i].dz ;
      tracker.hit[i].q   = hit[i].q  ;
      tracker.hit[i].track = 0 ;
   }
   tracker.nHits   = nHits ;
   tracker.nTracks = 0 ;

//   Process data 

   tracker.process();
  
//   Generate output table
  
   int nTracks = max(1,tracker.nTracks);   
   St_tpt_track *trackS = new St_tpt_track("l3Track",nTracks); 
   m_DataSet->Add(trackS);
//
   tpt_track_st*  track  = (tpt_track_st *)trackS->GetTable(); 
   table_head_st* trackH = (table_head_st *)trackS->GetHeader();
//
//   Copy ftf tracks to tpt_track_st table
//
   FtfTrack*     fTrk ;
   tpt_track_st* tTrk ;
   for ( int j = 0 ; j < tracker.nTracks ; j++ ) {
      fTrk = &(tracker.track[j]);
      if ( !fTrk ) continue ;
      tTrk = &(track[j]);
      tTrk->flag     = 1 ;
      tTrk->id       = fTrk->id ;
      tTrk->invp     = 1./fabs(fTrk->pt);
      tTrk->nfit     = fTrk->nHits ;
      tTrk->nrec     = fTrk->nHits ;
      tTrk->q        = fTrk->q;
      tTrk->chisq[0] = fTrk->chi2[0] ;
      tTrk->chisq[1] = fTrk->chi2[1] ;
      tTrk->length   = fTrk->trackLength ;
      tTrk->phi0     = fTrk->phi0 * toDeg ;
      tTrk->psi      = fTrk->psi  * toDeg ;
      tTrk->r0       = fTrk->r0   ;
      tTrk->tanl     = fTrk->tanl ;
      tTrk->z0       = fTrk->z0   ;
   }
//
//     Store track info
//
   for ( int k = 0 ; k < nHits ; k++ ) {
      if ( tracker.hit[k].track != 0 ) {
         hit[k].track = 1000 * tracker.hit[k].track->id ;
      }
   }
//
   trackH->nok = tracker.nTracks ;
//
   delete []tracker.hit ;
   delete []tracker.track ;
//
   MakeHistograms();
   return kStOk ;
}
void St_l3t_Maker::MakeHistograms() {

//		Get the table:
  St_tpt_track *tpr = (St_tpt_track *)m_DataSet->Find("l3Track");
  if (!tpr) return;
  tpt_track_st *r = tpr->GetTable();
  for(Int_t i=0; i<tpr->GetNRows();i++,r++){
    Int_t flag    = r->flag;
    Float_t rnrec = r->nrec;

    if(flag<=0) continue;

    m_l3_hits_on_track->Fill(rnrec);
    m_l3_azimuth->Fill(r->psi);
    m_l3_tan_dip->Fill(r->tanl);
    m_l3_r0->Fill(r->r0);
    m_l3_z0->Fill(r->z0);
    m_l3_pt->Fill(1./r->invp);
    m_l3_XyChi2->Fill(r->chisq[0]);
    m_l3_SzChi2->Fill(r->chisq[1]);
  }
}
//_____________________________________________________________________________


// $Id: St_l3t_Maker.cxx,v 1.49 2007/07/12 20:37:11 fisyak Exp $
//
// Revision 1.22  2000/03/28 20:22:15  fine
// Adjusted to ROOT 2.24
//
// Revision 1.21  2000/03/14 17:58:41  yepes
// change tracker setup for online case
//
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
#include "Stl3Util/ftf/FtfSl3.h"
#include "Stl3Util/gl3/gl3Conductor.h"
#include "Stl3Util/base/St_l3_Coordinate_Transformer.h"

#include <stdio.h>
#include <Stiostream.h>
#include "St_l3t_Maker.h"
#include "St_DataSetIter.h"
//#include "St_XDFFile.h"
#include "StDedxDefinitions.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"


// #include "gl3GeneralHistos.h"
// #include "gl3JPsi.h"
// #include "gl3GammaGamma.h"
// #include "gl3dEdx.h"
// #include "gl3HighPt.h"

#include "TH1.h"
#include "tables/St_hitarray_Table.h"
#include "StEventTypes.h"
#include "TMath.h"
#define gufld   gufld_
extern "C" {void gufld(Float_t *, Float_t *);}

// i386 is little endian:
#define UNIX_LITTLE_ENDIAN

ClassImp(St_l3t_Maker)
  
  //_____________________________________________________________________________
St_l3t_Maker::St_l3t_Maker(const char *name):
    StMaker(name)
{
  m_InputHitDataSetName="tpc_hits";
  m_InputHitName="tphit";
  firstEvent = 1 ;

}
//_____________________________________________________________________________
St_l3t_Maker::~St_l3t_Maker(){
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Init(){
  // Create tables
  St_DataSetIter       local(GetInputDB("l3"));
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
//_____________________________________________________________________________
//_____________________________________________________________________________
void St_l3t_Maker::SetInputHits(  const Char_t* DataSet, const Char_t * Table)
{
  m_InputHitDataSetName=DataSet;
  m_InputHitName=Table;
  m_Mode=2;
  fprintf(stderr,"  \"%s\".SetInputHits to: DataSet=\"%s\", Hit=\"%s\"\n",GetName(),m_InputHitDataSetName.Data(), m_InputHitName.Data());

}
//_____________________________________________________________________________
Int_t St_l3t_Maker::Make(){
//
//   Depending on m_Mode the online or offline makers are used.
//
  //m_Mode = 2;

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


    printf("run my l3t_maker-->>\n");
    
    St_l3_Coordinate_Transformer transformer ;
    transformer.Use_transformation_provided_by_db() ;
    //transformer.LoadTPCLookupTable("map.bin");
    
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
    fprintf ( stderr, "St_l3t_Maker:MakeOnLine: Online buffer space points as input for L3T \n" ) ;
    //
    //    Create tracker and gl3 objects
    //

    FtfSl3   tracker(&transformer) ;
    gl3Conductor gl3;
    gl3.setup(&transformer,1,32);
    //   gl3Conductor gl3(&transformer) ;
    
    //    gl3GeneralHistos  fillHistoModule ;
    //    gl3JPsi           jPsiM ;
    //    gl3GammaGamma     gammaGammaM ;
    //    gl3dEdx           dEdxM ;
    //    gl3HighPt         highPtM ;
    
    //    gl3.add ( &fillHistoModule ) ;
    //    gl3.add ( &jPsiM   ) ;
    //    gl3.add ( &gammaGammaM ) ;
    //    gl3.add ( &dEdxM ) ;
    //    gl3.add ( &highPtM ) ;
    
    gl3.init();
    gl3.setHitProcessing(2) ; // fill gl3Hit info
    
    int const maxBytes = 5000000 ;
    char* buffer = new char[maxBytes] ;
    char* endTrackBuffer = buffer + maxBytes;     
    L3_P *gl3Header = (L3_P *) buffer;
    memset (buffer, 0, sizeof (L3_P));
    memcpy (gl3Header->bh.bank_type, CHAR_L3_P, 8);
    gl3Header->bh.bank_id = 1;
    gl3Header->bh.format_ver = DAQ_RAW_FORMAT_VERSION;
    gl3Header->bh.byte_order = DAQ_RAW_FORMAT_ORDER;
    gl3Header->bh.format_number = 0;
    gl3Header->bh.token = 1;
    gl3Header->bh.w9 = DAQ_RAW_FORMAT_WORD9;
    gl3Header->bh.crc = 0;		//don't know yet....    
    
    char* trackDataPointer = buffer + sizeof(L3_P)  ;
    char* endTrackDataPointer = buffer + maxBytes ;

    // get magnetic field
    Float_t xval[3] = {0.,0.,0.};
    Float_t bval[3];
    gufld(xval,bval);
    //
    //    Set parameters
    //
    tracker.setup ( 30000, 3000 ) ;
    tracker.para.infoLevel = 10 ;
    tracker.para.infoLevel = 10 ;
    tracker.para.hitChi2Cut   = 50 ;
    tracker.para.trackChi2Cut = 10 ;
    tracker.para.goodHitChi2  = 20 ;
    tracker.para.dphi=0.1;
    tracker.para.deta=0.1;
    tracker.para.distanceMerge = 5 ;
    tracker.setXyError ( 0.12 ) ;
    tracker.setZError  ( 0.24 ) ;
    
    tracker.para.bField = bval[2];
    tracker.para.bFieldPolarity = (int)(bval[2]/fabs(bval[2]));

    tracker.para.ptMinHelixFit = 0.0;
    tracker.para.maxChi2Primary = 0;
    //tracker.para.maxChi2Primary = 50; // all tracks primary


    tracker.reset();
    
    gl3.setBField(bval[2]);
    
    //   Print parameters for first event
    if ( firstEvent ) {
	printf("LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL \n");
	printf( "333333333333333333333333333333333333333333333333333333 \n");
	printf( "St_l3t_Maker: tracking parameters \n");
	printf("FtfSl3: xyError          %f  \n", tracker.getXyError());
	printf("FtfSl3: zError           %f  \n", tracker.getZError());
	printf("FtfSl3: minTimeBin       %d  \n", tracker.minTimeBin);
	printf("FtfSl3: maxTimeBin       %d  \n", tracker.maxTimeBin);
	printf("FtfSl3: minClusterCharge %d  \n", tracker.minClusterCharge);
	printf("FtfSl3: maxClusterCharge %d  \n", tracker.maxClusterCharge);
	tracker.para.write ( stdout ) ;
	transformer.Print_parameters() ;
	printf("LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL \n");
	printf("333333333333333333333333333333333333333333333333333333 \n");
	firstEvent = 0 ;
    } 
    
    //
    //    Create hit table to store L3 clusters in offline format
    //
    int maxHits = 500000 ;
    int nHits = 0 ;
    int token = -999999;
    St_tcl_tphit *hitS = new St_tcl_tphit("l3Hit",maxHits); 
    m_DataSet->Add(hitS);
    tcl_tphit_st*  hit  = (tcl_tphit_st  *)hitS->GetTable();
    
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
	L3_SECP* sectorHeader = (L3_SECP *)trackDataPointer ;
	memset ( trackDataPointer, 0, sizeof(L3_SECP) ) ;   
	trackDataPointer += sizeof(L3_SECP);
	if ( trackDataPointer > endTrackDataPointer ) {
	    printf("St_l3tMaker::MakeOnline: maxBytes %d too short a buffer", 
		   maxBytes ) ;
	    return kStWarn;
	}
	//
	sectorHeader->bh.bank_id = (secIndex-1) * 2 + 1 ;
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
	int nBytes = tracker.fillTracks ( endTrackDataPointer-trackDataPointer,
					  trackDataPointer,
	                                token ) ;
	if ( nBytes <= 0 ) continue ;
	
	sectorHeader->trackp.off = (trackDataPointer-(char *)sectorHeader)/4;
	sectorHeader->trackp.len = nBytes/4;
	
	trackDataPointer += nBytes ;
	
	nBytes=tracker.fillHits (endTrackBuffer-trackDataPointer, trackDataPointer, token );
	//  set cluster offset and lenght
	sectorHeader->sl3clusterp.off = (trackDataPointer-(char *)sectorHeader)/4;
	sectorHeader->sl3clusterp.len = nBytes/4;
	
	trackDataPointer+=nBytes;                    
	//
	//   Update gl3 header
	//
	gl3Header->bh.token = token ;
	gl3Header->sector[secIndex].len = (trackDataPointer-(char *)sectorHeader)/4 ;
	gl3Header->sector[secIndex].off = ((char *)sectorHeader - buffer)/4  ; ;
	trackDataPointer += nBytes ;
	//
	//   Fill histos
	//
	m_l3_nHitsSector->Fill    ( tracker.nHits ) ;
	m_l3_nTracksSector->Fill  ( tracker.nTracks ) ;
	m_l3_cpuTimeSector->Fill  ( 1000.*tracker.cpuTime ) ;
	m_l3_realTimeSector->Fill ( 1000.*tracker.realTime ) ;
	fprintf (stderr, "St_sl3Maker: %d tracks, %d hits out of range \n", 
		 tracker.nTracks, tracker.nHitsOutOfRange ) ;
    } //for(Int_t secIndex=1;secIndex<=12; secIndex++)
    //
    //  Read event in gl3Event
    //
    
    EventDescriptor ed;
    ed.token = token;
    ed.TRG_DAQ_cmds = 16*4+0; // TRG_cmd = 4, DAQ_cmd=0
    ed.TRG_word = 0; //currently ignored

    gl3.processEvent(&ed, (L3_P*)buffer);
    //gl3.processEvent ( trackDataPointer-buffer, buffer ) ;
    gl3Event* eventP = 0 ;
    eventP = gl3.getEvent(token);
    //
    //   Generate output table
    //
    int nTracks = 1;
    int nMergedTracks = 1;
    if ( eventP ) nTracks       = max(1,eventP->getNTracks());   
    if ( eventP ) nMergedTracks = max(1,eventP->getNMergedTracks());   
    St_dst_track *trackS = new St_dst_track("l3Track", nTracks);
    St_dst_dedx  *dedxS  = new St_dst_dedx("l3Dedx", nTracks); 
    m_DataSet->Add(trackS);
    m_DataSet->Add(dedxS);
    fprintf(stderr," %s on-line:  Tracks %d Merged Tracks %d \n",
	    GetName(),nTracks, nMergedTracks );
    
    //
    dst_track_st *track     = (dst_track_st *)trackS->GetTable(); 
    dst_dedx_st  *trackDedx = (dst_dedx_st *)dedxS->GetTable();
    //
    //   Copy gl3 to dst_track_st table
    //
    int nTracksWithDedx;
    nTracksWithDedx = 0;
    
    if ( eventP ) {
	//
	//   Loop over hits
	//
	gl3Hit* gHit ;
	for (int ihit = 0; ihit < eventP->getNHits (); ihit++) {
	    gHit = eventP->getHit (ihit);
	    hit[ihit].id = nHits + 1;
	    hit[ihit].row = gHit->getRowSector ();
	    hit[ihit].x = gHit->getX ();
	    hit[ihit].y = gHit->getY ();
	    hit[ihit].z = gHit->getZ ();
	    hit[ihit].dx = tracker.getXyError();
	    hit[ihit].dy = tracker.getXyError();
	    hit[ihit].dz = tracker.getZError();
	    hit[ihit].q = gHit->getCharge ();
	    hit[ihit].track = gHit->getTrackId ();
	    hit[ihit].flag = (long) gHit->getFlags ();
	}
	
	nHits = eventP->getNHits ();
	
	gl3Track     *gTrk ;
	dst_track_st *tTrk ;
	dst_dedx_st  *tDedx;
	for ( int i = 0 ; i < (int)eventP->getNTracks() ; i++ ) {
	    gTrk = eventP->getTrack(i);
	    if ( !gTrk ) continue ;
	    tTrk = &(track[i]);
	    tTrk->id       = gTrk->id ;
	    tTrk->invpt    = 1./fabs(gTrk->pt);
	    tTrk->n_point  = gTrk->nHits ;
	    tTrk->icharge  = gTrk->q ;
	    tTrk->chisq[0] = gTrk->chi2[0] ;
	    tTrk->chisq[1] = gTrk->chi2[1] ;
	    tTrk->length   = gTrk->length ;
	    tTrk->phi0     = gTrk->phi0 * toDeg ;
	    tTrk->psi      = gTrk->psi  * toDeg ;
	    tTrk->r0       = gTrk->r0   ;
	    tTrk->tanl     = gTrk->tanl ;
	    tTrk->z0       = gTrk->z0   ;
	    
	    // fill dst_dedx table only for those tracks which have valid dedx information
	    if ( gTrk->dedx!=0 ) {
		tDedx = &(trackDedx[nTracksWithDedx]);
		tDedx->id_track = gTrk->id;
		tDedx->method   = kTruncatedMeanIdentifier;
		tDedx->dedx[0]  = gTrk->dedx ;
		tDedx->dedx[1]  = 0;
		tDedx->ndedx    = gTrk->nDedx;
		nTracksWithDedx++;
	    }
	    
	}
    }
    trackS->SetNRows(nTracks);
    dedxS->SetNRows(nTracksWithDedx);
    hitS->SetNRows(nHits);
    MakeHistograms();
    
    // Fill StEvent
    fillStEvent(trackS,dedxS,hitS) ;
    //
    //  delete buffer
    //
    delete []buffer ;
    
    return kStOk ;
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::MakeOffLine(){

   FtfFinder tracker ; 
   fprintf ( stderr, "St_l3t_Maker OffLine space points from Table as input for %s : Ds=%s, Hits=%s\n", m_InputHitDataSetName.Data(),m_InputHitName.Data(), GetName()) ;
   St_DataSet *tpc_data =  GetDataSet(m_InputHitDataSetName);
   St_tcl_tphit   *tphit = (St_tcl_tphit     *) tpc_data->Find(m_InputHitName);
   if (!tphit) {
      fprintf ( stderr, " St_l3t_Maker::MakeOffLine: No TPC space points \n" ) ;
      return kStWarn;
   }
   fprintf (stderr, "St_l3t_Maker=%s: start offline processing \n" , GetName()) ;

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
      tracker.hit[i].sector = (short) (hit[i].row/100);
      tracker.hit[i].row = hit[i].row%100 ;
      tracker.hit[i].x   = hit[i].x ;
      tracker.hit[i].y   = hit[i].y ;
      tracker.hit[i].z   = hit[i].z ;
      tracker.hit[i].dx  = hit[i].dx ;
      tracker.hit[i].dy  = hit[i].dy ;
      tracker.hit[i].dz  = hit[i].dz ;
      tracker.hit[i].q   = hit[i].q  ;
      tracker.hit[i].flags = hit[i].flag;
      tracker.hit[i].track = 0 ;
   }
   tracker.nHits   = nHits ;
   tracker.nTracks = 0 ;

//   Process data 

   tracker.process();

//   Generate output table
  
   int nTracks = max(1,tracker.nTracks);   
   St_dst_track *trackS = new St_dst_track("l3Track",nTracks); 
   m_DataSet->Add(trackS);
   fprintf(stderr," %s found Ntracks=%d\n",GetName(),nTracks);
//
   dst_track_st*  track  = (dst_track_st *)trackS->GetTable(); 
//
//   Copy ftf tracks to dst_track_st table
//
   FtfTrack*     fTrk ;
   dst_track_st* tTrk ;
   for ( int j = 0 ; j < tracker.nTracks ; j++ ) {
      fTrk = &(tracker.track[j]);
      if ( !fTrk ) continue ;
      tTrk = &(track[j]);
      tTrk->id       = fTrk->id ;
      tTrk->invpt    = 1./fabs(fTrk->pt);
      tTrk->n_point  = fTrk->nHits ;
      tTrk->icharge  = fTrk->q;
      tTrk->chisq[0] = fTrk->chi2[0] ;
      tTrk->chisq[1] = fTrk->chi2[1] ;
      tTrk->length   = fTrk->length ;
      tTrk->phi0     = fTrk->phi0 * toDeg ;
      tTrk->psi      = fTrk->psi  * toDeg ;
      tTrk->r0       = fTrk->r0   ;
      tTrk->tanl     = fTrk->tanl ;
      tTrk->z0       = fTrk->z0   ;
//       tTrk->dedx[0]  = fTrk->dedx ;
//       tTrk->ndedx    = fTrk->nDedx;
   }
//
//     Store track info
//
   for ( int k = 0 ; k < nHits ; k++ ) {
      if ( tracker.hit[k].track != 0 ) {
         hit[k].track = 1000 * ((FtfTrack *)tracker.hit[k].track)->id ;
      }
   }
//
   trackS->SetNRows( tracker.nTracks );
//
   delete []tracker.hit ;
   delete []tracker.track ;
//
   MakeHistograms();
   return kStOk ;
}
void St_l3t_Maker::MakeHistograms() {

//		Get the table:
  St_dst_track *tpr = (St_dst_track *)m_DataSet->Find("l3Track");
  if (!tpr) return;
  dst_track_st *r = tpr->GetTable();
  for(Int_t i=0; i<tpr->GetNRows();i++,r++){
    Float_t rnrec = r->n_point;


    m_l3_hits_on_track->Fill(rnrec);
    m_l3_azimuth->Fill(r->psi);
    m_l3_tan_dip->Fill(r->tanl);
    m_l3_r0->Fill(r->r0);
    m_l3_z0->Fill(r->z0);
    m_l3_pt->Fill(1./r->invpt);
    m_l3_XyChi2->Fill(r->chisq[0]);
    m_l3_SzChi2->Fill(r->chisq[1]);
  }
}
//_____________________________________________________________________________
Int_t St_l3t_Maker::fillStEvent(St_dst_track* trackS, St_dst_dedx* dedxS, St_tcl_tphit* pointS) 
{
    // Get StEvent if not return
    StEvent* myStEvent = (StEvent *) GetInputDS("StEvent") ;
    if (!myStEvent)
	{ 
	    printf("No StEvent\n") ;
	    return 0 ;
	}
    
    // Create Stl3Trigger and connect it
    StL3Trigger* myStL3Trigger = new StL3Trigger() ;
    myStEvent->setL3Trigger(myStL3Trigger) ;

    /////
    //      TRACKS
    //////
    // Get Track Nods
    StSPtrVecTrackNode& myTrackNodeVector = myStL3Trigger->trackNodes() ;
    StSPtrVecTrackDetectorInfo&  myTrackDetectorInfoVector = myStL3Trigger->trackDetectorInfo() ;
    const Int_t MaxNTracks = 10000 ;
    if (trackS->GetNRows()>MaxNTracks ) 
      {
	printf("Too many tracks for stevent ...\n") ;
	return 0 ;
      }
    StGlobalTrack* Store_Track_ids[MaxNTracks] ;

    // get magnetic field
    Float_t xval[3] = {0.,0.,0.};
    Float_t bval[3];
    gufld(xval,bval);
    
    // Loop over dst tracks and fill them into StEvent
    dst_track_st* dstTracks = trackS->GetTable();
    for(Int_t trackindex = 0 ; trackindex<trackS->GetNRows() ;  trackindex++)
      {
	////
	// spread track data into StTrack Objects
	////
	// detector info
	StTrackDetectorInfo* info = new StTrackDetectorInfo(dstTracks[trackindex]) ;
	myTrackDetectorInfoVector.push_back(info) ;
	// track geometry
	Double_t convert = 2*TMath::Pi()/360 ;
	StThreeVectorF origin( dstTracks[trackindex].r0 * cos(convert*dstTracks[trackindex].phi0),
			       dstTracks[trackindex].r0 * sin(convert*dstTracks[trackindex].phi0),
			       dstTracks[trackindex].z0 ) ;
	Double_t Pt;
	if (dstTracks[trackindex].invpt == 0 ) 
	  { Pt=0.000000001; }
	else 
	  { Pt= 1/dstTracks[trackindex].invpt; }
	StThreeVectorF momentum( Pt * cos(convert*dstTracks[trackindex].psi),
				 Pt * sin(convert*dstTracks[trackindex].psi),
				 Pt * dstTracks[trackindex].tanl ) ;

	// calculate helicity:
	short h = ((bval[2] * dstTracks[trackindex].icharge) > 0 ? -1 : 1);

	StHelixModel* helixModel = 
	    new StHelixModel( dstTracks[trackindex].icharge,
			      dstTracks[trackindex].psi,
			      0.0,
			      atan(dstTracks[trackindex].tanl), 
			      origin, 
 			      momentum,
 			      h) ;

	// global track
	StGlobalTrack* globalTrack = new StGlobalTrack(dstTracks[trackindex]) ;
	globalTrack->setDetectorInfo(info) ;
	globalTrack->setGeometry(helixModel);

	// fill vectors
	StTrackNode* trackNode = new StTrackNode() ;
	trackNode->addTrack(globalTrack) ;
	myTrackNodeVector.push_back(trackNode) ;
	  
	if (dstTracks[trackindex].id > 0 && 
		dstTracks[trackindex].id <= trackS->GetNRows())
	      {
		Store_Track_ids[dstTracks[trackindex].id] = globalTrack ;
	      }
	    else 
	      {
		printf("Bad track id.\n"); 
	      }
      }
    
    //test
    //StTrackNode* ttracknode = (StTrackNode*) myTrackNodeVector[1] ;
    //StGlobalTrack* glo = (StGlobalTrack*) ttracknode->track(0) ;
    //glo->key();

    //////
    //       POINTS
    //////
    StTpcHitCollection* myStTpcHitCollection = new StTpcHitCollection();
    myStL3Trigger->setTpcHitCollection(myStTpcHitCollection);

    // get hit table
    tcl_tphit_st* tcl_points = pointS->GetTable() ;


    // Loop over dst_point and fill them into StEvent and produce point-track connection
    // via StTrackDetectorInfo
    for(Int_t pointindex = 0 ; pointindex < pointS->GetNRows() ; pointindex++)
       {
	 ////
	 // Convert(kTRUE) to StTpcHit
	 ////
	 // position
	 StThreeVectorF pos(tcl_points[pointindex].x,
			    tcl_points[pointindex].y,
			    tcl_points[pointindex].z) ;
	 // position error
	 StThreeVectorF poserror(tcl_points[pointindex].dx,
				 tcl_points[pointindex].dy,
				 tcl_points[pointindex].dz) ;
	 // pack sec and row  and tpcid
	 ULong_t hw = 0 ;
	 ULong_t row = ULong_t (tcl_points[pointindex].row%100) ;
	 if ( row >=1 && row <=45 )  { row <<= 9 ; } else { row=0 ; } ;
   	 ULong_t sec = ULong_t ((tcl_points[pointindex].row-tcl_points[pointindex].row%100)/100) ;
	 if ( sec >=1 && sec <=24 )  { sec <<= 4 ; } else { sec=0 ; } ;
	 ULong_t tpcid = 1;
	 hw = hw | row ;
	 hw = hw | sec ;
	 hw = hw | tpcid ;
	 // charge
	 Float_t charge = tcl_points[pointindex].q ;
	 // track reference counter set always to 0
	 UChar_t c = 0 ;

	 // create hit
	 StTpcHit* tpcHit = new StTpcHit(pos,poserror,hw,charge,c) ;
	 // set flag
	 tpcHit->setFlag(UChar_t(tcl_points[pointindex].flag));

	 // add to hit collection
	 if (tpcHit) { myStTpcHitCollection->addHit(tpcHit) ;} else { delete tpcHit; return 0;}

	 // Add to detectorInfo (= connect to track)
	 Int_t id = tcl_points[pointindex].track ;
	 if (id < trackS->GetNRows() && Store_Track_ids[id] && id>0) 
	   {
	     StTrackDetectorInfo* info = Store_Track_ids[id]->detectorInfo();
	     info->addHit(tpcHit);
	   }
       }
    // 	else
    //	  {
// 	    // clean up and bad return
// 	    delete tpcHit ;
// 	    return 0;        
	 // 	  }
    

//     ///////
//     //    DE/DX
//     ///////
//     dst_dedx_st* my_dst_dedx = dedxS->GetTable() ;
//     for(Int_t dedxindex = 0 ; dedxindex<dedxS->GetNRows() ;  dedxindex++)
//       {
// 	if(Store_Track_ids[my_dst_dedx[dedxindex].id_track])
// 	  {
// 	    StDedxPidTraits* myStDedxPidTraits =  new StDedxPidTraits(my_dst_dedx[dedxindex]) ;
// 	    Store_Track_ids[my_dst_dedx[dedxindex].id_track]->addPidTraits(myStDedxPidTraits) ;
// 	  }
//       }

    // well done go home
    return 1;
}

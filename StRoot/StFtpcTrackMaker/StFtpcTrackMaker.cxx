// $Id: StFtpcTrackMaker.cxx,v 1.16 2001/04/27 10:20:24 oldi Exp $
// $Log: StFtpcTrackMaker.cxx,v $
// Revision 1.16  2001/04/27 10:20:24  oldi
// Moved function calls of StFtpcTrackEvalulator to constructor of StFtpcTrackEvalulator.
//
// Revision 1.15  2001/01/30 13:31:41  oldi
// New variable mTime introduced to count total time consumption.
//
// Revision 1.14  2001/01/25 15:22:25  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.13  2000/11/10 18:38:50  oldi
// Cleanup due to changes in other classes.
//
// Revision 1.12  2000/08/09 19:15:31  didenko
// remove unneeded include
//
// Revision 1.11  2000/08/07 00:20:03  jcs
// save prevertex information correctly
//
// Revision 1.10  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.9  2000/07/03 12:45:23  jcs
// get (pre)Vertex coordinates directly from (pre)Vertex table instead of from
// fptpars
//
// Revision 1.8  2000/06/26 22:10:44  fisyak
// remove params
//
// Revision 1.7  2000/06/15 09:13:34  oldi
// No tracking is performed (return kStWarn instead) if the z-position of the
// main vertex is off by more than 100 cm from z = 0. Different error messages
// (depending on how far the vertex is off) are printed.
//
// Revision 1.6  2000/06/13 14:25:56  oldi
// Changed cout to gMessMgr->Message().
// Printed output changed (slightly).
//
// Revision 1.5  2000/06/07 11:16:29  oldi
// Changed 0 pointers to NULL pointers.
// Function HandleSplitTracks() called.
//
// Revision 1.4  2000/05/15 14:28:12  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.3  2000/05/12 12:59:16  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:52  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:28  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 10.11.2000
//----------Copyright:     &copy MDO Production 1999

#include <iostream.h>
#include <math.h>

#include "StFtpcTrackMaker.h"
#include "StFtpcVertex.hh"
#include "StFtpcConfMapper.hh"
#include "StFtpcDisplay.hh"
#include "StFtpcTrackEvaluator.hh"

#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StChain.h"
#include "StVertexId.h"

#include "tables/St_fpt_fptrack_Table.h"
#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TCanvas.h"
#include "TFile.h"

#include "StMessMgr.h"


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcTrkMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackMaker)

//_____________________________________________________________________________
StFtpcTrackMaker::StFtpcTrackMaker(const char *name) : StMaker(name),  m_fdepar(0)
{
  // Default constructor.
}

//_____________________________________________________________________________
StFtpcTrackMaker::~StFtpcTrackMaker()
{
  // Destructor.
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Init()
{
  // Initialisation.

  St_DataSet *ftpcpars = GetInputDB("ftpc");
  assert(ftpcpars);
  St_DataSetIter  gime(ftpcpars);
  m_fdepar = (St_fde_fdepar *) gime("fdepars/fdepar");
  
  // Create Histograms    
  m_q            = new TH1F("fpt_q"         ,"FTPC track charge"                               ,  3,-2. ,  2.  );
  m_theta        = new TH1F("fpt_theta"     ,"FTPC theta"                                      ,100,-5.0,  5.0 );
  m_ndedx        = new TH1F("fde_ndedx"     ,"Number of points used in FTPC dE/dx calculation" , 10, 1. , 11.  );
  m_found        = new TH1F("fpt_nrec"      ,"FTPC: number of points found per track"          , 10, 1. ,  11. );
  m_track        = new TH1F("fpt_track"     ,"FTPC: number of tracks found"                    ,100, 1. ,5000. );    
  m_nrec_track   = new TH2F("fpt_hits_mom"  ,"FTPC: points found per track vs. momentum"       , 10, 1. ,  11. , 100, 1., 20.);
 
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Make()
{
  // Setup and tracking.

  gMessMgr->Message("", "I", "OST") << "Tracking (FTPC) started..." << endm;

  St_DataSet *ftpc_data = GetDataSet("ftpc_hits");
  
  if (!ftpc_data) {
    return kStWarn;
  }
  
  //  clusters exist -> do tracking
  St_fcl_fppoint *fcl_fppoint = (St_fcl_fppoint *)ftpc_data->Find("fcl_fppoint");
  
  if (!fcl_fppoint) {
    return kStWarn;
  }
  
  Int_t iflag = 0;
  
  //pointer to preVertex dataset
  St_DataSet *preVertex = GetDataSet("preVertex"); 
  
  //iterator
  St_DataSetIter preVertexI(preVertex);
  
  //pointer to preVertex
  St_dst_vertex  *preVtx  = (St_dst_vertex *)preVertexI("preVertex");
  
  gMessMgr->Message("", "I", "OST") << "Using primary vertex coordinates "; 

  Float_t primary_vertex_x = 0.0;
  Float_t primary_vertex_y = 0.0;
  Float_t primary_vertex_z = 0.0;
  Int_t   primary_vertex_id;
  
  if (preVtx) {
    dst_vertex_st *preVtxPtr = preVtx->GetTable();

    for (Int_t i = 0; i <preVtx->GetNRows();i++,preVtxPtr++) {
      
      if (preVtxPtr->iflag == 101) {
   	iflag = 101;
        primary_vertex_x =  preVtxPtr->x;
        primary_vertex_y =  preVtxPtr->y;
        primary_vertex_z =  preVtxPtr->z;
        primary_vertex_id = preVtxPtr->id;
	
	*gMessMgr << "(preVertex): ";
      }
    }
  }
  
  if ( iflag != 101 ) {
    //    preVertex not found  - compute and store Holm's preVertex
    *gMessMgr << "(Holm's vertex): ";
    
    StFtpcVertex *vertex = new StFtpcVertex(fcl_fppoint->GetTable(), fcl_fppoint->GetNRows());

    if (isnan(vertex->GetZ())) {
      // handles problem if there are not enough tracks and therefore a vertex cannot be found
      *gMessMgr << endm;
      gMessMgr->Message("", "E", "OST") << "No vertex found! Ftpc tracking stopped!" << endm;
      delete vertex;

      // No Tracking
      return kStWarn;
    }

    else {

      if (!preVtx) {
	// no preVertex table exists
	// create preVertex table with 1 row
	preVtx = new St_dst_vertex("preVertex", 1);
	preVtx->SetNRows(1);
	AddData(preVtx);
      }
      
      else {
	// correct preVertex not found
	// add a row to preVertex
	Int_t numRowPreVtx = preVtx->GetNRows(); 
	preVtx->ReAllocate(numRowPreVtx+1);
	preVtx->SetNRows(numRowPreVtx+1);
      }
      
      dst_vertex_st *preVtxPtr = preVtx->GetTable();
      preVtxPtr = preVtxPtr + preVtx->GetNRows() - 1;
      
      // save results in preVertex    
      preVtxPtr->x = 0.0;
      preVtxPtr->y = 0.0;
      preVtxPtr->z = vertex->GetZ();
      primary_vertex_z =  preVtxPtr->z;
      preVtxPtr->iflag = 301;
      preVtxPtr->det_id = 4;
      preVtxPtr->id = preVtx->GetNRows();
      primary_vertex_id = preVtxPtr->id;
      preVtxPtr->vtx_id = kEventVtxId;  
    }

    delete vertex;
  }
  
  *gMessMgr << " " << primary_vertex_x << ", " << primary_vertex_y << ", " << primary_vertex_z << "." << endm;

  // check for the position of the main vertex

  Double_t z = TMath::Abs(primary_vertex_z);
  
  if (z > 50.) {
    
    if (z > 162.45) {
      gMessMgr->Message("Found vertex lies inside of one Ftpc. No Ftpc tracking possible.", "E", "OTS");
      
      // No tracking!
      return kStWarn;   
    }
    
    else if (z > 100.) {
      gMessMgr->Message("Found vertex is more than 100 cm off from z = 0. Ftpc tracking makes no sense.", "E", "OTS");
      
      // No tracking!
      return kStWarn;
    }
    
    else {
      gMessMgr->Message("Found vertex is more than 50 cm off from z = 0 but  Ftpc tracking is still possible", "W", "OTS");
      // Do tracking.
    }
  }

  Double_t vertexPos[3] = {primary_vertex_x, primary_vertex_y, primary_vertex_z};
  StFtpcConfMapper *tracker = new StFtpcConfMapper(fcl_fppoint, vertexPos, Debug());

  // tracking 
  tracker->MainVertexTracking();

  // for the line above you have these possibilities
  //tracker->MainVertexTracking();
  //tracker->FreeTracking();
  //tracker->LaserTracking();

  St_fpt_fptrack *fpt_fptrack = new St_fpt_fptrack("fpt_fptrack", tracker->GetNumberOfTracks());
  m_DataSet->Add(fpt_fptrack);

  // momentum fit, dE/dx calculation, write tracks to tables
  tracker->FitAnddEdxAndWrite(fpt_fptrack, m_fdepar->GetTable(), -primary_vertex_id);

  if (Debug()) {
    gMessMgr->Message("", "I", "OST") << "Total time consumption         " << tracker->GetTime() << " s." << endm;
    tracker->SettingInfo();
    tracker->CutInfo();
    tracker->TrackingInfo();
  }

  else {
    tracker->TrackingInfo();
  }
    
  /*
  // Track Display
  
  // Uncomment this block if you want to see (I mean see!) the found tracks.
  
  StFtpcDisplay *display = new StFtpcDisplay(tracker->GetClusters(), tracker->GetTracks());
  //display->TrackInfo();
  //display->Info();
  //display->ShowClusters();
  display->ShowTracks();
  delete display;
  */

  /*
  // Track Evaluator
  
  // Uncomment this block to get information about the quality 
  // of the found tracks in comparison to the simulated input event.
  
  St_DataSet *geant = GetInputDS("geant");  
  
  StFtpcTrackEvaluator *eval = new StFtpcTrackEvaluator(geant, ftpc_data, tracker->GetVertex(), tracker->GetClusters(), tracker->GetTracks(), "ftpc_evaluator.root", "RECREATE");
  
  // Uncomment the following line if you want to 'see' the information (split tracks, unclean tracks, ...) 
  // evaluated by the TrackEvaluator.  
  //eval->ShowTracks();
  
  delete eval;
  */

  delete tracker;

  MakeHistograms();
  gMessMgr->Message("", "I", "OST") << "Tracking (FTPC) completed." << endm;

  return kStOK;;
}


//_____________________________________________________________________________
void StFtpcTrackMaker::MakeHistograms()
{
  // Fill histograms.

  St_DataSetIter ftpc_tracks(m_DataSet);

  //Get the table
  St_fpt_fptrack *trk = NULL;
  trk = (St_fpt_fptrack *) ftpc_tracks.Find("fpt_fptrack");

  if (trk) {
   // Fill histograms for FTPC fpt,fte,fde

    fpt_fptrack_st *r = trk->GetTable();
    for (Int_t i=0; i<trk->GetNRows();i++,r++) {
      m_found->Fill((float)(r->nrec));
      m_q->Fill((float)(r->q));
      m_theta->Fill(r->theta);
      m_ndedx->Fill((float)(r->ndedx));
      float mom=sqrt(r->p[0] * r->p[0] + r->p[1] * r->p[1] + r->p[2] * r->p[2]);
      m_nrec_track->Fill((float)(r->nrec),mom);
    }
  }
}


//_____________________________________________________________________________
void StFtpcTrackMaker::PrintInfo()
{
  // Prints information.

  gMessMgr->Message("", "I", "OST") << "******************************************************************" << endm;
  gMessMgr->Message("", "I", "OST") << "* $Id: StFtpcTrackMaker.cxx,v 1.16 2001/04/27 10:20:24 oldi Exp $ *" << endm;
  gMessMgr->Message("", "I", "OST") << "******************************************************************" << endm;
  
  if (Debug()) {
    StMaker::PrintInfo();
  }
}


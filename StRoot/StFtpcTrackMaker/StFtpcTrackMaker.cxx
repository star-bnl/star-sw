// $Id: StFtpcTrackMaker.cxx,v 1.4 2000/05/15 14:28:12 oldi Exp $
// $Log: StFtpcTrackMaker.cxx,v $
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
//----------Last Modified: 15.05.2000
//----------Copyright:     &copy MDO Production 1999

#include <iostream.h>

#include "StFtpcTrackMaker.h"
#include "StFtpcVertex.hh"
#include "StFtpcConfMapper.hh"
#include "StFtpcDisplay.hh"
#include "StFtpcTrackEvaluator.hh"

#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StChain.h"
#include "StVertexId.h"

#include "ftpc/St_fpt_Module.h"
#include "ftpc/St_fde_Module.h"
#include "tables/St_g2t_vertex_Table.h"

#include "tables/St_fpt_fptrack_Table.h"
#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TClonesArray.h"
#include "TCanvas.h"
#include "TFile.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcTrkMaker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackMaker)

//_____________________________________________________________________________
StFtpcTrackMaker::StFtpcTrackMaker(const char *name) : StMaker(name), m_fptpar(0), m_fdepar(0)
{
  // default constructor
}

//_____________________________________________________________________________
StFtpcTrackMaker::~StFtpcTrackMaker()
{
  // destructor
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Init()
{
  // Initialisation

  St_DataSet *ftpcpars = GetInputDB("params/ftpc");
  assert(ftpcpars);
  St_DataSetIter  gime(ftpcpars);
  m_fptpar = (St_fpt_fptpar *) gime("fptpars/fptpar");
  m_fdepar = (St_fde_fdepar *) gime("fdepars/fdepar");
  
  // Create Histograms    
  m_q            = new TH1F("fpt_q"         ,"FTPC track charge"                       ,3,-2.,2.);
  m_theta        = new TH1F("fpt_theta"     ,"FTPC theta"                              ,100,-5.0,5.0);
  m_ndedx        = new TH1F("fde_ndedx"     ,"Number of points used in FTPC dE/dx calculation" ,10,1.,11.);
  m_found         = new TH1F("fpt_nrec"      ,"FTPC: number of points found per track"  ,10,1.,11.);
  m_track        = new TH1F("fpt_track"     ,"FTPC: number of tracks found"            ,100,1.,5000.);    
  m_nrec_track   = new TH2F("fpt_hits_mom" ,"FTPC: points found per track vs. momentum",10,1.,11.,100,1.,20.);
 
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Make()
{
  // Here the real stuff happens...
  cout << endl << "Tracking (FTPC) started..." << endl;

  fpt_fptpar_st *fpt_fptpar = m_fptpar->GetTable();
  St_DataSet *ftpc_data = GetDataSet("ftpc_hits");
  St_fpt_fptrack *fpt_fptrack = 0;
  
  if (!ftpc_data) {
    return kStWarn;
  }
  
  //  clusters exist -> do tracking
  St_fcl_fppoint *fcl_fppoint = (St_fcl_fppoint *)ftpc_data->Find("fcl_fppoint");
  
  if (!fcl_fppoint) {
    return kStWarn;
  }
  
  cout<<"Using primary vertex coordinates ";
  Int_t iflag = 0;
  
  //pointer to preVertex dataset
  St_DataSet *preVertex = GetDataSet("preVertex"); 
  
  //iterator
  St_DataSetIter preVertexI(preVertex);
  
  //pointer to preVertex
  St_dst_vertex  *preVtx  = (St_dst_vertex *)preVertexI("preVertex");
  
  if (preVtx) {
    cout<<"(preVertex): ";
    dst_vertex_st *preVtxPtr = preVtx->GetTable();
    
    for (Int_t i = 0; i <preVtx->GetNRows();i++,preVtxPtr++) {
      
      if (preVtxPtr->iflag == 101) {
	fpt_fptpar->primary_vertex[0] = preVtxPtr->x;
	fpt_fptpar->primary_vertex[1] = preVtxPtr->y;
	fpt_fptpar->primary_vertex[2] = preVtxPtr->z;
	iflag = 101;
      }
    }
  }
  
  if ( iflag != 101 ) {
    //    preVertex not found  - compute and store Holm's preVertex
    cout<<"(Holm's vertex): ";

    StFtpcVertex *vertex = new StFtpcVertex(fcl_fppoint->GetTable(), fcl_fppoint->GetNRows());

    if (isnan(vertex->GetZ())) {
      // handels problem if there are not enough tracks and therefore a vertex cannot be found
      cout << "No vertex found! Ftpc tracking stopped!" << endl;
      delete vertex;
      return kStWarn;
    }

    else {

      if (!preVtx) {
	// no preVertex table exists
	// create preVertex table with 1 row
	preVtx = new St_dst_vertex("preVertex",1);
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
      
      fpt_fptpar->primary_vertex[0] = 0.0;
      fpt_fptpar->primary_vertex[1] = 0.0;
      fpt_fptpar->primary_vertex[2] = vertex->GetZ();
      
      // save results in preVertex    
      preVtxPtr->x = 0.0;
      preVtxPtr->y = 0.0;
      preVtxPtr->z = vertex->GetZ();
      preVtxPtr->iflag = 301;
      preVtxPtr->det_id = 4;
      preVtxPtr->id = preVtx->GetNRows();
      preVtxPtr->vtx_id = kEventVtxId;  
    }

    delete vertex;
  }
  
  cout << fpt_fptpar->primary_vertex[0] << ", " << fpt_fptpar->primary_vertex[1] << ", " << fpt_fptpar->primary_vertex[2] << "." << endl;
  
  Double_t vertexPos[3] = {fpt_fptpar->primary_vertex[0], fpt_fptpar->primary_vertex[1], fpt_fptpar->primary_vertex[2]}; 
  StFtpcConfMapper *tracker = new StFtpcConfMapper(fcl_fppoint, vertexPos, Debug());
  
  // settings
  tracker->SetMaxDca(1.);
  tracker->MainVertexSettings(3, 5, 1, 2, 1, 1);
  tracker->NonVertexSettings (3, 5, 1, 2, 1, 1);
  
  // cuts
  // with vertex constraint
  tracker->SetTrackletCuts(0.007, true);
  tracker->SetTrackCuts(0.007, 0.03, 30, true);
  
  // without vertex constraint
  tracker->SetTrackletCuts(0.007, false);
  tracker->SetTrackCuts(0.007, 0.03, 30., false);
  
  // tracking 
  tracker->MainVertexTracking();
  
  if (Debug()) {
    tracker->SettingInfo();
    tracker->CutInfo();
    tracker->TrackingInfo();
  }
  
  if (fpt_fptrack) delete fpt_fptrack;
  fpt_fptrack = new St_fpt_fptrack("fpt_fptrack", 20000);
  m_DataSet->Add(fpt_fptrack);
  tracker->FitAndWrite(fpt_fptrack);
  
  // dE/dx calculation
  if (Debug()) {
    cout << "start fde" << endl;
  }

  Int_t Res_fde = fde(fcl_fppoint, fpt_fptrack, m_fdepar);
  
  if(Debug()) {
    cout << "finish fde: " << Res_fde << endl;
  }
 
  /*
    // Track Display
    
    // Uncomment this block if you want to see (I mean see!) the found tracks.
    
    StFtpcDisplay *display = new StFtpcDisplay(tracker->GetClusters(), tracker->GetTracks(), (Bool_t)false);
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
    
    StFtpcTrackEvaluator *eval = new StFtpcTrackEvaluator(geant, ftpc_data, tracker->GetVertex(), tracker->GetClusters(), tracker->GetTracks(), "ftpc_evaluator.root", "UPDATE");
    eval->Info();
    eval->FillHitsOnTrack();
    eval->FillParentHistos();
    eval->FillMomentumHistos();
    eval->FillEventHistos();
    eval->FillCutHistos();
    eval->DivideHistos();
    eval->WriteHistos();
    
    // Uncomment the following line if you want to 'see' the information (split tracks, unclean tracks, ...) 
    // evaluated by the TrackEvaluator.  
    //eval->ShowTracks();
    
    delete eval;
  */

  delete tracker;

  /*
    // Refitting
    // To do refitting of the tracks after some other module has found a 'better' 
    // main vertex position include the following lines and insert the new vertex position. 
    
    St_DataSet *hit_data = GetDataSet("ftpc_hits");   
    St_fcl_fppoint *points = (St_fcl_fppoint *)hit_data->Find("fcl_fppoint");
    St_DataSet *track_data = GetDataSet("ftpc_tracks"); 
    St_fpt_fptrack *tracks = (St_fpt_fptrack *)track_data->Find("fpt_fptrack");
    
    StFtpcVertex *refit_vertex = new StFtpcVertex(0., 0., 0.);   // insert vertex position (x, y, z) here!
    StFtpcTracker *refitter = new StFtpcTracker(refit_vertex, points, tracks, 1.);
    refitter->FitAndWrite(tracks);
    delete refitter;
    delete refit_vertex;
  */

  MakeHistograms();
  cout << "Tracking (FTPC) completed." << endl << endl;

  return kStOK;;
}


//_____________________________________________________________________________
void StFtpcTrackMaker::MakeHistograms()
{
  // makes histograms
  St_DataSetIter ftpc_tracks(m_DataSet);

  //Get the table
  St_fpt_fptrack *trk = 0;
  trk               = (St_fpt_fptrack *) ftpc_tracks.Find("fpt_fptrack");
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
  // prints some information

  cout << "******************************************************************" << endl;
  cout << "* $Id: StFtpcTrackMaker.cxx,v 1.4 2000/05/15 14:28:12 oldi Exp $ *" << endl;
  cout << "******************************************************************" << endl;
  
  if (Debug()) {
    StMaker::PrintInfo();
  }
}


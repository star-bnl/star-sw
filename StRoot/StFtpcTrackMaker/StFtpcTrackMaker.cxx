// $Id: StFtpcTrackMaker.cxx,v 1.32 2002/04/09 16:10:13 oldi Exp $
// $Log: StFtpcTrackMaker.cxx,v $
// Revision 1.32  2002/04/09 16:10:13  oldi
// Method to get the magentic field factor moved to StFormulary. It works for
// simulation as well, now.
//
// Revision 1.31  2002/04/08 15:38:04  oldi
// Switch for magnetic field factor installed.
// Minor corrections/improvements.
//
// Revision 1.30  2002/04/05 16:51:00  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.29  2002/03/25 12:50:56  oldi
// Customization of Warnings.
//
// Revision 1.28  2002/03/25 09:52:55  jcs
// exit with warning if primary vertex calculation returns nan
//
// Revision 1.27  2002/03/15 10:04:41  oldi
// Adjust eta segments not only to z-position of vertex but to x,y as well.
// Avoid tracking if vertex position is outside of the inner radius of the Ftpc.
//
// Revision 1.26  2002/03/01 14:21:21  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.25  2002/02/20 16:11:15  jcs
// change loop over vertex table to test if first row is primary vertex
//
// Revision 1.24  2002/02/10 21:04:49  jcs
// Use primary vertex for tracking if it is available. Otherwise use preVertex.
//
// Revision 1.23  2002/02/05 13:53:09  jcs
// remove code for ZDC and Holm's primary vertex calculation methods
// if no preVertex available, stop FTPC tracking
//
// Revision 1.22  2001/09/19 20:58:40  jcs
// Use TPC preVertex if it exists, if not use ZDC vertex(slew correction hardwired)
//
// Revision 1.21  2001/07/26 13:42:27  oldi
// Two messages added for the case of missing data.
//
// Revision 1.20  2001/07/13 17:58:18  oldi
// Small change related to new StFtpcDisplay (commented).
//
// Revision 1.19  2001/07/12 13:08:49  oldi
// Remove display !@#$%^&*(.
//
// Revision 1.18  2001/07/12 13:05:00  oldi
// QA histogram of FTPC vertex estimation is generated.
// FTPC vertex estimation is stored as pre vertex (id = 301) in any case, now.
//
// Revision 1.17  2001/07/12 08:42:28  oldi
// Minor update.
//
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
// Changed couts to gMessMgr->Message().
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
#include "StFormulary.hh"

#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StChain.h"
#include "StVertexId.h"
#include "StMessMgr.h"

#include "tables/St_fpt_fptrack_Table.h"
#include "tables/St_ffs_gepoint_Table.h"
#include "tables/St_g2t_track_Table.h"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "tables/St_dst_TrgDet_Table.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TCanvas.h"
#include "TFile.h"

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
  m_vtx_pos      = new TH1F("fpt_vtx_pos"   ,"FTPC estimated vertex position"                  ,800, -400., 400.);
  m_q            = new TH1F("fpt_q"         ,"FTPC track charge"                               ,  3,-2. ,  2.  );
  m_theta        = new TH1F("fpt_theta"     ,"FTPC theta"                                      ,100,-5.0,  5.0 );
  m_ndedx        = new TH1F("fde_ndedx"     ,"Number of points used in FTPC dE/dx calculation" , 10, 1. , 11.  );
  m_found        = new TH1F("fpt_nrec"      ,"FTPC: number of points found per track"          , 10, 1. ,  11. );
  m_track        = new TH1F("fpt_track"     ,"FTPC: number of tracks found"                    ,100, 1. ,5000. );    
  m_nrec_track   = new TH2F("fpt_hits_mom"  ,"FTPC: points found per track vs. momentum"       , 10, 1. ,  11. , 100, 1., 20.);

  m_padvstime_West = new TH2F("fpt_padvstimeW", "FTPCW padlength vs. timelength", 12, 0.5, 12.5, 10, 0.5, 10.5);
  m_padvstime_East = new TH2F("fpt_padvstimeE", "FTPCE padlength vs. timelength", 12, 0.5, 12.5, 10, 0.5, 10.5);
  m_maxadc_West = new TH1F("fpt_maxadcW", "FTPCW MaxAdc", 50, 0.5, 50.5);
  m_maxadc_East = new TH1F("fpt_maxadcE", "FTPCE MaxAdc", 50, 0.5, 50.5);
  m_charge_West = new TH1F("fpt_chargeW", "FTPCW charge", 50, 0.5, 500.5);
  m_charge_East = new TH1F("fpt_chargeE", "FTPCE charge", 50, 0.5, 500.5);
 
  m_xres = new TH1F("fpt_x_res", "FTPC x residuals", 100, -0.25, 0.25);
  m_yres = new TH1F("fpt_y_res", "FTPC y residuals", 100, -0.25, 0.25);
  m_rres = new TH1F("fpt_r_res", "FTPC r residuals", 100, -0.25, 0.25);
  m_phires = new TH1F("fpt_phi_res", "FTPC phi residuals", 100, -0.01, 0.01);

  m_rres_vs_r_east = new TH2F("fpt_r_res_vs_r_east", "FTPC east r residuals vs. r", 100, -0.25, 0.25, 100, 6.5, 31.);
  m_phires_vs_r_east = new TH2F("fpt_phi_res_vs_r_east", "FTPC east phi residuals vs. r", 100, -0.01, 0.01, 100, 6.5, 31.);
  m_rres_vs_r_west = new TH2F("fpt_r_res_vs_r_west", "FTPC west r residuals vs. r", 100, -0.25, 0.25, 100, 6.5, 31.);
  m_phires_vs_r_west = new TH2F("fpt_phi_res_vs_r_west", "FTPC west phi residuals vs. r", 100, -0.01, 0.01, 100, 6.5, 31.);

  m_vertex_east_xy = new TH2F("fpt_vertex_east_xy", "FTPC east vertex xy estimation with resp. to TPC vertex", 80, -2., 2., 80, -2., 2.);
  m_vertex_east_z = new TH1F("fpt_vertex_east_z", "FTPC east vertex z estimation with resp. to TPC vertex", 100, -10., 10.);
  m_vertex_west_xy = new TH2F("fpt_vertex_west_xy", "FTPC west vertex xy estimation with resp. to TPC vertex", 80, -2., 2., 80, -2., 2.);
  m_vertex_west_z = new TH1F("fpt_vertex_west_z", "FTPC west vertex z estimation with resp. to TPC vertex", 100, -10., 10.);

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StFtpcTrackMaker::Make()
{
  // Setup and tracking.

  gMessMgr->Message("", "I", "OST") << "Tracking (FTPC) started..." << endm;

  St_DataSet *ftpc_data = GetDataSet("ftpc_hits");
  
  if (!ftpc_data) {
    gMessMgr->Message("", "W", "OST") << "No FTPC data available!" << endm;
    return kStWarn;
  }
  
  //  clusters exist -> do tracking
  St_fcl_fppoint *fcl_fppoint = (St_fcl_fppoint *)ftpc_data->Find("fcl_fppoint");
  
  if (!fcl_fppoint) {
    gMessMgr->Message("", "W", "OST") << "No FTPC clusters available!" << endm;
    return kStWarn;
  }
  
  Float_t primary_vertex_x = 0.;
  Float_t primary_vertex_y = 0.;
  Float_t primary_vertex_z = 0.;
  Float_t primary_vertex_x_err = 0.;
  Float_t primary_vertex_y_err = 0.;
  Float_t primary_vertex_z_err = 0.;
    Int_t primary_vertex_id = 0;
    Int_t iflag = 0;
  
  // Use Primary vertex if it exists
  St_DataSet * primary = GetDataSet("primary");
  if (primary) {
   St_dst_vertex *vertex = (St_dst_vertex *) primary->Find("vertex");
   if (vertex) {
     dst_vertex_st *primvtx = vertex->GetTable();
       for( Int_t no_rows=0; no_rows<vertex->GetNRows(); no_rows++,primvtx++){
          if( primvtx->vtx_id == kEventVtxId && primvtx->iflag == 1 ) {
             primary_vertex_x = primvtx->x;
             primary_vertex_y = primvtx->y;
             primary_vertex_z = primvtx->z;
             if (isnan(primary_vertex_x_err = TMath::Sqrt(primvtx->covar[0]))) primary_vertex_x_err = 0.;
             if (isnan(primary_vertex_y_err = TMath::Sqrt(primvtx->covar[2]))) primary_vertex_y_err = 0.;
             if (isnan(primary_vertex_z_err = TMath::Sqrt(primvtx->covar[5]))) primary_vertex_z_err = 0.;
	     iflag = primvtx->iflag;
	     primary_vertex_id = primvtx->id;
             break;
          }       
        }
     }  // end of if (vertex)
   }  // end of if (primary) 
   
   if (iflag == 0 ) {

    // Otherwise use TPC preVertex if it exists

   //pointer to preVertex dataset
   St_DataSet *preVertex = GetDataSet("preVertex");
   if (preVertex) {

      //iterator
      St_DataSetIter preVertexI(preVertex);

     //pointer to preVertex
     St_dst_vertex  *preVtx  = (St_dst_vertex *)preVertexI("preVertex");

     dst_vertex_st *preVtxPtr = preVtx->GetTable();
     for (Int_t i = 0; i < preVtx->GetNRows(); i++, preVtxPtr++) {
       if (preVtxPtr->iflag == 101) {
         primary_vertex_x =  preVtxPtr->x;
         primary_vertex_y =  preVtxPtr->y;
         primary_vertex_z =  preVtxPtr->z;
         if (isnan(primary_vertex_x_err = TMath::Sqrt(preVtxPtr->covar[0]))) primary_vertex_x_err = 0.;
         if (isnan(primary_vertex_y_err = TMath::Sqrt(preVtxPtr->covar[2]))) primary_vertex_y_err = 0.;
         if (isnan(primary_vertex_z_err = TMath::Sqrt(preVtxPtr->covar[5]))) primary_vertex_z_err = 0.;
	 iflag = preVtxPtr->iflag;
	 primary_vertex_id = preVtxPtr->id;
	 break;
       }
     }
    }  // end of if (preVertex)
  } // end of else (preVertex)
 
  if (iflag == 1) {
    // TPC  Vertex used
    gMessMgr->Message("", "I", "OST") << "Using Tpc Vertex (" << primary_vertex_x << "+-" << primary_vertex_x_err << ", " << primary_vertex_y << "+-" << primary_vertex_y_err << ", " << primary_vertex_z << "+-" << primary_vertex_z_err <<  ") for Ftpc tracking." << endm;
  }
  if (iflag == 101) {
    // TPC  preVertex used
    gMessMgr->Message("", "I", "OST") << "Using Tpc preVertex estimation (" << primary_vertex_x << "+-" << primary_vertex_x_err << ", " << primary_vertex_y << "+-" << primary_vertex_y_err << ", " << primary_vertex_z << "+-" << primary_vertex_z_err <<  ") for Ftpc tracking." << endm;
  }
  if (iflag == 0) {
    //  No vertex found, no FTPC tracking is possible
    gMessMgr->Message("", "W", "OST") << "StFtpcTrackMaker::Make() - no vertex found - no tracking" << endm;

    // ----------------------------------------------------
    // debug
    //primary_vertex_x=primary_vertex_y=primary_vertex_z=0;
    //gMessMgr->Message("", "I", "OST") << "DEBUG : Using Tpc Vertex (" << primary_vertex_x << ", " << primary_vertex_y << ", " << primary_vertex_z <<  ") for Ftpc tracking." << endm;
    // ----------------------------------------------------

    return kStWarn;
  }

  // check for the position of the main vertex
  if (isnan(primary_vertex_x) || isnan(primary_vertex_y) || isnan(primary_vertex_z)) {
      // No tracking!
      gMessMgr->Message("", "W", "OST") << "StFtpcTrackMaker::Make() - error in vertex calculation - no tracking" << endm;
      return kStWarn;
  } 

  Double_t z = TMath::Abs(primary_vertex_z);
  Double_t radius = TMath::Sqrt(primary_vertex_x*primary_vertex_x + primary_vertex_y*primary_vertex_y);
  
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
      gMessMgr->Message("Found vertex is more than 50 cm off from z = 0 but  Ftpc tracking is still possible.", "W", "OTS");
      // Do tracking.
    }
  }

  if (radius >= 7.73) {
    gMessMgr->Message("Found vertex x-z-position is greater than 7.73 cm (inner Ftpc radius). No Ftpc tracking possible.", "E", "OTS");
    
    // No tracking!
    return kStWarn; 
  }
  
  // get magnetic field
  Double_t mag_fld_factor = StFormulary::GetMagneticFieldFactor();
  Double_t vertexPos[6] = {primary_vertex_x,     primary_vertex_y,     primary_vertex_z, 
			   primary_vertex_x_err, primary_vertex_y_err, primary_vertex_z_err};
  StFtpcConfMapper *tracker = new StFtpcConfMapper(fcl_fppoint, vertexPos, kTRUE);

  // tracking 
  if (mag_fld_factor == 0.) {
    tracker->NoFieldTracking();
  }

  else {
    tracker->MainVertexTracking();
  }

  // for the line above you have these possibilities
  //tracker->MainVertexTracking();
  //tracker->FreeTracking();
  //tracker->NoFieldTracking();
  //tracker->LaserTracking();

  St_fpt_fptrack *fpt_fptrack = new St_fpt_fptrack("fpt_fptrack", tracker->GetNumberOfTracks());
  m_DataSet->Add(fpt_fptrack);

  // momentum fit, dE/dx calculation, write tracks to tables
  if (mag_fld_factor != 0.) {
    // momentum fit possible
    tracker->FitAnddEdxAndWrite(fpt_fptrack, m_fdepar->GetTable(), -primary_vertex_id);
    tracker->EstimateVertex(tracker->GetVertex(), 1);
  }

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
  //display->ShowTracks();
  display->WriteData("ftpc_display.root");
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

  MakeHistograms(tracker);

  delete tracker;

  //MakeHistograms();
  gMessMgr->Message("", "I", "OST") << "Tracking (FTPC) completed." << endm;

  return kStOK;;
}


//_____________________________________________________________________________
void StFtpcTrackMaker::MakeHistograms()
{
  // Fill histograms.

  St_DataSetIter ftpc_tracks(m_DataSet);

  //Get the table
  St_fpt_fptrack *trk = 0;
  trk = (St_fpt_fptrack *) ftpc_tracks.Find("fpt_fptrack");

  if (trk) 
    {
      // Fill histograms for FTPC fpt,fte,fde
      
      fpt_fptrack_st *r = trk->GetTable();
      for (Int_t i=0; i<trk->GetNRows();i++,r++) 
	{
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
void   StFtpcTrackMaker::MakeHistograms(StFtpcTracker *tracker)
{
  // Fill histograms.

  m_vertex_east_xy->Fill(tracker->GetVertexEast()->GetX()-tracker->GetVertex()->GetX(),
			 tracker->GetVertexEast()->GetY()-tracker->GetVertex()->GetY());
  m_vertex_east_z->Fill(tracker->GetVertexEast()->GetZ()-tracker->GetVertex()->GetZ());
  m_vertex_west_xy->Fill(tracker->GetVertexWest()->GetX()-tracker->GetVertex()->GetX(),
			 tracker->GetVertexWest()->GetY()-tracker->GetVertex()->GetY());
  m_vertex_west_z->Fill(tracker->GetVertexWest()->GetZ()-tracker->GetVertex()->GetZ());

  for (Int_t t_counter = 0; t_counter < tracker->GetTracks()->GetEntriesFast(); t_counter++) 
    {
      StFtpcTrack *tracks = (StFtpcTrack*) tracker->GetTracks()->At(t_counter);
      TObjArray   *fhits  = (TObjArray*) tracks->GetHits();
      
      m_nrec_track->Fill(tracks->GetNumberOfPoints(),tracks->GetP());
      m_found->Fill(tracks->GetNumberOfPoints());
      m_q->Fill(tracks->GetCharge());
      m_theta->Fill(tracks->GetTheta());
      m_ndedx->Fill(tracks->GetdEdx());

      for (Int_t h_counter = 0; h_counter < fhits->GetEntriesFast(); h_counter++) 
	{
	  StFtpcPoint *mhit = (StFtpcPoint *) fhits->At(h_counter);

	  // Residuals
	  if (mhit->GetUsage()) {
	    m_xres->Fill(mhit->GetXResidual());
	    m_yres->Fill(mhit->GetYResidual());
	    m_rres->Fill(mhit->GetRResidual());
	    m_phires->Fill(mhit->GetPhiResidual());
	  }

	  if (mhit->GetPadRow()<=10)
	    {
	      m_maxadc_West->Fill(mhit->GetMaxADC());
	      m_charge_West->Fill(mhit->GetCharge());
	      m_padvstime_West->Fill(mhit->GetNumberBins(),mhit->GetNumberPads());

	      if (mhit->GetUsage()) {
		m_rres_vs_r_west->Fill(mhit->GetRResidual(), mhit->GetRadius());
		m_phires_vs_r_west->Fill(mhit->GetPhiResidual(), mhit->GetRadius());
	      }
	    }

	  else if (mhit->GetPadRow()>=11)
	    {
	      m_maxadc_East->Fill(mhit->GetMaxADC());
	      m_charge_East->Fill(mhit->GetCharge());
	      m_padvstime_East->Fill(mhit->GetNumberBins(),mhit->GetNumberPads());

	      if (mhit->GetUsage()) {
		m_rres_vs_r_east->Fill(mhit->GetRResidual(), mhit->GetRadius());
		m_phires_vs_r_east->Fill(mhit->GetPhiResidual(), mhit->GetRadius());
	      }
	    }
	}
    }
}

//_____________________________________________________________________________
void StFtpcTrackMaker::PrintInfo()
{
  // Prints information.

  gMessMgr->Message("", "I", "OST") << "******************************************************************" << endm;
  gMessMgr->Message("", "I", "OST") << "* $Id: StFtpcTrackMaker.cxx,v 1.32 2002/04/09 16:10:13 oldi Exp $ *" << endm;
  gMessMgr->Message("", "I", "OST") << "******************************************************************" << endm;
  
  if (Debug()) {
    StMaker::PrintInfo();
  }
}


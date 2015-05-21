// $Id: StFtpcDisplay.cc,v 1.20 2015/05/21 21:53:38 genevb Exp $
// $Log: StFtpcDisplay.cc,v $
// Revision 1.20  2015/05/21 21:53:38  genevb
// Fixing memory leak (note: vers. 1.16 modifcations for old compilers no longer necessary)
//
// Revision 1.19  2009/08/25 19:41:19  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.18  2007/01/15 08:23:01  jcs
// replace printf, cout and gMesMgr with Logger commands
//
// Revision 1.17  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.16  2003/01/16 18:04:32  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.15  2002/11/06 13:45:14  oldi
// Code clean ups.
//
// Revision 1.14  2002/10/11 15:45:07  oldi
// Get FTPC geometry and dimensions from database.
// No field fit activated: Returns momentum = 0 but fits a helix.
// Bug in TrackMaker fixed (events with z_vertex > outer_ftpc_radius were cut).
// QA histograms corrected (0 was supressed).
// Code cleanup (several lines of code changed due to *params -> Instance()).
// cout -> gMessMgr.
//
// Revision 1.13  2002/04/29 15:49:55  oldi
// All tracking parameters moved to StFtpcTrackingParameters.cc/hh.
// In a future version the actual values should be moved to an .idl file (the
// interface should be kept as is in StFtpcTrackingParameters.cc/hh).
//
// Revision 1.12  2002/04/05 16:50:17  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.11  2001/09/27 14:00:35  oldi
// Small change to avoid ambiguous call of TPolyMarker() constructor.
//
// Revision 1.10  2001/07/13 18:00:50  oldi
// New function WriteData() added. It writes TPolyMarkers and TPolyLine3Ds to
// a file which can be used to display results (pictures) offline.
//
// Revision 1.9  2001/07/12 13:02:25  oldi
// Boundaries of FTPC set to correct values (7.73, 30.05).
//
// Revision 1.8  2001/04/25 17:53:42  perev
// HPcorrs
//
// Revision 1.7  2000/11/10 18:37:23  oldi
// It is now possible to display tracks which should be longer (so called 'short tracks').
//
// Revision 1.6  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.5  2000/06/07 11:33:01  oldi
// Changed 0 pointers to NULL pointers.
// Names of variables changed to make the code easier to understand.
// Major changes to allow to display unclean, split and good (found) tracks and
// clusters with different colors.
// Major changes to allow to display electron, non main vertex and good (GEANT) \tracks and clusters with different colors.
// Allow to display specific momentum ranges only.
//
// Revision 1.4  2000/05/15 14:28:09  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.3  2000/05/12 12:59:13  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.2  2000/05/11 15:14:43  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:13  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 17.10.2000
//----------Copyright:     &copy MDO Production 2000

#include "StFtpcDisplay.hh"
#include "StFtpcTrackingParams.hh"
#include "StFtpcConfMapPoint.hh"
#include "StFtpcTrack.hh"
#include "TMath.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TPolyMarker.h"
#include "TPolyMarker3D.h"
#include "TLine.h"
#include "TGraph.h"
#include "TMarker.h"
#include "TTUBE.h"
#include "TBRIK.h"
#include "TFile.h"
//#include "TCONE.h"


////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcDisplay class - displays tracks and clusters.                            //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////


ClassImp(StFtpcDisplay)


StFtpcDisplay::StFtpcDisplay() 
{
  // Default constructor.
  
  mNumRowSegment = StFtpcTrackingParams::Instance()->RowSegments();
  mNumPhiSegment = StFtpcTrackingParams::Instance()->PhiSegments();
  mNumEtaSegment = StFtpcTrackingParams::Instance()->EtaSegments();
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  
  mTrack = 0;
  mHit   = 0;
  mGeantTrack = 0;
  mGeantHit   = 0;
  
  mX_Y_Z      = 0;
  mX_Y_Zplus  = 0;
  mX_Y_Zminus = 0;

  mNode0 = 0;
  mNode1 = 0;
  mNode2 = 0;

  current_line = 0;
  found_line   = 0;
  geant_line   = 0;

  found_hit        = new TPolyMarker3D();
  found_hit_plus   = new TPolyMarker3D();
  found_hit_minus  = new TPolyMarker3D();
  geant_hit        = new TPolyMarker3D();
  geant_hit_plus   = new TPolyMarker3D();
  geant_hit_minus  = new TPolyMarker3D();
  unused_hit       = new TPolyMarker3D();
  unused_hit_plus  = new TPolyMarker3D();
  unused_hit_minus = new TPolyMarker3D();
  wrong_hit        = new TPolyMarker3D();
  wrong_hit_plus   = new TPolyMarker3D();
  wrong_hit_minus  = new TPolyMarker3D();

  found_value        = 0;
  found_value_minus  = 0;
  found_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  geant_value        = 0;
  geant_value_minus  = 0;
  geant_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  wrong_value        = 0;
  wrong_value_minus  = 0;
  wrong_value_plus   = 0;
}


StFtpcDisplay::StFtpcDisplay(TObjArray *hits, TObjArray *tracks) 
{
  // Ususal constructor.

  mNumRowSegment = StFtpcTrackingParams::Instance()->RowSegments();
  mNumPhiSegment = StFtpcTrackingParams::Instance()->PhiSegments();
  mNumEtaSegment = StFtpcTrackingParams::Instance()->EtaSegments();
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;

  mTrack = tracks;
  mHit   = hits;
  mGeantHit   = 0;
  mGeantTrack = 0;

  mX_Y_Z      = 0;
  mX_Y_Zplus  = 0;
  mX_Y_Zminus = 0;

  mNode0 = 0;
  mNode1 = 0;
  mNode2 = 0;

  current_line = 0;
  found_line   = 0;
  geant_line   = 0;

  found_hit        = new TPolyMarker3D();
  found_hit_plus   = new TPolyMarker3D();
  found_hit_minus  = new TPolyMarker3D();
  geant_hit        = new TPolyMarker3D();
  geant_hit_plus   = new TPolyMarker3D();
  geant_hit_minus  = new TPolyMarker3D();
  unused_hit       = new TPolyMarker3D();
  unused_hit_plus  = new TPolyMarker3D();
  unused_hit_minus = new TPolyMarker3D();
  wrong_hit        = new TPolyMarker3D();
  wrong_hit_plus   = new TPolyMarker3D();
  wrong_hit_minus  = new TPolyMarker3D();

  found_value        = 0;
  found_value_minus  = 0;
  found_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  geant_value        = 0;
  geant_value_minus  = 0;
  geant_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  wrong_value        = 0;
  wrong_value_minus  = 0;
  wrong_value_plus   = 0;
}


StFtpcDisplay::StFtpcDisplay(TObjArray *hits, TObjArray *tracks, TObjArray *geanthits, TObjArray *geanttracks)
{
  // Constructor for evaluator output.

  mNumRowSegment = StFtpcTrackingParams::Instance()->RowSegments();
  mNumPhiSegment = StFtpcTrackingParams::Instance()->PhiSegments();
  mNumEtaSegment = StFtpcTrackingParams::Instance()->EtaSegments();
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  
  mTrack = tracks;
  mHit   = hits;
  mGeantTrack = geanttracks;
  mGeantHit   = geanthits;

  mX_Y_Z      = 0;
  mX_Y_Zplus  = 0;
  mX_Y_Zminus = 0;

  mNode0 = 0;
  mNode1 = 0;
  mNode2 = 0;

  current_line = 0;
  found_line   = 0;
  geant_line   = 0;

  found_hit        = new TPolyMarker3D();
  found_hit_plus   = new TPolyMarker3D();
  found_hit_minus  = new TPolyMarker3D();
  geant_hit        = new TPolyMarker3D();
  geant_hit_plus   = new TPolyMarker3D();
  geant_hit_minus  = new TPolyMarker3D();
  unused_hit       = new TPolyMarker3D();
  unused_hit_plus  = new TPolyMarker3D();
  unused_hit_minus = new TPolyMarker3D();
  wrong_hit        = new TPolyMarker3D();
  wrong_hit_plus   = new TPolyMarker3D();
  wrong_hit_minus  = new TPolyMarker3D();

  found_value        = 0;
  found_value_minus  = 0;
  found_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  geant_value        = 0;
  geant_value_minus  = 0;
  geant_value_plus   = 0;
  unused_value       = 0;
  unused_value_minus = 0;
  unused_value_plus  = 0;
  wrong_value        = 0;
  wrong_value_minus  = 0;
  wrong_value_plus   = 0;
} 


StFtpcDisplay::~StFtpcDisplay()
{
  // Destructor.
  
  DeleteAll();

  delete found_hit;
  delete found_hit_plus;
  delete found_hit_minus;
  delete unused_value;
  delete unused_value_minus;
  delete unused_value_plus;
  delete geant_hit;
  delete geant_hit_plus;
  delete geant_hit_minus;
  delete unused_hit;
  delete unused_hit_plus;
  delete unused_hit_minus;
  delete wrong_hit;
  delete wrong_hit_plus;
  delete wrong_hit_minus;
}


void StFtpcDisplay::WriteData(Char_t *filename)
{
  // Writes clusters and tracks to file.
 
  TFile file(filename, "RECREATE");
  file.cd();
  
  Float_t x[100];
  Float_t y[100];
  Float_t z[100];

  Int_t track_entries = mTrack->GetEntriesFast(); 
      
  StFtpcConfMapPoint *cluster;
  StFtpcTrack *track;
  found_line = new TPolyLine3D[track_entries];
  
  // loop over all tracks
  for (Int_t tracks = 0; tracks < track_entries; tracks++) {
    track = (StFtpcTrack *)mTrack->At(tracks);
	
    Int_t cluster_entries = track->GetNumberOfPoints();
	
    // loop over all clusters
    for (Int_t clusters = 0; clusters < cluster_entries && clusters < 100; clusters++) {
      
      cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
      
      // fill point array
      x[clusters] = (Float_t)(cluster->GetX());
      y[clusters] = (Float_t)(cluster->GetY());
      z[clusters] = (Float_t)(cluster->GetZ());
    }
	
    // fill PolyLine for this track
    current_line = &(found_line[tracks]);
    current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
    current_line->SetLineColor(3);

    current_line->Write();
    delete current_line;
  }
	
  Int_t cluster_anz = mHit->GetEntriesFast();

  // coordinates of clusters (+, -, both)
  found_value_plus =   new Float_t[3*cluster_anz];
  found_value_minus =  new Float_t[3*cluster_anz];
  found_value =        new Float_t[3*cluster_anz];
  unused_value_plus =  new Float_t[3*cluster_anz];
  unused_value_minus = new Float_t[3*cluster_anz];
  unused_value =       new Float_t[3*cluster_anz];
    
  StFtpcConfMapPoint *h;
  Int_t cl_plus = 0;
  Int_t cl_minus = 0;  
  Int_t cl = 0;
  Int_t ucl_plus = 0;
  Int_t ucl_minus = 0;  
  Int_t ucl = 0;
  
  // loop over all clusters
  for (Int_t i = 0; i < cluster_anz; i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);
    
    if (h->GetUsage()) { 
      // fill (+, -, both) cluster arrays
      found_value[cl++] = h->GetX();
      found_value[cl++] = h->GetY();
      
      if ((found_value[cl++] = h->GetZ())>0) {
	found_value_plus[cl_plus++] = h->GetX();
	found_value_plus[cl_plus++] = h->GetY();
	found_value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	found_value_minus[cl_minus++] = h->GetX();
	found_value_minus[cl_minus++] = h->GetY();
	found_value_minus[cl_minus++] = h->GetZ();
      }
    }

    else { // unused clusters
      // fill (+, -, both) cluster arrays
      unused_value[ucl++] = h->GetX();
      unused_value[ucl++] = h->GetY();
      
      if ((unused_value[ucl++] = h->GetZ())>0) {
	unused_value_plus[ucl_plus++] = h->GetX();
	unused_value_plus[ucl_plus++] = h->GetY();
	unused_value_plus[ucl_plus++] = h->GetZ();
      }
      
      else {
	unused_value_minus[ucl_minus++] = h->GetX();
	unused_value_minus[ucl_minus++] = h->GetY();
	unused_value_minus[ucl_minus++] = h->GetZ();
      }
    }
  }
  
  // create PolyMarkers
  found_hit->SetPolyMarker(cl/3, found_value, 1);      
  found_hit_plus->SetPolyMarker(cl_plus/3, found_value_plus, 1);      
  found_hit_minus->SetPolyMarker(cl_minus/3, found_value_minus, 1);      
  unused_hit->SetPolyMarker(ucl/3, unused_value, 1);      
  unused_hit_plus->SetPolyMarker(ucl_plus/3, unused_value_plus, 1);      
  unused_hit_minus->SetPolyMarker(ucl_minus/3, unused_value_minus, 1);      
  
  // set colors
  found_hit->SetMarkerColor(2);
  found_hit_plus->SetMarkerColor(2);
  found_hit_minus->SetMarkerColor(2);
  unused_hit->SetMarkerColor(5);
  unused_hit_plus->SetMarkerColor(5);
  unused_hit_minus->SetMarkerColor(5);
     
  found_hit->Write();
  found_hit_plus->Write();
  found_hit_minus->Write();
  unused_hit->Write();
  unused_hit_plus->Write();
  unused_hit_minus->Write();

  file.Close();
}


void StFtpcDisplay::TrackInfo()
{
  // Information about a specific track.
  
  Char_t mode;
  Int_t address;
  Int_t number;

  TCanvas *track_canvas = new TCanvas("track_canvas", "Tracks", 1580, 600);
  track_canvas->Divide(3,1);
  TH2F *phi_frame  = new TH2F("phi_frame",  "phi_frame",   60, -StFtpcTrackingParams::Instance()->OuterRadius(),    StFtpcTrackingParams::Instance()->OuterRadius(), 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  TH2F *eta_frame1 = new TH2F("eta_frame1", "eta_frame1", 120, -270, -150, 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  TH2F *eta_frame2 = new TH2F("eta_frame2", "eta_frame2", 120,  150,  270, 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());

  TH2F *circle_frame = new TH2F("circle_frame", "circle_frame",  60,   -0.15,   0.15,  60, -0.15, 0.15);
  TH2F *z_frame      = new TH2F("z_frame",      "z_frame",      540, -270,    270,    700, -7,    7);

  TCanvas *fit_canvas = new TCanvas("fit_canvas", "Conformal Mapping Coordinates", 1000, 600);
  fit_canvas->Divide(2,1);

  track_canvas->cd(1);
  gPad->SetGridx();
  gPad->SetGridy();
  phi_frame->Draw();
  track_canvas->cd(2);
  gPad->SetGridx();
  gPad->SetGridy();
  eta_frame1->Draw();
  track_canvas->cd(3);
  gPad->SetGridx();
  gPad->SetGridy();
  eta_frame2->Draw();

  TLine *phi_line = new TLine[mNumPhiSegment];

  {for (Int_t i=0; i<mNumPhiSegment; i++) {
    phi_line[i] = TLine(0., 0., StFtpcTrackingParams::Instance()->OuterRadius()*TMath::Cos(i*2*TMath::Pi()/mNumPhiSegment), 
			StFtpcTrackingParams::Instance()->OuterRadius()*TMath::Sin(i*2*TMath::Pi()/mNumPhiSegment));
  }} 

  TLine *eta_line = new TLine[2*mNumEtaSegment+2];

  Double_t eta_value;
  {for (Int_t i=0; i<=mNumEtaSegment/2.; i++) {
    
    eta_value = (4.165-2.396)/(mNumEtaSegment/2.) * i + 2.396;
    
    eta_line[i*2] = TLine(-270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.),
			  -270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			   270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			   270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
    
    eta_line[2*i+1] = TLine( 270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			    -270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			    -270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			     270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
  }}
  
  TObjArray *hits;
  Int_t trackcluster;
  Int_t entries = mTrack->GetEntriesFast();

  TPolyMarker *phi_track = 0;
  TPolyMarker *eta_track = 0;

  TPolyMarker *circle_track = 0;
  TPolyMarker *z_track = 0;

  StFtpcTrack *track;
 
// The following cout's are online prompts - not Logger messages!

 while (kTRUE) {
    
    cout << "Mode: ";
    cin >> mode;

    switch (mode) {
      
    // Mode  =  s   skip this event
    case 's':
      return;
      
    // Mode  =  t   display track
    //              Number = number of track to be displayed
    case 't':
      cout << "Number: ";
      cin >> number;
      
      if (number > entries-1) {
	cout << "Only " << entries << " Tracks in this event found!" << endl;
	continue;
      }

      track = (StFtpcTrack *)mTrack->At(number);

      hits = (TObjArray *)(track->GetHits());
      trackcluster = hits->GetEntriesFast();
      
      if (phi_track) delete phi_track;
      phi_track = new TPolyMarker(trackcluster, (Float_t*)0, (Float_t*)0, "");
      phi_track->SetMarkerStyle(4);
      phi_track->SetMarkerSize(0.4);

      if (eta_track) delete eta_track;
      eta_track = new TPolyMarker(trackcluster, (Float_t*)0, (Float_t*)0, "");
      eta_track->SetMarkerStyle(4);
      eta_track->SetMarkerSize(0.4);

      if (circle_track) delete circle_track;
      circle_track = new TPolyMarker(trackcluster, (Float_t*)0, (Float_t*)0, "");
      circle_track->SetMarkerStyle(4);
      circle_track->SetMarkerSize(0.4);

      if (z_track) delete z_track;
      z_track = new TPolyMarker(trackcluster, (Float_t*)0, (Float_t*)0, "");
      z_track->SetMarkerStyle(4);
      z_track->SetMarkerSize(0.4);


      cout << "Track: " << number << " Cluster: " << trackcluster << endl;
      StFtpcConfMapPoint *h;
      {for (Int_t j = 0; j < trackcluster; j++) {
	
	h = (StFtpcConfMapPoint *)hits->At(j);
	cout << "#" << h->GetHitNumber() << " address:" <<  h << endl;
	phi_track->SetPoint(j, h->GetX(), h->GetY());
	eta_track->SetPoint(j, h->GetZ(), h->GetY());
	circle_track->SetPoint(j, h->GetXprime(), h->GetYprime());
       	  
	z_track->SetPoint(j, h->GetZv(), (track->GetRadius() * TMath::ASin((h->GetY() - track->GetCenterY()) / track->GetRadius()) - track->GetAlpha0())/1000.);
	cout << "z: " << h->GetZv() << ", " << (track->GetRadius() * TMath::ASin((h->GetY() - track->GetCenterY()) / track->GetRadius()) - track->GetAlpha0())/1000. << endl;
      }}
    
      fit_canvas->cd(1);
      circle_frame->Draw();
      circle_track->Draw("same");
      fit_canvas->cd(2);
      z_frame->Draw();
      z_track->Draw("same");
      fit_canvas->Update();
      
      track_canvas->cd(1);
      phi_frame->Draw();
      
      {for (Int_t i=0; i<mNumPhiSegment; i++) {
	phi_line[i].Draw("same");
      }} 
      
      phi_track->Draw("same");

      track_canvas->cd(2);
      eta_frame1->Draw();
      
      {for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
	eta_line[i].Draw("same");
      }} 

      eta_track->Draw("same");
      
      track_canvas->cd(3);
      eta_frame2->Draw();
      
      {for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
	eta_line[i].Draw("same");
      }}

      eta_track->Draw("same");
      track_canvas->Update();
      
      //delete phi_track;
      //delete eta_track;
      continue;

    // Mode  =  p   display point on track
    //              Address = address of point to be dislayed
    case 'p':
      cout << "Address: ";
      cin >> address;
     
      {for (Int_t i = 0; i < entries; i++) {
	track = (StFtpcTrack *)mTrack->At(i);
	
	hits = (TObjArray *)(track->GetHits());
	trackcluster = hits->GetEntriesFast();
	// cout << "Track: " << i << " Cluster: " << trackcluster << endl;
	for (Int_t j = 0; j < trackcluster; j++) {
	  
	  if ((StFtpcConfMapPoint *)address == (StFtpcConfMapPoint *)hits->At(j)) {
	    cout << "Track: " << i << " Cluster #" << j << endl;
	  }
	}
      }}

      continue;
      
    default:
      cout << "Invalid argument - only (t)rack or (p)oint allowed!" << endl;
      return;
    }
  }
 
 delete[] phi_line;
 delete[] eta_line;

  return;
}

/*
void StFtpcDisplay::Info() 
{
  // Information about clusters.

  TCanvas *c = new TCanvas("c", "Cluster Information", 1200, 1000);
  c->Divide(2, 1);
 
  TH1F *segments = new TH1F("segments", "segments", mBounds, 0., mBounds);
  TH1F *rowsegments = new TH1F("rowsegments", "rowsegments", mNumRowSegment, 0. ,mNumRowSegment);
  TH1F *phisegments = new TH1F("phisegments", "phisegments", mNumPhiSegment, 0. ,mNumPhiSegment);
  TH1F *etasegments = new TH1F("etasegments", "etasegments", mNumEtaSegment, 0. ,mNumEtaSegment);
  TH1F *pad = new TH1F("pad", "pad", mNumRowSegment, 1., mNumRowSegment+1);
  TH1F *phi = new TH1F("phi", "phi", mNumPhiSegment*10, 0. ,2*TMath::Pi());
  TH1F *eta = new TH1F("eta", "eta", mNumEtaSegment*10, -4.165, 4.165);
 
  TH2F *xy = new TH2F("xy", "xy", 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  TH2F *phi_eta = new TH2F("phi_eta", "phi_eta", 60, 0., 2*TMath::Pi(), 60, -4.165, 4.165);

  Int_t cluster_anz = mHit->GetEntriesFast();
  cout << endl 
       << "Cluster Information" << endl
       << "-------------------" 
       << endl << endl;
  cout << cluster_anz << " clusters in this event." << endl;
  
  StFtpcConfMapPoint *h;
  TPolyMarker *phi_points = new TPolyMarker(cluster_anz, 0, 0, "");
  TPolyMarker *eta_points = new TPolyMarker(cluster_anz, 0, 0, "");
  
  for (Int_t i = 0; i < cluster_anz; i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);

    cout << "No. " << i << " Pad: " << h->GetPadRow() << "/" << GetRowSegm(h) 
	 << " Phi: " << h->GetPhi() << "/" << GetPhiSegm(h) 
	 << " Eta: " << h->GetEta() << "/" << GetEtaSegm(h) 
	 << " Segm: " << GetSegm(GetRowSegm(h), GetPhiSegm(h), GetEtaSegm(h)) 
	 << endl;

    segments->Fill(GetSegm(GetRowSegm(h), GetPhiSegm(h), GetEtaSegm(h)));
    rowsegments->Fill(GetRowSegm(h));
    phisegments->Fill(GetPhiSegm(h));
    etasegments->Fill(GetEtaSegm(h));

    xy->Fill(h->GetX(), h->GetY(), 1);

    phi_eta->Fill(h->GetPhi(), h->GetEta(), 1);

    pad->Fill(h->GetPadRow());
    phi->Fill(h->GetPhi());
    eta->Fill(h->GetEta());
    phi_points->SetPoint(i, h->GetX(), h->GetY());
    eta_points->SetPoint(i, h->GetZ(), h->GetY());
  } 
 
  c->cd(1);
  gPad->Divide(1,7);
  gPad->cd(1);
  segments->Draw();
  c->cd(1);
  gPad->cd(2);
  rowsegments->Draw();
  c->cd(1);
  gPad->cd(3);
  pad->Draw();
  c->cd(1);
  gPad->cd(4);
  phisegments->Draw();
  c->cd(1);
  gPad->cd(5);
  phi->Draw();
  c->cd(1);
  gPad->cd(6);
  etasegments->Draw();
  c->cd(1);
  gPad->cd(7);
  eta->Draw();

  c->cd(2);
  gPad->Divide(1,2);
  gPad->cd(1);
  xy->Draw();
  c->cd(2);
  gPad->cd(2);
  phi_eta->Draw();

  c->Update();

  // Information about tracks.

  TCanvas *track_canvas = new TCanvas("track_canvas", "Tracks", 1580, 600);
  track_canvas->Divide(3,1);
  TH2F *phi_frame = new TH2F("phi_frame", "phi_frame", 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  TH2F *eta_frame1 = new TH2F("eta_frame1", "eta_frame1", 120, -270, -150, 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  TH2F *eta_frame2 = new TH2F("eta_frame2", "eta_frame2", 120, 150, 270, 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());

  track_canvas->cd(1);
  gPad->SetGridx();
  gPad->SetGridy();
  phi_frame->Draw();
  track_canvas->cd(2);
  gPad->SetGridx();
  gPad->SetGridy();
  eta_frame1->Draw();
  track_canvas->cd(3);
  gPad->SetGridx();
  gPad->SetGridy();
  eta_frame2->Draw();


  TPolyMarker *phi_all = new TPolyMarker(cluster_anz, 0, 0, "");
  TPolyMarker *phi_track = new TPolyMarker(10, 0, 0, "");
  TPolyMarker *eta_all = new TPolyMarker(cluster_anz, 0, 0, "");
  TPolyMarker *eta_track = new TPolyMarker(10, 0, 0, "");

  for (Int_t i = 0; i < cluster_anz; i ++) {
    phi_all->SetPoint(i, 0., 0.);
    eta_all->SetPoint(i, 0., 0.);
    
    if (i<10) {
      phi_track->SetPoint(i, 0., 0.);
      eta_track->SetPoint(i, 0., 0.);
    }
  }

  phi_track->SetMarkerStyle(4);
  phi_track->SetMarkerSize(0.4);
  eta_track->SetMarkerStyle(4);
  eta_track->SetMarkerSize(0.4);
  phi_all->SetMarkerColor(2);
  eta_all->SetMarkerColor(2);
  phi_all->SetMarkerStyle(4);
  phi_all->SetMarkerSize(0.4);
  eta_all->SetMarkerStyle(4);
  eta_all->SetMarkerSize(0.4);


  TLine phi_line[mNumPhiSegment];

  for (Int_t i=0; i<mNumPhiSegment; i++) {
    phi_line[i] = TLine(0., 0., StFtpcTrackingParams::Instance()->OuterRadius()*TMath::Cos(i*2*TMath::Pi()/mNumPhiSegment), 
			StFtpcTrackingParams::Instance()->OuterRadius()*TMath::Sin(i*2*TMath::Pi()/mNumPhiSegment));
  } 

  TLine eta_line[2*mNumEtaSegment+2];

  Double_t eta_value;
  for (Int_t i=0; i<=mNumEtaSegment/2.; i++) {
    
    eta_value = (4.165-2.396)/(mNumEtaSegment/2.) * i + 2.396;
    
    eta_line[2*i] = TLine(-270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.),
			-270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
    
    eta_line[2*i+1] = TLine(270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  -270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  -270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
  }

  Int_t track_anz = mTrack->GetEntriesFast();
  cout << endl 
       << "Track Information" << endl
       << "-----------------" 
       << endl << endl;
  cout << track_anz << " tracks in this event." << endl;

  Float_t x[track_anz][10];
  Float_t y[track_anz][10];
  Float_t z[track_anz][10];

  Bool_t non_stop = kFALSE; 
  Int_t hitcounter = 0;
  
  //track_canvas->cd(1);
  //phi_frame->Draw();
  
  //for (Int_t i=0; i<fNum_phi_segm; i++) {
  //   phi_line[i].Draw("same");
  //} 

  //trac_canvas->cd(2);
  //eta_frame1->Draw();
    
  //for (Int_t i=0; i<2*fNum_eta_segm+2; i++) {
  //  eta_line[i].Draw("same");
  //} 

  //track_canvas->cd(3);
  //eta_frame2->Draw();
  
  //for (Int_t i=0; i<2*fNum_eta_segm+2; i++) {
  //  eta_line[i].Draw("same");
  //} 
  

  TClonesArray g_xy("TGraph", track_anz);
  TClonesArray g_zy("TGraph", track_anz);

  for (Int_t i = 0; i < track_anz; i++) {
    StFtpcTrack *track = (StFtpcTrack *)mTrack->At(i);
    TObjArray *hits = (TObjArray *)(track->GetHits());
    Int_t trackcluster = hits->GetEntriesFast();
    cout << "Track: " << i << " Cluster: " << trackcluster << endl;

    new(g_xy[i]) TGraph(trackcluster, x[i], y[i]);
    new(g_zy[i]) TGraph(trackcluster, z[i], y[i]);

    for (Int_t j = 0; j < trackcluster; j++) {

    h = (StFtpcConfMapPoint *)hits->At(j);
      
      x[i][j] =  h->GetX();
      y[i][j] =  h->GetY();
      z[i][j] =  h->GetZ();

      phi_all->SetPoint(hitcounter, h->GetX(), h->GetY());
      phi_track->SetPoint(j, h->GetX(), h->GetY());
      eta_all->SetPoint(hitcounter, h->GetZ(), h->GetY());
      eta_track->SetPoint(j, h->GetZ(), h->GetY());
      hitcounter++;
    }
    
    // track_canvas->cd(1);
   
    //phi_track->Draw("same");
    
    // gPad->Update();
    
    // for (Int_t here = 0; here < trackcluster; here++) {
    //  phi_track->SetPoint(here, 0., 0.);
    //}
    
    // track_canvas->cd(2);
    //eta_track->Draw("same");
    //gPad->Update();

    //track_canvas->cd(3);
    //eta_track->Draw("same");
    //gPad->Update();
    
    if (non_stop == kFALSE){
      Char_t var;
      cin >> var;
      if (var == '!') non_stop = kTRUE;
    }

    for (Int_t here = 0; here < trackcluster; here++) {
      eta_track->SetPoint(here, 0., 0.);
    }    
  }

  phi_points->SetMarkerColor(4);
  eta_points->SetMarkerColor(4);
  phi_points->SetMarkerStyle(4);
  eta_points->SetMarkerStyle(4);
  phi_points->SetMarkerSize(0.4);
  eta_points->SetMarkerSize(0.4);
  
  track_canvas->cd(1);
  phi_frame->Draw();
  
  for (Int_t i=0; i<mNumPhiSegment; i++) {
    phi_line[i].Draw("same");
  } 

  phi_points->Draw("same");
  phi_all->Draw("same");
  //((TGraph *) g_xy.At(1))->Draw("AC*");
  

//  for(Int_t i = 0; i< track_anz; i++) {
//   ((TGraph *) g_xy.At(i))->Draw("AC*same");
// }
  
  gPad->Update();
 
  track_canvas->cd(2);
  eta_frame1->Draw();
  
  for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
    eta_line[i].Draw("same");
  } 
  
  eta_points->Draw("same");
  eta_all->Draw("same");
  gPad->Update();

  track_canvas->cd(3);
  eta_frame2->Draw();
  
  for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
    eta_line[i].Draw("same");
  } 
  
  eta_points->Draw("same");
  eta_all->Draw("same");
  gPad->Update();

  TCanvas *X_Y = new TCanvas("X_Y", "Blick in Beamrichtung", 600, 600);
  X_Y->cd();
 
  //X_Y->SetOptStat(0);
  phi_frame->Draw();
  phi_points->Draw("same");
  phi_all->Draw("same");
  
}
*/

void StFtpcDisplay::ShowClusters() 
{
  // Displays the clusters (2D view).

  Int_t cluster_anz = mHit->GetEntriesFast();

  TCanvas *X_Yplus = new TCanvas("X_Yplus", "Blick in Beamrichtung +", 1100, 1100);
  TCanvas *X_Yminus = new TCanvas("X_Yminus", "Blick in Beamrichtung -", 1100, 1100);

  TH2F *xy = new TH2F("xy", "xy", 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), 60, -StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius());
  X_Yplus->cd();
  xy->Draw();
  X_Yminus->cd();
  xy->Draw();

  TMarker *ma = new TMarker[cluster_anz];
  StFtpcConfMapPoint *h;
  for (Int_t i = 0; i < cluster_anz; i++) {
    h = (StFtpcConfMapPoint *)mHit->At(i);
    ma[i] = TMarker(h->GetX(), h->GetY(), 4);

    ma[i].SetMarkerSize((TMath::Sqrt(h->GetX()*h->GetX()+h->GetY()*h->GetY()+h->GetZ()*h->GetZ())-163.)/98.*10.*0.1);

    Int_t tn = h->GetTrackNumber();

    if (tn == -1) {
      ma[i].SetMarkerColor(2);
      ma[i].SetMarkerStyle(8);
    }

    else {
      Int_t color = ((tn%54)%6) +1;
      Int_t style = ((tn%54)%9) +2;

      if (color >= 2) color++;
      if (style >= 6) style+=18;
      if (style == 29) style=30;
      
      ma[i].SetMarkerColor(color);
      ma[i].SetMarkerStyle(style);
    }
    if (h->GetZ() > 0) X_Yplus->cd();
    else X_Yminus->cd();
    ma[i].Draw("same");
  }
  
  X_Yplus->Update();
  X_Yminus->Update();

  delete[] ma;
}


void StFtpcDisplay::ShowTracks(Int_t trackanz, Int_t trackarray[]) 
{
  // Displays the found tracks and the clusters in a nice 3D view.
  // You will be asked if you want to see only one of the Ftpcs ('+' or '-') or both.
  //
  // ShowTracks() or ShowTracks(0) displays all tracks and all clusters
  // Showtracks(-1) displays all unused clusters
  // ShowTracks(-2) displays all tracks but no clusters
  // Showtracks(trackanz, x[trackanz]) displays all tracks given by the numbers in the array x[] and all clusters

  // create 3 canvases (for +, -, and both Ftpcs)
  TCanvas *X_Y_Zplus  = new TCanvas("X_Y_Zplus",  "Event +", 600, 600);
  TCanvas *X_Y_Zminus = new TCanvas("X_Y_Zminus", "Event -", 600, 600);
  TCanvas *X_Y_Z      = new TCanvas("X_Y_Z",      "Event",   600, 600);

  // create point of origin (our vertex has the shape of a cube, of course)
  TBRIK *origin = new TBRIK("origin", "origin", "void", 0.1, 0.1, 0.1);
  
  // create 4 tubes (cylinders) - two big ones (out) and two small ones (in) - to draw the Ftpcs
  TTUBE *ftpc1_out = new TTUBE("ftpc1_out", "Ftpc + (out)", "void", StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc1_in =  new TTUBE("ftpc1_in",  "Ftpc + (in)",  "void", StFtpcTrackingParams::Instance()->InnerRadius(), StFtpcTrackingParams::Instance()->InnerRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc2_out = new TTUBE("ftpc2_out", "Ftpc - (out)", "void", StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc2_in =  new TTUBE("ftpc2_in",  "Ftpc - (in)",  "void", StFtpcTrackingParams::Instance()->InnerRadius(), StFtpcTrackingParams::Instance()->InnerRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);

  // set divisions of tubes
  ftpc1_out->SetNumberOfDivisions(50);
  ftpc1_in->SetNumberOfDivisions(50);
  ftpc2_out->SetNumberOfDivisions(50);
  ftpc2_in->SetNumberOfDivisions(50);

  // set colors
  origin->SetLineColor(1);
  ftpc1_out->SetLineColor(4);
  ftpc1_in->SetLineColor(4);
  ftpc2_out->SetLineColor(4);
  ftpc2_in->SetLineColor(4);

  // create 3 nodes (+, -, both) to generate the dependencies of the different geometric shapes
  TNode *node0 = new TNode("node0", "node0", "origin");
  TNode *node2 = new TNode("node2", "node2", "origin");
  TNode *node1 = new TNode("node1", "node1", "origin");

  // create dependencies for 'both' Ftpcs
  X_Y_Z->cd();
  node0->cd();  
  TNode *node01_out = new TNode("node01_out", "node01_out", "ftpc1_out", 0, 0,  StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node01_in  = new TNode("node01_in",  "node01_in",  "ftpc1_in",  0, 0,  StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node02_out = new TNode("node02_out", "node02_out", "ftpc2_out", 0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node02_in  = new TNode("node02_in",  "node02_in",  "ftpc2_in",  0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);  

  // create dependencies for '-' Ftpc
  X_Y_Zminus->cd();
  node2->cd();
  TNode *node2_out = new TNode("node2_out", "node2_out", "ftpc2_out", 0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node2_in = new TNode("node2_in", "node2_in", "ftpc2_in", 0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);

  // create dependencies for '+' Ftpc
  X_Y_Zplus->cd();
  node1->cd();
  TNode *node1_out = new TNode("node1_out", "node1_out", "ftpc1_out", 0, 0, StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node1_in  = new TNode("node1_in",  "node1_in",  "ftpc1_in",  0, 0, StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);

  // The following lines are unnecessary. They avoid compiler warnings. They can be simply deleted.
  node0->cd();
  node1->cd();
  node2->cd();
  node1_in->cd();
  node2_in->cd();
  node1_out->cd();
  node2_out->cd();
  node01_in->cd();
  node02_in->cd();
  node01_out->cd();
  node02_out->cd();

  // fill histograms with tracks and clusters

  // coordinates of trackpoints
  Float_t x[100];
  Float_t y[100];
  Float_t z[100];

  if (trackanz != -1) { // do everything if the '-1' option is not given 
    
    if (trackanz == 0 || trackanz == -2) { // show all tracks
      Int_t track_entries = mTrack->GetEntriesFast(); 
      
      StFtpcConfMapPoint *cluster;
      StFtpcTrack *track;
      found_line = new TPolyLine3D[track_entries];
      
      // loop over all tracks
      for (Int_t tracks = 0; tracks < track_entries; tracks++) {
	track = (StFtpcTrack *)mTrack->At(tracks);
	
	Int_t cluster_entries = track->GetNumberOfPoints();
	
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries && clusters < 100; clusters++) {

	  cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) X_Y_Zplus->cd();
	  else X_Y_Zminus->cd();   
	}
	
	// fill PolyLine for this track
	current_line = &(found_line[tracks]);
	current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	Int_t color = tracks%50+51;
	current_line->SetLineColor(color);

	// draw track in the right canvas
	current_line->Draw("Csame");
	// and draw track in the canvas for both Ftpcs
	X_Y_Z->cd();
	current_line->Draw("Csame");
      }
    }
    
    else { // option was set to a 'trackanz' != (0, -1, or -2) -> show only given tracks
      Int_t track_entries = trackanz;
      
      StFtpcConfMapPoint *cluster;
      StFtpcTrack *track;
      found_line = new TPolyLine3D[track_entries];
      
      // loop over all tracks specified by the given trackarray      
      for (Int_t tracks = 0; tracks < track_entries; tracks++) {
	track = (StFtpcTrack *)mTrack->At(trackarray[tracks]); 
	
	Int_t cluster_entries = track->GetNumberOfPoints();
	
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	  
	  cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) X_Y_Zplus->cd();
	  else X_Y_Zminus->cd();   
	}

	// fill PolyLine for this track
	current_line = &(found_line[tracks]);
	current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	current_line->SetLineColor(3);
	
	// draw track in the right canvas
	current_line->Draw("same");
	// and draw track in the canvas for both Ftpcs
	X_Y_Z->cd();
	current_line->Draw("same");
      }
    }
  }

  if (trackanz != -2) { // show clusters only if '-2' option is not given
    Int_t cluster_anz = mHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    found_value_plus =  new Float_t[3*cluster_anz];
    found_value_minus = new Float_t[3*cluster_anz];
    found_value =       new Float_t[3*cluster_anz];
    
    StFtpcConfMapPoint *h;
    Int_t cl_plus = 0;
    Int_t cl_minus = 0;  
    Int_t cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapPoint *)mHit->At(i);
      
      if (trackanz == -1 && h->GetUsage() == 1) { // don't show used clusters if '-1' option was given 
	continue; 
      }

      // fill (+, -, both) cluster arrays
      found_value[cl++] = h->GetX();
      found_value[cl++] = h->GetY();
      
      if ((found_value[cl++] = h->GetZ())>0) {
	found_value_plus[cl_plus++] = h->GetX();
	found_value_plus[cl_plus++] = h->GetY();
	found_value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	found_value_minus[cl_minus++] = h->GetX();
	found_value_minus[cl_minus++] = h->GetY();
	found_value_minus[cl_minus++] = h->GetZ();
      }
    }
    
    // create PolyMarkers
    found_hit->SetPolyMarker(cl/3, found_value, 1);      
    found_hit_plus->SetPolyMarker(cl_plus/3, found_value_plus, 1);      
    found_hit_minus->SetPolyMarker(cl_minus/3, found_value_minus, 1);      

    // set colors
    found_hit->SetMarkerColor(2);
    found_hit_plus->SetMarkerColor(2);
    found_hit_minus->SetMarkerColor(2);
    
    // switch to right canvas and draw clusters
    X_Y_Z->cd();
    found_hit->Draw("same");
    X_Y_Zplus->cd();
    found_hit_plus->Draw("same");
    X_Y_Zminus->cd();
    found_hit_minus->Draw("same");
  }

  // draw nodes in right canvases
  X_Y_Zplus->cd();
  node1->cd();
  node1->Draw("same");
  X_Y_Zplus->Update();

  X_Y_Zminus->cd();
  node2->cd();
  node2->Draw("same");
  X_Y_Zminus->Update();

  X_Y_Z->cd();
  node0->cd();
  node0->Draw("same");
  X_Y_Z->Update();

  // ask user, which Ftpc(s) ('+', '-', 'b'oth) to display in 3D view
  Char_t a;
  
  while (1) {
    cout << "Please enter '+', '-' or 'b' to see one or both Ftpc's, 'q' to quit: "; 
    cin >> a;

    // call the x3d function (this does the actual 3D displaying) for the right canvas
    if (a == '+') X_Y_Zplus->x3d();
    if (a == '-') X_Y_Zminus->x3d();
    if (a == 'b') X_Y_Z->x3d();
    if (a == 'q') break;
  }  

  // cleanup
  delete X_Y_Zplus;
  delete X_Y_Zminus;
  delete X_Y_Z;

  DeleteFound();

  return;
}


void StFtpcDisplay::ShowEvalTracks(MIntArray *splitArr, MIntArray *uncleanArr, MIntArray *clusterArr) 
{
  // Displays the tracks and the clusters in a nice 3D view (GEANT ant found!).

  // create 3 canvases (for +, -, and both Ftpcs)
  mX_Y_Zplus  = new TCanvas("X_Y_Zplus",  "Event +", 600, 600);
  mX_Y_Zminus = new TCanvas("X_Y_Zminus", "Event -", 600, 600);
  mX_Y_Z      = new TCanvas("X_Y_Z",      "Event",   600, 600);

  // create point of origin (our vertex has the shape of a cube, of course)
  TBRIK *origin = new TBRIK("origin", "origin", "void", 0.1, 0.1, 0.1);
  
  // create 4 tubes (cylinders) - two big ones (out) and two small ones (in) - to draw the Ftpcs
  TTUBE *ftpc1_out = new TTUBE("ftpc1_out", "Ftpc + (out)", "void", StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc1_in  = new TTUBE("ftpc1_in",  "Ftpc + (in)",  "void", StFtpcTrackingParams::Instance()->InnerRadius(), StFtpcTrackingParams::Instance()->InnerRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc2_out = new TTUBE("ftpc2_out", "Ftpc - (out)", "void", StFtpcTrackingParams::Instance()->OuterRadius(), StFtpcTrackingParams::Instance()->OuterRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);
  TTUBE *ftpc2_in  = new TTUBE("ftpc2_in",  "Ftpc - (in)",  "void", StFtpcTrackingParams::Instance()->InnerRadius(), StFtpcTrackingParams::Instance()->InnerRadius(), (StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2., 1);

  // set divisions of tubes
  ftpc1_out->SetNumberOfDivisions(50);
  ftpc1_in->SetNumberOfDivisions(50);
  ftpc2_out->SetNumberOfDivisions(50);
  ftpc2_in->SetNumberOfDivisions(50);

  // set colors
  origin->SetLineColor(1);
  ftpc1_out->SetLineColor(4);
  ftpc1_in->SetLineColor(4);
  ftpc2_out->SetLineColor(4);
  ftpc2_in->SetLineColor(4);

  /*
  TCONE *cone = new TCONE("cone", "cone", "void", 5., StFtpcTrackingParams::Instance()->OuterRadius()/StFtpcTrackingParams::Instance()->PadRowPosZ(9)*254.20-0.22, StFtpcTrackingParams::Instance()->OuterRadius()/StFtpcTrackingParams::Instance()->PadRowPosZ(9)*254.20, StFtpcTrackingParams::Instance()->OuterRadius()/StFtpcTrackingParams::Instance()->PadRowPosZ(9)*258.7-0.22,StFtpcTrackingParams::Instance()->OuterRadius()/StFtpcTrackingParams::Instance()->PadRowPosZ(9)*258.7 );
  cone->SetNumberOfDivisions(100);
  cone->SetLineColor(1);
  */

  // create 3 nodes (+, -, both) to generate the dependencies of the different geometric shapes
  mNode0 = new TNode("node0", "node0", "origin");
  mNode2 = new TNode("node2", "node2", "origin");
  mNode1 = new TNode("node1", "node1", "origin");

  // create dependencies for 'both' Ftpcs
  mX_Y_Z->cd();
  mNode0->cd();  
  TNode *node01_out = new TNode("node01_out", "node01_out", "ftpc1_out", 0, 0,  StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node01_in =  new TNode("node01_in",  "node01_in",  "ftpc1_in",  0, 0,  StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node02_out = new TNode("node02_out", "node02_out", "ftpc2_out", 0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node02_in =  new TNode("node02_in",  "node02_in",  "ftpc2_in",  0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);  

  // create dependencies for '-' Ftpc
  mX_Y_Zminus->cd();
  mNode2->cd();
  TNode *node2_out = new TNode("node2_out", "node2_out", "ftpc2_out", 0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node2_in  = new TNode("node2_in",  "node2_in",  "ftpc2_in",  0, 0, -StFtpcTrackingParams::Instance()->PadRowPosZ(0)-(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);

  // create dependencies for '+' Ftpc
  mX_Y_Zplus->cd();
  mNode1->cd();
  //TNode *cone_1 = new TNode("con1_1", "cone_1", "cone", 0, 0, StFtpcTrackingParams::Instance()->PadRowPosZ(9));
  TNode *node1_out = new TNode("node1_out", "node1_out", "ftpc1_out", 0, 0, StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);
  TNode *node1_in =  new TNode("node1_in",  "node1_in",  "ftpc1_in",  0, 0, StFtpcTrackingParams::Instance()->PadRowPosZ(0)+(StFtpcTrackingParams::Instance()->PadRowPosZ(9)-StFtpcTrackingParams::Instance()->PadRowPosZ(0))/2.);

  // The following lines are unnecessary. They avoid compiler warnings. They can be simply deleted.
  mNode0->cd();
  mNode1->cd();
  mNode2->cd();
  node1_in->cd();
  node2_in->cd();
  node1_out->cd();
  node2_out->cd();
  node01_in->cd();
  node02_in->cd();
  node01_out->cd();
  node02_out->cd();
  //cone_1->cd();

  Char_t a;
  
  Bool_t geant =      (Bool_t) kFALSE;
  Bool_t found =      (Bool_t) kTRUE;
  Bool_t electrons =  (Bool_t) kTRUE;
  Bool_t non_vtx =    (Bool_t) kTRUE;
  Bool_t geant_hits = (Bool_t) kTRUE;
  Bool_t good_geant = (Bool_t) kTRUE;
  Bool_t good_found = (Bool_t) kTRUE;
  Bool_t split =      (Bool_t) kTRUE;
  Bool_t unclean =    (Bool_t) kTRUE;
  Bool_t st =         (Bool_t) kFALSE;
  Bool_t found_hits = (Bool_t) kTRUE;
  Bool_t blue =       (Bool_t) kFALSE;

  Float_t eta_low_geant = 2.0;
  Float_t eta_up_geant  = 4.4;
  Float_t pt_low_geant  = 0.0;
  Float_t pt_up_geant   = 5.0;

  Float_t eta_low_found = 2.0;
  Float_t eta_up_found  = 4.4;
  Float_t pt_low_found  = 0.0;
  Float_t pt_up_found   = 5.0;

  while (1) {
    //gSystem->Exec("/usr/bin/clear");

    cout << endl << endl;
    cout << "Display (f)ound tracks..................: ";
    OnOff(found);
    cout << "---------------------------------------------" << endl;
    cout << "   Show (g)ood tracks..............[red]: ";
    OnOff(good_found);
    cout << "        (s)plit tracks...........[green]: ";
    OnOff(split);
    cout << "        (u)nclean tracks........[yellow]: ";
    OnOff(unclean);
    cout << "        sho(r)t tracks..................: ";
    OnOff(st);
    cout << "        (c)lusters......................: ";
    OnOff(found_hits);
    cout << endl;
    cout << "Display (G)EANT tracks..................: ";
    OnOff(geant);
    cout << "---------------------------------------------" << endl;
    cout << "   Show (e)lectron tracks..........[red]: ";
    OnOff(electrons);
    cout << "        (n)on main vertex tracks [green]: ";
    OnOff(non_vtx);
    cout << "        g(o)od tracks...........[yellow]: ";
    OnOff(good_geant);
    cout << "        c(l)usters................[grey]: ";
    OnOff(geant_hits);
    cout << "Color of tracks changed to (b)lue.......: ";
    OnOff(blue);
    cout << endl;
    cout << "(E)ta range              (2.0 - 4.4)    : " << eta_low_geant << " - " << eta_up_geant << endl;
    cout << "(p)t range               (0.0 - 5.0 GeV): " << pt_low_geant << " - " << pt_up_geant << endl;
    cout << endl;
    cout << "Show (+), (-), or bo(t)h Ftpcs or (q)uit: ";
    cin >> a;
    
    if (a == 'f' || a == 'g' || a == 's' || a == 'u' || a == 'c' || a == 'G' || a == 'e' || a == 'n' || a == 'o' || a == 'l' || a== 'E' || a == 'p' || a == 'b' || a == 'r') {
      
      if (a == 'f') found = !found;
      if (a == 'g') good_found = !good_found;
      if (a == 's') split = !split  ;
      if (a == 'u') unclean = !unclean;
      if (a == 'r') st = !st;
      if (a == 'c') found_hits = !found_hits;
      if (a == 'G') geant = !geant;
      if (a == 'e') electrons = !electrons;
      if (a == 'n') non_vtx = !non_vtx;
      if (a == 'o') good_geant = !good_geant;
      if (a == 'l') geant_hits = !geant_hits;
      if (a == 'b') blue = !blue;
      
      
      if (a == 'E') {
	cout << endl;
	cout << "Enter eta lower and upper limit (" << eta_low_geant << " - " << eta_up_geant << "): ";
	cin >> eta_low_geant >> eta_up_geant;
      }
      
      if (a == 'p') {
	cout << endl;
	cout << "Enter pt lower and upper limit (" << pt_low_geant << " - " << pt_up_geant << "): ";
	cin >> pt_low_geant >> pt_up_geant;
      }  
    }
    
    else {

      if (a == 'q') break;
      
      else {
	cout << endl;
	MIntArray *sp = 0;
	MIntArray *uncl = 0;
	MIntArray *hits = 0;
	
	if (split) sp = splitArr;
	if (unclean) uncl = uncleanArr;
	if (found_hits) hits = clusterArr;
	
	DrawNodes();
	
	if (geant) {
	  FillGeant(electrons, non_vtx, good_geant, geant_hits, eta_low_geant, eta_up_geant, pt_low_geant, pt_up_geant, blue);
	}
	
	if (found) {
	  FillFound(good_found, st, sp, uncl, hits, eta_low_found, eta_up_found, pt_low_found, pt_up_found);
	}
	
	// call the x3d function (this does the actual 3D displaying) for the right canvas
	if (a == '+') mX_Y_Zplus->x3d();
	if (a == '-') mX_Y_Zminus->x3d();
	if (a == 't') mX_Y_Z->x3d();
      }
    }
  }  
  
  // cleanup
  delete mX_Y_Zplus;
  delete mX_Y_Zminus;
  delete mX_Y_Z;
  
  DeleteAll();
  
  return;
}


void StFtpcDisplay::FillGeant(Bool_t electrons, Bool_t non_vtx, Bool_t good, Bool_t geant_hits, Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up, Bool_t blue)
{
  // Fill histograms with tracks and clusters.

  DeleteGeant();

  StFtpcConfMapPoint *cluster;
  StFtpcTrack *track;
  
  // coordinates of trackpoints
  Float_t x[100];
  Float_t y[100];
  Float_t z[100];

  Float_t shift = .05;

  Int_t track_entries = mGeantTrack->GetEntriesFast();   
  geant_line = new TPolyLine3D[track_entries];
  
  // loop over all tracks
  for (Int_t tracks = 0; tracks < track_entries; tracks++) {
    track = (StFtpcTrack *)mGeantTrack->At(tracks);

    if ((track->GetPid() <= 3 && !electrons) || 
	(!track->ComesFromMainVertex() && track->GetPid()>3 && !non_vtx) || 
	(track->ComesFromMainVertex() && track->GetPid()>3 && !good) ||
	TMath::Abs(track->GetEta()) < eta_low || TMath::Abs(track->GetEta()) > eta_up ||
	track->GetPt() < pt_low || track->GetPt() > pt_up) {
      continue;
    }
    
    else {
      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries && clusters < 100; clusters++) {
	
	cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX()-shift);
	y[clusters] = (Float_t)(cluster->GetY()-shift);
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      current_line = &(geant_line[tracks]);
      current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors
      Int_t color;
      
      if (blue) {
	color = 4;
      }

      else {
	
	if (track->GetPid()<=3) {
	  color = 2;
	}
	
	else {
	  
	  if (track->ComesFromMainVertex()) {
	    color = 5;
	  }
	  
	  else {
	    color = 3;
	  }
	}
      }

      current_line->SetLineColor(color);

      // draw track in the right canvas
      current_line->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      current_line->Draw("same");
    }  
  }
  
  // show clusters
  if (geant_hits) {
    Int_t cluster_anz = mGeantHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    geant_value_plus  = new Float_t[3*cluster_anz];
    geant_value_minus = new Float_t[3*cluster_anz];
    geant_value       = new Float_t[3*cluster_anz];
    
    StFtpcConfMapPoint *h;
    Int_t cl_plus = 0;
    Int_t cl_minus = 0;  
    Int_t cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapPoint *)mGeantHit->At(i);
      
      // fill (+, -, both) cluster arrays
      geant_value[cl++] = h->GetX()-shift;
      geant_value[cl++] = h->GetY()-shift;
      
      if ((geant_value[cl++] = h->GetZ())>0) {
	geant_value_plus[cl_plus++] = h->GetX()-shift;
	geant_value_plus[cl_plus++] = h->GetY()-shift;
	geant_value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	geant_value_minus[cl_minus++] = h->GetX()-shift;
	geant_value_minus[cl_minus++] = h->GetY()-shift;
	geant_value_minus[cl_minus++] = h->GetZ();
      }
    }
    
    // create PolyMarkers
    geant_hit->SetPolyMarker(cl/3, geant_value, 1);      
    geant_hit_plus->SetPolyMarker(cl_plus/3, geant_value_plus, 1);      
    geant_hit_minus->SetPolyMarker(cl_minus/3, geant_value_minus, 1);      

    // set colors
    geant_hit->SetMarkerColor(1);
    geant_hit_plus->SetMarkerColor(1);
    geant_hit_minus->SetMarkerColor(1);
    
    // switch to right canvas and draw clusters
    mX_Y_Z->cd();
    geant_hit->Draw("same");
    mX_Y_Zplus->cd();
    geant_hit_plus->Draw("same");
    mX_Y_Zminus->cd();
    geant_hit_minus->Draw("same");
  }
  
  // update canvases
  mX_Y_Zplus->Update();
  mX_Y_Zminus->Update();
  mX_Y_Z->Update();

  return;
}


void StFtpcDisplay::FillFound(Bool_t good_found, Bool_t st, MIntArray *split, MIntArray *unclean, MIntArray *found_hits, Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up)
{
  // Fill histograms with tracks and clusters.

  DeleteFound();

  StFtpcConfMapPoint *cluster;
  StFtpcTrack *track;
  
  // coordinates of trackpoints
  Float_t *x = new Float_t[StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()];
  Float_t *y = new Float_t[StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()];
  Float_t *z = new Float_t[StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide()];

  Int_t track_entries = mTrack->GetEntriesFast();
  Bool_t *good_track_to_show = new Bool_t[track_entries];

  found_line = new TPolyLine3D[track_entries];
  
  for (Int_t good_counter = 0; good_counter < track_entries; good_counter++) {
   StFtpcTrack *track = (StFtpcTrack *)mTrack->At(good_counter);
   
   if (TMath::Abs(track->GetEta()) < eta_low || TMath::Abs(track->GetEta()) > eta_up || track->GetPt() < pt_low || track->GetPt() > pt_up) {
     good_track_to_show[good_counter] = (Bool_t) kFALSE;
   }
   
   else {
     good_track_to_show[good_counter] = (Bool_t) kTRUE;
   }
  }

  if (unclean) {

    for (Int_t unclean_counter = 0; unclean_counter < unclean->GetSize(); unclean_counter++) {
      good_track_to_show[unclean->At(unclean_counter)] = (Bool_t) kFALSE;
    }
  }
    
  if (split) {
    
    for (Int_t split_counter = 0; split_counter < split->GetSize(); split_counter++) {
      good_track_to_show[split->At(split_counter)] = (Bool_t) kFALSE;
    }
  }

  Int_t entry = 0;

  if (split) {
    for (Int_t split_counter = 0; split_counter < split->GetSize(); split_counter++) {
      track = (StFtpcTrack *)mTrack->At(split->At(split_counter));

      if (TMath::Abs(track->GetEta()) < eta_low || TMath::Abs(track->GetEta()) > eta_up || track->GetPt() < pt_low || track->GetPt() > pt_up) {
	continue;
      }

      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	
	cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX());
	y[clusters] = (Float_t)(cluster->GetY());
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      current_line = &(found_line[entry++]);
      current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors
      current_line->SetLineColor(3);
    
      // draw track in the right canvas
      current_line->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      current_line->Draw("same");
    }  
  }

  if (unclean) {

    for (Int_t unclean_counter = 0; unclean_counter < unclean->GetSize(); unclean_counter++) {
      track = (StFtpcTrack *)mTrack->At(unclean->At(unclean_counter));

      if (TMath::Abs(track->GetEta()) < eta_low || TMath::Abs(track->GetEta()) > eta_up || track->GetPt() < pt_low || track->GetPt() > pt_up) {
	continue;
      }

      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	
	cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX());
	y[clusters] = (Float_t)(cluster->GetY());
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      current_line = &(found_line[entry++]);
      current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors
      current_line->SetLineColor(5);
    
      // draw track in the right canvas
      current_line->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      current_line->Draw("same");
    }  
  }

  if (good_found) {

    // loop over all tracks
    for (Int_t tracks = 0; tracks < track_entries; tracks++) {
      
      if (!good_track_to_show[tracks]) {
	continue;
      }
      
      else {
	track = (StFtpcTrack *)mTrack->At(tracks);
	
	Int_t cluster_entries = track->GetNumberOfPoints();
	
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	  
	  cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) mX_Y_Zplus->cd();
	  else mX_Y_Zminus->cd();   
	}
	
	// fill PolyLine for this track
	current_line = &(found_line[tracks]);
	current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	current_line->SetLineColor(2);
	
	// draw track in the right canvas
	current_line->Draw("same");
	// and draw track in the canvas for both Ftpcs
	mX_Y_Z->cd();
	current_line->Draw("same");
      }   
    }
  }

  if (st) {

    // loop over all tracks
    for (Int_t tracks = 0; tracks < track_entries; tracks++) {
          
      track = (StFtpcTrack *)mTrack->At(tracks);

      Int_t cluster_entries = track->GetNumberOfPoints();
      
      if (cluster_entries < StFtpcTrackingParams::Instance()->NumberOfPadRowsPerSide() && track->GetRFirst() > StFtpcTrackingParams::Instance()->InnerRadius() && track->GetRLast() < StFtpcTrackingParams::Instance()->OuterRadius()) { 
      
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	  
	  cluster = (StFtpcConfMapPoint *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) mX_Y_Zplus->cd();
	  else mX_Y_Zminus->cd();   
	}
	
	// fill PolyLine for this track
	current_line = &(found_line[tracks]);
	current_line = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	current_line->SetLineColor(2);
	
	// draw track in the right canvas
	current_line->Draw("same");
	// and draw track in the canvas for both Ftpcs
	mX_Y_Z->cd();
	current_line->Draw("same");   
      }
    }
  }

  // show clusters
  if (found_hits) {
    Int_t cluster_anz = mHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    found_value_plus = new Float_t[3*cluster_anz];
    found_value_minus = new Float_t[3*cluster_anz];
    found_value = new Float_t[3*cluster_anz];
    unused_value_plus = new Float_t[3*cluster_anz];
    unused_value_minus = new Float_t[3*cluster_anz];
    unused_value = new Float_t[3*cluster_anz];
    wrong_value_plus = new Float_t[3*cluster_anz];
    wrong_value_minus = new Float_t[3*cluster_anz];
    wrong_value = new Float_t[3*cluster_anz];
    
    StFtpcConfMapPoint *h;
    Int_t found_cl_plus = 0;
    Int_t found_cl_minus = 0;  
    Int_t found_cl = 0;
    Int_t unused_cl_plus = 0;
    Int_t unused_cl_minus = 0;  
    Int_t unused_cl = 0;
    Int_t wrong_cl_plus = 0;
    Int_t wrong_cl_minus = 0;  
    Int_t wrong_cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapPoint *)mHit->At(i);

      if (found_hits->At(h->GetHitNumber()) == 1) {
	
	// fill (+, -, both) cluster arrays
	found_value[found_cl++] = h->GetX();
	found_value[found_cl++] = h->GetY();
	
	if ((found_value[found_cl++] = h->GetZ())>0) {
	  found_value_plus[found_cl_plus++] = h->GetX();
	  found_value_plus[found_cl_plus++] = h->GetY();
	  found_value_plus[found_cl_plus++] = h->GetZ();
	}
	
	else {
	  found_value_minus[found_cl_minus++] = h->GetX();
	  found_value_minus[found_cl_minus++] = h->GetY();
	  found_value_minus[found_cl_minus++] = h->GetZ();
	}
      }

      else if (found_hits->At(h->GetHitNumber()) == 0) {

	unused_value[unused_cl++] = h->GetX();
	unused_value[unused_cl++] = h->GetY();
	
	if ((unused_value[unused_cl++] = h->GetZ())>0) {
	  unused_value_plus[unused_cl_plus++] = h->GetX();
	  unused_value_plus[unused_cl_plus++] = h->GetY();
	  unused_value_plus[unused_cl_plus++] = h->GetZ();
	}
	
	else {
	  unused_value_minus[unused_cl_minus++] = h->GetX();
	  unused_value_minus[unused_cl_minus++] = h->GetY();
	  unused_value_minus[unused_cl_minus++] = h->GetZ();
	}
      }
	
      else if (found_hits->At(h->GetHitNumber()) == -1) {
	
	wrong_value[wrong_cl++] = h->GetX();
	wrong_value[wrong_cl++] = h->GetY();
	
	if ((wrong_value[wrong_cl++] = h->GetZ())>0) {
	  wrong_value_plus[wrong_cl_plus++] = h->GetX();
	  wrong_value_plus[wrong_cl_plus++] = h->GetY();
	  wrong_value_plus[wrong_cl_plus++] = h->GetZ();
	}
	
	else {
	  wrong_value_minus[wrong_cl_minus++] = h->GetX();
	  wrong_value_minus[wrong_cl_minus++] = h->GetY();
	  wrong_value_minus[wrong_cl_minus++] = h->GetZ();
	}
      }
    }
    
    // create PolyMarkers
    found_hit->SetPolyMarker(found_cl/3, found_value, 1);
    found_hit_plus->SetPolyMarker(found_cl_plus/3, found_value_plus, 1);      
    found_hit_minus->SetPolyMarker(found_cl_minus/3, found_value_minus, 1);      
    unused_hit->SetPolyMarker(unused_cl/3, unused_value, 1);
    unused_hit_plus->SetPolyMarker(unused_cl_plus/3, unused_value_plus, 1);      
    unused_hit_minus->SetPolyMarker(unused_cl_minus/3, unused_value_minus, 1);      
    wrong_hit->SetPolyMarker(wrong_cl/3, wrong_value, 1);
    wrong_hit_plus->SetPolyMarker(wrong_cl_plus/3, wrong_value_plus, 1);      
    wrong_hit_minus->SetPolyMarker(wrong_cl_minus/3, wrong_value_minus, 1);      
    
    // set colors
    found_hit->SetMarkerColor(5);
    found_hit_plus->SetMarkerColor(5);
    found_hit_minus->SetMarkerColor(5);
    unused_hit->SetMarkerColor(1);
    unused_hit_plus->SetMarkerColor(1);
    unused_hit_minus->SetMarkerColor(1);
    wrong_hit->SetMarkerColor(2);
    wrong_hit_plus->SetMarkerColor(2);
    wrong_hit_minus->SetMarkerColor(2);
    
    // switch to right canvas and draw clusters
    mX_Y_Z->cd();
    found_hit->Draw("same");
    unused_hit->Draw("same");
    wrong_hit->Draw("same");
    mX_Y_Zplus->cd();
    found_hit_plus->Draw("same");
    unused_hit_plus->Draw("same");
    wrong_hit_plus->Draw("same");
    mX_Y_Zminus->cd();
    found_hit_minus->Draw("same");
    unused_hit_minus->Draw("same");
    wrong_hit_minus->Draw("same");
  }
  
  // update canvases
  mX_Y_Zplus->Update();
  mX_Y_Zminus->Update();
  mX_Y_Z->Update();

  delete[] x;
  delete[] y;
  delete[] z;
  delete[] good_track_to_show;

  return;
}


void StFtpcDisplay::DeleteFound()
{
  // Deletes objects of found tracks.

  if (found_line) {
    delete[] found_line;
    found_line = 0;
  }

  if (found_value) {
    delete[] found_value;
    found_value = 0;
  }

  if (found_value_plus) {
    delete[] found_value_plus;
    found_value_plus = 0;
  }

  if (found_value_minus) {
    delete[] found_value_minus;
    found_value_minus = 0;
  }

  if (unused_value) {
    delete[] unused_value;
    unused_value = 0;
  }

  if (unused_value_plus) {
    delete[] unused_value_plus;
    unused_value_plus = 0;
  }

  if (unused_value_minus) {
    delete[] unused_value_minus;
    unused_value_minus = 0;
  }

  if (wrong_value) {
    delete[] wrong_value;
    wrong_value = 0;
  }

  if (wrong_value_plus) {
    delete[] wrong_value_plus;
    wrong_value_plus = 0;
  }

  if (wrong_value_minus) {
    delete[] wrong_value_minus;
    wrong_value_minus = 0;
  }

  return;
}


void StFtpcDisplay::DeleteGeant()
{
  // Deletes objects of geant tracks.

  if (geant_line) {
    delete[] geant_line;
    geant_line = 0;
  }

  if (geant_value) {
    delete[] geant_value;
    geant_value = 0;
  }

  if (geant_value_plus) {
    delete[] geant_value_plus;
    geant_value_plus = 0;
  }

  if (geant_value_minus) {
    delete[] geant_value_minus;
    geant_value_minus = 0;
  }

  return;
}

void StFtpcDisplay::DrawNodes()
{
  // Draw nodes in the right canvases and clears the canvases before.
  mX_Y_Zplus->cd();
  mNode1->cd();
  mNode1->Draw("");

  mX_Y_Zminus->cd();
  mNode2->cd();
  mNode2->Draw("");

  mX_Y_Z->cd();
  mNode0->cd();
  mNode0->Draw("");

  return;
}


void StFtpcDisplay::DeleteAll() 
{
  // Deletes objects of found and geant tracks.

  DeleteFound();
  DeleteGeant();
}


void StFtpcDisplay::OnOff(Bool_t on)
{
  // Prints "On" or "Off".

  if (on) {
    cout << "On" << endl;
  }
  
  else {
    cout << "Off" << endl;
  }

  return;
}

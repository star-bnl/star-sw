// $Id: StFtpcDisplay.cc,v 1.1 2000/05/10 13:39:13 oldi Exp $
// $Log: StFtpcDisplay.cc,v $
// Revision 1.1  2000/05/10 13:39:13  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 10.05.2000
//----------Copyright:     &copy MDO Production 2000

#include "StFtpcDisplay.hh"
#include "StFtpcConfMapHit.hh"
#include "StFtpcTrack.hh"
#include "TMath.h"
#include "TSystem.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TPolyMarker.h"
#include "TLine.h"
#include "TGraph.h"
#include "TMarker.h"
#include "TPolyLine3D.h"
#include "TPolyMarker3D.h"
#include "TTUBE.h"
#include "TBRIK.h"


////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// StFtpcDisplay class - displays tracks and clusters.                            //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////


ClassImp(StFtpcDisplay)


StFtpcDisplay::StFtpcDisplay() 
{
  // Default constructor.
  
  mNumRowSegment = 20;
  mNumPhiSegment = 100;
  mNumEtaSegment = 200;
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  
  mTrack = 0;
  mHit = 0;
  mGeantTrack = 0;
  mGeantHit = 0;
  
  mIsGeant = (Bool_t) false;
}


StFtpcDisplay::StFtpcDisplay(TObjArray *hits, TObjArray *tracks) 
{
  // Ususal constructor.
  mNumRowSegment = 20;
  mNumPhiSegment = 100;
  mNumEtaSegment = 200;
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;

  mTrack = tracks;
  mHit = hits;
  mGeantHit = 0;
  mGeantTrack = 0;

  mIsGeant = (Bool_t) false;
}


StFtpcDisplay::StFtpcDisplay(TObjArray *hits, TObjArray *tracks, TObjArray *geanthits, TObjArray *geanttracks)
{
  // Constructor for evaluator.

  mNumRowSegment = 20;
  mNumPhiSegment = 100;
  mNumEtaSegment = 200;
  mBounds = mNumRowSegment * mNumPhiSegment * mNumEtaSegment;
  
  mTrack = tracks;
  mHit = hits;
  mGeantTrack = geanttracks;
  mGeantHit = geanthits;

  mIsGeant = (Bool_t) false;
} 


StFtpcDisplay::~StFtpcDisplay()
{
  // Destructor.
}


void StFtpcDisplay::TrackInfo()
{
  // Information about a specific track.
  
  Char_t mode;
  Int_t address;
  Int_t number;

  TCanvas *track_canvas = new TCanvas("track_canvas", "Tracks", 1580, 600);
  track_canvas->Divide(3,1);
  TH2F *phi_frame = new TH2F("phi_frame", "phi_frame", 60, -30, 30, 60, -30, 30);
  TH2F *eta_frame1 = new TH2F("eta_frame1", "eta_frame1", 120, -270, -150, 60, -30, 30);
  TH2F *eta_frame2 = new TH2F("eta_frame2", "eta_frame2", 120, 150, 270, 60, -30, 30);

  TH2F *circle_frame = new TH2F("circle_frame", "circle_frame", 60, -0.15, 0.15, 60, -0.15, 0.15);
  TH2F *z_frame = new TH2F("z_frame", "z_frame",  540, -270, 270, 700, -7, 7);

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

  for (Int_t i=0; i<mNumPhiSegment; i++) {
    phi_line[i] = TLine(0., 0., 30*TMath::Cos(i*2*TMath::Pi()/mNumPhiSegment), 
			30*TMath::Sin(i*2*TMath::Pi()/mNumPhiSegment));
  } 

  TLine *eta_line = new TLine[2*mNumEtaSegment+2];

  Double_t eta_value;
  for (Int_t i=0; i<=mNumEtaSegment/2.; i++) {
    
    eta_value = (4.165-2.396)/(mNumEtaSegment/2.) * i + 2.396;
    
    eta_line[i*2] = TLine(-270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.),
			-270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
    
    eta_line[2*i+1] = TLine(270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  -270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  -270*TMath::Cos(TMath::ATan(TMath::Exp(-eta_value))*2.), 
			  270*TMath::Sin(TMath::ATan(TMath::Exp(-eta_value))*2.));
  }
  
  TObjArray *hits;
  Int_t trackcluster;
  Int_t entries = mTrack->GetEntriesFast();

  TPolyMarker *phi_track = 0;
  TPolyMarker *eta_track = 0;

  TPolyMarker *circle_track = 0;
  TPolyMarker *z_track = 0;

  StFtpcTrack *track;
 

 while (true) {
    
    cout << "Mode: ";
    cin >> mode;

    switch (mode) {
      
    case 's':
      return;
      
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
      phi_track = new TPolyMarker(trackcluster, 0, 0, "");
      phi_track->SetMarkerStyle(4);
      phi_track->SetMarkerSize(0.4);

      if (eta_track) delete eta_track;
      eta_track = new TPolyMarker(trackcluster, 0, 0, "");
      eta_track->SetMarkerStyle(4);
      eta_track->SetMarkerSize(0.4);

      if (circle_track) delete circle_track;
      circle_track = new TPolyMarker(trackcluster, 0, 0, "");
      circle_track->SetMarkerStyle(4);
      circle_track->SetMarkerSize(0.4);

      if (z_track) delete z_track;
      z_track = new TPolyMarker(trackcluster, 0, 0, "");
      z_track->SetMarkerStyle(4);
      z_track->SetMarkerSize(0.4);


      cout << "Track: " << number << " Cluster: " << trackcluster << endl;
      StFtpcConfMapHit *h;
      for (Int_t j = 0; j < trackcluster; j++) {
	
	h = (StFtpcConfMapHit *)hits->At(j);
	cout << "#" << h->GetHitNumber() << " address:" << (Int_t) h << endl;
	phi_track->SetPoint(j, h->GetX(), h->GetY());
	eta_track->SetPoint(j, h->GetZ(), h->GetY());
	circle_track->SetPoint(j, h->GetXprime(), h->GetYprime());
       	  
	z_track->SetPoint(j, h->GetZv(), (track->GetRadius() * TMath::ASin((h->GetY() - track->GetCenterY()) / track->GetRadius()) - track->GetAlpha0())/1000.);
	cout << "z: " << h->GetZv() << ", " << (track->GetRadius() * TMath::ASin((h->GetY() - track->GetCenterY()) / track->GetRadius()) - track->GetAlpha0())/1000. << endl;
      }
    
      fit_canvas->cd(1);
      circle_frame->Draw();
      circle_track->Draw("same");
      fit_canvas->cd(2);
      z_frame->Draw();
      z_track->Draw("same");
      fit_canvas->Update();
      
      track_canvas->cd(1);
      phi_frame->Draw();
      
      for (Int_t i=0; i<mNumPhiSegment; i++) {
	phi_line[i].Draw("same");
      } 
      
      phi_track->Draw("same");

      track_canvas->cd(2);
      eta_frame1->Draw();
      
      for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
	eta_line[i].Draw("same");
      } 

      eta_track->Draw("same");
      
      track_canvas->cd(3);
      eta_frame2->Draw();
      
      for (Int_t i=0; i<2*mNumEtaSegment+2; i++) {
	eta_line[i].Draw("same");
      } 

      eta_track->Draw("same");
      track_canvas->Update();
      
      //delete phi_track;
      //delete eta_track;
      continue;

    case 'p':
      cout << "Address: ";
      cin >> address;
     
      for (Int_t i = 0; i < entries; i++) {
	track = (StFtpcTrack *)mTrack->At(i);
	
	hits = (TObjArray *)(track->GetHits());
	trackcluster = hits->GetEntriesFast();
	// cout << "Track: " << i << " Cluster: " << trackcluster << endl;
	for (Int_t j = 0; j < trackcluster; j++) {
	  
	  if ((StFtpcConfMapHit *)address == (StFtpcConfMapHit *)hits->At(j)) {
	    cout << "Track: " << i << " Cluster #" << j << endl;
	  }
	}
      }

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
 
  TH2F *xy = new TH2F("xy", "xy", 60, -30., 30., 60, -30., 30.);
  TH2F *phi_eta = new TH2F("phi_eta", "phi_eta", 60, 0., 2*TMath::Pi(), 60, -4.165, 4.165);

  Int_t cluster_anz = mHit->GetEntriesFast();
  cout << endl 
       << "Cluster Information" << endl
       << "-------------------" 
       << endl << endl;
  cout << cluster_anz << " clusters in this event." << endl;
  
  StFtpcConfMapHit *h;
  TPolyMarker *phi_points = new TPolyMarker(cluster_anz, 0, 0, "");
  TPolyMarker *eta_points = new TPolyMarker(cluster_anz, 0, 0, "");
  
  for (Int_t i = 0; i < cluster_anz; i++) {
    h = (StFtpcConfMapHit *)mHit->At(i);

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
  TH2F *phi_frame = new TH2F("phi_frame", "phi_frame", 60, -30, 30, 60, -30, 30);
  TH2F *eta_frame1 = new TH2F("eta_frame1", "eta_frame1", 120, -270, -150, 60, -30, 30);
  TH2F *eta_frame2 = new TH2F("eta_frame2", "eta_frame2", 120, 150, 270, 60, -30, 30);

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
    phi_line[i] = TLine(0., 0., 30*TMath::Cos(i*2*TMath::Pi()/mNumPhiSegment), 
			30*TMath::Sin(i*2*TMath::Pi()/mNumPhiSegment));
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

  Bool_t non_stop = false; 
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

    h = (StFtpcConfMapHit *)hits->At(j);
      
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
    
    if (non_stop == false){
      Char_t var;
      cin >> var;
      if (var == '!') non_stop = true;
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

  TH2F *xy = new TH2F("xy", "xy", 60, -30., 30., 60, -30., 30.);
  X_Yplus->cd();
  xy->Draw();
  X_Yminus->cd();
  xy->Draw();

  TMarker *m = new TMarker[cluster_anz];
  StFtpcConfMapHit *h;
  for (Int_t i = 0; i < cluster_anz; i++) {
    h = (StFtpcConfMapHit *)mHit->At(i);
    m[i] = TMarker(h->GetX(), h->GetY(), 4);

    m[i].SetMarkerSize((TMath::Sqrt(h->GetX()*h->GetX()+h->GetY()*h->GetY()+h->GetZ()*h->GetZ())-163.)/98.*10.*0.1);

    Int_t tn = h->GetTrackNumber();

    if (tn == -1) {
      m[i].SetMarkerColor(2);
      m[i].SetMarkerStyle(8);
    }

    else {
      Int_t color = ((tn%54)%6) +1;
      Int_t style = ((tn%54)%9) +2;

      if (color >= 2) color++;
      if (style >= 6) style+=18;
      if (style == 29) style=30;
      
      m[i].SetMarkerColor(color);
      m[i].SetMarkerStyle(style);
    }
    if (h->GetZ() > 0) X_Yplus->cd();
    else X_Yminus->cd();
    m[i].Draw("same");
  }
  
  X_Yplus->Update();
  X_Yminus->Update();

  delete[] m;
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
  TCanvas *X_Y_Zplus = new TCanvas("X_Y_Zplus", "Event +", 600, 600);
  TCanvas *X_Y_Zminus = new TCanvas("X_Y_Zminus", "Event -", 600, 600);
  TCanvas *X_Y_Z = new TCanvas("X_Y_Z", "Event", 600, 600);

  // create point of origin (our vertex has the shape of a cube, of course)
  TBRIK *origin = new TBRIK("origin","origin","void",0.1,0.1,0.1);
  
  // create 4 tubes (cylinders) - two big ones (out) and two small ones (in) - to draw the Ftpcs
  TTUBE *ftpc1_out = new TTUBE("ftpc1_out", "Ftpc + (out)", "void", 30, 30, (256.45-162.75)/2., 1);
  TTUBE *ftpc1_in = new TTUBE("ftpc1_in", "Ftpc + (in)", "void", 8, 8, (256.45-162.75)/2., 1);
  TTUBE *ftpc2_out = new TTUBE("ftpc2_out", "Ftpc - (out)", "void", 30, 30, (256.45-162.75)/2., 1);
  TTUBE *ftpc2_in = new TTUBE("ftpc2_in", "Ftpc - (in)", "void", 8, 8, (256.45-162.75)/2., 1);

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
  TNode *node0 = new TNode("node0","node0","origin");
  TNode *node2 = new TNode("node2","node2","origin");
  TNode *node1 = new TNode("node1","node1","origin");

  // create dependencies for 'both' Ftpcs
  X_Y_Z->cd();
  node0->cd();  
  TNode *node01_out = new TNode("node01_out", "node01_out", "ftpc1_out", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node01_in = new TNode("node01_in", "node01_in", "ftpc1_in", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node02_out = new TNode("node02_out", "node02_out", "ftpc2_out", 0, 0, -162.75-(256.45-162.75)/2.);
  TNode *node02_in = new TNode("node02_in", "node02_in", "ftpc2_in", 0, 0, -162.75-(256.45-162.75)/2.);  

  // create dependencies for '-' Ftpc
  X_Y_Zminus->cd();
  node2->cd();
  TNode *node2_out = new TNode("node2_out", "node2_out", "ftpc2_out", 0, 0, -162.75-(256.45-162.75)/2.);
  TNode *node2_in = new TNode("node2_in", "node2_in", "ftpc2_in", 0, 0, -162.75-(256.45-162.75)/2.);

  // create dependencies for '+' Ftpc
  X_Y_Zplus->cd();
  node1->cd();
  TNode *node1_out = new TNode("node1_out", "node1_out", "ftpc1_out", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node1_in = new TNode("node1_in", "node1_in", "ftpc1_in", 0, 0, 162.75+(256.45-162.75)/2.);

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

  TPolyLine3D *l = 0;
  Float_t *value = 0;
  Float_t *value_plus = 0;
  Float_t *value_minus = 0;
  
  // coordinates of trackpoints
  Float_t x[100];
  Float_t y[100];
  Float_t z[100];

  //Int_t all_clusters = 0;

  if (trackanz != -1) { // do everything if the '-1' option is not given 
    
    if (trackanz == 0 || trackanz == -2) { // show all tracks
      Int_t track_entries = mTrack->GetEntriesFast(); 
      
      StFtpcConfMapHit *cluster;
      StFtpcTrack *track;
      TPolyLine3D *k;
      l = new TPolyLine3D[track_entries];
      
      // loop over all tracks
      for (Int_t tracks = 0; tracks < track_entries; tracks++) {
	track = (StFtpcTrack *)mTrack->At(tracks);
	
	Int_t cluster_entries = track->GetNumberOfPoints();
	
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries && clusters < 100; clusters++) {

	  cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) X_Y_Zplus->cd();
	  else X_Y_Zminus->cd();   
	}
	
	// fill PolyLine for this track
	k = &(l[tracks]);
	k = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	Int_t color = tracks%50+51;
	k->SetLineColor(color);

	// draw track in the right canvas
	k->Draw("Csame");
	// and draw track in the canvas for both Ftpcs
	X_Y_Z->cd();
	k->Draw("Csame");
      }
    }
    
    else { // option was set to a 'trackanz' != (0, -1, or -2) -> show only given tracks
      Int_t track_entries = trackanz;
      
      StFtpcConfMapHit *cluster;
      StFtpcTrack *track;
      TPolyLine3D *k;
      l = new TPolyLine3D[track_entries];
      
      // loop over all tracks specified by the given trackarray      
      for (Int_t tracks = 0; tracks < track_entries; tracks++) {
	track = (StFtpcTrack *)mTrack->At(trackarray[tracks]); 
	
	Int_t cluster_entries = track->GetNumberOfPoints();
	
	// loop over all clusters
	for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	  
	  cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) X_Y_Zplus->cd();
	  else X_Y_Zminus->cd();   
	}

	// fill PolyLine for this track
	k = &(l[tracks]);
	k = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	k->SetLineColor(3);
	
	// draw track in the right canvas
	k->Draw("same");
	// and draw track in the canvas for both Ftpcs
	X_Y_Z->cd();
	k->Draw("same");
      }
    }
  }

  if (trackanz != -2) { // show clusters only if '-2' option is not given
    Int_t cluster_anz = mHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    Float_t *value_plus = new Float_t[3*cluster_anz];
    Float_t *value_minus = new Float_t[3*cluster_anz];
    Float_t *value = new Float_t[3*cluster_anz];
    
    StFtpcConfMapHit *h;
    Int_t cl_plus = 0;
    Int_t cl_minus = 0;  
    Int_t cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapHit *)mHit->At(i);
      
      if (trackanz == -1 && h->GetUsage() == 1) { // don't show used clusters if '-1' option was given 
	continue; 
      }

      // fill (+, -, both) cluster arrays
      value[cl++] = h->GetX();
      value[cl++] = h->GetY();
      
      if ((value[cl++] = h->GetZ())>0) {
	value_plus[cl_plus++] = h->GetX();
	value_plus[cl_plus++] = h->GetY();
	value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	value_minus[cl_minus++] = h->GetX();
	value_minus[cl_minus++] = h->GetY();
	value_minus[cl_minus++] = h->GetZ();
      }
    }
    
    // create PolyMarkers
    TPolyMarker3D *m = new TPolyMarker3D(cl/3, value, 1);      
    TPolyMarker3D *m_plus = new TPolyMarker3D(cl_plus/3, value_plus, 1);      
    TPolyMarker3D *m_minus = new TPolyMarker3D(cl_minus/3, value_minus, 1);      

    // set colors
    m->SetMarkerColor(2);
    m_plus->SetMarkerColor(2);
    m_minus->SetMarkerColor(2);
    
    // switch to right canvas and draw clusters
    X_Y_Z->cd();
    m->Draw("same");
    X_Y_Zplus->cd();
    m_plus->Draw("same");
    X_Y_Zminus->cd();
    m_minus->Draw("same");
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

  if (l) delete[] l;
  if (value) delete[] value;
  if (value_plus) delete[] value_minus;
  if (value_minus) delete[] value_minus;

  return;
}


void StFtpcDisplay::ShowEvalTracks(MIntArray *splitArr, MIntArray *uncleanArr) 
{
  // Displays the found tracks and the clusters in a nice 3D view.

  // create 3 canvases (for +, -, and both Ftpcs)
  mX_Y_Zplus = new TCanvas("X_Y_Zplus", "Event +", 600, 600);
  mX_Y_Zminus = new TCanvas("X_Y_Zminus", "Event -", 600, 600);
  mX_Y_Z = new TCanvas("X_Y_Z", "Event", 600, 600);

  // create point of origin (our vertex has the shape of a cube, of course)
  TBRIK *origin = new TBRIK("origin","origin","void",0.1,0.1,0.1);
  
  // create 4 tubes (cylinders) - two big ones (out) and two small ones (in) - to draw the Ftpcs
  TTUBE *ftpc1_out = new TTUBE("ftpc1_out", "Ftpc + (out)", "void", 30, 30, (256.45-162.75)/2., 1);
  TTUBE *ftpc1_in = new TTUBE("ftpc1_in", "Ftpc + (in)", "void", 8, 8, (256.45-162.75)/2., 1);
  TTUBE *ftpc2_out = new TTUBE("ftpc2_out", "Ftpc - (out)", "void", 30, 30, (256.45-162.75)/2., 1);
  TTUBE *ftpc2_in = new TTUBE("ftpc2_in", "Ftpc - (in)", "void", 8, 8, (256.45-162.75)/2., 1);

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
  mNode0 = new TNode("node0","node0","origin");
  mNode2 = new TNode("node2","node2","origin");
  mNode1 = new TNode("node1","node1","origin");

  // create dependencies for 'both' Ftpcs
  mX_Y_Z->cd();
  mNode0->cd();  
  TNode *node01_out = new TNode("node01_out", "node01_out", "ftpc1_out", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node01_in = new TNode("node01_in", "node01_in", "ftpc1_in", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node02_out = new TNode("node02_out", "node02_out", "ftpc2_out", 0, 0, -162.75-(256.45-162.75)/2.);
  TNode *node02_in = new TNode("node02_in", "node02_in", "ftpc2_in", 0, 0, -162.75-(256.45-162.75)/2.);  

  // create dependencies for '-' Ftpc
  mX_Y_Zminus->cd();
  mNode2->cd();
  TNode *node2_out = new TNode("node2_out", "node2_out", "ftpc2_out", 0, 0, -162.75-(256.45-162.75)/2.);
  TNode *node2_in = new TNode("node2_in", "node2_in", "ftpc2_in", 0, 0, -162.75-(256.45-162.75)/2.);

  // create dependencies for '+' Ftpc
  mX_Y_Zplus->cd();
  mNode1->cd();
  TNode *node1_out = new TNode("node1_out", "node1_out", "ftpc1_out", 0, 0, 162.75+(256.45-162.75)/2.);
  TNode *node1_in = new TNode("node1_in", "node1_in", "ftpc1_in", 0, 0, 162.75+(256.45-162.75)/2.);

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

  Char_t a;
  
  Bool_t electrons = (Bool_t) true;
  Bool_t non_vtx = (Bool_t) true;
  Bool_t geant_hits = (Bool_t) true;
  Bool_t good = (Bool_t) true;
  Bool_t good_found = (Bool_t) true;
  Bool_t split = (Bool_t) true;
  Bool_t unclean = (Bool_t) true;
  Bool_t found_hits = (Bool_t) true;

  while (1) {
    gSystem->Exec("/usr/bin/clear");
    
    if (mIsGeant) {
      cout << "Display of GEANT tracks (type 'f' for found tracks)" << endl;
      cout << endl;
      cout << "Show (e)lectron tracks..........[red]: ";
      OnOff(electrons);
      cout << "     (n)on main vertex tracks [green]: ";
      OnOff(non_vtx);
      cout << "     (g)ood tracks...........[yellow]: ";
      OnOff(good);
      cout << "     (c)lusters................[grey]: ";
      OnOff(geant_hits);
      cout << endl;
      cout << "Show (+), (-), or (b)oth Ftpcs or (q)uit: ";
      cin >> a;

      if (a == 'e' || a == 'n' || a == 'g' || a == 'c' || a == 'G' || a == 'f') {
	
	if (a == 'e') electrons = !electrons;
	if (a == 'n') non_vtx = !non_vtx;
	if (a == 'g') good = !good;
	if (a == 'c') geant_hits = !geant_hits;
	if (a == 'G') mIsGeant = (Bool_t)true;
	if (a == 'f') mIsGeant = (Bool_t)false;
      }


      else {
	if (a == 'q') break;
	
	else {
	  FillGeant(electrons, non_vtx, good, geant_hits);
    
	  // call the x3d function (this does the actual 3D displaying) for the right canvas
	  if (a == '+') mX_Y_Zplus->x3d();
	  if (a == '-') mX_Y_Zminus->x3d();
	  if (a == 'b') mX_Y_Z->x3d();
	}
      }
    }  

    else {
      cout << "Display of found tracks (type 'G' for GEANT tracks)" << endl;
      cout << endl;
      cout << "Show (g)ood tracks..........[red]: ";
      OnOff(good_found);
      cout << "     (s)plit tracks...... [green]: ";
      OnOff(split);
      cout << "     (u)nclean tracks....[yellow]: ";
      OnOff(unclean);
      cout << "     (c)lusters............[grey]: ";
      OnOff(found_hits);
      cout << endl;
      cout << "Show (+), (-), or (b)oth Ftpcs or (q)uit: ";
      cin >> a;

      if (a == 'g' || a == 's' || a == 'u' || a == 'c' || a == 'G' || a == 'f') {
	
	if (a == 'g') good_found = !good_found;
	if (a == 's') split = !split  ;
	if (a == 'u') unclean = !unclean;
	if (a == 'c') found_hits = !found_hits;
	if (a == 'G') mIsGeant = (Bool_t)true;
	if (a == 'f') mIsGeant = (Bool_t)false;
      }

      else{
	if (a == 'q') break;
	
	else {
	  MIntArray *sp = 0;
	  MIntArray *uncl = 0;

	  if (split) sp = splitArr;
	  if (unclean) uncl = uncleanArr;

	  FillFound(good_found, sp, uncl, found_hits);
    
	  // call the x3d function (this does the actual 3D displaying) for the right canvas
	  if (a == '+') mX_Y_Zplus->x3d();
	  if (a == '-') mX_Y_Zminus->x3d();
	  if (a == 'b') mX_Y_Z->x3d();
	}
      }
    } 
  }
    
  // cleanup
  delete mX_Y_Zplus;
  delete mX_Y_Zminus;
  delete mX_Y_Z;
  
  if (l) delete[] l;
  if (value) delete[] value;
  if (value_plus) delete[] value_minus;
  if (value_minus) delete[] value_minus;
  
  return;
}


void StFtpcDisplay::FillGeant(Bool_t electrons, Bool_t non_vtx, Bool_t good, Bool_t geant_hits)
{
  // Fill histograms with tracks and clusters.

  // draw nodes
  mX_Y_Zplus->cd();
  mNode1->cd();
  mNode1->Draw("");

  mX_Y_Zminus->cd();
  mNode2->cd();
  mNode2->Draw("");

  mX_Y_Z->cd();
  mNode0->cd();
  mNode0->Draw("");

  if (l) delete[] l;
  if (value) delete[] value;
  if (value_plus) delete[] value_minus;
  if (value_minus) delete[] value_minus;
  
  StFtpcConfMapHit *cluster;
  StFtpcTrack *track;
  
  // coordinates of trackpoints
  Float_t x[100];
  Float_t y[100];
  Float_t z[100];

  Int_t track_entries = mGeantTrack->GetEntriesFast();   
  l = new TPolyLine3D[track_entries];
  
  // loop over all tracks
  for (Int_t tracks = 0; tracks < track_entries; tracks++) {
    track = (StFtpcTrack *)mGeantTrack->At(tracks);

    if ((track->GetPid() <= 3 && !electrons) || 
	(!track->ComesFromMainVertex() && track->GetPid()>3 && !non_vtx) || 
	(track->ComesFromMainVertex() && track->GetPid()>3 && !good)) {
      continue;
    }
    
    else {
      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries && clusters < 100; clusters++) {
	
	cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX());
	y[clusters] = (Float_t)(cluster->GetY());
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      k = &(l[tracks]);
      k = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors
      Int_t color;
      
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
      
      k->SetLineColor(color);
  
      // draw track in the right canvas
      k->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      k->Draw("same");
    }  
  }

  // show clusters
  if (geant_hits) {
    Int_t cluster_anz = mGeantHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    Float_t *value_plus = new Float_t[3*cluster_anz];
    Float_t *value_minus = new Float_t[3*cluster_anz];
    Float_t *value = new Float_t[3*cluster_anz];
    
    StFtpcConfMapHit *h;
    Int_t cl_plus = 0;
    Int_t cl_minus = 0;  
    Int_t cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapHit *)mGeantHit->At(i);
      
      // fill (+, -, both) cluster arrays
      value[cl++] = h->GetX();
      value[cl++] = h->GetY();
      
      if ((value[cl++] = h->GetZ())>0) {
	value_plus[cl_plus++] = h->GetX();
	value_plus[cl_plus++] = h->GetY();
	value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	value_minus[cl_minus++] = h->GetX();
	value_minus[cl_minus++] = h->GetY();
	value_minus[cl_minus++] = h->GetZ();
      }
    }
    
    // create PolyMarkers
    TPolyMarker3D *m = new TPolyMarker3D(cl/3, value, 1);      
    TPolyMarker3D *m_plus = new TPolyMarker3D(cl_plus/3, value_plus, 1);      
    TPolyMarker3D *m_minus = new TPolyMarker3D(cl_minus/3, value_minus, 1);      

    // set colors
    m->SetMarkerColor(1);
    m_plus->SetMarkerColor(1);
    m_minus->SetMarkerColor(1);
    
    // switch to right canvas and draw clusters
    mX_Y_Z->cd();
    m->Draw("same");
    mX_Y_Zplus->cd();
    m_plus->Draw("same");
    mX_Y_Zminus->cd();
    m_minus->Draw("same");
  }
  
  // update canvases
  mX_Y_Zplus->Update();
  mX_Y_Zminus->Update();
  mX_Y_Z->Update();

  return;
}


void StFtpcDisplay::FillFound(Bool_t good_found, MIntArray *split, MIntArray *unclean, Bool_t found_hits)
{
  // Fill histograms with tracks and clusters.

  // draw nodes
  mX_Y_Zplus->cd();
  mNode1->cd();
  mNode1->Draw("");

  mX_Y_Zminus->cd();
  mNode2->cd();
  mNode2->Draw("");

  mX_Y_Z->cd();
  mNode0->cd();
  mNode0->Draw("");

  if (l) delete[] l;
  if (value) delete[] value;
  if (value_plus) delete[] value_minus;
  if (value_minus) delete[] value_minus;
  
  StFtpcConfMapHit *cluster;
  StFtpcTrack *track;
  
  // coordinates of trackpoints
  Float_t x[10];
  Float_t y[10];
  Float_t z[10];

  Int_t track_entries = mTrack->GetEntriesFast();
  Bool_t *good_track_to_show = new Bool_t[track_entries];

  l = new TPolyLine3D[track_entries];
  
  for (Int_t good_counter = 0; good_counter < track_entries; good_counter++) {
    good_track_to_show[good_counter] = (Bool_t) true;
  }

  if (unclean) {

    for (Int_t unclean_counter = 0; unclean_counter < unclean->GetSize(); unclean_counter++) {
      good_track_to_show[unclean->At(unclean_counter)] = (Bool_t) false;
    }
  }
    
  if (split) {
    
    for (Int_t split_counter = 0; split_counter < split->GetSize(); split_counter++) {
      good_track_to_show[split->At(split_counter)] = (Bool_t) false;
    }
  }

  Int_t entry = 0;

  if (split) {
    for (Int_t split_counter = 0; split_counter < split->GetSize(); split_counter++) {
      track = (StFtpcTrack *)mTrack->At(split->At(split_counter));

      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	
	cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX());
	y[clusters] = (Float_t)(cluster->GetY());
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      k = &(l[entry++]);
      k = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors

      k->SetLineColor(3);
    
      // draw track in the right canvas
      k->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      k->Draw("same");
    }  
  }

  if (unclean) {

    for (Int_t unclean_counter = 0; unclean_counter < unclean->GetSize(); unclean_counter++) {
      track = (StFtpcTrack *)mTrack->At(unclean->At(unclean_counter));

      Int_t cluster_entries = track->GetNumberOfPoints();
      
      // loop over all clusters
      for (Int_t clusters = 0; clusters < cluster_entries; clusters++) {
	
	cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	
	// fill point array
	x[clusters] = (Float_t)(cluster->GetX());
	y[clusters] = (Float_t)(cluster->GetY());
	z[clusters] = (Float_t)(cluster->GetZ());
	
	// decide in which canvas (+,-) this track belongs
	if (z[clusters]>0) mX_Y_Zplus->cd();
	else mX_Y_Zminus->cd();   
      }
      
      // fill PolyLine for this track
      k = &(l[entry++]);
      k = new TPolyLine3D(cluster_entries, x, y, z, "");
      
      // set colors

      k->SetLineColor(5);
    
      // draw track in the right canvas
      k->Draw("same");
      // and draw track in the canvas for both Ftpcs
      mX_Y_Z->cd();
      k->Draw("same");
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
	  
	  cluster = (StFtpcConfMapHit *)track->GetHits()->At(clusters);
	  
	  // fill point array
	  x[clusters] = (Float_t)(cluster->GetX());
	  y[clusters] = (Float_t)(cluster->GetY());
	  z[clusters] = (Float_t)(cluster->GetZ());
	  
	  // decide in which canvas (+,-) this track belongs
	  if (z[clusters]>0) mX_Y_Zplus->cd();
	  else mX_Y_Zminus->cd();   
	}
	
	// fill PolyLine for this track
	k = &(l[tracks]);
	k = new TPolyLine3D(cluster_entries, x, y, z, "");
	
	// set colors
	k->SetLineColor(2);
	
	// draw track in the right canvas
	k->Draw("same");
	// and draw track in the canvas for both Ftpcs
	mX_Y_Z->cd();
	k->Draw("same");
      }   
    }
  }

  // show clusters
  if (found_hits) {
    Int_t cluster_anz = mHit->GetEntriesFast();

    // coordinates of clusters (=, -, both)
    Float_t *value_plus = new Float_t[3*cluster_anz];
    Float_t *value_minus = new Float_t[3*cluster_anz];
    Float_t *value = new Float_t[3*cluster_anz];
    
    StFtpcConfMapHit *h;
    Int_t cl_plus = 0;
    Int_t cl_minus = 0;  
    Int_t cl = 0;
    
    // loop over all clusters
    for (Int_t i = 0; i < cluster_anz; i++) {
      h = (StFtpcConfMapHit *)mHit->At(i);
      
      // fill (+, -, both) cluster arrays
      value[cl++] = h->GetX();
      value[cl++] = h->GetY();
      
      if ((value[cl++] = h->GetZ())>0) {
	value_plus[cl_plus++] = h->GetX();
	value_plus[cl_plus++] = h->GetY();
	value_plus[cl_plus++] = h->GetZ();
      }
      
      else {
	value_minus[cl_minus++] = h->GetX();
	value_minus[cl_minus++] = h->GetY();
	value_minus[cl_minus++] = h->GetZ();
      }
    }
    
    // create PolyMarkers
    TPolyMarker3D *m = new TPolyMarker3D(cl/3, value, 1);      
    TPolyMarker3D *m_plus = new TPolyMarker3D(cl_plus/3, value_plus, 1);      
    TPolyMarker3D *m_minus = new TPolyMarker3D(cl_minus/3, value_minus, 1);      

    // set colors
    m->SetMarkerColor(1);
    m_plus->SetMarkerColor(1);
    m_minus->SetMarkerColor(1);
    
    // switch to right canvas and draw clusters
    mX_Y_Z->cd();
    m->Draw("same");
    mX_Y_Zplus->cd();
    m_plus->Draw("same");
    mX_Y_Zminus->cd();
    m_minus->Draw("same");
  }
  
  // update canvases
  mX_Y_Zplus->Update();
  mX_Y_Zminus->Update();
  mX_Y_Z->Update();

  return;
}


void StFtpcDisplay::OnOff(Bool_t on)
{

  if (on) {
    cout << "On" << endl;
  }
  
  else {
    cout << "Off" << endl;
  }

  return;
}

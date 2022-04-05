/*************************************************
 *
 * $Id: StMcQaMaker.cxx,v 1.4 2010/06/01 20:46:47 perev Exp $
 * Author: Manuel Calderon de la Barca
 * Make standard Histograms for
 * StMcEvent 
 * $Log: StMcQaMaker.cxx,v $
 * Revision 1.4  2010/06/01 20:46:47  perev
 * const added
 *
 * Revision 1.3  2010/01/28 18:13:18  perev
 * WarningOff
 *
 * Revision 1.2  2007/10/17 19:32:06  fisyak
 * The pixel story, remove Igt and Fst
 *
 * Revision 1.1  2007/03/21 16:48:49  fisyak
 * maker for side by side comparision of GEANT and VMC simulations
 *
 *
 *************************************************/
//#include <assert.h>
#include <Stiostream.h>
#include <stdlib.h>
//#include <string>
//#include <vector>
//#include <cmath>

#include "TH1.h"
#include "TH2.h"
#include "TFile.h"

#include "StMcQaMaker.h"
//#include "PhysicalConstants.h"
//#include "SystemOfUnits.h"

#include "StThreeVectorF.hh"
#include "StEnumerations.h"
#include "StMcEventTypes.hh"

#include "StMcEvent.hh"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "TMath.h"
ClassImp(StMcQaMaker)

//_________________________________________________
Int_t StMcQaMaker::Init() {
  QAPlots(0);
  return StMaker::Init();
}
//_________________________________________________
Int_t StMcQaMaker::Make() {
  // StMcEvent
  StMcEvent* mEvent = (StMcEvent*) GetDataSet("StMcEvent");
  if (mEvent) QAPlots(mEvent);
  return kStOk;
}

//--------------------------------------------------------------------------------
void StMcQaMaker::QAPlots(StMcEvent *mEvent) {
  // Histograms of StMcEvent
  // quantities.
  struct TrackDetector_t {
    StDetectorId    Id;
    const Char_t *Name;
    Int_t         NHMax; // max. no. of hit per track per detector
    TH1F         *NHits[2];// 
    Int_t         NZ;
    Double_t      Zmax;
    TH1F         *Zhit[2];
    Int_t         NX, NY;
    Double_t      Xmax, Ymax;
    TH2F         *XYhit[2];
    TH2F         *TdEdxP[2];
  };
  struct CalorimeterDetector_t {
    StDetectorId    Id;
    const Char_t       *Name;
    Int_t         NHMax; // max. no. of hit per track per detector
    TH1F         *NHits[2];// 
    Int_t         Neta, Nphi;
    Double_t      EtaMin,EtaMax;
    TH2F         *EtaPhiHit[2];
    TH2F         *dEEtaPhiHit[2];
  };
  static TrackDetector_t TrackDetectors[] = {
    //   Id                 Name     NHMax          NZ                 NX   NY               XY      dEdx
    {kTpcId                ,"Tpc",	50, {0, 0}, 210, 210., {0, 0}, 100, 100, 200., 200., {0, 0}, {0, 0}},
    {kSvtId       	   ,"Svt",	10, {0, 0},  30,  30., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},	
    {kRichId      	   ,"Rich",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},		 
    //    {kFtpcWestId  	   ,"FtpcWest",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},		 
    //    {kFtpcEastId  	   ,"FtpcEast",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},		 
    {kFtpcEastId  	   ,"Ftpc",	10, {0, 0}, 150, 300., {0, 0}, 100, 100,  40.,  40., {0, 0}, {0, 0}},		 
    {kTofId       	   ,"Tof",	10, {0, 0}, 200, 200., {0, 0}, 100, 100, 200., 200., {0, 0}, {0, 0}},
    {kCtbId       	   ,"Ctb",	10, {0, 0}, 100, 300., {0, 0}, 100, 100, 300., 300., {0, 0}, {0, 0}},
    {kSsdId       	   ,"Ssd",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  40.,  40., {0, 0}, {0, 0}},	          
    //    {kZdcWestId            ,"ZdcWest",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	  
    //    {kZdcEastId   	   ,"ZdcEast",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	  
    {kZdcEastId   	   ,"Zdc",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	  
    //    {kMwpcWestId  	   ,"MwpcWest",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	 
    //    {kMwpcEastId  	   ,"MwpcEast",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	 
    {kMwpcEastId  	   ,"Mwpc",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	 
    {kPhmdCpvId   	   ,"PhmdCpv",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	  
    {kPhmdId      	   ,"Phmd",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}},	     
    {kPxlId     	   ,"Pxl",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},	    
    {kIstId       	   ,"Ist",	10, {0, 0},  60,  60., {0, 0}, 100, 100,  20.,  20., {0, 0}, {0, 0}},		     
    {kFgtId   	           ,"Fgt",	10, {0, 0}, 250, 250., {0, 0}, 250, 250, 250., 250., {0, 0}, {0, 0}}
  };
  static const Int_t NTdetectors = sizeof(TrackDetectors)/sizeof(TrackDetector_t);
  static CalorimeterDetector_t CalorimeterDetectors[] = {
    //   Id                 Name     NHMax        Neta  Nphi EtaMin Max EtaPhi  dEEtaPhi
    {kBarrelEmcTowerId     ,"Bemc",	10, {0, 0},  40, 480,  -1., 1., {0, 0}, {0, 0}}, // (D_eta,D_phi) = (0.05,0.05) 	
    {kBarrelEmcPreShowerId ,"Bprs",	10, {0, 0}, 240, 240,  -1., 1., {0, 0}, {0, 0}},	
    {kBarrelSmdEtaStripId  ,"Bsmde",	10, {0, 0}, 240, 480,  -1., 1., {0, 0}, {0, 0}},	
    {kBarrelSmdPhiStripId  ,"Bsmdp",	10, {0, 0},  40, 480,  -1., 1., {0, 0}, {0, 0}},	
    {kEndcapEmcTowerId     ,"Eemc",	10, {0, 0}, 120, 240,   1., 3., {0, 0}, {0, 0}},	
    {kEndcapEmcPreShowerId ,"Eprs",	10, {0, 0}, 120, 240,   1., 3., {0, 0}, {0, 0}},	
    {kEndcapSmdUStripId    ,"Esmdu",	10, {0, 0}, 120, 240,   1., 3., {0, 0}, {0, 0}},	
    {kEndcapSmdVStripId    ,"Esmdv",	10, {0, 0}, 120, 240,   1., 3., {0, 0}, {0, 0}}
  };  
  static const Int_t NCdetectors = sizeof(CalorimeterDetectors)/sizeof(CalorimeterDetector_t);
  // Detector Information
  // per-Event               [k] = 0 for primaries, 1 for all
  static TH1F*     mVertexZ[2] = {0,0};   //! Vertex Z position.
  static TH2F*     mVertexXY[2] = {0,0};
  // Track Information
  
  // eventwise particle multiplicity
  static TH1F*     mTracks[2] = {0, 0};           //! 
  static TH1F*     mGeantIds[2] = {0, 0};
  
  // trackwise distributions
  static TH1F*     mPt[2] = {0, 0};
  static TH1F*     mEta[2] = {0, 0};
  if (! mEvent) {// Book
    Int_t Begin = gDirectory->GetList()->GetSize();
    TString Names[2] = {"Primary","All"};
    TString Name("");
    TString Title("");
    for (Int_t k = 0; k < 2; k++) {
      Name = "McQAVertexZ";      Name += Names[k];
      mVertexZ[k] = new TH1F(Name,"Vertex Z Position",300,-300,300);
      Name = "McQAVertexXY";      Name += Names[k];
      mVertexXY[k] = new TH2F(Name,"Vertex XY Position",200,-300,300,200,-300,300);
      Name = "McQATracks";      Name += Names[k];
      mTracks[k] = new TH1F(Name,"All Tracks",1000,0,10000);
      Name = "McGeantIds";      Name += Names[k];
      mGeantIds[k] = new TH1F(Name,"GeantId for all tracks", 50, 0, 50); 
      Name = "McQApT";      Name += Names[k];
      mPt[k] = new TH1F(Name,"pT",200,0,10);
      Name = "McQAEta";      Name += Names[k];
      mEta[k] = new TH1F(Name,"eta",200,-5,5);
      // Track detectors
      for (Int_t det = 0; det < NTdetectors; det++) {
	Name = "McQAN"; Name += TrackDetectors[det].Name; Name += Names[k];
	Title = "No. of Hits in "; Title += TrackDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	TrackDetectors[det].NHits[k] =  new TH1F(Name,Title,TrackDetectors[det].NHMax,0,TrackDetectors[det].NHMax);
	Name = "McQAZ"; Name += TrackDetectors[det].Name; Name += Names[k];
	Title = "Z of Hit in "; Title += TrackDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	TrackDetectors[det].Zhit[k] =  new TH1F(Name,Title,TrackDetectors[det].NZ,-TrackDetectors[det].Zmax,TrackDetectors[det].Zmax);
	Name = "McQAXY"; Name += TrackDetectors[det].Name; Name += Names[k];
	Title = "XY of Hit in "; Title += TrackDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	TrackDetectors[det].XYhit[k] =  new TH2F(Name,Title,
					    TrackDetectors[det].NX,-TrackDetectors[det].Xmax,TrackDetectors[det].Xmax,
					    TrackDetectors[det].NY,-TrackDetectors[det].Ymax,TrackDetectors[det].Ymax);
	Name = "McQAdEdx"; Name += TrackDetectors[det].Name; Name += Names[k];
	Title = "log10(dE/dx) (keV/cm)) versus log10(p(GeV/c) in "; 
	Title += TrackDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	TrackDetectors[det].TdEdxP[k] =  new TH2F(Name,Title,200,-2.,2., 1600,-1.,7.0);
      }
      // Calorimeters
      for (Int_t det = 0; det < NCdetectors; det++) {
	Name = "McQAN"; Name += CalorimeterDetectors[det].Name; Name += Names[k];
	Title = "No. of Hits in "; Title += CalorimeterDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	CalorimeterDetectors[det].NHits[k] =  new TH1F(Name,Title,CalorimeterDetectors[det].NHMax,0,CalorimeterDetectors[det].NHMax);
	Name = "McQAEP"; Name += CalorimeterDetectors[det].Name; Name += Names[k];
	Title = "Eta/Phi of Hit in "; Title += CalorimeterDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	CalorimeterDetectors[det].EtaPhiHit[k] = new TH2F(Name,Title,
							  CalorimeterDetectors[det].Neta,0,CalorimeterDetectors[det].Neta,
							  CalorimeterDetectors[det].Nphi,0,CalorimeterDetectors[det].Nphi);
	Name = "McQAdEEP"; Name += CalorimeterDetectors[det].Name; Name += Names[k];
	Title = "Sum of dE";  Title += CalorimeterDetectors[det].Name; Title += " for "; Title += Names[k]; Title += " tracks";
	CalorimeterDetectors[det].dEEtaPhiHit[k] = new TH2F(Name,Title,
							    CalorimeterDetectors[det].Neta,0,CalorimeterDetectors[det].Neta,
							    CalorimeterDetectors[det].Nphi,0,CalorimeterDetectors[det].Nphi); 
      }
    }
    // Move them into hist TDataSet
    Int_t Last = gDirectory->GetList()->GetSize() -1;
    for (Int_t i = Last; i >= Begin; i--) AddHist((TH1*) gDirectory->GetList()->At(i));
    return;
  } 
  // Fill
  //
  // Vertex
  //
  for (Int_t k = 0; k < 2; k++) {
    StMcVertex *vertex = 0;
    StPtrVecMcTrack *tracks = 0;
    Int_t nVertices = 0;
    if (k == 0) {
      vertex = mEvent->primaryVertex();
      nVertices = 1;
      tracks = &vertex->daughters();
    } else {
      nVertices = mEvent->vertices().size();
      tracks = &mEvent->tracks();
    }
    for (int i = 0; i < nVertices; i++) {
      if (k) vertex = (mEvent->vertices()[i]);
      if (vertex) {
	mVertexZ[k]->Fill(vertex->position().z());
	mVertexXY[k]->Fill(vertex->position().x(),vertex->position().y());
	if (tracks) {
	  StPtrVecMcTrack &Tracks = *tracks;
	  mTracks[k]->Fill(Tracks.size());
	  for (size_t iTrk=0; iTrk<Tracks.size(); ++iTrk) {
	    StMcTrack* trk = Tracks[iTrk];
	    mPt[k]->Fill(trk->momentum().perp());
	    mEta[k]->Fill(trk->momentum().pseudoRapidity());
	    Int_t gId = trk->geantId();
	    mGeantIds[k]->Fill(gId);
	    // hits
	    for (Int_t det = 0; det < NTdetectors; det++) {
	      const StPtrVecMcHit *hits = trk->Hits(TrackDetectors[det].Id);
	      if (! hits) continue;
	      const StPtrVecMcHit &Hits = *hits;
	      Int_t Nhits = Hits.size();
	      TrackDetectors[det].NHits[k]->Fill(Nhits);
	      for (Int_t h = 0; h < Nhits; h++) {
		StMcHit *hit = Hits[h];
		if (! hit) continue;
		TrackDetectors[det].Zhit[k]->Fill(hit->position().z());
		TrackDetectors[det].XYhit[k]->Fill(hit->position().x(),hit->position().y());
		Double_t dEdx = 1e6*TMath::Abs(hit->dE());
		if (dEdx < 1.e-10) continue;
		if (hit->dS() < 1.e-10) continue;
		dEdx /= hit->dS();
		Double_t pmom = hit->localMomentum().mag();
		TrackDetectors[det].TdEdxP[k]->Fill(TMath::Log10(pmom),TMath::Log10(dEdx));
	      }
	    }
	    for (Int_t det = 0; det < NCdetectors; det++) {
	      const StPtrVecMcCalorimeterHit *hits = trk->CalorimeterHits(CalorimeterDetectors[det].Id);
	      if (! hits) continue;
	      const StPtrVecMcCalorimeterHit &Hits = *hits;
	      Int_t Nhits = Hits.size();
	      CalorimeterDetectors[det].NHits[k]->Fill(Nhits);
	      for (Int_t h = 0; h < Nhits; h++) {
		StMcCalorimeterHit *hit = Hits[h];
		if (! hit) continue;
		CalorimeterDetectors[det].EtaPhiHit[k]->Fill(hit->eta(),2*hit->module()+hit->sub());
		CalorimeterDetectors[det].dEEtaPhiHit[k]->Fill(hit->eta(),2*hit->module()+hit->sub(),hit->dE());
	      }
	    }
	  }
	}
      }
    }
  }
}

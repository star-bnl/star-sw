/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

#include "EventDisplayHists.h"

#include "TF1.h"
#include "TF2.h"
#include "TEllipse.h"
#include "TMarker.h"

#include "VecBosJet.h"
#include "WBosEvent.h"


ClassImp(EventDisplayHists)

using namespace std;


/** Default constructor. */
EventDisplayHists::EventDisplayHists() : PlotHelper()
{
   BookHists();
}


EventDisplayHists::EventDisplayHists(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


EventDisplayHists::EventDisplayHists(TDirectory *dir, const VecBosEvent &ev) : PlotHelper(dir)
{
   BookHists(ev);
}


/** Default destructor. */
EventDisplayHists::~EventDisplayHists()
{
}


/** */
void EventDisplayHists::BookHists()
{
}


/** */
void EventDisplayHists::BookHists(const VecBosEvent &ev)
{
   string shName;
   TH1*   hist;

   fDir->cd();

   //o["hBTowEnergy"] = hist = new TH2F("hBTowEnergy", "; #phi, Tower Id; #eta, Tower Id; Energy", 40, 0.5, 40.5, 120, 0.5, 120.5);
   o["hBTowEnergy"] = hist = new TH2F("hBTowEnergy", "; #eta, Tower Id; #phi, Tower Id; Energy", 40, -1, 1, 120, 0, 2*M_PI);
   hist->SetOption("COLZ");
}


/** */
void EventDisplayHists::Fill(ProtoEvent &ev)
{
   WBosEvent& wbEvent = (WBosEvent&) ev;

   TH2*  hBTowEnergy = ((TH2*) o["hBTowEnergy"]);
   Int_t softId;

   for (int iEtaBin=1; iEtaBin<=hBTowEnergy->GetNbinsX(); iEtaBin++)
   {
      for (int iPhiBin=1; iPhiBin<=hBTowEnergy->GetNbinsY(); iPhiBin++)
      {
         Double_t eta_c = hBTowEnergy->GetXaxis()->GetBinCenter(iEtaBin);
         Double_t phi_c = hBTowEnergy->GetYaxis()->GetBinCenter(iPhiBin);

         gBTowGeom->getId((Float_t) phi_c, (Float_t) eta_c, softId);
         hBTowEnergy->SetBinContent(iEtaBin, iPhiBin, wbEvent.bemc.eneTile[kBTow][softId - 1]);
      }
   }

   const VecBosTrack  &eleTrack = wbEvent.GetElectronTrack();
   const VecBosVertex *vbVertex = wbEvent.FindVertexById(eleTrack.GetVertexId());

   TVector3 trkP3(eleTrack.GetCoordAtBTow() );

   TMarker *markerEleTrack = new TMarker(trkP3.Eta(), fmod(2*M_PI + trkP3.Phi(), 2*M_PI), 30);
   markerEleTrack->SetMarkerColor(kRed);
   markerEleTrack->SetMarkerSize(3);

   hBTowEnergy->GetListOfFunctions()->SetOwner(kTRUE);
   hBTowEnergy->GetListOfFunctions()->Add(markerEleTrack);

   TVector3 cluP3( eleTrack.mCluster2x2.position );

   TMarker *markerEleCluster = new TMarker(cluP3.Eta(), fmod(2*M_PI + cluP3.Phi(), 2*M_PI), 30);
   markerEleCluster->SetMarkerColor(kBlue);
   markerEleCluster->SetMarkerSize(3);
   hBTowEnergy->GetListOfFunctions()->Add(markerEleCluster);

   // Draw jets as ellipses
   VecBosJetPtrSetConstIter iJet = wbEvent.mJets.begin();
   for (short iColor=1 ; iJet != wbEvent.mJets.end(); ++iJet, iColor+=1)
   {
      VecBosJet &vbJet = **iJet;

      vbJet.SetEvent(&wbEvent); // XXX a temporary hack. VecBosJet::mEvent should be set when read from a file
      //vbJet.FindTracksInCone();
      //vbJet.Print();

      TEllipse *jetEllipse = new TEllipse(vbJet.CalcDetEta(), fmod(2*M_PI + vbJet.Phi(), 2*M_PI), VecBosEvent::sMaxJetCone/sqrt(2));
      jetEllipse->SetFillStyle(0);
      jetEllipse->SetLineColor(iColor);
      jetEllipse->SetLineWidth(2);
      hBTowEnergy->GetListOfFunctions()->Add(jetEllipse);

      VecBosTrackPtrSet& jetTracks = vbJet.GetTracks();

      VecBosTrackPtrSetIter iTrack = jetTracks.begin();
      for ( ; iTrack != jetTracks.end(); ++iTrack)
      {
         TVector3 jetTrkP3((*iTrack)->GetCoordAtBTow() );
         //Info("Fill", "iTrack: %x", *iTrack);
         //(*iTrack)->Print();
         //Info("Fill", "jetTrkP3:");
         //jetTrkP3.Print();

         TMarker *markerTrack = new TMarker(jetTrkP3.Eta(), fmod(2*M_PI + jetTrkP3.Phi(), 2*M_PI), kFullStar);
         markerTrack->SetMarkerColor(iColor);
         markerTrack->SetMarkerSize(2);
         hBTowEnergy->GetListOfFunctions()->Add(markerTrack);

         TLine *lineTrack2Jet = new TLine(jetEllipse->GetX1(), jetEllipse->GetY1(), markerTrack->GetX(), markerTrack->GetY());
         lineTrack2Jet->SetLineWidth(2);
         lineTrack2Jet->SetLineColor(iColor);
         hBTowEnergy->GetListOfFunctions()->Add(lineTrack2Jet);
      }
   }
}

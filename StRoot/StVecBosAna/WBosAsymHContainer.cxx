#include <math.h>

#include "WBosAsymHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "utils/utils.h"
#include "utils/MultiGraph.h"
#include "utils/H1I.h"
#include "utils/H1D.h"
#include "utils/H2I.h"
#include "utils/H2D.h"

#include "AsymCalculator.h"
#include "WBosEvent.h"


ClassImp(WBosAsymHContainer)

using namespace std;


/** Default constructor. */
WBosAsymHContainer::WBosAsymHContainer() : AsymHContainer()
{
   BookHists();
}


WBosAsymHContainer::WBosAsymHContainer(TDirectory *dir, EAsymType asymType) : AsymHContainer(dir, asymType)
{
   BookHists();
}


/** */
void WBosAsymHContainer::BookHists()
{
   string  shName;
   TH1I   *h1i;
   TH1F   *h1f;
   TH1D   *h1d;
   TH2I   *h2i;
   TH2D   *h2d;
   rh::MultiGraph *mg;

   DoubleSpinStateSetIter iDSS = gDoubleSpinStateSet.begin();
   Double_t xBinsPt[7]  = {0.5, 1, 2.5, 4, 5.5, 7, 10};
   //Double_t xBinsPt[8]  = {0.5, 1, 2, 3, 4, 5, 6, 10};
   //Double_t xBinsRap[5] = {-0.6, -0.2, 0, 0.2, 0.6};
   if (RapBins == 4) {
     xBinsRap[0] = -0.6;
     xBinsRap[1] = -0.25;
     xBinsRap[2] = 0;
     xBinsRap[3] = 0.25;
     xBinsRap[4] = 0.6;
   } else if (RapBins == 3){
     xBinsRap[0] = -0.6;
     xBinsRap[1] = -0.2;
     xBinsRap[2] = 0.2;
     xBinsRap[3] = 0.6;
   }

   for ( ; iDSS!=gDoubleSpinStateSet.end(); ++iDSS)
   {
      EDoubleSpinState dss = *iDSS;
      string sDblSpinState = AsString(dss);

      shName = "hWBosonPhiVsEta_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; W Boson #eta; W Boson #phi;", 6, -6, 6, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Dss[dss] = h2i;

      shName = "hWBosonPhiVsRap_" + sDblSpinState;
      //h2i = new rh::H2I(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", 6, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      h2i = new rh::H2I(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", RapBins, xBinsRap, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Dss[dss] = h2i;

      shName = "hWBosonPhiVsPt_" + sDblSpinState;
      //h2i = new rh::H2I(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 10, 0, 10, 8, -M_PI, M_PI, "colz DUMP");
      h2i = new rh::H2I(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 6, xBinsPt, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Dss[dss] = h2i;
   }

   SingleSpinStateSetIter iSSS = gSingleSpinStateSet.begin();
   for ( ; iSSS!=gSingleSpinStateSet.end(); ++iSSS)
   {
      ESingleSpinState sss  = *iSSS;
      string sSnglSpinState = AsString(sss);

      shName = "hWBosonPhiVsEta_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; W Boson #eta; W Boson #phi;", 6, -6, 6, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Sss[sss] = h2i;

      shName = "hWBosonPhiVsRap_" + sSnglSpinState;
      //h2i = new rh::H2I(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", 6, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      h2i = new rh::H2I(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", RapBins, xBinsRap, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Sss[sss] = h2i;

      shName = "hWBosonPhiVsPt_" + sSnglSpinState;
      //h2i = new rh::H2I(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 10, 0, 10, 8, -M_PI, M_PI, "colz DUMP");
      h2i = new rh::H2I(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 6, xBinsPt, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Sss[sss] = h2i;

      shName = "hWBosonPhi_PtProj_" + sSnglSpinState;
      h1i = new rh::H1I(shName.c_str(), "; W Boson #phi; Events;", 8, -M_PI, M_PI, "E1 GRIDX GRIDY");
      o[shName] = fYieldPhi_PtProj_Sss[sss] = h1i;
   }

   BeamIdSetIter iBeam = gBeams.begin();
   for ( ; iBeam!=gBeams.end(); ++iBeam)
   {
      EBeamId beamId = *iBeam;
      string sBeam = AsString(beamId);

      // Asymmetry vs Phi vs eta
      shName = "hWBosonAsymVsPhiVsEta_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; W Boson #eta; W Boson #phi;", 6, -6, 6, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsEta_Beam[beamId] = h2d;

      // Asymmetry amplitude vs eta
      shName = "hWBosonAsymAmpVsEta_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; W Boson #eta; Asym Amp.;", 6, -6, 6, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
      o[shName] = fAsymAmpVsEta_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrWBosonAsymVsPhi_EtaBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; W Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1.5, 1.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_EtaBins_Beam[beamId] = mg;

      // Asymmetry vs Phi vs rapidity
      shName = "hWBosonAsymVsPhiVsRap_" + sBeam;
      //h2d = new rh::H2D(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", 6, -1.5, 1.5, 8, -M_PI, M_PI, "colz");
      h2d = new rh::H2D(shName.c_str(), "; W Boson Rapidity; W Boson #phi;", RapBins, xBinsRap, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsRap_Beam[beamId] = h2d;

      // Asymmetry amplitude vs rapidity
      shName = "hWBosonAsymAmpVsRap_" + sBeam;
      //h1d = new rh::H1D(shName.c_str(), "; W Boson Rapidity; Asym Amp.;", 6, -1.5, 1.5, "E1 GRIDX GRIDY");
      h1d = new rh::H1D(shName.c_str(), "; W Boson Rapidity; Asym Amp.;", RapBins, xBinsRap, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
      o[shName] = fAsymAmpVsRap_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrWBosonAsymVsPhi_RapBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; W Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1.5, 1.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_RapBins_Beam[beamId] = mg;

      // Asymmetry vs phi vs p_T
      shName = "hWBosonAsymVsPhiVsPt_" + sBeam;
      //h2d = new rh::H2D(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 10, 0, 10, 8, -M_PI, M_PI, "colz");
      h2d = new rh::H2D(shName.c_str(), "; W Boson P_{T}; W Boson #phi;", 6, xBinsPt, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsPt_Beam[beamId] = h2d;

      // Asymmetry amplitude vs p_T
      shName = "hWBosonAsymAmpVsPt_" + sBeam;
      //h1d = new rh::H1D(shName.c_str(), "; W Boson P_{T}; Asym Amp.;", 10, 0, 10, "E1 GRIDX GRIDY");
      h1d = new rh::H1D(shName.c_str(), "; W Boson P_{T}; Asym Amp.;", 6, xBinsPt, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
      o[shName] = fAsymAmpVsPt_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrWBosonAsymVsPhi_PtBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; W Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1.5, 1.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_PtBins_Beam[beamId] = mg;
   }

   shName = "hWBosonAsymAmpVsEta_";
   h1d = new rh::H1D(shName.c_str(), "; W Boson #eta; Asym Amp.;", 6, -6, 6, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
   o[shName] = fAsymAmpVsEta = h1d;

   shName = "hWBosonAsymAmpVsRap_";
   //h1d = new rh::H1D(shName.c_str(), "; W Boson Rapidity; Asym Amp.;", 6, -1.5, 1.5, "E1 GRIDX GRIDY");
   h1d = new rh::H1D(shName.c_str(), "; W Boson Rapidity; Asym Amp.;", RapBins, xBinsRap, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
   o[shName] = fAsymAmpVsRap = h1d;

   shName = "hWBosonAsymAmpVsPt_";
   //h1d = new rh::H1D(shName.c_str(), "; W Boson P_{T}; Asym Amp.;", 10, 0, 10, "E1 GRIDX GRIDY");
   h1d = new rh::H1D(shName.c_str(), "; W Boson P_{T}; Asym Amp.;", 6, xBinsPt, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1.5, 1.5);
   o[shName] = fAsymAmpVsPt = h1d;
}


/** */
void WBosAsymHContainer::Fill(ProtoEvent &ev)
{
   WBosEvent& event = (WBosEvent&) ev;

   if ( gDoubleSpinStateSet.find((EDoubleSpinState) event.mSpinPattern4Bits) == gDoubleSpinStateSet.end())
      return;

   EDoubleSpinState dblSpinState = (EDoubleSpinState) event.mSpinPattern4Bits;

   string sDblSpinState = AsString( dblSpinState );

   // XXX:ds: Do we really need to check the number of candidate tracks here?
   //if (event.mTracksCandidate.size() > 0)
   //{
      TLorentzVector wBoson = event.GetVecBosonP4();
      string shName;

    if ( wBoson.Pt() < 10 ) { // S.F. 25Aug2015 (was 7)
      shName = "hWBosonPhiVsEta_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(wBoson.Eta(), wBoson.Phi());

      shName = "hWBosonPhiVsRap_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(wBoson.Rapidity(), wBoson.Phi());
    }

      shName = "hWBosonPhiVsPt_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(wBoson.Pt(), wBoson.Phi());
   //}
}

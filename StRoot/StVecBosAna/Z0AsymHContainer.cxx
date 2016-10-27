#include <math.h>

#include "Z0AsymHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "utils/utils.h"
#include "utils/MultiGraph.h"
#include "utils/H1I.h"
#include "utils/H1D.h"
#include "utils/H2I.h"
#include "utils/H2D.h"

#include "AsymCalculator.h"
#include "ZBosEvent.h"


ClassImp(Z0AsymHContainer)

using namespace std;


/** Default constructor. */
Z0AsymHContainer::Z0AsymHContainer() : AsymHContainer()
{
   BookHists();
}


Z0AsymHContainer::Z0AsymHContainer(TDirectory *dir, EAsymType asymType) : AsymHContainer(dir, asymType)
{
   BookHists();
}


/** */
void Z0AsymHContainer::BookHists()
{
   string  shName;
   TH1I   *h1i;
   TH1F   *h1f;
   TH1D   *h1d;
   TH2I   *h2i;
   TH2D   *h2d;
   rh::MultiGraph *mg;

   DoubleSpinStateSetIter iDSS = gDoubleSpinStateSet.begin();
   for ( ; iDSS!=gDoubleSpinStateSet.end(); ++iDSS)
   {
      EDoubleSpinState dss = *iDSS;
      string sDblSpinState = AsString(dss);

      shName = "hZBosonPhiVsEta_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson #eta; Z Boson #phi;", 1, -6, 6, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Dss[dss] = h2i;

      shName = "hZBosonPhiVsRap_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson Rapidity; Z Boson #phi;", 1, -0.8, 0.8, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Dss[dss] = h2i;

      shName = "hZBosonPhiVsPt_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson P_{T}; Z Boson #phi;", 1, 0, 25, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Dss[dss] = h2i;
   }

   SingleSpinStateSetIter iSSS = gSingleSpinStateSet.begin();
   for ( ; iSSS!=gSingleSpinStateSet.end(); ++iSSS)
   {
      ESingleSpinState sss  = *iSSS;
      string sSnglSpinState = AsString(sss);

      shName = "hZBosonPhiVsEta_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson #eta; Z Boson #phi;", 1, -6, 6, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Sss[sss] = h2i;

      shName = "hZBosonPhiVsRap_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson Rapidity; Z Boson #phi;", 1, -0.8, 0.8, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Sss[sss] = h2i;

      shName = "hZBosonPhiVsPt_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Z Boson P_{T}; Z Boson #phi;", 1, 0, 25, 6, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Sss[sss] = h2i;

      shName = "hZBosonPhi_PtProj_" + sSnglSpinState;
      h1i = new rh::H1I(shName.c_str(), "; Z Boson #phi; Events;", 6, -M_PI, M_PI, "E1 GRIDX GRIDY");
      o[shName] = fYieldPhi_PtProj_Sss[sss] = h1i;
   }

   BeamIdSetIter iBeam = gBeams.begin();
   for ( ; iBeam!=gBeams.end(); ++iBeam)
   {
      EBeamId beamId = *iBeam;
      string sBeam = AsString(beamId);

      // Asymmetry vs Phi vs eta
      shName = "hZBosonAsymVsPhiVsEta_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Z Boson #eta; Z Boson #phi;", 1, -6, 6, 6, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsEta_Beam[beamId] = h2d;

      // Asymmetry amplitude vs eta
      shName = "hZBosonAsymAmpVsEta_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Z Boson #eta; Asym Amp.;", 1, -6, 6, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1, 1);
      o[shName] = fAsymAmpVsEta_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrZBosonAsymVsPhi_EtaBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Z Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1, 1);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_EtaBins_Beam[beamId] = mg;

      // Asymmetry vs Phi vs rapidity
      shName = "hZBosonAsymVsPhiVsRap_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Z Boson Rapidity; Z Boson #phi;", 1, -0.8, 0.8, 6, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsRap_Beam[beamId] = h2d;

      // Asymmetry amplitude vs rapidity
      shName = "hZBosonAsymAmpVsRap_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Z Boson Rapidity; Asym Amp.;", 1, -0.8, 0.8, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1, 1);
      o[shName] = fAsymAmpVsRap_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrZBosonAsymVsPhi_RapBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Z Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1, 1);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_RapBins_Beam[beamId] = mg;

      // Asymmetry vs phi vs p_T
      shName = "hZBosonAsymVsPhiVsPt_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Z Boson P_{T}; Z Boson #phi;", 1, 0, 25, 6, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsPt_Beam[beamId] = h2d;

      // Asymmetry amplitude vs p_T
      shName = "hZBosonAsymAmpVsPt_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Z Boson P_{T}; Asym Amp.;", 1, 0, 25, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-1, 1);
      o[shName] = fAsymAmpVsPt_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrZBosonAsymVsPhi_PtBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Z Boson #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-1, 1);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_PtBins_Beam[beamId] = mg;
   }

   shName = "hZBosonAsymAmpVsEta_";
   h1d = new rh::H1D(shName.c_str(), "; Z Boson #eta; Asym Amp.;", 1, -6, 6, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1, 1);
   o[shName] = fAsymAmpVsEta = h1d;

   shName = "hZBosonAsymAmpVsRap_";
   h1d = new rh::H1D(shName.c_str(), "; Z Boson Rapidity; Asym Amp.;", 1, -0.8, 0.8, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1, 1);
   o[shName] = fAsymAmpVsRap = h1d;

   shName = "hZBosonAsymAmpVsPt_";
   h1d = new rh::H1D(shName.c_str(), "; Z Boson P_{T}; Asym Amp.;", 1, 0, 25, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-1, 1);
   o[shName] = fAsymAmpVsPt = h1d;
}


/** */
void Z0AsymHContainer::Fill(ProtoEvent &ev)
{
   ZBosEvent& event = (ZBosEvent&) ev;

   if ( gDoubleSpinStateSet.find((EDoubleSpinState) event.mSpinPattern4Bits) == gDoubleSpinStateSet.end())
      return;

   EDoubleSpinState dblSpinState = (EDoubleSpinState) event.mSpinPattern4Bits;

   string sDblSpinState = AsString( dblSpinState );

   // XXX:ds: Do we really need to check the number of candidate tracks here?
   //if (event.mTracksCandidate.size() > 1)
   //{
      TLorentzVector zBoson = event.GetVecBosonP4();
      string shName;

      shName = "hZBosonPhiVsEta_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(zBoson.Eta(), zBoson.Phi());

      shName = "hZBosonPhiVsRap_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(zBoson.Rapidity(), zBoson.Phi());

      shName = "hZBosonPhiVsPt_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(zBoson.Pt(), zBoson.Phi());
   //}
}

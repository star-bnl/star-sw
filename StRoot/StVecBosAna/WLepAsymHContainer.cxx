#include <math.h>

#include "WLepAsymHContainer.h"

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


ClassImp(WLepAsymHContainer)

using namespace std;


/** Default constructor. */
WLepAsymHContainer::WLepAsymHContainer() : AsymHContainer()
{
   BookHists();
}


WLepAsymHContainer::WLepAsymHContainer(TDirectory *dir, EAsymType asymType) : AsymHContainer(dir, asymType)
{
   BookHists();
}


/** */
void WLepAsymHContainer::BookHists()
{
   string shName;
   TH1I   *h1i;
   TH1F   *h1f;
   TH1D   *h1d;
   TH2I   *h2i;
   TH2D   *h2d;
   rh::MultiGraph* mg;

   DoubleSpinStateSetIter iDSS = gDoubleSpinStateSet.begin();
   for ( ; iDSS!=gDoubleSpinStateSet.end(); ++iDSS)
   {
      EDoubleSpinState dss = *iDSS;
      string sDblSpinState = AsString(dss);

      shName = "hLeptonPhiVsEta_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton #eta; Lepton #phi;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Dss[dss] = h2i;

      shName = "hLeptonPhiVsRap_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton Rapidity; Lepton Rapidity;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Dss[dss] = h2i;

      shName = "hLeptonPhiVsPt_" + sDblSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton P_{T}; Lepton #phi;", 10, 0, 50, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Dss[dss] = h2i;
   }

   SingleSpinStateSetIter iSSS = gSingleSpinStateSet.begin();
   for ( ; iSSS!=gSingleSpinStateSet.end(); ++iSSS)
   {
      ESingleSpinState sss  = *iSSS;
      string sSnglSpinState = AsString(sss);

      shName = "hLeptonPhiVsEta_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton #eta; Lepton #phi;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsEta_Sss[sss] = h2i;

      shName = "hLeptonPhiVsRap_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton Rapidity; Lepton #phi;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsRap_Sss[sss] = h2i;

      shName = "hLeptonPhiVsPt_" + sSnglSpinState;
      h2i = new rh::H2I(shName.c_str(), "; Lepton P_{T}; Lepton #phi;", 10, 0, 50, 8, -M_PI, M_PI, "colz DUMP");
      o[shName] = fYieldPhiVsPt_Sss[sss] = h2i;

      shName = "hLeptonPhi_PtProj_" + sSnglSpinState;
      h1i = new rh::H1I(shName.c_str(), "; Lepton #phi; Events;", 8, -M_PI, M_PI, "E1 GRIDX GRIDY");
      o[shName] = fYieldPhi_PtProj_Sss[sss] = h1i;
   }

   BeamIdSetIter iBeam = gBeams.begin();
   for ( ; iBeam!=gBeams.end(); ++iBeam)
   {
      EBeamId beamId = *iBeam;
      string sBeam = AsString(beamId);

      // Asymmetry vs Phi vs eta
      shName = "hLeptonAsymVsPhiVsEta_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Lepton #eta; Lepton #phi;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsEta_Beam[beamId] = h2d;

      // Asymmetry amplitude vs eta
      shName = "hLeptonAsymAmpVsEta_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Lepton #eta; Asym Amp.;", 3, -1.5, 1.5, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
      o[shName] = fAsymAmpVsEta_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrLeptonAsymVsPhi_EtaBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Lepton #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-0.5, 0.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_EtaBins_Beam[beamId] = mg;

      // Asymmetry vs Phi vs rapidity
      shName = "hLeptonAsymVsPhiVsRap_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Lepton Rapidity; Lepton #phi;", 3, -1.5, 1.5, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsRap_Beam[beamId] = h2d;

      // Asymmetry amplitude vs rapidity
      shName = "hLeptonAsymAmpVsRap_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Lepton Rapidity; Asym Amp.;", 3, -1.5, 1.5, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
      o[shName] = fAsymAmpVsRap_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrLeptonAsymVsPhi_RapBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Lepton #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-0.5, 0.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_RapBins_Beam[beamId] = mg;

      // Asymmetry vs phi vs p_T
      shName = "hLeptonAsymVsPhiVsPt_" + sBeam;
      h2d = new rh::H2D(shName.c_str(), "; Lepton P_{T}; Lepton #phi;", 10, 0, 50, 8, -M_PI, M_PI, "colz");
      o[shName] = fAsymVsPhiVsPt_Beam[beamId] = h2d;

      // Asymmetry amplitude vs p_T
      shName = "hLeptonAsymAmpVsPt_" + sBeam;
      h1d = new rh::H1D(shName.c_str(), "; Lepton P_{T}; Asym Amp.;", 10, 0, 50, "E1 GRIDX GRIDY");
      h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
      o[shName] = fAsymAmpVsPt_Beam[beamId] = h1d;

      // Multigraph container with graphs for individual slices/bins
      shName = "mgrLeptonAsymVsPhi_PtBins_" + sBeam;
      mg = new rh::MultiGraph(shName, shName);
      h1f = new TH1F(shName.c_str(), "; Lepton #phi; Asym.;", 1, -M_PI, M_PI);
      h1f->GetYaxis()->SetRangeUser(-0.5, 0.5);
      mg->SetHistogram(h1f);
      o[shName] = fAsymVsPhi_PtBins_Beam[beamId] = mg;
   }

   shName = "hLeptonAsymAmpVsEta_";
   h1d = new rh::H1D(shName.c_str(), "; Lepton #eta; Asym Amp.;", 3, -1.5, 1.5, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
   o[shName] = fAsymAmpVsEta = h1d;

   shName = "hLeptonAsymAmpVsRap_";
   h1d = new rh::H1D(shName.c_str(), "; Lepton Rapidity; Asym Amp.;", 3, -1.5, 1.5, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
   o[shName] = fAsymAmpVsRap = h1d;

   shName = "hLeptonAsymAmpVsPt_";
   h1d = new rh::H1D(shName.c_str(), "; Lepton P_{T}; Asym Amp.;", 10, 0, 50, "E1 GRIDX GRIDY");
   h1d->GetYaxis()->SetRangeUser(-0.5, 0.5);
   o[shName] = fAsymAmpVsPt = h1d;
}


/** */
void WLepAsymHContainer::Fill(ProtoEvent &ev)
{
   WBosEvent& event = (WBosEvent&) ev;

   if ( gDoubleSpinStateSet.find((EDoubleSpinState) event.mSpinPattern4Bits) == gDoubleSpinStateSet.end())
      return;

   EDoubleSpinState dblSpinState = (EDoubleSpinState) event.mSpinPattern4Bits;

   string sDblSpinState = AsString( dblSpinState );

   // XXX:ds: Do we really need to check the number of candidate tracks here?
   //if (event.mTracksCandidate.size() > 0)
   //{
      TVector3 eleCandidate = event.GetElectronP3();
      string shName;

      shName = "hLeptonPhiVsEta_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(eleCandidate.Eta(), eleCandidate.Phi());

      shName = "hLeptonPhiVsRap_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(eleCandidate.Eta(), eleCandidate.Phi());
      //((TH2*) o[shName])->Fill(eleCandidate.Rapidity(), eleCandidate.Phi());

      shName = "hLeptonPhiVsPt_" + sDblSpinState;
      ((TH2*) o[shName])->Fill(eleCandidate.Pt(), eleCandidate.Phi());
   //}
}

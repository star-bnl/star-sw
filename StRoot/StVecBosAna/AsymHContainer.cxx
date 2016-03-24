#include <math.h>

#include "AsymHContainer.h"

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


ClassImp(AsymHContainer)

using namespace std;


/** Default constructor. */
AsymHContainer::AsymHContainer() : PlotHelper()
   , fAsymType(kAsymPlain)
   , fYieldPhiVsEta_Sss()
   , fYieldPhiVsRap_Sss()
   , fYieldPhiVsPt_Sss()
   , fYieldPhi_PtProj_Sss()
   , fYieldPhiVsEta_Dss()
   , fYieldPhiVsRap_Dss()
   , fYieldPhiVsPt_Dss()
   , fAsymVsPhiVsEta_Beam()
   , fAsymAmpVsEta_Beam()
{
}


AsymHContainer::AsymHContainer(TDirectory *dir, EAsymType asymType) : PlotHelper(dir)
   , fAsymType(asymType)
   , fYieldPhiVsEta_Sss()
   , fYieldPhiVsRap_Sss()
   , fYieldPhiVsPt_Sss()
   , fYieldPhi_PtProj_Sss()
   , fYieldPhiVsEta_Dss()
   , fYieldPhiVsRap_Dss()
   , fYieldPhiVsPt_Dss()
   , fAsymVsPhiVsEta_Beam()
   , fAsymAmpVsEta_Beam()
{
}


/** */
void AsymHContainer::FillDerived()
{
   Info("FillDerived()", "Called");

   SingleSpinStateSetIter iSSS = gSingleSpinStateSet.begin();
   for ( ; iSSS!=gSingleSpinStateSet.end(); ++iSSS)
   {
      ESingleSpinState sss  = *iSSS;

      DoubleSpinStateSetIter iDSS = gDoubleSpinStateSet.begin();
      for ( ; iDSS!=gDoubleSpinStateSet.end(); ++iDSS)
      {
         EDoubleSpinState dss = *iDSS;

         if (dss & sss) {
            fYieldPhiVsEta_Sss[sss]->Add(fYieldPhiVsEta_Dss[dss]);
            fYieldPhiVsRap_Sss[sss]->Add(fYieldPhiVsRap_Dss[dss]);
            fYieldPhiVsPt_Sss[sss]->Add(fYieldPhiVsPt_Dss[dss]);
         }
      }

      // Create projections on phi axis
      //TH1D* hProjTmp = hLeptonPhiVsEta_sngl->ProjectionY();
      //utils::CopyBinContentError(hProjTmp, hLeptonPhi_EtaProj_sngl);

      TH1D* hProjTmp = fYieldPhiVsPt_Sss[sss]->ProjectionY();
      utils::CopyBinContentError(hProjTmp, fYieldPhi_PtProj_Sss[sss]);
      delete hProjTmp;
   }
}


/** */
void AsymHContainer::PostFill()
{
   Info("PostFill", "Called");

   AsymCalculator::sAsymType = fAsymType;

   /*
   TH1I* hLeptonPhi_BU        = (TH1I*) o["hLeptonPhi_EtaProj_u0"];
   TH1I* hLeptonPhi_BD        = (TH1I*) o["hLeptonPhi_EtaProj_d0"];
   TH1D* hLeptonAsymVsPhi_BLU = (TH1D*) o["hLeptonAsymVsPhi_BLU"];

   AsymCalculator::CalcAsimAsym(*hLeptonPhi_BU, *hLeptonPhi_BD, *hLeptonAsymVsPhi_BLU);
   AsymCalculator::FitAsimAsym(*hLeptonAsymVsPhi_BLU);

   TH1I* hLeptonPhi_YU        = (TH1I*) o["hLeptonPhi_EtaProj_0u"];
   TH1I* hLeptonPhi_YD        = (TH1I*) o["hLeptonPhi_EtaProj_0d"];
   TH1D* hLeptonAsymVsPhi_YEL = (TH1D*) o["hLeptonAsymVsPhi_YEL"];

   AsymCalculator::CalcAsimAsym(*hLeptonPhi_YU, *hLeptonPhi_YD, *hLeptonAsymVsPhi_YEL);
   AsymCalculator::FitAsimAsym(*hLeptonAsymVsPhi_YEL);
   */

   BeamIdSetIter iBeam = gBeams.begin();
   for ( ; iBeam!=gBeams.end(); ++iBeam)
   {
      EBeamId beamId = *iBeam;
      ESingleSpinState sssUp = AsSingleSpinState(beamId, kSPIN_UP);
      ESingleSpinState sssDn = AsSingleSpinState(beamId, kSPIN_DOWN);

      Info("PostFill", "Considering beam: %s, and sssUp: %s, sssDn: %s", AsString(beamId).c_str(), AsString(sssUp).c_str(), AsString(sssDn).c_str());

      // Asymmetry vs. eta for beamId
      AsymCalculator::CalcAsimAsym(*fYieldPhiVsEta_Sss[sssUp], *fYieldPhiVsEta_Sss[sssDn], *fAsymVsPhiVsEta_Beam[beamId]);
      AsymCalculator::FitAsimAsym(*fAsymVsPhiVsEta_Beam[beamId], *fAsymAmpVsEta_Beam[beamId], fAsymVsPhi_EtaBins_Beam[beamId]);

      // Asymmetry vs. rapidity for beamId
      AsymCalculator::CalcAsimAsym(*fYieldPhiVsRap_Sss[sssUp], *fYieldPhiVsRap_Sss[sssDn], *fAsymVsPhiVsRap_Beam[beamId]);
      AsymCalculator::FitAsimAsym(*fAsymVsPhiVsRap_Beam[beamId], *fAsymAmpVsRap_Beam[beamId], fAsymVsPhi_RapBins_Beam[beamId]);

      // Asymmetry vs. P_T for beamId
      AsymCalculator::CalcAsimAsym(*fYieldPhiVsPt_Sss[sssUp], *fYieldPhiVsPt_Sss[sssDn], *fAsymVsPhiVsPt_Beam[beamId]);
      AsymCalculator::FitAsimAsym(*fAsymVsPhiVsPt_Beam[beamId], *fAsymAmpVsPt_Beam[beamId], fAsymVsPhi_PtBins_Beam[beamId]);

      // XXX:ds: The same as above but with different binning
   }

   AsymCalculator::CombineAsimAsym(*fAsymAmpVsEta_Beam[kBLUE_BEAM], *fAsymAmpVsEta_Beam[kYELLOW_BEAM], *fAsymAmpVsEta, true);
   AsymCalculator::CombineAsimAsym(*fAsymAmpVsRap_Beam[kBLUE_BEAM], *fAsymAmpVsRap_Beam[kYELLOW_BEAM], *fAsymAmpVsRap, true);
   AsymCalculator::CombineAsimAsym(*fAsymAmpVsPt_Beam[kBLUE_BEAM], *fAsymAmpVsPt_Beam[kYELLOW_BEAM], *fAsymAmpVsPt);

   /* S.F. - should we really fit a straight line? 
   TF1 fitFunc("fitFunc", "[0]");
   fitFunc.SetParNames("Avrg.");
   fAsymAmpVsRap->Fit(&fitFunc);
   */

   // This is mainly to cross check that the asymmetry sign flip works
   //TH1D* hWBosonAsymAmpVsEta_YEL_rev = new TH1D(*hWBosonAsymAmpVsEta_YEL);
   //o["hWBosonAsymAmpVsEta_YEL_rev"] = hWBosonAsymAmpVsEta_YEL_rev;
   //hWBosonAsymAmpVsEta_YEL_rev->SetName("hWBosonAsymAmpVsEta_YEL_rev");
   //utils::CopyReversedBinContentError(hWBosonAsymAmpVsEta_YEL, hWBosonAsymAmpVsEta_YEL_rev);
}

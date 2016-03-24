#ifndef AsymHContainer_h
#define AsymHContainer_h

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"

#include "Globals.h"


/**
 * An abstract base class for asymmetry containers for various objects, i.e.  lepton, W boson, Z
 * boson, etc.
 */
class AsymHContainer : public PlotHelper
{
public:

   AsymHContainer();
   AsymHContainer(TDirectory *dir, EAsymType asymType=kAsymPlain);

   // To unhide the virtual function that are not overwritten
   using PlotHelper::FillDerived;
   using PlotHelper::PostFill;

   void Fill(ProtoEvent &ev) = 0;
   void FillDerived();
   void PostFill();

protected:

   EAsymType  fAsymType; ///< Type of the asymmetry calculated in this containter

   Sss2TH2IMap fYieldPhiVsEta_Sss;
   Sss2TH2IMap fYieldPhiVsRap_Sss;
   Sss2TH2IMap fYieldPhiVsPt_Sss;

   Sss2TH1IMap fYieldPhi_PtProj_Sss;

   Dss2TH2IMap fYieldPhiVsEta_Dss;
   Dss2TH2IMap fYieldPhiVsRap_Dss;
   Dss2TH2IMap fYieldPhiVsPt_Dss;

   BeamId2TH2DMap fAsymVsPhiVsEta_Beam;
   BeamId2TH2DMap fAsymVsPhiVsRap_Beam;
   BeamId2TH2DMap fAsymVsPhiVsPt_Beam;

   BeamId2TH1DMap fAsymAmpVsEta_Beam;
   BeamId2TH1DMap fAsymAmpVsRap_Beam;
   BeamId2TH1DMap fAsymAmpVsPt_Beam;

   TH1D* fAsymAmpVsEta;
   TH1D* fAsymAmpVsRap;
   TH1D* fAsymAmpVsPt;

   BeamId2MultGraphMap fAsymVsPhi_EtaBins_Beam;
   BeamId2MultGraphMap fAsymVsPhi_RapBins_Beam;
   BeamId2MultGraphMap fAsymVsPhi_PtBins_Beam;


   ClassDef(AsymHContainer, 1)
};

#endif

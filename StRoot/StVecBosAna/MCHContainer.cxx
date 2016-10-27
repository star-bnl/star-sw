#include "MCHContainer.h"

#include "TNtuple.h"
#include "TF1.h"
#include "TProfile.h"
#include "TText.h"

#include "WBosEvent.h"
#include "WBosMcEvent.h"
#include "Globals.h"

#include "utils/H1I.h"
#include "utils/H1F.h"
#include "utils/H1D.h"
#include "utils/H2D.h"
#include "utils/H2I.h"
#include "utils/H2F.h"

ClassImp(MCHContainer)

using namespace std;


/** Default constructor. */
MCHContainer::MCHContainer() : PlotHelper()
{
   BookHists();
}


MCHContainer::MCHContainer(TDirectory *dir, Int_t McType) : PlotHelper(dir)
{
   mMcType = McType;
   BookHists();
}


/** */
void MCHContainer::BookHists()
{
   string shName;
   TH1*   hist;

   // Define the W-Pt binning
   //Double_t  xBinsPt[0] = {0.5,1,2,3,4,5,6,10}
   /*
   xBinsPt[0] = 0.5; 
   xBinsPt[1] = 1; 
   xBinsPt[2] = 2; 
   xBinsPt[3] = 3; 
   xBinsPt[4] = 4; 
   xBinsPt[5] = 5; 
   xBinsPt[6] = 6; 
   xBinsPt[7] = 10;
   */
   xBinsPt[0] = 0.5; 
   xBinsPt[1] = 1; 
   xBinsPt[2] = 2.5; 
   xBinsPt[3] = 4; 
   xBinsPt[4] = 5.5; 
   xBinsPt[5] = 7;  
   xBinsPt[6] = 10;

   // Define the W-rapidity binning
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
     Double_t binvar = 0.1;
     for (int i=0; i <= RapBins; ++i) {
       xBinsRapPR[i] = xBinsRap[i] + binvar;
       xBinsRapMR[i] = xBinsRap[i] - binvar;
     }
   }

   fDir->cd();

   o["ntAnEvolution"] = new TNtuple("ntAnEvolution","A_N prediction with evolution","Wy_gen:Wy_rec:An_evol");

   o["hResolutionPt_bin1"]  = hist = new TH1D("hResolutionPt_bin1", "bin 1; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin2"]  = hist = new TH1D("hResolutionPt_bin2", "bin 2; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin3"]  = hist = new TH1D("hResolutionPt_bin3", "bin 3; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin4"]  = hist = new TH1D("hResolutionPt_bin4", "bin 4; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin5"]  = hist = new TH1D("hResolutionPt_bin5", "bin 5; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin6"]  = hist = new TH1D("hResolutionPt_bin6", "bin 6; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPt_bin7"]  = hist = new TH1D("hResolutionPt_bin7", "bin 7; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);

   o["hResolutionPtCorr_bin1"]  = hist = new TH1D("hResolutionPtCorr_bin1", "bin 1; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin2"]  = hist = new TH1D("hResolutionPtCorr_bin2", "bin 2; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin3"]  = hist = new TH1D("hResolutionPtCorr_bin3", "bin 3; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin4"]  = hist = new TH1D("hResolutionPtCorr_bin4", "bin 4; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin5"]  = hist = new TH1D("hResolutionPtCorr_bin5", "bin 5; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin6"]  = hist = new TH1D("hResolutionPtCorr_bin6", "bin 6; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);
   o["hResolutionPtCorr_bin7"]  = hist = new TH1D("hResolutionPtCorr_bin7", "bin 7; (Gen-Rec)/Gen WBoson P_{T}; Events", 80, -5, 5.);

   o["hResolution_RecoilInAcc_Pt_bin1"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin1", "bin 1; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin2"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin2", "bin 2; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin3"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin3", "bin 3; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin4"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin4", "bin 4; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin5"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin5", "bin 5; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin6"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin6", "bin 6; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hResolution_RecoilInAcc_Pt_bin7"]  = hist = new TH1D("hResolution_RecoilInAcc_Pt_bin7", "bin 7; (Gen-Rec)/Gen Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);

   o["hAbsoResolution_RecoilInAcc_Pt_bin1"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin1", "bin 1; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin2"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin2", "bin 2; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin3"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin3", "bin 3; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin4"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin4", "bin 4; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin5"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin5", "bin 5; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin6"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin6", "bin 6; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);
   o["hAbsoResolution_RecoilInAcc_Pt_bin7"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Pt_bin7", "bin 7; Gen-Rec Recoil (in acceptance) P_{T}; Events", 80, -5, 5.);

   o["hResolutionRap"]       = hist = new TH1D("hResolutionRap", "; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
   //hist->SetOption("hist GRIDX");
   if (RapBins == 4) {
      o["hResolutionRap_bin1"]  = hist = new TH1D("hResolutionRap_bin1", "bin 1; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hResolutionRap_bin2"]  = hist = new TH1D("hResolutionRap_bin2", "bin 2; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hResolutionRap_bin3"]  = hist = new TH1D("hResolutionRap_bin3", "bin 3; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hResolutionRap_bin4"]  = hist = new TH1D("hResolutionRap_bin4", "bin 4; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
   } else if (RapBins == 3) {
      o["hResolutionRap_bin1"]  = hist = new TH1D("hResolutionRap_bin1", "bin 1; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hResolutionRap_bin2"]  = hist = new TH1D("hResolutionRap_bin2", "bin 2; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hResolutionRap_bin3"]  = hist = new TH1D("hResolutionRap_bin3", "bin 3; (Gen-Rec)/Gen WBoson y; Events", 80, -5, 5.);
      o["hAbsoResolutionRap_bin1"]  = hist = new TH1D("hAbsoResolutionRap_bin1", "bin 1; Gen-Rec WBoson y; Events", 80, -1, 1.);
      o["hAbsoResolutionRap_bin2"]  = hist = new TH1D("hAbsoResolutionRap_bin2", "bin 2; Gen-Rec WBoson y; Events", 80, -1, 1.);
      o["hAbsoResolutionRap_bin3"]  = hist = new TH1D("hAbsoResolutionRap_bin3", "bin 3; Gen-Rec WBoson y; Events", 80, -1, 1.);
      o["hResolution_RecoilInAcc_Rap_bin1"]  = hist = new TH1D("hResolution_RecoilInAcc_Rap_bin1", "bin 1; (Gen-Rec)/Gen Recoil (in acceptance) y; Events", 80, -5, 5.);
      o["hResolution_RecoilInAcc_Rap_bin2"]  = hist = new TH1D("hResolution_RecoilInAcc_Rap_bin2", "bin 2; (Gen-Rec)/Gen Recoil (in acceptance) y; Events", 80, -5, 5.);
      o["hResolution_RecoilInAcc_Rap_bin3"]  = hist = new TH1D("hResolution_RecoilInAcc_Rap_bin3", "bin 3; (Gen-Rec)/Gen Recoil (in acceptance) y; Events", 80, -5, 5.);
      o["hAbsoResolution_RecoilInAcc_Rap_bin1"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Rap_bin1", "bin 1; Gen-Rec Recoil (in acceptance) y; Events", 80, -1, 1.);
      o["hAbsoResolution_RecoilInAcc_Rap_bin2"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Rap_bin2", "bin 2; Gen-Rec Recoil (in acceptance) y; Events", 80, -1, 1.);
      o["hAbsoResolution_RecoilInAcc_Rap_bin3"]  = hist = new TH1D("hAbsoResolution_RecoilInAcc_Rap_bin3", "bin 3; Gen-Rec Recoil (in acceptance) y; Events", 80, -1, 1.);
   }

   o["hRecoVsGenLeptonPhi"]        = new rh::H2I("hRecoVsGenLeptonPhi", "; Gen. Lepton #phi; Reco. Lepton #phi", 50, -M_PI, M_PI, 50, -M_PI, M_PI, "colz LOGZ");
   o["hRecoVsGenLeptonPt"]         = new rh::H2I("hRecoVsGenLeptonPt",  "; Gen. Lepton P_{T}, GeV; Reco. Lepton P_{T}, GeV", 50, 10, 60, 50, 10, 60, "colz LOGZ");
   o["hRecoVsGenLeptonPz"]         = new rh::H2I("hRecoVsGenLeptonPz",  "; Gen. Lepton P_{z}, GeV; Reco. Lepton P_{z}, GeV", 50, -50, 50, 50, -50, 50, "colz LOGZ");
   o["hRecoVsGenLeptonEta"]        = new rh::H2I("hRecoVsGenLeptonEta", "; Gen. Lepton #eta; Reco. Lepton #eta", 50, -2, 2, 50, -2, 2, "colz LOGZ");
   o["hRecoVsGenLeptonEta_10bins"] = new rh::H2D("hRecoVsGenLeptonEta_10bins", "; Gen. Lepton #eta; Reco. Lepton #eta", 10, -2, 2, 10, -2, 2, "colz LOGZ");
   o["hRecoVsGenLeptonEta_8bins"]  = new rh::H2D("hRecoVsGenLeptonEta_8bins", "; Gen. Lepton #eta; Reco. Lepton #eta", 8, -1, 1, 8, -1, 1, "colz LOGZ");

   o["hWBosonE"]   = new rh::H1F("hWBosonE", "; W energy [GeV]; Events", 100, 20., 200., "hist GRIDX");
   o["hWBosonPz"]  = new rh::H1F("hWBosonPz", "; P^{W}_{z} [GeV/c]; Events", 50, -80., 80., "hist GRIDX");
   o["hWBosonPt"]  = new rh::H1F("hWBosonPt", "; P^{W}_{T} [GeV/c]; Events", 40, 0., 40., "hist GRIDX");
   o["hWBosonPt_zoomin"] = new rh::H1F("hWBosonPt_zoomin", "; P^{W}_{T} [GeV/c]; Events", 20, 0., 10., "hist GRIDX");
   o["hWBosonEta"] = new rh::H1F("hWBosonEta", "; W pseudo-rapidity #eta; Events", 60, -6, 6, "hist GRIDX");
   o["hWBosonEta_20bins"] = new rh::H1F("hWBosonEta_20bins", "; W pseudo-rapidity #eta; Events", 20, -4, 4, "hist GRIDX");
   o["hWBosonRap"]        = new rh::H1F("hWBosonRap", "; W rapidity y; Events", 60, -1.5, 1.5, "hist GRIDX");
   o["hWBosonRap_20bins"] = new rh::H1F("hWBosonRap_20bins", "; W rapidity y; Events", 20, -1.5, 1.5, "hist GRIDX");
   o["hWBosonRap_5bins"]  = new rh::H1F("hWBosonRap_5bins", "; W rapidity y; Events", 5, -0.6, 0.6, "hist GRIDX");

   o["hNeutrinoE"]  = new rh::H1I("hNeutrinoE", "; #nu energy [GeV]; Events", 50, 0., 200., "hist GRIDX");
   o["hNeutrinoPx"] = new rh::H1I("hNeutrinoPx", "; P^{#nu}_{x} [GeV/c]; Events", 100, -150., 150., "hist GRIDX");
   o["hNeutrinoPy"] = new rh::H1I("hNeutrinoPy", "; P^{#nu}_{y} [GeV/c]; Events", 100, -150., 150., "hist GRIDX");
   o["hNeutrinoPz"] = new rh::H1I("hNeutrinoPz", "; P^{#nu}_{z} [GeV/c]; Events", 100, -150., 150., "hist GRIDX");
   o["hNeutrinoPt"] = new rh::H1I("hNeutrinoPt", "; P^{#nu}_{T} [GeV/c]; Events", 100, 0., 150., "hist GRIDX");

   o["hLeptonE"]  = new rh::H1I("hLeptonE", "; electron energy [GeV]; Events", 80, 0., 80., "hist GRIDX");
   o["hLeptonEta"]= new rh::H1I("hLeptonEta", "; electron pseudo-rapidity #eta; Events", 20, -2, 2, "hist GRIDX");
   o["hLeptonEta_8bins"]= new rh::H1I("hLeptonEta_8bins", "; electron pseudo-rapidity #eta; Events", 8, -1, 1, "hist GRIDX");
   o["hLeptonPx"] = new rh::H1I("hLeptonPx", "; P^{#nu}_{x} [GeV/c]; Events", 80, -80., 80., "hist GRIDX");
   o["hLeptonPy"] = new rh::H1I("hLeptonPy", "; P^{#nu}_{y} [GeV/c]; Events", 80, -80., 80., "hist GRIDX");
   o["hLeptonPz"] = new rh::H1I("hLeptonPz", "; P^{#nu}_{z} [GeV/c]; Events", 80, -80., 80., "hist GRIDX");
   o["hLeptonPt"] = new rh::H1I("hLeptonPt", "; P^{#nu}_{T} [GeV/c]; Events", 80, 0., 80., "hist GRIDX");

   o["hRecoVsGenNeutrinoPhi"] = new rh::H2I("hRecoVsGenNeutrinoPhi", "; Gen. Neutrino #phi; Reco. Neutrino #phi", 50, -M_PI, M_PI, 50, -M_PI, M_PI, "colz LOGZ");
   o["hRecoVsGenNeutrinoPt"]  = new rh::H2I("hRecoVsGenNeutrinoPt",  "; Gen. Neutrino P_{T}, GeV; Reco. Neutrino P_{T}, GeV", 50, 10, 60, 50, 10, 60, "colz LOGZ");
   o["hRecoVsGenNeutrinoPz"]  = new rh::H2I("hRecoVsGenNeutrinoPz",  "; Gen. Neutrino P_{z}, GeV; Reco. Neutrino P_{z}, GeV", 50, -70, 70, 50, -70, 70, "colz LOGZ");
   o["hRecoVsGenNeutrinoPz_c"]  = new rh::H2I("hRecoVsGenNeutrinoPz_c",  "; Gen. Neutrino P_{z}, GeV; Reco. Neutrino P_{z}, GeV", 50, -70, 70, 50, -70, 70, "colz LOGZ");
   o["hRecoVsGenNeutrinoPz_f"]  = new rh::H2I("hRecoVsGenNeutrinoPz_f",  "; Gen. Neutrino P_{z}, GeV; Reco. Neutrino P_{z}, GeV", 50, -70, 70, 50, -70, 70, "colz LOGZ");
   o["hRecoVsGenNeutrinoEta"] = new rh::H2I("hRecoVsGenNeutrinoEta", "; Gen. Neutrino #eta; Reco. Neutrino #eta", 50, -2, 2, 50, -2, 2, "colz LOGZ");

   o["hRecoVsGenWBosonPhi"] = new rh::H2I("hRecoVsGenWBosonPhi", "; Gen. W Boson #phi; Reco. W Boson #phi", 50, -M_PI, M_PI, 50, -M_PI, M_PI, "colz LOGZ");
   o["hRecoVsGenWBosonPt"]  = new rh::H2I("hRecoVsGenWBosonPt",  "; Gen. W Boson P_{T}, GeV; Reco. W Boson P_{T}, GeV", 50, 0, 25, 50, 0, 25, "colz LOGZ");
   o["hRecoVsGenWBosonPz"]  = new rh::H2F("hRecoVsGenWBosonPz",  "; Gen. W Boson P_{z}, GeV; Reco. W Boson P_{z}, GeV", 50, -80, 80, 50, -80, 80, "colz LOGZ");
   o["hRecoVsGenWBosonEta"] = new rh::H2F("hRecoVsGenWBosonEta", "; Gen. W Boson #eta; Reco. W Boson #eta", 50, -6, 6, 50, -6, 6, "colz LOGZ");
   o["hRecoVsGenWBosonRap"] = new rh::H2F("hRecoVsGenWBosonRap", "; Gen. W Boson y; Reco. W Boson y", 50, -1, 1, 50, -1, 1, "colz LOGZ");
   o["hRecoVsGenWBosonPz_FirstSol"]  = new rh::H2F("hRecoVsGenWBosonPz_FirstSol",  "; Gen. W Boson P_{z}, GeV; Reco. W Boson P_{z} (First Solution), GeV", 50, -80, 80, 50, -80, 80, "colz LOGZ");
   o["hRecoVsGenWBosonPz_OtherSol"]  = new rh::H2F("hRecoVsGenWBosonPz_OtherSol",  "; Gen. W Boson P_{z}, GeV; Reco. W Boson P_{z} (Other Solution), GeV", 50, -80, 80, 50, -80, 80, "colz LOGZ");

   o["hGen_WBosonEtaVsElectronEta"]   = hist = new TH2D("hGen_WBosonEtaVsElectronEta", "; W Boson #eta; Electron #eta", 20, -4, 4, 20, -2, 2);
   hist->SetOption("colz LOGZ");
   o["hReco_WBosonEtaVsElectronEta"]  = hist = new TH2D("hReco_WBosonEtaVsElectronEta", "; W Boson #eta; Electron #eta", 20, -4, 4, 20, -2, 2);
   hist->SetOption("colz LOGZ");

   // W recoil momentum components
   o["hGenRecoilE"]  = new rh::H1I("hGenRecoilE", "; Gen. W Recoil E (GeV); Events", 100, 0., 200., "hist GRIDX");
   o["hGenRecoilPz"] = new rh::H1I("hGenRecoilPz", "; Gen. W Recoil p_{z} (GeV); Events", 100, -160., 160., "hist GRIDX");
   o["hGenRecoilPt"] = new rh::H1I("hGenRecoilPt",  "; Gen. W Recoil p_{T} (GeV); Events", 50, 0., 25., "hist GRIDX");
   o["hGenRecoilPhi"] = new rh::H1I("hGenRecoilPhi", "; Gen. W Recoil #phi; Events", 50, -M_PI, M_PI, "hist GRIDX");

   // Recoil in acceptance
   o["hRecInAccEnergy"]    = new rh::H1I("hRecInAccEenergy", "; Recoil energy [GeV]; Events", 100, 0., 200., "hist GRIDX");
   o["hRecInAccMomentumX"] = new rh::H1I("hRecInAccMomentumX", "; P^{Recoil}_{x} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecInAccMomentumY"] = new rh::H1I("hRecInAccMomentumY", "; P^{Recoil}_{y} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecInAccMomentumZ"] = new rh::H1I("hRecInAccMomentumZ", "; P^{Recoil}_{z} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecoilInAcceptPt"]  = new rh::H1I("hRecoilInAcceptPt", "; P^{Recoil}_{t} [GeV/c]; Events", 50, 0., 25., "hist GRIDX");

   // Recoil outside of detector acceptance
   o["hRecOutAccEnergy"]    = new rh::H1I("hRecOutAccEenergy", "; Recoil energy [GeV]; Events", 100, 0., 200., "hist GRIDX");
   o["hRecOutAccMomentumX"] = new rh::H1I("hRecOutAccMomentumX", "; P^{Recoil}_{x} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecOutAccMomentumY"] = new rh::H1I("hRecOutAccMomentumY", "; P^{Recoil}_{y} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecOutAccMomentumZ"] = new rh::H1I("hRecOutAccMomentumZ", "; P^{Recoil}_{z} [GeV/c]; Events", 100, -160., 160., "hist GRIDX");
   o["hRecOutAccPt"]        = new rh::H1I("hRecOutAccPt", "; P^{Recoil}_{t} [GeV/c]; Events", 50, 0., 50., "hist GRIDX");

   o["hGenRecoilVsWBosonPt"]       = new rh::H2I("hGenRecoilVsWBosonPt", "; Gen. W Boson p_{T}; Gen. W Recoil p_{T};", 50, 0, 25, 50, 0, 25, "colz LOGZ");
   o["hGenRecoilInAccVsWBosonPt"]  = new rh::H2I("hGenRecoilInAccVsWBosonPt",  "; Gen. W Boson p_{T}; Gen. W Recoil p_{T} In Accept.;", 50, 0, 25, 50, 0, 25, "colz LOGZ");
   o["hGenRecoilOutAccVsWBosonPt"] = new rh::H2I("hGenRecoilOutAccVsWBosonPt", "; Gen. W Boson p_{T}; Gen. W Recoil p_{T} Out of Accept.;", 50, 0, 25, 50, 0, 25, "colz LOGZ");
   o["hGenRecoilOutVsInAccPt"]     = new rh::H2I("hGenRecoilOutVsInAccPt", "; Gen. W Recoil p_{T} In Accept.; Gen. W Recoil p_{T} Out of Accept.;", 50, 0, 25, 50, 0, 25, "colz LOGZ");
   o["hGenRecoilInOutDeltaPhi"]    = new rh::H1I("hGenRecoilInOutDeltaPhi", "; Gen. W Recoil #Delta#phi(In, Out) ; Events;", 30, -M_PI, M_PI, "hist GRIDX GRIDY");

   o["hJetRecoilPtVsWBosonPt"]   = new rh::H2I("hJetRecoilPtVsWBosonPt", "; W Boson P_{T}; Jet-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hJetRecoilPt_GenOverReco"] = new rh::H2F("hJetRecoilPt_GenOverReco", ";Jet-based Recoil P_{T}; Correction factor",40, 0, 40, 50, 0, 20, "colz LOGZ");
   o["hJetRecoilPt_GenOverReco_zoomin"] = new rh::H2F("hJetRecoilPt_GenOverReco_zoomin", ";Jet-based Recoil P_{T}; Correction factor",20, 0, 10, 50, 0, 20, "colz LOGZ");
   o["hJetRecoilPhiVsWBosonPhi"] = new rh::H2I("hJetRecoilPhiVsWBosonPhi", "; W Boson #phi; Jet-based Recoil #phi", 50, -M_PI, M_PI, 50, -M_PI, M_PI, "colz LOGZ");

   o["hTrackRecoilPtVsWBosonPt"]    = new rh::H2I("hTrackRecoilPtVsWBosonPt", "; W Boson P_{T}; Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilPhiVsWBosonPhi"]  = new rh::H2I("hTrackRecoilPhiVsWBosonPhi", "; W Boson #phi; Track-based Recoil #phi", 50, -M_PI, M_PI, 50, -M_PI, M_PI, "colz LOGZ");
   o["hTrackRecoilTpcPtVsWBosonPt"] = new rh::H2I("hTrackRecoilTpcPtVsWBosonPt", "; W Boson P_{T}; Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPtVsWBosonPt"] = new rh::H2I("hTrackRecoilTpcNeutralsPtVsWBosonPt", "; W Boson P_{T}; Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt"] = new rh::H2I("hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt", "; W Boson P_{T}; Corrected Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPtVsWBosonPt_copy"] = new rh::H2I("hTrackRecoilTpcNeutralsPtVsWBosonPt_copy", "; W Boson P_{T}; Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt_copy"] = new rh::H2I("hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt_copy", "; W Boson P_{T}; Corrected Track-based Recoil P_{T}", 50, 0., 50., 50, 0., 50., "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPt_GenOverReco"] = new rh::H2F("hTrackRecoilTpcNeutralsPt_GenOverReco", ";Track-based Recoil P_{T}; Correction factor",40, 0, 40, 50, 0, 20, "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin"] = new rh::H2F("hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin", "; Track-based Recoil P_{T}; Correction factor",20, 0, 10, 50, 0, 20, "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc100k-200k"] = new rh::H2F("hTrackRecoilTpcNeutralsPt_GenOverReco_zdc100k-200k", ";Track-based Recoil P_{T}; Correction factor",40, 0, 40, 50, 0, 20, "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc200k-300k"] = new rh::H2F("hTrackRecoilTpcNeutralsPt_GenOverReco_zdc200k-300k", ";Track-based Recoil P_{T}; Correction factor",40, 0, 40, 50, 0, 20, "colz LOGZ");
   o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc300k-400k"] = new rh::H2F("hTrackRecoilTpcNeutralsPt_GenOverReco_zdc300k-400k", ";Track-based Recoil P_{T}; Correction factor",40, 0, 40, 50, 0, 20, "colz LOGZ");

   o["hJetRecoilPtRelDiff"]   = new rh::H1F("hJetRecoilPtRelDiff", "; Jet-based Recoil P_{T}, Rel. Diff.; ", 50, -2, 2., "hist");
   o["hTrackRecoilPtRelDiff"] = new rh::H1F("hTrackRecoilPtRelDiff", "; Track-based Recoil P_{T}, Rel. Diff.; ", 50, -2, 2., "hist");
   o["hTrackRecoilTpcNeutralsPtRelDiff"] = new rh::H1F("hTrackRecoilTpcNeutralsPtRelDiff", "; Track-based Recoil P_{T}, Rel. Diff.; ", 50, -2, 2., "hist");
   o["hJetTrackRecoilPtRelDiff"] = new rh::H1F("hJetTrackRecoilPtRelDiff", "; Jet- Track-based Recoil P_{T}, Rel. Diff.; ", 50, -2, 2., "hist");
   o["hJetTrackTpcNeutralsRecoilPtRelDiff"] = new rh::H1F("hJetTrackTpcNeutralsRecoilPtRelDiff", "; Jet- Track-based Recoil P_{T}, Rel. Diff.; ", 50, -2, 2., "hist");

   // A_N prediction from Zhongbo Kang
   if (mMcType == 1 ) {   // limits for W+

   o["hAn_evol_ZK"]  = new rh::H1F("hAn_evol_ZK", "; W A_{N}; Events", 20, -0.02, 0.0002, "hist GRIDX");
   o["hAn_evol_ZK_Vs_PtGen"]  = new rh::H2D("hAn_evol_ZK_Vs_PtGen", "; W P_{T}^{GEN}; W A_{N}; Events", 6, xBinsPt, 10, -0.02, 0.0002, "colz LOGZ");
   o["hAn_evol_ZK_Vs_PtRec"]  = new rh::H2D("hAn_evol_ZK_Vs_PtRec", "; W P_{T}^{REC}; W A_{N}; Events", 6, xBinsPt, 10, -0.02, 0.0002, "colz LOGZ");
   o["hAn_noevo_ZK"]  = new rh::H1F("hAn_noevo_ZK", "; W A_{N}; Events", 20, -0.2, 0.2, "hist GRIDX");
   o["hAn_noevo_ZK_Vs_PtGen"]  = new rh::H2D("hAn_noevo_ZK_Vs_PtGen", "; W P_{T}^{GEN}; W A_{N}; Events", 6, xBinsPt, 10, -0.2, 0.2, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtRec"]  = new rh::H2D("hAn_noevo_ZK_Vs_PtRec", "; W P_{T}^{REC}; W A_{N}; Events", 6, xBinsPt, 10, -0.2, 0.2, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtGen_zoomin"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtGen_zoomin", "; W P_{T}^{GEN}; W A_{N}; Events", 8, 2., 15., 40, -0.2, 0.2, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtRec_zoomin"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtRec_zoomin", "; W P_{T}^{REC}; W A_{N}; Events", 8, 2., 15., 40, -0.2, 0.2, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapGen"]  = new rh::H2D("hAn_evol_ZK_Vs_RapGen", "; W y^{GEN}; W A_{N}; Events", RapBins, xBinsRap, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapRec"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRec", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRap, 50, 0.0, 0.035, "colz LOGZ");

   o["hAn_evol_ZK_Vs_RapRecPR"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRecPR", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRapPR, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapRecMR"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRecMR", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRapMR, 50, 0.0, 0.035, "colz LOGZ");

   o["hAn_evol_ZK_y>0"]  = new rh::H1F("hAn_evol_ZK_y>0", "; W A_{N}; Events", 20, -0.02, 0.0002, "hist GRIDX");
   o["hAn_evol_ZK_Vs_PtGen_y>0"]  = new rh::H2F("hAn_evol_ZK_Vs_PtGen_y>0", "; W P_{T}^{GEN}; W A_{N}; Events", 15, 0., 15., 10, -0.02, 0.0002, "colz LOGZ");
   o["hAn_evol_ZK_Vs_PtRec_y>0"]  = new rh::H2F("hAn_evol_ZK_Vs_PtRec_y>0", "; W P_{T}^{REC}; W A_{N}; Events", 15, 0., 15., 10, -0.02, 0.0002, "colz LOGZ");
   o["hAn_noevo_ZK_y>0"]  = new rh::H1F("hAn_noevo_ZK_y>0", "; W A_{N}; Events", 20, -0.2, 0.2, "hist GRIDX");
   o["hAn_noevo_ZK_Vs_PtGen_y>0"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtGen_y>0", "; W P_{T}^{GEN}; W A_{N}; Events", 15, 0., 15., 10, -0.2, 0.2, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtRec_y>0"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtRec_y>0", "; W P_{T}^{REC}; W A_{N}; Events", 15, 0., 15., 10, -0.2, 0.2, "colz LOGZ");

   o["hAnEvolVsWgen_WrongReco_FirstSol"]  = new rh::H2D("hAnEvolVsWgen_WrongReco_FirstSol",  "Test wrong Reco events A_{N}; Gen. W Boson P_{z}, GeV; A_{N} evol", 50, -80, 80, 10, -0.02, 0.0002, "colz LOGZ");
   o["hAnEvolVsWgen_FirstSol"]  = new rh::H2D("hAnEvolVsWgen_FirstSol",  "Test well Reco events WPz_reco < 0 (GeV); Gen. W Boson P_{z}, GeV; A_{N} evol;", 1, -50, -47, 20, -0.02, 0.0002, "colz LOGZ");

   } else if (mMcType == 2) {   // limits for W-

   o["hAn_evol_ZK"]  = new rh::H1F("hAn_evol_ZK", "; W A_{N}; Events", 20, 0.0, 0.035, "hist GRIDX");
   o["hAn_evol_ZK_Vs_PtGen"]  = new rh::H2D("hAn_evol_ZK_Vs_PtGen", "; W P_{T}^{GEN}; W A_{N}; Events", 6, xBinsPt, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_PtRec"]  = new rh::H2D("hAn_evol_ZK_Vs_PtRec", "; W P_{T}^{REC}; W A_{N}; Events", 6, xBinsPt, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapGen"]  = new rh::H2D("hAn_evol_ZK_Vs_RapGen", "; W y^{GEN}; W A_{N}; Events", RapBins, xBinsRap, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapRec"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRec", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRap, 50, 0.0, 0.035, "colz LOGZ");

   o["hAn_evol_ZK_Vs_RapRecPR"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRecPR", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRapPR, 50, 0.0, 0.035, "colz LOGZ");
   o["hAn_evol_ZK_Vs_RapRecMR"]  = new rh::H2D("hAn_evol_ZK_Vs_RapRecMR", "; W y^{REC}; W A_{N}; Events", RapBins, xBinsRapMR, 50, 0.0, 0.035, "colz LOGZ");

   o["hAn_noevo_ZK"]  = new rh::H1F("hAn_noevo_ZK", "; W A_{N}; Events", 20, 0., 0.45, "hist GRIDX");
   o["hAn_noevo_ZK_Vs_PtGen"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtGen", "; W P_{T}^{GEN}; W A_{N}; Events", 15, 0., 15., 100, 0., 0.45, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtRec"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtRec", "; W P_{T}^{REC}; W A_{N}; Events", 15, 0., 15., 100, 0., 0.45, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtGen_zoomin"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtGen_zoomin", "; W P_{T}^{GEN}; W A_{N}; Events", 8, 2., 15., 40, 0., 0.03, "colz LOGZ");
   o["hAn_noevo_ZK_Vs_PtRec_zoomin"]  = new rh::H2F("hAn_noevo_ZK_Vs_PtRec_zoomin", "; W P_{T}^{REC}; W A_{N}; Events", 8, 2., 15., 40, 0., 0.03, "colz LOGZ");


   o["hAnEvolVsWgen_WrongReco_FirstSol"]  = new rh::H2D("hAnEvolVsWgen_WrongReco_FirstSol",  "Test wrong Reco events WPz_reco > 0 (GeV); Gen. W Boson P_{z}, GeV; A_{N} evol;", 1, -50, -47, 20, 0.0, 0.01, "colz LOGZ");
   o["hAnEvolVsWgen_FirstSol"]  = new rh::H2D("hAnEvolVsWgen_FirstSol",  "Test well Reco events WPz_reco < 0 (GeV); Gen. W Boson P_{z}, GeV; A_{N} evol;", 1, -50, -47, 20, 0.0, 0.01, "colz LOGZ");
   } 

   // Efficiency Plots
   // -------------------------------------
   if (mMcType == 1 || mMcType == 2) { // W+ or W- Monte Carlo 
      
     o["hLeptonInAccEta_TEST"]= new rh::H1I("hLeptonInAccEta_TEST", "; electron pseudo-rapidity #eta; Events", 20, -2, 2, "hist GRIDX");
     o["hLeptonInAccEta"]= new rh::H1I("hLeptonInAccEta", "; electron pseudo-rapidity #eta; Events", 20, -2, 2, "hist GRIDX");
     //o["hTrigEff_LeptonEta"]= new rh::H1I("hTrigEff_LeptonEta", "; electron pseudo-rapidity #eta; Events", 20, -2, 2, "hist GRIDX");
     //o["hVertexEff_LeptonEta"]= new rh::H1I("hVertexEff_LeptonEta", "; electron pseudo-rapidity #eta; Events", 20, -2, 2, "hist GRIDX");
   }
   //--------------------------------------

}


/**
 * Fill histograms using information about the Monte-Carlo event.
 */
void MCHContainer::Fill(ProtoEvent &ev)
{
   WBosEvent& event = (WBosEvent&) ev;
   WBosMcEvent* mcEvent = (WBosMcEvent*) event.mMcEvent;

   ((TH1*) o["hWBosonE"])        ->Fill(mcEvent->mP4WBoson.E());
   ((TH1*) o["hWBosonPz"])       ->Fill(mcEvent->mP4WBoson.Pz());
   ((TH1*) o["hWBosonPt"])       ->Fill(mcEvent->mP4WBoson.Pt());
   ((TH1*) o["hWBosonPt_zoomin"])->Fill(mcEvent->mP4WBoson.Pt());

   if (mcEvent->mP4WBoson.Mag() > 0) {
      ((TH1*) o["hWBosonEta"])        ->Fill(mcEvent->mP4WBoson.Eta());
      ((TH1*) o["hWBosonEta_20bins"]) ->Fill(mcEvent->mP4WBoson.Eta());
      ((TH1*) o["hWBosonRap"])        ->Fill(mcEvent->mP4WBoson.Rapidity());
      ((TH1*) o["hWBosonRap_20bins"]) ->Fill(mcEvent->mP4WBoson.Rapidity());
      if (fabs(event.GetVecBosonP4().Pz()) < 50) {   // this cut is for Pl reconstruction
           ((TH1*) o["hWBosonRap_5bins"])  ->Fill(mcEvent->mP4WBoson.Rapidity());
      }
   } else {
      ((TH1*) o["hWBosonEta"])        ->Fill(1e5);
      ((TH1*) o["hWBosonEta_20bins"]) ->Fill(1e5);
      ((TH1*) o["hWBosonRap"])        ->Fill(1e5);
      ((TH1*) o["hWBosonRap_20bins"]) ->Fill(1e5);
      ((TH1*) o["hWBosonRap_5bins"])  ->Fill(1e5);
   }

   ((TH1*) o["hNeutrinoE"]) ->Fill(mcEvent->mP4Neutrino.E());
   ((TH1*) o["hNeutrinoPx"])->Fill(mcEvent->mP4Neutrino.Px());
   ((TH1*) o["hNeutrinoPy"])->Fill(mcEvent->mP4Neutrino.Py());
   ((TH1*) o["hNeutrinoPz"])->Fill(mcEvent->mP4Neutrino.Pz());
   ((TH1*) o["hNeutrinoPt"])->Fill(mcEvent->mP4Neutrino.Pz());

   ((TH1*) o["hLeptonE"])   ->Fill(mcEvent->mP4Lepton.E());
   ((TH1*) o["hLeptonEta"]) ->Fill(mcEvent->mP4Lepton.Eta());
   ((TH1*) o["hLeptonEta_8bins"]) ->Fill(mcEvent->mP4Lepton.Eta());
   ((TH1*) o["hLeptonPx"])  ->Fill(mcEvent->mP4Lepton.Px());
   ((TH1*) o["hLeptonPy"])  ->Fill(mcEvent->mP4Lepton.Py());
   ((TH1*) o["hLeptonPz"])  ->Fill(mcEvent->mP4Lepton.Pz());
   ((TH1*) o["hLeptonPt"])  ->Fill(mcEvent->mP4Lepton.Pt());

   ((TH2*) o["hRecoVsGenLeptonPhi"])        ->Fill(mcEvent->mP4Lepton.Phi(),   event.GetElectronP3().Phi());
   ((TH2*) o["hRecoVsGenLeptonPt"])         ->Fill(mcEvent->mP4Lepton.Pt(),    event.GetElectronP3().Pt());
   ((TH2*) o["hRecoVsGenLeptonPz"])         ->Fill(mcEvent->mP4Lepton.Pz(),    event.GetElectronP3().Pz());

   //if (mcEvent->mP4Lepton.Mag() > 0 && event.GetElectronP3().Mag() > 0) {
     ((TH2*) o["hRecoVsGenLeptonEta"])        ->Fill(mcEvent->mP4Lepton.Eta(),   event.GetElectronP3().Eta());
     ((TH2*) o["hRecoVsGenLeptonEta_10bins"]) ->Fill(mcEvent->mP4Lepton.Eta(),   event.GetElectronP3().Eta());
     ((TH2*) o["hRecoVsGenLeptonEta_8bins"] ) ->Fill(mcEvent->mP4Lepton.Eta(),   event.GetElectronP3().Eta());
   //}

   ((TH2*) o["hRecoVsGenNeutrinoPhi"])->Fill(mcEvent->mP4Neutrino.Phi(), event.GetNeutrinoP3().Phi());
   ((TH2*) o["hRecoVsGenNeutrinoPt"]) ->Fill(mcEvent->mP4Neutrino.Pt(),  event.GetNeutrinoP3().Pt());
   ((TH2*) o["hRecoVsGenNeutrinoPz"]) ->Fill(mcEvent->mP4Neutrino.Pz(),  event.GetNeutrinoP3().Pz());

   if ( fabs(mcEvent->mP4Neutrino.Pz() - event.GetNeutrinoP3().Pz()) <
        fabs(mcEvent->mP4Neutrino.Pz() - event.GetNeutrinoP3Other().Pz()) )
   {
      ((TH2*) o["hRecoVsGenNeutrinoPz_c"]) ->Fill(mcEvent->mP4Neutrino.Pz(),  event.GetNeutrinoP3().Pz());
      ((TH2*) o["hRecoVsGenNeutrinoPz_f"]) ->Fill(mcEvent->mP4Neutrino.Pz(),  event.GetNeutrinoP3Other().Pz());
   } else {
      ((TH2*) o["hRecoVsGenNeutrinoPz_c"]) ->Fill(mcEvent->mP4Neutrino.Pz(),  event.GetNeutrinoP3Other().Pz());
      ((TH2*) o["hRecoVsGenNeutrinoPz_f"]) ->Fill(mcEvent->mP4Neutrino.Pz(),  event.GetNeutrinoP3().Pz());
   }

   if (mcEvent->mP4Neutrino.Mag() > 0 && event.GetNeutrinoP3().Mag() > 0) {
      ((TH2*) o["hRecoVsGenNeutrinoEta"])->Fill(mcEvent->mP4Neutrino.Eta(), event.GetNeutrinoP3().Eta());
   }

   ((TH2*) o["hRecoVsGenWBosonPhi"])  ->Fill(mcEvent->mP4WBoson.Phi(),   event.GetVecBosonP3().Phi());
   ((TH2*) o["hRecoVsGenWBosonPt"])   ->Fill(mcEvent->mP4WBoson.Pt(),    event.GetVecBosonP3().Pt());
   ((TH2*) o["hRecoVsGenWBosonPz"])   ->Fill(mcEvent->mP4WBoson.Pz(),    event.GetVecBosonP3().Pz());

   if (mcEvent->mP4WBoson.Mag() > 0 && event.GetVecBosonP3().Mag() > 0) {
      ((TH2*) o["hRecoVsGenWBosonEta"])  ->Fill(mcEvent->mP4WBoson.Eta(),   event.GetVecBosonP3().Eta());
   }
   ((TH1*) o["hRecoVsGenWBosonPz_FirstSol"])   ->Fill(mcEvent->mP4WBoson.Pz(),    event.GetVecBosonP3FirstSolution().Pz());
   ((TH1*) o["hRecoVsGenWBosonPz_OtherSol"])   ->Fill(mcEvent->mP4WBoson.Pz(),    event.GetVecBosonP3OtherSolution().Pz());

   ((TH2*) o["hGen_WBosonEtaVsElectronEta"]) -> Fill(mcEvent->mP4WBoson.Eta(), mcEvent->mP4Lepton.Eta());

   if (event.GetVecBosonP3().Mag() > 0 && event.GetElectronP3().Mag() > 0)
      ((TH2*) o["hReco_WBosonEtaVsElectronEta"])-> Fill(event.GetVecBosonP3().Eta(), event.GetElectronP3().Eta());

   ((TH1*) o["hGenRecoilE"]) ->Fill(mcEvent->mP4Recoil.E());
   ((TH1*) o["hGenRecoilPz"])->Fill(mcEvent->mP4Recoil.Pz());
   ((TH1*) o["hGenRecoilPt"])->Fill(mcEvent->mP4Recoil.Pt());
   ((TH1*) o["hGenRecoilPhi"])->Fill(mcEvent->mP4Recoil.Phi());

   ((TH1*) o["hRecInAccEnergy"])    ->Fill(mcEvent->mP4RecoilInAccept.E());
   ((TH1*) o["hRecInAccMomentumX"]) ->Fill(mcEvent->mP4RecoilInAccept.Px());
   ((TH1*) o["hRecInAccMomentumY"]) ->Fill(mcEvent->mP4RecoilInAccept.Py());
   ((TH1*) o["hRecInAccMomentumZ"]) ->Fill(mcEvent->mP4RecoilInAccept.Pz());
   ((TH1*) o["hRecoilInAcceptPt"])  ->Fill(mcEvent->mP4RecoilInAccept.Pt());
   ((TH1*) o["hRecOutAccEnergy"])   ->Fill(mcEvent->mP4RecoilOutAccept.E());
   ((TH1*) o["hRecOutAccMomentumX"])->Fill(mcEvent->mP4RecoilOutAccept.Px());
   ((TH1*) o["hRecOutAccMomentumY"])->Fill(mcEvent->mP4RecoilOutAccept.Py());
   ((TH1*) o["hRecOutAccMomentumZ"])->Fill(mcEvent->mP4RecoilOutAccept.Pz());
   ((TH1*) o["hRecOutAccPt"])       ->Fill(mcEvent->mP4RecoilOutAccept.Pt());

   ((TH1*) o["hGenRecoilVsWBosonPt"])      ->Fill(mcEvent->mP4WBoson.Pt(), mcEvent->mP4Recoil.Pt());
   ((TH1*) o["hGenRecoilInAccVsWBosonPt"]) ->Fill(mcEvent->mP4WBoson.Pt(), mcEvent->mP4RecoilInAccept.Pt());
   ((TH1*) o["hGenRecoilOutAccVsWBosonPt"])->Fill(mcEvent->mP4WBoson.Pt(), mcEvent->mP4RecoilOutAccept.Pt());
   ((TH1*) o["hGenRecoilOutVsInAccPt"])    ->Fill(mcEvent->mP4RecoilInAccept.Pt(), mcEvent->mP4RecoilOutAccept.Pt());
   ((TH1*) o["hGenRecoilInOutDeltaPhi"])   ->Fill(mcEvent->mP4RecoilInAccept.DeltaPhi( mcEvent->mP4RecoilOutAccept ));


   if (mMcType == 1 || mMcType == 2) { // W+ or W- Monte Carlo 
     if (event.GetVecBosonP3().Pt() > 0) {
        ((TH1*) o["hAn_evol_ZK"])                    ->Fill(event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_PtGen"])           ->Fill(mcEvent->mP4WBoson.Pt(), event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_PtRec"])           ->Fill(event.GetVecBosonP3().Pt(), event.An_evol_ZK);
        ((TH1*) o["hAn_noevo_ZK"])                   ->Fill(event.An_noevo_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen"])          ->Fill(mcEvent->mP4WBoson.Pt(), event.An_noevo_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec"])          ->Fill(event.GetVecBosonP3().Pt(), event.An_noevo_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen_zoomin"])   ->Fill(mcEvent->mP4WBoson.Pt(), event.An_noevo_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec_zoomin"])   ->Fill(event.GetVecBosonP3().Pt(), event.An_noevo_ZK);
	// TEST 
        Double_t genPz = mcEvent->mP4WBoson.Pz();
	Double_t recPz = event.GetVecBosonP3FirstSolution().Pz();
        if (recPz > 0) {
          ((TH2*) o["hAnEvolVsWgen_WrongReco_FirstSol"])   ->Fill(genPz, event.An_evol_ZK);
        } else {
          ((TH2*) o["hAnEvolVsWgen_FirstSol"])   ->Fill(genPz, event.An_evol_ZK);
        }
     } else {
        ((TH1*) o["hAn_evol_ZK"])                     ->Fill(1e5);
        ((TH2*) o["hAn_evol_ZK_Vs_PtGen"])            ->Fill(1e5, 1e5);
        ((TH2*) o["hAn_evol_ZK_Vs_PtRec"])            ->Fill(1e5, 1e5);
        ((TH1*) o["hAn_noevo_ZK"])                    ->Fill(1e5);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen"])           ->Fill(1e5, 1e5);  
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec"])           ->Fill(1e5, 1e5);  
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen_zoomin"])    ->Fill(1e5, 1e5);   
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec_zoomin"])    ->Fill(1e5, 1e5);  
     }

     if (event.GetVecBosonP4().Rapidity() ) {

	Wy_gen  = mcEvent->mP4WBoson.Rapidity();
        Wy_rec  = event.GetVecBosonP4().Rapidity();
	An_evol = event.An_evol_ZK;
        ((TNtuple*) o["ntAnEvolution"]) ->Fill(Wy_gen, Wy_rec, An_evol);

        ((TH2*) o["hAn_evol_ZK_Vs_RapGen"])           ->Fill(mcEvent->mP4WBoson.Rapidity(), event.An_evol_ZK);

        //Double_t xBinsRap[5] = {-0.6, -0.25, 0, 0.25, 0.6};

        Double_t RapReco = event.GetVecBosonP4().Rapidity();

        //((TH2*) o["hAn_evol_ZK_Vs_RapRec"])           ->Fill(event.GetVecBosonP4().Rapidity(), event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_RapRec"])           ->Fill(RapReco, event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_RapRecPR"])         ->Fill(RapReco, event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_RapRecMR"])         ->Fill(RapReco, event.An_evol_ZK);
        ((TH2*) o["hRecoVsGenWBosonRap"])             ->Fill(mcEvent->mP4WBoson.Rapidity(),   event.GetVecBosonP4().Rapidity());
        ((TH1*) o["hResolutionRap"])                  ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());

	// Calculate the Pt resolution in each of the Pt bins we use for A_N
        Double_t Wpt = mcEvent->mP4WBoson.Pt();
        if (Wpt >= xBinsPt[0] && Wpt < xBinsPt[1]) {
             ((TH1*) o["hResolutionPt_bin1"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin1"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
      	} else if (Wpt >= xBinsPt[1] && Wpt < xBinsPt[2]) {
             ((TH1*) o["hResolutionPt_bin2"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin2"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
	} else if (Wpt >= xBinsPt[2] && Wpt < xBinsPt[3]) {
             ((TH1*) o["hResolutionPt_bin3"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin3"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
 	} else if (Wpt >= xBinsPt[3] && Wpt < xBinsPt[4]) {
             ((TH1*) o["hResolutionPt_bin4"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin4"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
 	} else if (Wpt >= xBinsPt[4] && Wpt < xBinsPt[5]) {
             ((TH1*) o["hResolutionPt_bin5"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin5"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
 	} else if (Wpt >= xBinsPt[5] && Wpt < xBinsPt[6]) {
             ((TH1*) o["hResolutionPt_bin6"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
             ((TH1*) o["hResolutionPtCorr_bin6"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
 	} 
	//else if (Wpt >= xBinsPt[6] && Wpt < xBinsPt[7]) {
	//             ((TH1*) o["hResolutionPt_bin7"])     ->Fill((mcEvent->mP4WBoson.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4WBoson.Pt());
	//             ((TH1*) o["hResolutionPtCorr_bin7"]) ->Fill((mcEvent->mP4WBoson.Pt()-event.GetVecBosonP3().Pt())/mcEvent->mP4WBoson.Pt());
	//	}

        Double_t RecoInAccPt = mcEvent->mP4RecoilInAccept.Pt();
        if (RecoInAccPt >= xBinsPt[0] && RecoInAccPt < xBinsPt[1]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin1"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin1"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
      	} else if (RecoInAccPt >= xBinsPt[1] && RecoInAccPt < xBinsPt[2]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin2"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin2"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
	} else if (RecoInAccPt >= xBinsPt[2] && RecoInAccPt < xBinsPt[3]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin3"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin3"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
 	} else if (RecoInAccPt >= xBinsPt[3] && RecoInAccPt < xBinsPt[4]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin4"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin4"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
 	} else if (RecoInAccPt >= xBinsPt[4] && RecoInAccPt < xBinsPt[5]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin5"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin5"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
 	} else if (RecoInAccPt >= xBinsPt[5] && RecoInAccPt < xBinsPt[6]) {
             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin6"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin6"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
 	}
	// else if (RecoInAccPt >= xBinsPt[6] && RecoInAccPt < xBinsPt[7]) {
	//             ((TH1*) o["hResolution_RecoilInAcc_Pt_bin7"])     ->Fill((mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt())/mcEvent->mP4RecoilInAccept.Pt());
	//             ((TH1*) o["hAbsoResolution_RecoilInAcc_Pt_bin7"]) ->Fill(mcEvent->mP4RecoilInAccept.Pt()-event.GetTrackRecoilTpcNeutrals().Pt());
	//	}

	// Calculate the rapidity resolution in each of the rapidity bins we use for A_N
        Double_t WRap = mcEvent->mP4WBoson.Rapidity();
        Double_t RecoInAccRap = mcEvent->mP4RecoilInAccept.Rapidity();
        if (RapBins == 4) {

          if (WRap >= xBinsRap[0] && WRap < xBinsRap[1]) {
             ((TH1*) o["hResolutionRap_bin1"])   ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
      	  } else if (WRap >= xBinsRap[1] && WRap < xBinsRap[2]) {
             ((TH1*) o["hResolutionRap_bin2"])   ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
	  } else if (WRap >= xBinsRap[2] && WRap < xBinsRap[3]) {
             ((TH1*) o["hResolutionRap_bin3"])   ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
 	  } else if (WRap >= xBinsRap[3] && WRap < xBinsRap[4]) {
             ((TH1*) o["hResolutionRap_bin4"])   ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
	  }

	} else if (RapBins == 3) {

          if (WRap >= xBinsRap[0] && WRap < xBinsRap[1]) {
             ((TH1*) o["hResolutionRap_bin1"])     ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
             ((TH1*) o["hAbsoResolutionRap_bin1"]) ->Fill(mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity());
      	  } else if (WRap >= xBinsRap[1] && WRap < xBinsRap[2]) {
             ((TH1*) o["hResolutionRap_bin2"])     ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
             ((TH1*) o["hAbsoResolutionRap_bin2"]) ->Fill(mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity());
	  } else if (WRap >= xBinsRap[2] && WRap < xBinsRap[3]) {
             ((TH1*) o["hResolutionRap_bin3"])     ->Fill((mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity())/mcEvent->mP4WBoson.Rapidity());
             ((TH1*) o["hAbsoResolutionRap_bin3"]) ->Fill(mcEvent->mP4WBoson.Rapidity()-event.GetVecBosonP4().Rapidity());
 	  }

          //if (RecoInAccRap >= xBinsRap[0] && RecoInAccRap < xBinsRap[1]) {
	  //  ((TH1*) o["hResolution_RecoilInAcc_Rap_bin1"])     ->Fill((mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity())/mcEvent->mP4RecoilInAccept.Rapidity());
          //   ((TH1*) o["hAbsoResolution_RecoilInAcc_Rap_bin1"]) ->Fill(mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity());
      	  //} else if (RecoInAccRap >= xBinsRap[1] && RecoInAccRap < xBinsRap[2]) {
          //   ((TH1*) o["hResolution_RecoilInAcc_Rap_bin2"])     ->Fill((mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity())/mcEvent->mP4RecoilInAccept.Rapidity());
          //   ((TH1*) o["hAbsoResolution_RecoilInAcc_Rap_bin2"]) ->Fill(mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity());
	  //} else if (RecoInAccRap >= xBinsRap[2] && RecoInAccRap < xBinsRap[3]) {
          //   ((TH1*) o["hResolution_RecoilInAcc_Rap_bin3"])     ->Fill((mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity())/mcEvent->mP4RecoilInAccept.Rapidity());
          //   ((TH1*) o["hAbsoResolution_RecoilInAcc_Rap_bin3"]) ->Fill(mcEvent->mP4RecoilInAccept.Rapidity()-event.GetTrackRecoilTpcNeutrals().Rapidity());				 
	  // }
	}
     }
   }


   //if (mMcType == 2) {  // W- Monte Carlo   
     //if (event.GetVecBosonP4().Rapidity() ) {
       //((TH2*) o["hAn_evol_ZK_Vs_RapGen"])           ->Fill(mcEvent->mP4WBoson.Rapidity(), event.An_evol_ZK);
       //((TH2*) o["hAn_evol_ZK_Vs_RapRec"])           ->Fill(event.GetVecBosonP4().Rapidity(), event.An_evol_ZK);
     //((TH2*) o["hAn_noevo_ZK_Vs_RapGen"])          ->Fill(mcEvent->mP4WBoson.Rapidity(), event.An_noevo_ZK);
     //((TH2*) o["hAn_noevo_ZK_Vs_RapRec"])          ->Fill(event.GetVecBosonP4().Rapidity(), event.An_noevo_ZK);
     //((TH2*) o["hAn_noevo_ZK_Vs_RapGen_zoomin"])   ->Fill(mcEvent->mP4WBoson.Rapidity(), event.An_noevo_ZK);
     //((TH2*) o["hAn_noevo_ZK_Vs_RapRec_zoomin"])   ->Fill(event.GetVecBosonP4().Rapidity(), event.An_noevo_ZK);
     //}
   //}

   if (mMcType == 1) { // W+ Monte Carlo
     if (event.GetVecBosonP4().Rapidity() > 0) {

        ((TH1*) o["hAn_evol_ZK_y>0"])                ->Fill(event.An_evol_ZK);
        ((TH2*) o["hAn_evol_ZK_Vs_PtRec_y>0"])       ->Fill(event.GetVecBosonP3().Pt(), event.An_evol_ZK);
        ((TH1*) o["hAn_noevo_ZK_y>0"])               ->Fill(event.An_noevo_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec_y>0"])      ->Fill(event.GetVecBosonP3().Pt(), event.An_noevo_ZK);   

     } else {
        ((TH1*) o["hAn_evol_ZK_y>0"])                ->Fill(1e5);
        ((TH2*) o["hAn_evol_ZK_Vs_PtRec_y>0"])       ->Fill(1e5, 1e5);
        ((TH1*) o["hAn_noevo_ZK_y>0"])               ->Fill(1e5);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtRec_y>0"])      ->Fill(1e5, 1e5);  
     }
   
     if (mcEvent->mP4WBoson.Rapidity() > 0) {

        ((TH2*) o["hAn_evol_ZK_Vs_PtGen_y>0"])       ->Fill(mcEvent->mP4WBoson.Pt(), event.An_evol_ZK);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen_y>0"])      ->Fill(mcEvent->mP4WBoson.Pt(), event.An_noevo_ZK);

     } else {
        ((TH2*) o["hAn_evol_ZK_Vs_PtGen_y>0"])       ->Fill(1e5, 1e5);
        ((TH2*) o["hAn_noevo_ZK_Vs_PtGen_y>0"])      ->Fill(1e5, 1e5); 
     }
   }

   ((TH2*) o["hJetRecoilPtVsWBosonPt"])         ->Fill(mcEvent->mP4WBoson.Pt(),  event.GetJetRecoil().Pt());
   ((TH2*) o["hJetRecoilPhiVsWBosonPhi"])       ->Fill(mcEvent->mP4WBoson.Phi(), event.GetJetRecoil().Phi());
   ((TH2*) o["hJetRecoilPt_GenOverReco"])       ->Fill(event.GetJetRecoil().Pt(), mcEvent->mP4WBoson.Pt()/event.GetJetRecoil().Pt());
   ((TH2*) o["hJetRecoilPt_GenOverReco_zoomin"])->Fill(event.GetJetRecoil().Pt(), mcEvent->mP4WBoson.Pt()/event.GetJetRecoil().Pt());

   ((TH2*) o["hTrackRecoilPtVsWBosonPt"])                    ->Fill(mcEvent->mP4WBoson.Pt(),  event.GetTrackRecoil().Pt());
   ((TH2*) o["hTrackRecoilPhiVsWBosonPhi"])                  ->Fill(mcEvent->mP4WBoson.Phi(), event.GetTrackRecoil().Phi());
   ((TH2*) o["hTrackRecoilTpcPtVsWBosonPt"])                 ->Fill(mcEvent->mP4WBoson.Pt(),  event.mP3TrackRecoilTpc.Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPtVsWBosonPt"])         ->Fill(mcEvent->mP4WBoson.Pt(),  event.GetTrackRecoilTpcNeutrals().Pt());
   //((TH2*) o["hTrackRecoilTpcNeutralsPtVsWBosonPt"])         ->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt"])->Fill(mcEvent->mP4WBoson.Pt(),  event.GetTrackRecoilTpcNeutralsCorrected().Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPtVsWBosonPt_copy"])         ->Fill(mcEvent->mP4WBoson.Pt(),  event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt_copy"])->Fill(mcEvent->mP4WBoson.Pt(),  event.GetTrackRecoilTpcNeutralsCorrected().Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco"])       ->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt()/event.GetTrackRecoilTpcNeutrals().Pt());
   ((TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin"])->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt()/event.GetTrackRecoilTpcNeutrals().Pt());

   if ( event.zdcRate > 100000 && event.zdcRate < 200000 )
   ((TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc100k-200k"])       ->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt()/event.GetTrackRecoilTpcNeutrals().Pt());

   if ( event.zdcRate > 200000 && event.zdcRate < 300000 )
   ((TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc200k-300k"])       ->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt()/event.GetTrackRecoilTpcNeutrals().Pt());

   if ( event.zdcRate > 300000 && event.zdcRate < 400000 )
   ((TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco_zdc300k-400k"])       ->Fill(event.GetTrackRecoilTpcNeutrals().Pt(), mcEvent->mP4WBoson.Pt()/event.GetTrackRecoilTpcNeutrals().Pt());

   Double_t recoilRelDiff = (event.GetJetRecoil().Pt() - mcEvent->mP4WBoson.Pt())/mcEvent->mP4WBoson.Pt();
   ((TH1*) o["hJetRecoilPtRelDiff"]) ->Fill( recoilRelDiff );

   recoilRelDiff = (event.GetTrackRecoil().Pt() - mcEvent->mP4WBoson.Pt())/mcEvent->mP4WBoson.Pt();
   ((TH1*) o["hTrackRecoilPtRelDiff"]) ->Fill( recoilRelDiff );

   recoilRelDiff = (event.GetTrackRecoilTpcNeutrals().Pt() - mcEvent->mP4WBoson.Pt())/mcEvent->mP4WBoson.Pt();
   ((TH1*) o["hTrackRecoilTpcNeutralsPtRelDiff"]) ->Fill( recoilRelDiff );

   recoilRelDiff = (event.GetTrackRecoil().Pt() - event.GetJetRecoil().Pt())/event.GetJetRecoil().Pt();
   ((TH1*) o["hJetTrackRecoilPtRelDiff"]) ->Fill( recoilRelDiff );

   recoilRelDiff = (event.GetTrackRecoilTpcNeutrals().Pt() - event.GetJetRecoil().Pt())/event.GetJetRecoil().Pt();
   ((TH1*) o["hJetTrackTpcNeutralsRecoilPtRelDiff"]) ->Fill( recoilRelDiff );



   // Efficiency Plots
   // -------------------------------------
   if (mMcType == 1 || mMcType == 2) { // W+ or W- Monte Carlo 
      if (mcEvent->mP4LeptonInAccept.Mag() > 0)   
        ((TH1*) o["hLeptonInAccEta_TEST"])      -> Fill(mcEvent->mP4LeptonInAccept.Eta()); 
     //if (mcEvent->mP4Lepton_TrigEff.Mag() > 0)      
     //((TH1*) o["hTrigEff_LeptonEta"])   -> Fill(mcEvent->mP4Lepton_TrigEff.Eta());
     
      if (fabs(mcEvent-> mP4Lepton.Eta()) < 1.)   
        ((TH1*) o["hLeptonInAccEta"])      -> Fill(mcEvent->mP4Lepton.Eta());

       
      //if (event.l2bitET);  //pass the trigger trig
      //((TH1*) o["hTrigEff_LeptonEta"])   -> Fill(mcEvent->mP4Lepton.Eta());
     
      //if (event.mVertices.size() <= 0) return; // has a good vertex
      //((TH1*) o["hVertexEff_LeptonEta"]) -> Fill(mcEvent->mP4Lepton.Eta());
       
   }
   //--------------------------------------
}


/** */
void MCHContainer::PostFill()
{
   Info("PostFill", "Called");

   /*

   // Fit the correction curve
   TH2* hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin = (TH2*) o["hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin"];
   o["hRecoilCorrection"] = (TProfile*) hTrackRecoilTpcNeutralsPt_GenOverReco_zoomin->ProfileX("hRecoilCorrection");
   TProfile* hRecoilCorrection = (TProfile*) o["hRecoilCorrection"];
   hRecoilCorrection->Print("all");

   if (hRecoilCorrection->Integral() ) {
      hRecoilCorrection->Fit("pol3", "W +", "", 0, 5);  // The W option helps with low stat when error bar is 0
      hRecoilCorrection->Fit("pol0", "W +", "", 5, 10);
   }

   // Fit the means
   TH2I* hRecoilVsWBosonPt = (TH2I*) o["hTrackRecoilTpcNeutralsPtVsWBosonPt"];
   TProfile* hRecoilVsWBosonPt_pfx = hRecoilVsWBosonPt->ProfileX();
   TF1 fitFunc("fitFunc", "[0] + [1]*x", hRecoilVsWBosonPt_pfx->GetXaxis()->GetXmin(), hRecoilVsWBosonPt_pfx->GetXaxis()->GetXmax());
   fitFunc.SetParNames("Offset", "Slope");
   hRecoilVsWBosonPt_pfx->Fit(&fitFunc);
   hRecoilVsWBosonPt->GetListOfFunctions()->Add(hRecoilVsWBosonPt_pfx->Clone(), "same");

   // Fit the means of the corrected recoil p_T
   hRecoilVsWBosonPt = (TH2I*) o["hTrackRecoilTpcNeutralsPtCorrectedVsWBosonPt"];
   hRecoilVsWBosonPt_pfx = hRecoilVsWBosonPt->ProfileX();
   TF1 fitFuncCorrected("fitFuncCorrected", "[0] + [1]*x", hRecoilVsWBosonPt_pfx->GetXaxis()->GetXmin(), hRecoilVsWBosonPt_pfx->GetXaxis()->GetXmax());
   fitFuncCorrected.SetParNames("Offset", "Slope");
   hRecoilVsWBosonPt_pfx->Fit(&fitFuncCorrected);
   hRecoilVsWBosonPt->GetListOfFunctions()->Add(hRecoilVsWBosonPt_pfx->Clone(), "same");

   // Calculate the fraction of missreconstructed events
   rh::H2F* hRecoVsGenWBosonPz = (rh::H2F*) o["hRecoVsGenWBosonPz"];

   TF1 funcLow ("funcLow", "-30 + x", hRecoVsGenWBosonPz->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz->GetXaxis()->GetXmax());
   TF1 funcHigh("funcHigh", "30 + x", hRecoVsGenWBosonPz->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz->GetXaxis()->GetXmax());

   double integralAboveLow  = hRecoVsGenWBosonPz->CalcIntegralAbove(funcLow);
   double integralAboveHigh = hRecoVsGenWBosonPz->CalcIntegralAbove(funcHigh);
   double integral = hRecoVsGenWBosonPz->Integral();

   char textFrac[10];
   sprintf(textFrac, "frac = %5.3f", (integralAboveLow-integralAboveHigh)/integral);

   hRecoVsGenWBosonPz->GetListOfFunctions()->Add(funcLow.Clone());
   hRecoVsGenWBosonPz->GetListOfFunctions()->Add(funcHigh.Clone());
   TText *text = new TText(0.5, 0.92, textFrac);
   text->SetNDC(true);
   hRecoVsGenWBosonPz->GetListOfFunctions()->Add(text);
   hRecoVsGenWBosonPz->SetStats(0);
 

   rh::H2F* hRecoVsGenWBosonPz_FirstSol = (rh::H2F*) o["hRecoVsGenWBosonPz_FirstSol"];

   TF1 funcLowf ("funcLow", "-30 + x", hRecoVsGenWBosonPz_FirstSol->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz_FirstSol->GetXaxis()->GetXmax());
   TF1 funcHighf("funcHigh", "30 + x", hRecoVsGenWBosonPz_FirstSol->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz_FirstSol->GetXaxis()->GetXmax());

   double integralAboveLowf  = hRecoVsGenWBosonPz_FirstSol->CalcIntegralAbove(funcLow);
   double integralAboveHighf = hRecoVsGenWBosonPz_FirstSol->CalcIntegralAbove(funcHigh);
   double integralf = hRecoVsGenWBosonPz_FirstSol->Integral();

   char textFracf[10];
   sprintf(textFracf, "frac = %5.3f", (integralAboveLowf-integralAboveHighf)/integralf);

   hRecoVsGenWBosonPz_FirstSol->GetListOfFunctions()->Add(funcLowf.Clone());
   hRecoVsGenWBosonPz_FirstSol->GetListOfFunctions()->Add(funcHighf.Clone());
   TText *textf = new TText(0.5, 0.92, textFracf);
   textf->SetNDC(true);
   hRecoVsGenWBosonPz_FirstSol->GetListOfFunctions()->Add(textf);
   hRecoVsGenWBosonPz_FirstSol->SetStats(0);
 

   Int_t range = 10;
   TString basename("hRecoVsGenWBosonPz_FirstSol_pjy");
   Int_t ybins = hRecoVsGenWBosonPz_FirstSol->GetNbinsY();

   o["hWBosonPz_GoodRecoFraction"] = new rh::H1D("hWBosonPz_GoodRecoFraction", "First solution; W-P_{Z} [GeV/c]; Fraction", ybins, hRecoVsGenWBosonPz_FirstSol->GetYaxis()->GetXmin(), hRecoVsGenWBosonPz_FirstSol->GetYaxis()->GetXmax(), "P GRIDX");
   TH1D* hWBosonPz_GoodRecoFraction = (TH1D*) o["hWBosonPz_GoodRecoFraction"]; 
   hWBosonPz_GoodRecoFraction -> SetMarkerStyle(20);
   hWBosonPz_GoodRecoFraction -> SetStats(0);

   for (int i = 1; i <= ybins; ++i) {
     TString histname(basename);
     histname += i;
     o[histname.Data()] = (TProfile*) hRecoVsGenWBosonPz_FirstSol->ProjectionY(histname.Data(), i, i);
     TH1D* hRecoVsGenWBosonPz_FirstSol_pjy = (TH1D*) o[histname.Data()];

     Int_t prjBins = hRecoVsGenWBosonPz_FirstSol_pjy->GetNbinsX();
     Int_t minval = i - range;
     Int_t maxval = i + range;
     if (minval <= 0) minval = 1;
     if (maxval >= prjBins) maxval = prjBins;
     double   integral_pjy_inner   = hRecoVsGenWBosonPz_FirstSol_pjy->Integral(minval, maxval);
     double   integral_pjy_total   = hRecoVsGenWBosonPz_FirstSol_pjy->Integral();
     Double_t GoodRecoFraction_pjy = integral_pjy_inner/integral_pjy_total;
     cout << "integral_pjy_inner: "   << integral_pjy_inner   << endl; 
     cout << "integral_pjy_total: "   << integral_pjy_total   << endl;  
     cout << "GoodRecoFraction_pjy: " << GoodRecoFraction_pjy << endl;
     hWBosonPz_GoodRecoFraction -> SetBinContent(i, GoodRecoFraction_pjy);
   } // for

   TLine *l10 = new TLine(20,  0.18, 20, 0.96);
   l10 -> SetLineColor(kRed);
   l10 -> SetLineWidth(2);
   l10 -> SetLineStyle(2);
   TLine *lm10 = new TLine(-20, 0.18, -20, 0.96);
   lm10 -> SetLineColor(kRed);
   lm10 -> SetLineWidth(2);
   lm10 -> SetLineStyle(2);

   hWBosonPz_GoodRecoFraction -> GetListOfFunctions() -> Add(l10);
   hWBosonPz_GoodRecoFraction -> GetListOfFunctions() -> Add(lm10);
  
   rh::H2F* hRecoVsGenWBosonPz_OtherSol = (rh::H2F*) o["hRecoVsGenWBosonPz_OtherSol"];

   TF1 funcLow_OtherSol ("funcLow_OtherSol", "-30 + x", hRecoVsGenWBosonPz_OtherSol->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz_OtherSol->GetXaxis()->GetXmax());
   TF1 funcHigh_OtherSol("funcHigh_OtherSol", "30 + x", hRecoVsGenWBosonPz_OtherSol->GetXaxis()->GetXmin(), hRecoVsGenWBosonPz_OtherSol->GetXaxis()->GetXmax());

   double integralAboveLow_OtherSol  = hRecoVsGenWBosonPz_OtherSol->CalcIntegralAbove(funcLow);
   double integralAboveHigh_OtherSol = hRecoVsGenWBosonPz_OtherSol->CalcIntegralAbove(funcHigh);
   double integral_OtherSol          = hRecoVsGenWBosonPz_OtherSol->Integral();

   char textFrac_OtherSol[10];
   sprintf(textFrac_OtherSol, "frac = %5.3f", (integralAboveLow_OtherSol-integralAboveHigh_OtherSol)/integral_OtherSol);

   hRecoVsGenWBosonPz_OtherSol->GetListOfFunctions()->Add(funcLow_OtherSol.Clone());
   hRecoVsGenWBosonPz_OtherSol->GetListOfFunctions()->Add(funcHigh_OtherSol.Clone());
   TText *text_OtherSol = new TText(0.5, 0.92, textFrac_OtherSol);
   text_OtherSol->SetNDC(true);
   hRecoVsGenWBosonPz_OtherSol->GetListOfFunctions()->Add(text_OtherSol);
   hRecoVsGenWBosonPz_OtherSol->SetStats(0);


   TString basenameo("hRecoVsGenWBosonPz_OtherSol_pjy");
   Int_t ybinso = hRecoVsGenWBosonPz_OtherSol->GetNbinsY();

   o["hWBosonPz_GoodRecoFraction_OtherSol"] = new rh::H1D("hWBosonPz_GoodRecoFraction_OtherSol", "Other solution; W-P_{Z} [GeV/c]; Fraction", ybinso, hRecoVsGenWBosonPz_OtherSol->GetYaxis()->GetXmin(), hRecoVsGenWBosonPz_OtherSol->GetYaxis()->GetXmax(), "P GRIDX");
   TH1D* hWBosonPz_GoodRecoFraction_OtherSol = (TH1D*) o["hWBosonPz_GoodRecoFraction_OtherSol"]; 
   hWBosonPz_GoodRecoFraction_OtherSol -> SetMarkerStyle(20);
   hWBosonPz_GoodRecoFraction_OtherSol -> SetStats(0);

   for (int i = 1; i <= ybinso; ++i) {
     TString histnameo(basenameo);
     histnameo += i;
     o[histnameo.Data()] = (TProfile*) hRecoVsGenWBosonPz_OtherSol->ProjectionY(histnameo.Data(), i, i);
     TH1D* hRecoVsGenWBosonPz_OtherSol_pjy = (TH1D*) o[histnameo.Data()];

     Int_t prjBins = hRecoVsGenWBosonPz_OtherSol_pjy->GetNbinsX();
     Int_t minval = i - range;
     Int_t maxval = i + range;
     if (minval <= 0) minval = 1;
     if (maxval >= prjBins) maxval = prjBins;
     double   integral_pjy_inner   = hRecoVsGenWBosonPz_OtherSol_pjy->Integral(minval, maxval);
     double   integral_pjy_total   = hRecoVsGenWBosonPz_OtherSol_pjy->Integral();
     Double_t GoodRecoFraction_pjy = integral_pjy_inner/integral_pjy_total;
     hWBosonPz_GoodRecoFraction_OtherSol -> SetBinContent(i, GoodRecoFraction_pjy);
   } // for

 
   // Fit the RESOLUTION plots
   TH1D* hResolutionRap_bin1 = (TH1D*) o["hResolutionRap_bin1"];
   TH1D* hResolutionRap_bin2 = (TH1D*) o["hResolutionRap_bin2"];
   TH1D* hResolutionRap_bin3 = (TH1D*) o["hResolutionRap_bin3"];
   //TH1D* hResolutionRap_bin4 = (TH1D*) o["hResolutionRap_bin4"];
   //TF1 fitGaus("fitGaus", "gaus", -4, 4);
   //hResolutionRap -> Fit(&fitGaus);
   hResolutionRap_bin1 -> Fit("gaus");
   hResolutionRap_bin2 -> Fit("gaus", "","", -3, 3);
   hResolutionRap_bin3 -> Fit("gaus", "","", -3, 3);
   //hResolutionRap_bin4 -> Fit("gaus");
   //TF1 *fResRap = hResolutionRap -> GetFunction("gaus");
   //hResolutionRap -> GetListOfFunctions(fResRap);
   double sigma_ResoBin1 =  hResolutionRap_bin1 -> GetFunction("gaus") -> GetParameter(2);   
   double sigma_ResoBin2 =  hResolutionRap_bin2 -> GetFunction("gaus") -> GetParameter(2);      
   double sigma_ResoBin3 =  hResolutionRap_bin3 -> GetFunction("gaus") -> GetParameter(2);     
   //double sigma_ResoBin4 =  hResolutionRap_bin4 -> GetFunction("gaus") -> GetParameter(2);   
   cout << "sigma_ResoBin1 = " << sigma_ResoBin1 << endl;   
   cout << "sigma_ResoBin2 = " << sigma_ResoBin2 << endl;   
   cout << "sigma_ResoBin3 = " << sigma_ResoBin3 << endl;   
   //cout << "sigma_ResoBin4 = " << sigma_ResoBin4 << endl;


   // Calculate the survival probability per Rapidity bin *****************************
   TNtuple* ntAnEvolution = (TNtuple*) o["ntAnEvolution"];
   Double_t xBinUpper = -0.2;

   o["hWy_RecVsGen"] = new TH2D("hWy_RecVsGen", "; W Rapidity Gen; W Rapidity Rec", RapBins, xBinsRap, RapBins, xBinsRap);
   TH2D* hWy_RecVsGen = (TH2D*) o["hWy_RecVsGen"];
   hWy_RecVsGen -> SetOption("colz");

   o["hWy_rec_gbin1"] = new TH1D("hWy_rec_gbin1", "Generated bin 1; W Rapidity Rec; Events", 30, -1.5, 1.5);
   TH1D* hWy_rec_gbin1 = (TH1D*) o["hWy_rec_gbin1"];
   o["hWy_rec_gbin2"] = new TH1D("hWy_rec_gbin2", "Generated bin 2; W Rapidity Rec; Events", 30, -1.5, 1.5);
   TH1D* hWy_rec_gbin2 = (TH1D*) o["hWy_rec_gbin2"];

   ntAnEvolution -> Project("hWy_rec_gbin1", "Wy_rec", "Wy_gen > -0.6 && Wy_gen < -0.2"); 

   ntAnEvolution -> Project("hWy_rec_gbin2", "Wy_rec", "Wy_gen > -0.2 && Wy_gen < 0."); 

   ntAnEvolution -> Project("hWy_RecVsGen", "Wy_rec:Wy_gen"); 
       
   for (int i = 1; i <= RapBins; ++i) {
     Double_t gbin = hWy_RecVsGen -> Integral(i, i, i, i);
     Double_t col  = hWy_RecVsGen -> Integral(i, i, 0, 5); //includes under and over-flows events
     cout << "gbin =" << gbin << endl;
     cout << "col ="  << col  << endl;
     cout << "survival probability =" << gbin/col << endl; 
     TString histnameo("hWy_RecVsGen_pjy");
     histnameo += i;
     o[histnameo.Data()] = (TProfile*) hWy_RecVsGen -> ProjectionY(histnameo.Data(), i, i);
     TH1D* hWy_RecVsGen_pjy = (TH1D*) o[histnameo.Data()];
   }
   //**************  END SURVIVAL PROBABILITY CALC. ***********************

   TH2D* hAn_evol_ZK_Vs_RapRec = (TH2D*) o["hAn_evol_ZK_Vs_RapRec"];
   TH2D* hAn_evol_ZK_Vs_RapRecPR = (TH2D*) o["hAn_evol_ZK_Vs_RapRecPR"];
   TH2D* hAn_evol_ZK_Vs_RapRecMR = (TH2D*) o["hAn_evol_ZK_Vs_RapRecMR"];

   TString hAn_evol_ZK_Vs_RapRec_nameo("hAn_evol_ZK_Vs_RapRec_pjy");
   TString hAn_evol_ZK_Vs_RapRecPR_nameo("hAn_evol_ZK_Vs_RapRecPR_pjy");
   TString hAn_evol_ZK_Vs_RapRecMR_nameo("hAn_evol_ZK_Vs_RapRecMR_pjy");
   TString bin("bin");
   o["hSystematics_evol_PR"] = new TH1D("hSystematics_evol_PR", "; y; systematic uncertainty (%)", RapBins, xBinsRap);
   o["hSystematics_evol_MR"] = new TH1D("hSystematics_evol_MR", "; y; systematic uncertainty (%)", RapBins, xBinsRap);
   TH1D* hSystematics_evol_PR = (TH1D*) o["hSystematics_evol_PR"];
   TH1D* hSystematics_evol_MR = (TH1D*) o["hSystematics_evol_MR"];
   hSystematics_evol_PR -> SetOption("P");
   hSystematics_evol_PR -> SetMarkerColor(kRed);
   hSystematics_evol_PR -> SetMarkerStyle(22);
   hSystematics_evol_PR -> SetMarkerSize(3);
   hSystematics_evol_PR -> SetStats(0);

   hSystematics_evol_MR -> SetOption("P");
   hSystematics_evol_MR -> SetMarkerColor(kBlue);
   hSystematics_evol_MR -> SetMarkerStyle(23);
   hSystematics_evol_MR -> SetMarkerSize(3);
   hSystematics_evol_MR -> SetStats(0);

   for (int i = 1; i <= RapBins; ++i) {
     TString histnameo(hAn_evol_ZK_Vs_RapRec_nameo);
     TString histnameoPR(hAn_evol_ZK_Vs_RapRecPR_nameo);
     TString histnameoMR(hAn_evol_ZK_Vs_RapRecMR_nameo);
     TString bino(bin);
     histnameo += i;
     histnameoPR += i;
     histnameoMR += i;
     bino += i;
     o[histnameo.Data()]   = (TProfile*) hAn_evol_ZK_Vs_RapRec   -> ProjectionY(histnameo.Data(), i, i);
     o[histnameoPR.Data()] = (TProfile*) hAn_evol_ZK_Vs_RapRecPR -> ProjectionY(histnameoPR.Data(), i, i);
     o[histnameoMR.Data()] = (TProfile*) hAn_evol_ZK_Vs_RapRecMR -> ProjectionY(histnameoMR.Data(), i, i);
     TH1D* hAn_evol_ZK_Vs_RapRec_pjy   = (TH1D*) o[histnameo.Data()];
     TH1D* hAn_evol_ZK_Vs_RapRecPR_pjy = (TH1D*) o[histnameoPR.Data()];
     TH1D* hAn_evol_ZK_Vs_RapRecMR_pjy = (TH1D*) o[histnameoMR.Data()];
     Double_t mean   = hAn_evol_ZK_Vs_RapRec_pjy   -> GetMean();
     Double_t meanPR = hAn_evol_ZK_Vs_RapRecPR_pjy -> GetMean();
     Double_t meanMR = hAn_evol_ZK_Vs_RapRecMR_pjy -> GetMean();
     cout << "mean " << bino << " = " << mean << endl;
     cout << "meanPR " << bino << " = " << meanPR << endl;
     cout << "meanMR " << bino << " = " << meanMR << endl;

     Double_t diffPR = meanPR - mean;
     Double_t diffMR = meanMR - mean;  
     cout << "diffPR " << bino << " = " << diffPR << endl; 
     cout << "diffMR " << bino << " = " << diffMR << endl;
     //Double_t systPR = fabs(meanPR - mean) / mean;
     //Double_t systMR = fabs(meanMR - mean) / mean; 
     Double_t systPR = (meanPR - mean) / mean;
     Double_t systMR = (meanMR - mean) / mean; 
     cout << "systPR " << bino << " = " << systPR << endl;
     cout << "systMR " << bino << " = " << systMR << endl;

     hSystematics_evol_PR -> SetBinContent(i, systPR*100);
     hSystematics_evol_MR -> SetBinContent(i, systMR*100);

   } // for
   */

}

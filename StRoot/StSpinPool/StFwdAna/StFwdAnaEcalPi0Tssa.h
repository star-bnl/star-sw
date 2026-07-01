/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  Apply selection criteria on #FcsPairCandidate and fill histograms needed to perform the Transverse Single Spin Assymetry (TSSA) analysis for neutral pion (pi^0)

  DESCRIPTION
  Loop over all the #FcsPairCandidate in #StMuFcsAnaData::mPairArr and apply a selection criteria to isolate pi0s and fill a histogram to count number of pi0s over azimuthal angle, Feynman-x (x_F), and spin information. This histogram will be used to compute the TSSA. Also, does QA on the found pi0s. Applies several cuts on #FcsPairCandidate: only FCS points are used, a z vertex cut is applied, a Z_gg cut of 0.7 is applied, a p_T cut based on the trigger p_T is applied (It is also split by FCS EM trigger). Lastly, an EPD nMIP cut is applied; the "Ph" acronym stands for "photon" and reprsents an EPD cut less than #StMuFcsAnaData::mEpdNmipCut; the "Ch" acronym stands for "charged" and represent an EPD cut greater than or equal to #StMuFcsAnaData::mEpdNmipCut; they are so named because real photons should not leave a MIP in the EPD, only a charged particle should. The pi0s that are used are ones where both #FcsPhotonCandidate have their nMIP set to less than #StMuFcsAnaData::mEpdNmipCut. It also compares the "Ph" to the "Ch" candidates. To do the TSSA analysis it is neccessary to define a signal region and a background region. This analysis will use the invariant mass of 0.1-0.2 as the signal region; two background regions exist: 0.3-0.4 and 0.7-0.9.

  LOG
  @[January 14, 2026] > First instance where relevant functionality was copied from #StMuFcsPi0TreeMaker
  @[June 8, 2026] > Changed all 'FcsPi0Candidate' to the new #FcsPairCandidate
  @[June 17, 2026] > Added histogram #mH1F_Pi0FromPh to track the distribution of #FcsPairCandidate::mFromPh. Changed #DoMake() to use new meaning of #FcsPairCandidate::mFromPh but to have the same logic as before
  @[July 1, 2026] > Changed name from StMuFcsAnaPi0Tssa to StFwdAnaEcalPi0Tssa since this analysis module relies only on Ecal and EPD information being there
*/


#ifndef STFWDANA_STFWDANAECALPI0TSSA_HH
#define STFWDANA_STFWDANAECALPI0TSSA_HH

#include "StFwdAnaVirtual.h"

class StFwdAnaEcalPi0Tssa : public StFwdAnaVirtual
{
public:
  StFwdAnaEcalPi0Tssa();
  ~StFwdAnaEcalPi0Tssa();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata);
  virtual Int_t DoMake(StFwdAnaData* anadata);
  
  //void MergeForTssa( TH1* totalhistinc[][2], TH1* totalhistbg1[][2], TH1* totalhistbg2[][2], TH3* mergedinvmass, TH1* mergedpolblue, TH1* mergedpolyell, TH1* mergedpolblueerr, TH1* mergedpolyellerr );

  void PaintAllPi0(TCanvas* canv,  const char* savename = "testallpi0.png")  const;
  void PaintNoEpdCut(TCanvas* canv,  const char* savename="testnoepdcutpi0.png")  const;
  void PaintEpdPhPi0(TCanvas* canv, const char* savename = "testepdphpi0.png") const;
  void PaintEpdChPi0(TCanvas* canv, const char* savename = "testepdchpi0.png") const;
  void PaintEpdSinglePh(TCanvas* canv, const char* savename = "testepdsingleph.png") const;
  void PaintEpdSingleCh(TCanvas* canv, const char* savename = "testepdsinglech.png") const;
  void PaintPi0Overlap(TCanvas* canv, const char* savename = "testpi0overlap.png") const;
  void PaintInvMassEpdQa(TCanvas* canv, const char* savename = "testinvmasscutqa.png") const;
  void PaintEpdQa(TCanvas* canv, const char* savename = "testepdsingleqa.png") const;
  void PaintPi0Cuts(TCanvas* canv, const char* savename = "testpi0cuts.png") const;
  void PaintInvMassCuts(TCanvas* canv, const char* savename = "testinvmasscuts.pdf") const;
  void PaintNpi0Inc(TCanvas* canv, const char* savename = "testnpi0inc.png") const;
  void PaintNpi0Bg1(TCanvas* canv, const char* savename = "testnpi0bg1.png") const;
  void PaintNpi0Bg2(TCanvas* canv, const char* savename = "testnpi0bg2.png") const;

  void PaintAllHistOneTrigger(TCanvas* canv, int trigidx, const char* savename) const;
  
  void PaintOneHistAllTrigger(TCanvas* canv, TObjArray* histarr, const char* drawoption, const char* savename) const;
  void PaintAllTrigInvMass(TCanvas* canv, const char* savename="testAllTrigInvMass.png") const{ PaintOneHistAllTrigger(canv,mH1F_InvMassAllCuts,"hist e",savename); }
  void PaintAllTrigPi0Mult(TCanvas* canv, const char* savename="testAllTrigPi0Mult.png") const{ PaintOneHistAllTrigger(canv,mH1F_Pi0MultAllCuts,"hist e",savename); }
  void PaintAllTrigxF(TCanvas* canv, const char* savename="testAllTrigxF.png") const{ PaintOneHistAllTrigger(canv,mH1F_AllCuts_xF,"hist e",savename); }
  void PaintAllTrigxFZoom(TCanvas* canv, const char* savename="testAllTrigxFZoom.png") const{ PaintOneHistAllTrigger(canv,mH1F_AllCuts_xFZoom,"hist e",savename); }
  void PaintAllTrigZgg(TCanvas* canv, const char* savename="testAllTrigZgg.png") const{ PaintOneHistAllTrigger(canv,mH1F_AllCuts_Zgg,"hist e",savename); }
  void PaintAllTrigDgg(TCanvas* canv, const char* savename="testAllTrigDgg.png") const{ PaintOneHistAllTrigger(canv,mH1F_AllCuts_Dgg,"hist e",savename); }
  void PaintAllTrigPi0En(TCanvas* canv, const char* savename="testAllTrigPi0En.png") const{ PaintOneHistAllTrigger(canv,mH1F_AllCuts_Pi0En,"hist e",savename); }
  void PaintAllTrigPi0massVen(TCanvas* canv, const char* savename="testAllTrigPi0massVen.png") const{ PaintOneHistAllTrigger(canv,mH2F_AllCuts_Pi0_massVen,"colz",savename); }
  void PaintAllTrigPi0xfVen(TCanvas* canv, const char* savename="testAllTrigPi0xfVen.png") const{ PaintOneHistAllTrigger(canv,mH2F_AllCuts_Pi0_xfVen,"colz",savename); }
  void PaintAllTrigPi0ptVeta(TCanvas* canv, const char* savename="testAllTrigPi0ptVeta.png") const{ PaintOneHistAllTrigger(canv,mH2F_AllCuts_Pi0_ptVeta,"colz",savename); }
  void PaintAllTrigPi0etaVphi(TCanvas* canv, const char* savename="testAllTrigPi0etaVphi.png") const{ PaintOneHistAllTrigger(canv,mH2F_AllCuts_Pi0_etaVphi,"colz",savename); }
  void PaintAllTrigPi0yVx(TCanvas* canv, const char* savename="testAllTrigPi0yVx.png") const{ PaintOneHistAllTrigger(canv,mH2F_AllCuts_Pi0_yVx,"colz",savename); }
  
protected:
  Double_t mEpdNmipCut = 0;             ///< Copy over from #StFwdAnaData in LoadHists

  TH1* mH1F_Pi0FromPh = 0;              ///< Pair Candidate mFromPh according to this analysis

  TH1* mH1F_AllPi0Mult = 0;             ///< pi0 multiplicity for all pairs of points
  TH1* mH1F_AllPi0Zgg = 0;              ///< Zgg of pi0 for all pairs of points
  TH1* mH2F_AllPi0_etaVphi = 0;         ///< eta vs. phi for all pairs of points
  TH1* mH1F_AllPi0En = 0;               ///< Energy for all pairs of points
  TH1* mH1F_AllPi0Pt = 0;               ///< Pt for all pairs of points
  TH1* mH1F_AllPi0Mass = 0;             ///< Invariant mass for all pairs of points

  TH1* mH1F_NoEpdCutPi0Mult = 0;        ///< Pi0 multiplicity after all cuts except Epd nMIP one
  TH1* mH1F_NoEpdCutZgg = 0;         ///< Pi0 Zgg after all cuts except Epd nMIP one
  TH1* mH2F_NoEpdCut_etaVphi = 0;       ///< Pi0 eta vs. phi after all cuts except Epd nMIP one
  TH1* mH1F_NoEpdCutEn = 0;             ///< Pi0 energy after all cuts except Epd nMIP one
  TH1* mH1F_NoEpdCutPt = 0;             ///< Pi0 pt after all cuts except Epd nMIP one
  TH1* mH1F_NoEpdCutAllMass = 0;        ///< Invariant Mass of all point pairs after all cuts except the EPD nmip cuts

  //TH1* mH1F_EpdPhInvMass = 0;           ///< Invariant mass with EPD nmip cut to isolate uncharged particles (EpdPh stands for Epd Photon, as in uncharged particle)
  //TH1* mH2F_EpdPhHeatMap = 0;           ///< x,y locations for EpdPh
  TH1* mH1F_EpdPhPi0Mult = 0;           ///< Pi0 Multiplicity for EpdPh
  TH1* mH1F_EpdPhZgg = 0;               ///< Zgg of for EpdPh
  TH1* mH2F_EpdPh_etaVphi = 0;          ///< Azimuthal angle for EpdPh
  //TH1* mH1F_EpdPhEta = 0;             ///< Psuedorapidity for EpdPh
  TH1* mH1F_EpdPhEn = 0;                ///< Energy for EpdPh
  TH1* mH1F_EpdPhPt = 0;                ///< Pt for EpdPh
  TH1* mH1F_EpdPhAllMass = 0;           ///< Invariant mass of all point pairs with EPD nmip cut to isolate uncharged particles

  //TH1* mH1F_EpdChInvMass = 0;           ///< Invariant mass with EPD nmip cut to isolate charged particles (EpdCh stands for Epd Charged, as in charged particle)
  //TH1* mH2F_EpdChHeatMap = 0;           ///< x,y locations for EpdCh
  TH1* mH1F_EpdChPi0Mult = 0;           ///< Pi0 multiplicty for EpdCh
  TH1* mH1F_EpdChZgg = 0;               ///< Zgg for EpdCh
  TH1* mH2F_EpdCh_etaVphi = 0;          ///< Psuedorapidity (eta) vs. Azimuthal (phi) angle for EpdCh
  TH1* mH1F_EpdChEn = 0;                ///< Energy for EpdCh
  TH1* mH1F_EpdChPt = 0;                ///< Pt for EpdCh
  TH1* mH1F_EpdChAllMass = 0;           ///< Invariant mass of all point pairs with EPD nmip cut to isolate charged particles

  TH1* mH1F_EpdSinglePhPi0Mult = 0;     ///< Pi0 multiplicty for all point pairs that pass a single photon requirement on Epd
  TH1* mH1F_EpdSinglePhZgg = 0;         ///< Zgg for all point pairs that pass a single photon requirment on Epd
  TH1* mH2F_EpdSinglePh_etaVphi = 0;    ///< eta V phi for all point pairs that pass a single photon requirement on Epd
  TH1* mH1F_EpdSinglePhEn = 0;          ///< Energy for all point pairs that pass a single photon requirement on Epd
  TH1* mH1F_EpdSinglePhPt = 0;          ///< Pt for all point pairs that pass a single photon requirement on Epd
  TH1* mH1F_EpdSinglePhAllMass = 0;     ///< Invariant mass of all point pairs with EPD nmip cut on a single photon that passed the photon level cut

  TH1* mH1F_EpdSingleChPi0Mult = 0;     ///< Pi0 multiplicty for all point pairs that pass a single electron requirement on Epd
  TH1* mH1F_EpdSingleChZgg = 0;         ///< Zgg for all point pairs that pass a single electron requirment on Epd
  TH1* mH2F_EpdSingleCh_etaVphi = 0;    ///< eta V phi for all point pairs that pass a single electron requirement on Epd
  TH1* mH1F_EpdSingleChEn = 0;          ///< Energy for all point pairs that pass a single electron requirement on Epd
  TH1* mH1F_EpdSingleChPt = 0;          ///< Pt for all point pairs that pass a single electron requirement on Epd
  TH1* mH1F_EpdSingleChAllMass = 0;     ///< Invariant mass of all point pairs with EPD nmip cut on a single electron that passed the electron level cut
  
  TObjArray* mH1F_InvMassAllCuts = 0;              ///< Invariant Mass of all potential pi0s after all cuts applied and the "neutral" particle criteria for EPD nmip, one for every EM trigger
  TObjArray* mH1F_Pi0MultAllCuts = 0;              ///< Number of "good pi0s" i.e. number of potential pi0s after all cuts applied
  //(TH1* mH1F_AllPi0_xF = 0;             ///< Feynman-x (xF) of all pi0s
  //TH1* mH1F_NFoundPhiBin = 0;           ///< Number of valid phi bins found. This is a cross check to make sure that I am not double counting pi0s and finding more than one valid bin when I loop over the phi bins
  TObjArray* mH1F_AllCuts_xF = 0;               ///< Feynman-x (xF) of the pi0s that pass all the cuts
  TObjArray* mH1F_AllCuts_xFZoom = 0;           ///< Feynman-x (xF) of the pi0s that pass all the cuts on a smaller scale and more bining
  TObjArray* mH1F_AllCuts_Zgg = 0;              ///< Zgg after all cuts
  TObjArray* mH1F_AllCuts_Dgg = 0;              ///< Dgg after all cuts
  TObjArray* mH1F_AllCuts_Pi0En = 0;            ///< Pi0 energy distribution with all cuts
  TObjArray* mH2F_AllCuts_Pi0_massVen = 0;      ///< Pi0 Invariant mass vs. energy with all cuts
  TObjArray* mH2F_AllCuts_Pi0_xfVen = 0;        ///< Pi0 xF vs. Energy
  TObjArray* mH2F_AllCuts_Pi0_ptVeta = 0;       ///< Pi0 p_T vs. psuedorapidity (eta)
  //TH1* mH1F_AllCuts_Pi0Phi = 0;         ///< Pi0 phi distribution with all cuts
  TObjArray* mH2F_AllCuts_Pi0_etaVphi = 0;      ///< Pi0 eta vs. phi distributions, phi binning matches #NPHIBIN after all cuts
  //TH1* mH2F_AllCuts_Poi_yVx = 0;        ///< Point y vs. x distributions after all cuts this gets complicated because you have two point
  TObjArray* mH2F_AllCuts_Pi0_yVx = 0;          ///< Pi0 FCS projected y vs. x distributions after all cuts
  //TH1* mH1F_InvMassAllCutsByEnByPhi[NENERGYBIN][NPHIBIN]; ///< Invariant mass of reconstructed pions using all cuts by energy and phi bin
  //TH1* mH1F_NPi0ByEnByPhi[NENERGYBIN][NPHIBIN];  ///< Number of pions in a given energy and phi bin. The hisotram represents the split by spin state (0-10, 20-40, 40-60, 80-100, 100+)
  TH1* mH3F_AllCutsInvMass_xfVphi = 0;      ///< Pi0 invariant mass after all cuts vs. xf vs. phi used for the fits to estimate background
  TH1* mH2F_NPi0Inc_xfVphi[2][2];           ///< x_F and phi where pi0 candidate was found [blue,yellow][up,down] for invariant mass range of 0.1-0.2
  TH1* mH2F_NPi0Bg1_xfVphi[2][2];           ///< x_F and phi where pi0 candidate was found [blue,yellow][up,down] for invariant mass range of 0.3-0.4
  TH1* mH2F_NPi0Bg2_xfVphi[2][2];           ///< x_F and phi where pi0 candidate was found [blue,yellow][up,down] for invariant mass range of 0.7-0.9
  //Add mass vs. energy hisotgram??
  
  ClassDef(StFwdAnaEcalPi0Tssa,1)
};

#endif


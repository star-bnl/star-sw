#include "WBosEvent.h"
#include "WBosMcEvent.h"

#include "utils/utils.h"

ClassImp(WBosEvent)

using namespace std;


bool WBosEvent::sUseOtherSolution = false;
const float WBosEvent::sMinElectronPtLight = 15;
const float WBosEvent::sMinElectronPtHard  = 25;
const float WBosEvent::sMinNeutrinoPt      = 18;  // In Justin's Note was set at 15 GeV 
const float WBosEvent::sMinRecoilPt        = 0.5; // Minimum P_T of the recoil before correction
const float WBosEvent::sMaxRecoilPt        = 40;  // Maximum P_T of the recoil before correction

//FILE  *fAnEvol  = fopen("/direct/star+u/fazio/vbasym/macros/curves/ZK_evolution_Wp_asymmetry.txt","r");


WBosEvent::WBosEvent(float minTrackPt, int RhicRunId, bool otherSolution) : VecBosEvent()
   , mWBosMass(80.385)
   , mElectronP3()
   , mNeutrinoP3()
   , mNeutrinoP3Other()
{
   VecBosEvent::sMinRecoilTrackPt = minTrackPt;
   VecBosEvent::sRhicRunId         = RhicRunId;
   sUseOtherSolution = otherSolution;
   //cout << "sMinRecoilTrackPt: " <<  sMinRecoilTrackPt << endl;
   //cout << "WBosEvent: otherSolution: " << otherSolution << endl;
   //cout << "WBosEvent: sUseOtherSolution: " << sUseOtherSolution << endl;
}


VecBosTrack& WBosEvent::GetElectronTrack() const
{
   return *(*mTracksCandidate.begin());
}


TVector3 WBosEvent::GetElectronP3() const { return mElectronP3; }
TVector3 WBosEvent::GetNeutrinoP3() const { return mNeutrinoP3; }
TVector3 WBosEvent::GetNeutrinoP3Other() const { return mNeutrinoP3Other; }


TVector3 WBosEvent::CalcMissingEnergyP3() const
{
   //return -1*(mP3TrackRecoilTpcNeutrals + mElectronP3);
   return -1*(mP3TrackRecoilTpcNeutralsCorrected + mElectronP3);

   // Other definitions
   //return -1*(mP3TrackRecoilTow + mElectronP3);
   //return -1*(mP4JetRecoil + mElectronP3);
}


double WBosEvent::CalcSignedPtBalance() const
{
   return mPtBalanceCosPhiFromTracks;
}


TVector3 WBosEvent::GetVecBosonP3FirstSolution() const { return mElectronP3 + mNeutrinoP3; }
TVector3 WBosEvent::GetVecBosonP3OtherSolution() const { return mElectronP3 + mNeutrinoP3Other; }
TVector3 WBosEvent::GetVecBosonP3() const { return mElectronP3 + (sUseOtherSolution ? mNeutrinoP3Other : mNeutrinoP3); }
/*
TVector3 WBosEvent::GetVecBosonP3() const
{
  TVector3  wBosonP3; 
  wBosonP3 = mElectronP3 + mNeutrinoP3;
  if ( abs(wBosonP3.Pz()) > 40 ) wBosonP3 = mElectronP3 + mNeutrinoP3Other;
  return wBosonP3;
}
*/

TLorentzVector WBosEvent::GetVecBosonP4() const
{
   TLorentzVector wBosonP4;
   wBosonP4.SetVectM(GetVecBosonP3(), mWBosMass);
   return wBosonP4;
}

TLorentzVector WBosEvent::GetVecBosonP4OtherSolution() const
{
   TLorentzVector wBosonP4OtherSolution;
   wBosonP4OtherSolution.SetVectM(GetVecBosonP3OtherSolution(), mWBosMass);
   return wBosonP4OtherSolution;
}

/**
 * The primary method to identify and reconstruct the event with a W boson.
 */
void WBosEvent::Process()
{
   VecBosEvent::Process();
   ProcessPersistent();
   //PredictionAnEvol();
}


void WBosEvent::ProcessPersistent()
{
   VecBosEvent::ProcessPersistent();

   // Proceed only if this is a W event, i.e. it conforms to W event signature
   //if ( !PassedCutWBos() ) return;
   if ( !HasCandidateEle() ) return;

      mElectronP3 = GetElectronTrack().GetP3EScaled();
      mNeutrinoP3 = CalcMissingEnergyP3(); // here we set only x and y components, and reconstruct the z one later
      mNeutrinoP3Other = mNeutrinoP3;

      ReconstructNeutrinoZ();
}


void WBosEvent::ProcessMC(int McType)
{
   cout << "Monte Carlo Type (1-> W+, 2-> W-, 3-> other) = "  << McType  << endl;

   fIsMc = true;
   StMcEvent *stMcEvent = (StMcEvent *) StMaker::GetChain()->GetDataSet("StMcEvent");
   assert(stMcEvent);

   mMcEvent = new WBosMcEvent();
   ((WBosMcEvent*) mMcEvent)->CalcRecoil(*stMcEvent);

   if (McType == 1 || McType == 2) {  // do it only for W+ and W- MCs
      PredictionAnEvol(McType);
      PredictionAn(McType);  
   }
}


void WBosEvent::Clear(const Option_t* opt)
{
   VecBosEvent::Clear();
   mElectronP3.SetXYZ(0, 0, 0);
   mNeutrinoP3.SetXYZ(0, 0, 0);
   mNeutrinoP3Other.SetXYZ(0, 0, 0);
}


void WBosEvent::Print(const Option_t* opt) const
{
   Info("Print", ":");
   VecBosEvent::Print(opt);
   cout << "sUseOtherSolution: " << sUseOtherSolution << endl;
   cout << "mElectronP3: " << endl;
   mElectronP3.Print();
   cout << "mNeutrinoP3: " << endl;
   mNeutrinoP3.Print();
   cout << "mNeutrinoP3Other: " << endl;
   mNeutrinoP3Other.Print();
}


bool WBosEvent::PassedCutWBos(float minElePt) const
{
   //cout << "sMinRecoilPt: " << sMinRecoilPt << endl;
   if ( HasCandidateEle() &&
        mP3TrackRecoilTpcNeutrals.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutrals.Pt() < sMaxRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() < sMaxRecoilPt &&
        mPtBalanceCosPhiFromTracks >= sMinNeutrinoPt &&
        //mNeutrinoP3.Pt() >= sMinNeutrinoPt &&
        mElectronP3.Pt() >= minElePt &&
        fabs(mElectronP3.Eta()) <= 1.0 )
   {
      return true;
   }

   return false;
}


bool WBosEvent::PassedCutWBosNoEndcap(float minElePt) const
{
   if ( HasCandidateEleNoETOW() &&
        mP3TrackRecoilTpcNeutrals.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutrals.Pt() < sMaxRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() < sMaxRecoilPt &&
        mPtBalanceCosPhiFromTracks >= sMinNeutrinoPt &&
        mElectronP3.Pt() >= minElePt &&
        fabs(mElectronP3.Eta()) <= 1.0 )
   {
      return true;
   }

   return false;
}


bool WBosEvent::PassedCutWBosPlus(float minElePt) const
{
   return PassedCutWBos(minElePt) && GetElectronTrack().GetChargeSign() > 0;
}


bool WBosEvent::PassedCutWBosPlusNoEndcap(float minElePt) const
{
   return PassedCutWBosNoEndcap(minElePt) && GetElectronTrack().GetChargeSign() > 0;
}


bool WBosEvent::PassedCutWBosPlusPl(float minElePt) const
{
  return PassedCutWBosPlus(minElePt) && fabs(GetVecBosonP3().Pz()) < 50;
}


bool WBosEvent::PassedCutWBosMinus(float minElePt) const
{
   return PassedCutWBos(minElePt) && GetElectronTrack().GetChargeSign() < 0;
}


bool WBosEvent::PassedCutWBosMinusNoEndcap(float minElePt) const
{
   return PassedCutWBosNoEndcap(minElePt) && GetElectronTrack().GetChargeSign() < 0;
}


bool WBosEvent::PassedCutWBosMinusPl(float minElePt) const
{
  return PassedCutWBosMinus(minElePt) && fabs(GetVecBosonP3().Pz()) < 50;
}


bool WBosEvent::PassedCutWPlusAn(float minElePt) const
{
   return PassedCutWBosPlus(minElePt) && GetVecBosonP3().Pt() <= 7;
}

bool WBosEvent::PassedCutWMinusAn(float minElePt) const
{
   return PassedCutWBosMinus(minElePt) && GetVecBosonP3().Pt() <= 7;
}


bool WBosEvent::PassedCutQcdBkg(float minElePt) const
{
  //if ( HasCandidateEle() && mPtBalanceCosPhiFromTracks < sMinNeutrinoPt &&
  // S.F. Feb. 17, 2015 - To study the QCD background we set the Pt balance cut back to 15. as in Justin's note, this will select a region dominated by QCD events
   if ( HasCandidateEle() && 
        mP3TrackRecoilTpcNeutrals.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() > sMinRecoilPt &&
        mP3TrackRecoilTpcNeutrals.Pt() < sMaxRecoilPt &&
        mP3TrackRecoilTpcNeutralsCorrected.Pt() < sMaxRecoilPt &&
        mPtBalanceCosPhiFromTracks < 15. &&   // S.F. Feb. 17, 2015 - Pt imblance cut set to 15
        mElectronP3.Pt() >= minElePt &&
        fabs(mElectronP3.Eta()) <= 1.0 )
   {
      return true;
   }

   return false;
}

bool WBosEvent::PassedCutQcdBkgPlus(float minElePt) const
{
   return PassedCutQcdBkg(minElePt) && GetElectronTrack().GetChargeSign() > 0;
}

bool WBosEvent::PassedCutQcdBkgMinus(float minElePt) const
{
   return PassedCutQcdBkg(minElePt) && GetElectronTrack().GetChargeSign() < 0;
}

void WBosEvent::ReconstructNeutrinoZ()
{
   double A = mWBosMass*mWBosMass/2 + mElectronP3.Px() * mNeutrinoP3.Px() + mElectronP3.Py() * mNeutrinoP3.Py();
   double a = mElectronP3.Pt() * mElectronP3.Pt();
   double b = -2 * A * mElectronP3.Pz();
   double c = mNeutrinoP3.Pt() * mNeutrinoP3.Pt() * mElectronP3.Mag() * mElectronP3.Mag() - A*A;

   double d = b*b - 4*a*c;
   if (d < 0 ) {
      Warning("ReconstructNeutrinoZ", "Problem reconstructing neutrino, d=%f will be set to 0", d);
      Info("ReconstructNeutrinoZ", "mElectronP3.Pt: %f, mElectronP3.Pz: %f, mNeutrinoP3.Pt: %f, A: %f", mElectronP3.Pt(), mElectronP3.Pz(), mNeutrinoP3.Pt(), A);
      d = 0;
   }
   double p_nu_z1  = (-b + sqrt(d) ) / 2 / a;
   double p_nu_z2  = (-b - sqrt(d) ) / 2 / a;

   if (fabs(p_nu_z1) < fabs(p_nu_z2)) {
      mNeutrinoP3.SetZ(p_nu_z1);
      mNeutrinoP3Other.SetZ(p_nu_z2);
   } else {
      mNeutrinoP3.SetZ(p_nu_z2);
      mNeutrinoP3Other.SetZ(p_nu_z1);
   }
}


void WBosEvent::PredictionAnEvol(int McType)
{

  // This function read the TNtuple file created (using the macro: PtCorrPlot.C in macros directory) from the ASCII text file provided by Zhongbo Kang and containing the predictions for W+ An asymmetry when evolution is included.
 
  TString inPathCurves = "/star/institutions/bnl_me/fazio/zk_an_predictions/";
  TFile *fileAnEvol;

  if (McType == 1) {           // this file is for W+ Monte Carlo:

     delete gROOT->GetListOfFiles()->FindObject(inPathCurves + "ZK_evolution_Wp_asymmetry.root"); // clear memory of file
     fileAnEvol = TFile::Open(inPathCurves + "ZK_evolution_Wp_asymmetry.root");
     cout << " Open File: ZK_evolution_Wp_asymmetry.root" << endl; 

  } else  if (McType == 2) {   // this file is for W- Monte Carlo:

     delete gROOT->GetListOfFiles()->FindObject(inPathCurves + "ZK_evolution_Wm_asymmetry.root"); // clear memory of file
     fileAnEvol = TFile::Open(inPathCurves + "ZK_evolution_Wm_asymmetry.root");
     cout << " Open File: ZK_evolution_Wm_asymmetry.root" << endl; 
  }

   //cout << " inPathCurves = " << inPathCurves << endl; 
 
   // get a pointer to the tree
   TTree *tAnEvol = (TTree *)fileAnEvol->Get("ntAnEvol");


   // define the width of the y,pt binning in the original file/tree
   float delta_y  = 0.1;
   float delta_pt = 0.5;

   //std::unique_ptr<WBosMcEvent> mWMcEvent(new WBosMcEvent());
   Float_t PtGen       = ((WBosMcEvent*) mMcEvent) -> mP4WBoson.Pt();
   Float_t RapidityGen = ((WBosMcEvent*) mMcEvent) -> mP4WBoson.Rapidity();

   cout << "W Pt Generated = "       << PtGen       << " GeV" << endl;
   cout << "W Rapidity Generated = " << RapidityGen << " GeV" << endl;

   Float_t y_min    = RapidityGen - (delta_y/2);
   Float_t y_max    = RapidityGen + (delta_y/2);
   Float_t pt_min   = PtGen - (delta_pt/2);
   Float_t pt_max   = PtGen + (delta_pt/2);
   //cout << "y_min = "  << y_min  << endl;
   //cout << "y_max = "  << y_max  << endl;
   //cout << "pt_min = " << pt_min << endl;
   //cout << "pt_max = " << pt_max << endl;

   std::ostringstream ssymin;
        ssymin << y_min;
   std::string symin(ssymin.str());
   std::ostringstream ssymax;
        ssymax << y_max;
   std::string symax(ssymax.str());
   std::ostringstream ssptmin;
        ssptmin << pt_min;
   std::string sptmin(ssptmin.str());
   std::ostringstream ssptmax;
        ssptmax << pt_max;
   std::string sptmax(ssptmax.str());

   symin = "y >= " + symin;
   //printf("string symin: %s \n", symin.c_str());
   symax = "y < " + symax;
   //printf("string symax: %s \n", symax.c_str());
   sptmin = "pt >= " + sptmin;
   sptmax = "pt < " + sptmax;

   TString tsymin = symin;
   //printf("TString tsymin: %s \n", tsymin.Data());
   TString tsymax = symax;
   //printf("TString tsymax: %s \n", tsymax.Data());
   TString tsptmin = sptmin;
   //printf("TString tsptmin: %s \n", tsptmin.Data());
   TString tsptmax = sptmax;
   //printf("TString tsptmax: %s \n", tsptmax.Data());

   TCut cymin(tsymin.Data());
   TCut cymax(tsymax.Data());
   TCut cptmin(tsptmin.Data());
   TCut cptmax(tsptmax.Data());

   TH1F *h1 = new TH1F("h1","",100,-1,1); 

   tAnEvol->Draw("an >> h1", cymin && cymax && cptmin && cptmax);

   //tAnEvol->Draw("an >> h1", "y>= 1.35 && y < 1.45 && pt == 5.000"); //test

   Double_t entries=h1->GetEntries();

   if (PtGen < ( 0.5-(delta_pt/2)) || PtGen >= (15+(delta_pt/2)) ) { 
       printf("WARNING! Generated Pt out of range of Zhongbo's An prediction [1.;15.] GeV -> PtGen= %f \n", PtGen);
       An_evol_ZK = -999;
       printf("Value of A_N prediction set to An_evol_ZK= %f \n", An_evol_ZK);
   }

   else if (RapidityGen < (-1.8-(delta_y/2)) || RapidityGen >= (1.8+(delta_y/2)) ) { 
       printf("WARNING! Generated rapidity out of range of Zhongbo's An prediction [-1.8;1.8] GeV -> RapidityGen= %f \n", RapidityGen);
       An_evol_ZK = -999;
       printf("Value of A_N prediction set to An_evol_ZK= %f \n", An_evol_ZK);
   }

   else if (entries != 1 ) { 
       printf("WARNING! Entires in Zhongbo's An prediction histogram > 1 -> #entries= %f \n", entries);
       An_evol_ZK = -999;
       printf("Value of A_N prediction set to An_evol_ZK= %f \n", An_evol_ZK);
   }

   else {
       Double_t mean=h1->GetMean();
       //cout << "Entries in the A_N istogram = " << entries << endl; 
       cout << "A_N Prediction (with Evol.) = " << mean << endl;

       An_evol_ZK = mean; 
   }
  
   delete h1;

}

void WBosEvent::PredictionAn(int McType)
{

   // This function read the TNtuple file created (using the macro: PtCorrPlot.C in macros directory) from the ASCII text file provided by Zhongbo Kang and containing the predictions for W+ An asymmetry without evolution is included.
 
   TString inPathCurves = "/star/institutions/bnl_me/fazio/zk_an_predictions/";
   TFile *fileAnNoEvo;

  if (McType == 1) {           // this file is for W+ Monte Carlo:
  
     delete gROOT->GetListOfFiles()->FindObject(inPathCurves + "ZK_noevo_Wp_asymmetry.root"); // clear memory of file
     fileAnNoEvo = TFile::Open(inPathCurves + "ZK_noevo_Wp_asymmetry.root");
     cout << " Open File: ZK_noevo_Wp_asymmetry.root" << endl; 

  } else  if (McType == 2) {   // this file is for W- Monte Carlo:

     delete gROOT->GetListOfFiles()->FindObject(inPathCurves + "ZK_noevo_Wm_asymmetry.root"); // clear memory of file
     fileAnNoEvo = TFile::Open(inPathCurves + "ZK_noevo_Wm_asymmetry.root");
     cout << " Open File: ZK_noevo_Wm_asymmetry.root" << endl; 
  }

   //cout << " inPathCurves = " << inPathCurves << endl; 
 
   // get a pointer to the tree
   TTree *tAnNoEvo = (TTree *)fileAnNoEvo->Get("ntAnEvol");


   // define the width of the y,pt binning in the original file/tree
   float delta_y  = 0.1;
   float delta_pt = 0.5;

   //std::unique_ptr<WBosMcEvent> mWMcEvent(new WBosMcEvent());
   Float_t PtGen       = ((WBosMcEvent*) mMcEvent) -> mP4WBoson.Pt();
   Float_t RapidityGen = ((WBosMcEvent*) mMcEvent) ->mP4WBoson.Rapidity();

   //cout << "W Pt Generated = "       << PtGen       << " GeV" << endl;
   //cout << "W Rapidity Generated = " << RapidityGen << " GeV" << endl;

   Float_t y_min    = RapidityGen - (delta_y/2);
   Float_t y_max    = RapidityGen + (delta_y/2);
   Float_t pt_min   = PtGen - (delta_pt/2);
   Float_t pt_max   = PtGen + (delta_pt/2);
   //cout << "y_min = "  << y_min  << endl;
   //cout << "y_max = "  << y_max  << endl;
   //cout << "pt_min = " << pt_min << endl;
   //cout << "pt_max = " << pt_max << endl;

   std::ostringstream ssymin;
        ssymin << y_min;
   std::string symin(ssymin.str());
   std::ostringstream ssymax;
        ssymax << y_max;
   std::string symax(ssymax.str());
   std::ostringstream ssptmin;
        ssptmin << pt_min;
   std::string sptmin(ssptmin.str());
   std::ostringstream ssptmax;
        ssptmax << pt_max;
   std::string sptmax(ssptmax.str());

   symin = "y >= " + symin;
   //printf("string symin: %s \n", symin.c_str());
   symax = "y < " + symax;
   //printf("string symax: %s \n", symax.c_str());
   sptmin = "pt >= " + sptmin;
   sptmax = "pt < " + sptmax;

   TString tsymin = symin;
   //printf("TString tsymin: %s \n", tsymin.Data());
   TString tsymax = symax;
   //printf("TString tsymax: %s \n", tsymax.Data());
   TString tsptmin = sptmin;
   //printf("TString tsptmin: %s \n", tsptmin.Data());
   TString tsptmax = sptmax;
   //printf("TString tsptmax: %s \n", tsptmax.Data());

   TCut cymin(tsymin.Data());
   TCut cymax(tsymax.Data());
   TCut cptmin(tsptmin.Data());
   TCut cptmax(tsptmax.Data());

   TH1F *h1 = new TH1F("h1","",100,-1,1); 

   tAnNoEvo->Draw("an >> h1", cymin && cymax && cptmin && cptmax);

   Double_t entries=h1->GetEntries();

   if (PtGen < ( 0.5-(delta_pt/2)) || PtGen >= (15+(delta_pt/2)) ) { 
     //printf("WARNING! Generated Pt out of range of Zhongbo's An prediction [1.;15.] GeV -> PtGen= %f \n", PtGen);
       An_noevo_ZK = -999;
       printf("Value of A_N prediction set to An_noevo_ZK= %f \n", An_noevo_ZK);
   }

   else if (RapidityGen < (-1.8-(delta_y/2)) || RapidityGen >= (1.8+(delta_y/2)) ) { 
     //printf("WARNING! Generated rapidity out of range of Zhongbo's (No evol.) An prediction [-1.8;1.8] GeV -> RapidityGen= %f \n", RapidityGen);
       An_noevo_ZK = -999;
       printf("Value of A_N prediction set to An_noevo_ZK= %f \n", An_noevo_ZK);
   }

   else if (entries != 1 ) { 
     //printf("WARNING! Entires in Zhongbo's An prediction (No evol.) histogram > 1 -> #entries= %f \n", entries);
       An_noevo_ZK = -999;
       printf("Value of A_N prediction set to An_noevo_ZK= %f \n", An_noevo_ZK);
   }

   else {
       Double_t mean=h1->GetMean();
       //cout << "Entries in the A_N istogram = " << entries << endl; 
       cout << "A_N Prediction (No evol.) = " << mean << endl;

       An_noevo_ZK = mean; 
   }
  
   delete h1;

}

void WBosEvent::Streamer(TBuffer &R__b)
{
   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(WBosEvent::Class(), this);
   }
   else {
      R__b.WriteClassBuffer(WBosEvent::Class(), this);
   }
}

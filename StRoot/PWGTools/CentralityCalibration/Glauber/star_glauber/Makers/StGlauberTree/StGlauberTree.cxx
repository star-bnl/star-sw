//====================================================================================================
// $Id: StGlauberTree.cxx,v 1.2 2012/04/25 04:45:26 hmasui Exp $
// $Log: StGlauberTree.cxx,v $
// Revision 1.2  2012/04/25 04:45:26  hmasui
// Expand branches for eccentricity in the main tree, and deformation parameters in header
//
//====================================================================================================

#include <assert.h>

#include "TError.h"
#include "TFile.h"
#include "TMath.h"
#include "TTree.h"
#include "StMessMgr.h"
#include "StGlauberTree.h"

ClassImp(StGlauberTree)

//____________________________________________________________________________________________________
// Default constructor
StGlauberTree::StGlauberTree(const UInt_t mode)
  : mMode(mode)
{
  switch ( mMode ){
    case 0: LOG_INFO << "StGlauberTree  read  mode" << endm; break ;
    case 1: LOG_INFO << "StGlauberTree  write mode" << endm; break ;
    default:
     Error("StGlauberTree", "Mode should 0(read) or 1(write). abort");
     assert(0);
  }

  mFile = 0 ;
  mTree = 0 ;

  Clear() ;

  // Initialize header stuffs only once
  sprintf(mNameNucleusA, "%s", "") ;
  sprintf(mNameNucleusB, "%s", "") ;
  mMassNumberA       = 0 ;
  mMassNumberB       = 0 ;
  mRadiusA           = -9999. ;
  mRadiusA           = -9999. ;
  mSkinDepthA        = -9999. ;
  mSkinDepthB        = -9999. ;
  mBeta2A            = -9999. ;
  mBeta4A            = -9999. ;
  mBeta2B            = -9999. ;
  mBeta4B            = -9999. ;
  mSigmaNN           = -9999. ;
  mSqrtSNN           = -9999. ;
  mRepulsionD        = -9999. ;
  mTotalXsec         = -9999. ;
  mTotalXsecError    = 0.0 ;
  mSmearHardCore     = 0 ;
  mSmearGaussian     = 0 ;
  mCollisionHardCore = 0 ;
  mCollisionGaussian = 0 ;
  mBMax              = -9999. ;
  mNeventsAccept     = 0 ;
  mNeventsThrow      = 0 ;
  mNpp               = -9999. ;
  mK                 = -9999. ;
  mX                 = -9999. ;
  mEfficiency        = -9999. ;
  mIsConstEfficiency = 0 ;
  mVersion           = 0 ;
}

//____________________________________________________________________________________________________
// Default destructor
StGlauberTree::~StGlauberTree()
{
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::Clear()
{
  /// Clear data members in tree
  mB = -9999. ;
  mNpart = 0 ;
  mNcoll = 0 ;
  mMultiplicity = 0 ;

  for(UInt_t i=0;i<2;i++){
    mTheta[i] = 0.0 ;
    mPhi[i]   = 0.0 ;
  }

  for(UInt_t i=0;i<4;i++){
    mSumX[i] = 0.0 ;
    mSumY[i] = 0.0 ;
    mSumX2[i] = 0.0 ;
    mSumY2[i] = 0.0 ;
    mSumXY[i] = 0.0 ;
    mEccRP2[i] = -9999. ;
    mEccPP2[i] = -9999. ;
    mEccPP3[i] = -9999. ;
    mEccPP4[i] = -9999. ;
    mPP2[i] = -9999. ;
    mPP3[i] = -9999. ;
    mPP4[i] = -9999. ;
  }

  return 1 ;
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::InitBranch()
{
  // event-wise tree
  mTree->SetMakeClass(1);
  mTree->SetBranchAddress("b", &mB, &b_b);
  mTree->SetBranchAddress("npart", &mNpart, &b_npart);
  mTree->SetBranchAddress("ncoll", &mNcoll, &b_ncoll);
  mTree->SetBranchAddress("mult", &mMultiplicity, &b_mult);
  mTree->SetBranchAddress("theta", mTheta, &b_theta);
  mTree->SetBranchAddress("phi", mPhi, &b_phi);
  mTree->SetBranchAddress("sumx", mSumX, &b_sumx);
  mTree->SetBranchAddress("sumy", mSumY, &b_sumy);
  mTree->SetBranchAddress("sumx2", mSumX2, &b_sumx2);
  mTree->SetBranchAddress("sumy2", mSumY2, &b_sumy2);
  mTree->SetBranchAddress("sumxy", mSumXY, &b_sumxy);
  mTree->SetBranchAddress("eccrp2", mEccRP2, &b_eccrp2);
  mTree->SetBranchAddress("eccpp2", mEccPP2, &b_eccpp2);
  mTree->SetBranchAddress("eccpp3", mEccPP3, &b_eccpp3);
  mTree->SetBranchAddress("eccpp4", mEccPP4, &b_eccpp4);
  mTree->SetBranchAddress("pp2", mPP2, &b_pp2);
  mTree->SetBranchAddress("pp3", mPP3, &b_pp3);
  mTree->SetBranchAddress("pp4", mPP4, &b_pp4);

  // header
  mHeader->SetMakeClass(1);
  mHeader->SetBranchAddress("nameA", mNameNucleusA, &b_nameA);
  mHeader->SetBranchAddress("nameB", mNameNucleusB, &b_nameB);
  mHeader->SetBranchAddress("massNumberA", &mMassNumberA, &b_massNumberA);
  mHeader->SetBranchAddress("massNumberB", &mMassNumberB, &b_massNumberB);
  mHeader->SetBranchAddress("radiusA", &mRadiusA, &b_radiusA);
  mHeader->SetBranchAddress("radiusB", &mRadiusB, &b_radiusB);
  mHeader->SetBranchAddress("skinDepthA", &mSkinDepthA, &b_skinDepthA);
  mHeader->SetBranchAddress("skinDepthB", &mSkinDepthB, &b_skinDepthB);
  mHeader->SetBranchAddress("beta2A", &mBeta2A, &b_beta2A);
  mHeader->SetBranchAddress("beta4A", &mBeta4A, &b_beta4A);
  mHeader->SetBranchAddress("beta2B", &mBeta2B, &b_beta2B);
  mHeader->SetBranchAddress("beta4B", &mBeta4B, &b_beta4B);
  mHeader->SetBranchAddress("sigmaNN", &mSigmaNN, &b_sigmaNN);
  mHeader->SetBranchAddress("sqrtSNN", &mSqrtSNN, &b_sqrtSNN);
  mHeader->SetBranchAddress("repulsionD", &mRepulsionD, &b_repulsionD);
  mHeader->SetBranchAddress("totalXsec", &mTotalXsec, &b_totalXsec);
  mHeader->SetBranchAddress("totalXsecError", &mTotalXsecError, &b_totalXsecError);
  mHeader->SetBranchAddress("smearHardCore", &mSmearHardCore, &b_smearHardCore);
  mHeader->SetBranchAddress("smearGaussian", &mSmearGaussian, &b_smearGaussian);
  mHeader->SetBranchAddress("collisionHardCore", &mCollisionHardCore, &b_collisionHardCore);
  mHeader->SetBranchAddress("collisionGaussian", &mCollisionGaussian, &b_collisionGaussian);
  mHeader->SetBranchAddress("maxB", &mBMax, &b_maxB);
  mHeader->SetBranchAddress("neventsAccept", &mNeventsAccept, &b_neventsAccept);
  mHeader->SetBranchAddress("neventsThrow", &mNeventsThrow, &b_neventsThrow);
  mHeader->SetBranchAddress("npp", &mNpp, &b_npp);
  mHeader->SetBranchAddress("k", &mK, &b_k);
  mHeader->SetBranchAddress("x", &mX, &b_x);
  mHeader->SetBranchAddress("efficiency", &mEfficiency, &b_efficiency);
  mHeader->SetBranchAddress("isConstEfficiency", &mIsConstEfficiency, &b_isConstEfficiency);
  mHeader->SetBranchAddress("version", &mVersion, &b_version);

  return 1;
}


//____________________________________________________________________________________________________
Int_t StGlauberTree::Open(const TString filename)
{
  if(mMode==0){
    // Read mode
    mFile = TFile::Open(filename);
    if(!mFile || !mFile->IsOpen()){
      Error("StGlauberTree::Open", "can't open %s", filename.Data());
      assert(0);
    }
    LOG_INFO << "StGlauberTree::Open  Open input ROOT file: " << mFile->GetName() << endm;

    // Get tree
    mTree = (TTree*) mFile->Get("tree");
    if(!mTree){
      Error("StGlauberTree::Open", "No tree found in %s", filename.Data());
      assert(0);
    }

    mHeader = (TTree*) mFile->Get("header");
    if(!mHeader){
      Error("StGlauberTree::Open", "No header found in %s", filename.Data());
      assert(0);
    }

    InitBranch() ;
  }
  else{
    // Make sure mFile/mTree/mHeader are NULL
    if(mFile)   delete mFile ;
    if(mTree)   delete mTree ;
    if(mHeader) delete mHeader ;

    // Write mode
    mFile = TFile::Open(filename, "recreate", "", 9); // max compression
    if(!mFile || !mFile->IsOpen()){
      Error("StGlauberTree::Open", "can't open %s", filename.Data());
      assert(0);
    }
    LOG_INFO << "StGlauberTree::Open  Open output ROOT file: " << mFile->GetName() << endm;

    // Initialization of tree
    LOG_INFO << "StGlauberTree::Open  Init tree" << endm;
    mTree = new TTree("tree", "Event-wise information for Fast MC glauber tree");
    mTree->Branch("b",      &mB,             "b/D")           ; // Impact parameter
    mTree->Branch("npart",  &mNpart,         "npart/i")       ; // Number of participants
    mTree->Branch("ncoll",  &mNcoll,         "ncoll/i")       ; // Number of collicipants
    mTree->Branch("mult",   &mMultiplicity,  "mult/i")        ; // Multiplicity
    mTree->Branch("theta",   mTheta,         "theta[2]/D")    ; // Polar angle rotation for deformed nuclei
    mTree->Branch("phi",     mPhi,           "phi[2]/D")      ; // Azimuthal angle rotation for deformed nuclei
    mTree->Branch("sumx",    mSumX,          "sumx[4]/D")     ; // <x>
    mTree->Branch("sumy",    mSumY,          "sumy[4]/D")     ; // <y>
    mTree->Branch("sumx2",   mSumX2,         "sumx2[4]/D")    ; // <x^2>
    mTree->Branch("sumy2",   mSumY2,         "sumy2[4]/D")    ; // <y^2>
    mTree->Branch("sumxy",   mSumXY,         "sumxy[4]/D")    ; // <xy>
    mTree->Branch("eccrp2",  mEccRP2,        "eccrp2[4]/D")   ; // 2nd order Reaction plane eccentricity
    mTree->Branch("eccpp2",  mEccPP2,        "eccpp2[4]/D")   ; // 2nd order Participant plane eccentricity
    mTree->Branch("eccpp3",  mEccPP3,        "eccpp3[4]/D")   ; // 3rd order Participant plane eccentricity
    mTree->Branch("eccpp4",  mEccPP4,        "eccpp4[4]/D")   ; // 4th order Participant plane eccentricity
    mTree->Branch("pp2",     mPP2,           "pp2[4]/D")      ; // 2nd order participant plane
    mTree->Branch("pp3",     mPP3,           "pp3[4]/D")      ; // 3rd order participant plane
    mTree->Branch("pp4",     mPP4,           "pp4[4]/D")      ; // 4th order participant plane

    mHeader = new TTree("header", "Constants used in the Fast MC glauber");
    mHeader->Branch("nameA",               mNameNucleusA,        "nameA[2]/C");
    mHeader->Branch("nameB",               mNameNucleusB,        "nameB[2]/C");
    mHeader->Branch("massNumberA",        &mMassNumberA,         "massNumberA/i");
    mHeader->Branch("massNumberB",        &mMassNumberB,         "massNumberB/i");
    mHeader->Branch("radiusA",            &mRadiusA,             "radiusA/F");
    mHeader->Branch("radiusB",            &mRadiusB,             "radiusB/F");
    mHeader->Branch("skinDepthA",         &mSkinDepthA,          "skinDepthA/F");
    mHeader->Branch("skinDepthB",         &mSkinDepthB,          "skinDepthB/F");
    mHeader->Branch("beta2A",             &mBeta2A,              "beta2A/F");
    mHeader->Branch("beta4A",             &mBeta4A,              "beta4A/F");
    mHeader->Branch("beta2B",             &mBeta2B,              "beta2B/F");
    mHeader->Branch("beta4B",             &mBeta4B,              "beta4B/F");
    mHeader->Branch("sigmaNN",            &mSigmaNN,             "sigmaNN/F");
    mHeader->Branch("sqrtSNN",            &mSqrtSNN,             "sqrtSNN/F");
    mHeader->Branch("repulsionD",         &mRepulsionD,          "repulsionD/F");
    mHeader->Branch("totalXsec",          &mTotalXsec,           "totalXsec/F");
    mHeader->Branch("totalXsecError",     &mTotalXsecError,      "totalXsecError/F");
    mHeader->Branch("smearHardCore",      &mSmearHardCore,       "smearHardCore/i");
    mHeader->Branch("smearGaussian",      &mSmearGaussian,       "smearGaussian/i");
    mHeader->Branch("collisionHardCore",  &mCollisionHardCore,   "collisionHardCore/i");
    mHeader->Branch("collisionGaussian",  &mCollisionGaussian,   "collisionGaussian/i");
    mHeader->Branch("maxB",               &mBMax,                "maxB/F");
    mHeader->Branch("neventsAccept",      &mNeventsAccept,       "neventsAccept/i");
    mHeader->Branch("neventsThrow",       &mNeventsThrow,        "neventsThrow/i");
    mHeader->Branch("npp",                &mNpp,                 "npp/F");
    mHeader->Branch("k",                  &mK,                   "k/F");
    mHeader->Branch("x",                  &mX,                   "x/F");
    mHeader->Branch("efficiency",         &mEfficiency,          "efficiency/F");
    mHeader->Branch("isConstEfficiency",  &mIsConstEfficiency,   "isConstEfficiency/i");
    mHeader->Branch("version",            &mVersion,             "version/i");
  }

  return 1;
}

//____________________________________________________________________________________________________
void StGlauberTree::Sort()
{
  /// Sort objects in ROOT file (write mode only)
  if( mMode == 1 ) mFile->GetList()->Sort() ;
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::Fill()
{
  return mTree->Fill();
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::FillHeader()
{
  return mHeader->Fill();
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::Close()
{
  LOG_INFO << "StGlauberTree::Close  close ROOT file : " << mFile->GetName() << endm;
  if(mMode==1) mFile->Write();
  mFile->Close();

  return 1;
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::GetEntries() const
{
  return mTree->GetEntries() ;
}

//____________________________________________________________________________________________________
Int_t StGlauberTree::GetEntry(const Int_t ievent)
{
  return mTree->GetEntry(ievent);
}

//____________________________________________________________________________________________________
Double_t StGlauberTree::GetSigmaA2(const Double_t a2, const Double_t a) const
{
  const Double_t val = a2 - a*a ;
  if( val < 0.0 ){
    Error("GetSigmaA2", "{a^2}-{a}^2 < 0 (= %1.3f)", val);
    return -9999. ;
  }

  return a2 - a*a ;
}

//____________________________________________________________________________________________________
Double_t StGlauberTree::GetSigmaXY(const Double_t xy, const Double_t x, const Double_t y) const
{
  return xy - x*y ;
}

//____________________________________________________________________________________________________
Double_t StGlauberTree::GetSRP(const UInt_t id) const
{
  /// S_{RP} = pi * sqrt(sigma_x^2 * sigma_y^2)

  if( id >= 4 ){
    Error("StGlauberTree::GetSRP", "Unknown id, id=%3d. abort", id);
    assert(0);
  }

  const Double_t sigmax2 = GetSigmaA2(mSumX2[id], mSumX[id]);
  const Double_t sigmay2 = GetSigmaA2(mSumY2[id], mSumY[id]);
  const Double_t area    = TMath::Pi() * TMath::Sqrt(sigmax2*sigmay2) ;

  return area ;
}

//____________________________________________________________________________________________________
Double_t StGlauberTree::GetSPP(const UInt_t id) const
{
  /// S_{PP} = pi * sqrt(sigma_x^2 * sigma_y^2 - sigma_{xy}^2}

  if( id >= 4 ){
    Error("StGlauberTree::GetSPP", "Unknown id, id=%3d. abort", id);
    assert(0);
  }

  const Double_t sigmax2 = GetSigmaA2(mSumX2[id], mSumX[id]);
  const Double_t sigmay2 = GetSigmaA2(mSumY2[id], mSumY[id]);
  const Double_t sigmaxy = GetSigmaXY(mSumXY[id], mSumX[id], mSumY[id]);
  const Double_t term    = sigmax2*sigmay2 - sigmaxy*sigmaxy ;
  if( term < 0 ) return -9999. ;

  return TMath::Pi() * TMath::Sqrt(term) ;
}

//____________________________________________________________________________________________________
void StGlauberTree::SetNameNucleusA(const Char_t* val)
{
  sprintf(mNameNucleusA, "%s", val);
}

//____________________________________________________________________________________________________
void StGlauberTree::SetNameNucleusB(const Char_t* val)
{
  sprintf(mNameNucleusB, "%s", val);
}


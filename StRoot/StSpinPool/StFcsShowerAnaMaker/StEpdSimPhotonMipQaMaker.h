/*
  PURPOSE
  The purpose of this StMaker is to understand the number of photons that leave a MIP signal in the EPD. This needs to be checked to understand how many photons convert into $e^+$ and $e^-$ pairs in the STAR detector before reaching the EPD. The idea is to simulate photons (or pions) and then see how many charged particles we get in the EPD

  DESCRIPTION
  Read and process an fzd simulation file of single particles and then look at the hits in the EPD for these particles. Make histograms of how many "MIPs" there are because a "MIP" represents a charged particle and photons should not be leaving a MIP in the EPD. Hits with an nMIP value larger than #mnMIPCut are taken as charged particles. Also make histograms of the information about the simulated particles

  CAVEATS
  This class relies on grabbing events from StEvent and using the EpdHitMaker to fill the hit strucuture so these two makers need to be called before this class can be used.

  LOG
  @[November 7, 2025](David Kapukchyan) > First instance

  @[July 7, 2026](David Kapukchyan) > Got rid of dependency on MyTools and changed to use #HistManager in StSpinPool/StFwdData so that it can be compiled with cons
 */

#ifndef StEpdSimPhotonMipQaMaker_H
#define StEpdSimPhotonMipQaMaker_H

//C/C++ headers
#include <algorithm>

//ROOT headers
#include "TFile.h"
#include "TLorentzVector.h"
#include "TString.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TCanvas.h"

//STAR headers
#include "StThreeVectorD.hh"
#include "StMaker.h"
#include "StEpdHitMaker/StEpdHitMaker.h"

//Custom Headers
#include "StSpinPool/StFwdData/HistManager.h"

class StEpdGeom;

class StEpdSimPhotonMipQaMaker : public StMaker
{
  
 public:
  StEpdSimPhotonMipQaMaker(const char* name="StEpdSimPhotonMipQaMaker");
  ~StEpdSimPhotonMipQaMaker();
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  void setFileName(const char* name){ mFileName = name; }    ///< set file name that #Finish() will use to write to a file

  UInt_t LoadHistograms( TFile* file, HistManager* histman);

  void PaintEpd(TCanvas* canvas, const char* savename="testepdphoton.png");
  void PaintSimEpd(TCanvas* canvas, const char* savename="testepdsimphoton.png");
  
  
 protected:
  StEpdGeom* mEpdGeo              = 0;
  
  TClonesArray* mMuEpdHits       = 0;
  StEpdHitMaker* mEpdHitMkr      = 0;
  StEpdCollection* mEpdColl      = 0;

  TString mFileName = "";        //< name of output file name
  
  //Histograms
  TH1* mH1F_NumberOfWestHits = 0;    ///< Histogram of number of MIPs
  TH1* mH1F_NumberOfMips = 0;        ///< Histogram of number of MIPs
  TH1* mH2F_Hit_yVx = 0;             ///< Histogram of hit x and y positions
  //TH1* mH2F_SimPhoton_yVx = 0;         ///< Histogram of hit x and y positions
  //TH1* mH2F_SimPhoton_En = 0;         ///< Histogram of hit x and y positions
  TH1* mH1F_NTracks = 0;             ///< Number tracks from GEANT
  TH1* mH1F_GeantId = 0;             ///< Geant Ids of tracks
  TH1* mH1F_NVertex = 0;             ///< Number of "vertices" from GEANT

  double mnMIPCut = 0.7;  //!< Cut for nMIP

private:
  HistManager* mHistsMan = 0;             //! Array to hold histograms and TFile
  //std::ofstream* mTestFile = 0;
  
  ClassDef(StEpdSimPhotonMipQaMaker,1);
};

#endif

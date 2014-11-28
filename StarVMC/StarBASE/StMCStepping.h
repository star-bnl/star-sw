// $Id: StMCStepping.h,v 1.3 2011/03/04 19:30:17 jwebb Exp $
//
//
// Class StMCStepping
// ------------------


#ifndef STMC_STEPPING_H
#define STMC_STEPPING_H

#include "TString.h"
#include "TLorentzVector.h"
#include "GCall.h"
#include "THashTable.h"

#include <iostream>
#include <vector>
#include <map>

class TGeoNode;
class TGeoVolume;
class TGeoMedium;
class TGeoMaterial;
class TGeoNavigator;

class TH1D;
class TH2F;
class TFile;

class StMCStepping : public GCall
{
  

 public:
  //   All cases
  enum SteppingCase {
    kNewTrack 		   =   1,
    kRootGeometrySupported =   2,
    kTrackAlive 	   =   4,
    kTrackDisappeared 	   =   8,
    kTrackEntering 	   =  16,
    kTrackExiting 	   =  32,
    kTrackInside           =  64,
    kTrackOut              = 128,
    kTrackStop             = 256};
  
  //   Main cases (KaZes)
  enum SteppingKaze {
    kNEWtrack 		= 1,
    kENTERtrack		= 2,
    kCONTINUEtrack	= 4,
    kEXITtrack		= 8,
    kENDEDtrack		=16,
    kOUTtrack		=32,
    kIgnore    		=64};
  
  enum MediumPars {
    kIsvol =0,
    kIfield=1,
    kFieldm=2,
    kTmaxfd=3,
    kStemax=4,
    kDeemax=5,
    kEpsil =6,
    kStmin =7,
  };
		  
  StMCStepping(const char *name="",const char *tit="");
  ~StMCStepping();
  // methods
  virtual int  Fun();
  virtual void Print(const Option_t* opt=0) const;
  static  TString CaseAsString(int kase);
  static  TString KazeAsString(int kase);

  // Histograming

  void  BookHistograms();

  TH2F *h_ncount_rz;
  TH2F *h_radlen_rz;
  TH2F *h_abslen_rz;
  TH2F *h_pathlen_rz;
  TH2F *h_distance_rz;

  TH1D *h_radlen_total_eta;  // material balance plot
  TH1D *h_ncount_total_eta;
  
  void       bookVolume( const Char_t *name );

  std::vector<const Char_t *>     fListOfVolumes;

  Int_t                  mNumberOfVolumes;
  std::vector<TH1D *>    hRadlenHist1D;
  std::vector<TH1D *>    hCountsHist1D;
  std::vector<TH2F *>    hRadlenHist2D;
  std::vector<TH2F *>    hCountsHist2D;

  std::vector<TH1D *>    hRadlenAccu1D;

  
  std::vector<TH1D *>    hRadlenHist1D_z;
  std::vector<TH1D *>    hCountsHist1D_z;
  std::vector<TH1D *>    hRadlenHist1D_r;
  std::vector<TH1D *>    hCountsHist1D_r;
  std::vector<TH1D *>    hRadlenHist1D_phi;
  std::vector<TH1D *>    hCountsHist1D_phi;
  
  

  std::vector<Double_t>   mRadlenSum;   // Sum within the specified subsystem
  std::vector<Double_t>   mRadlenEnter; // Accumulated radiation length at entrance

  std::vector<Bool_t>     mHasEntered; // Flag indicating volumes which have been entered
  Double_t                mRadlenAcc; // Accumulated radiation length along the track

  void postTrack();
  
 protected:
  
  void Case();
  // data members
  TLorentzVector fStartPosition;
  TLorentzVector fEnterPosition;
  TLorentzVector fCurrentPosition;
  TLorentzVector fEnterMomentum;
  TLorentzVector fCurrentMomentum;
  float  fEnterLength;
  float  fCurrentLength;
  float  fCharge;
  float  fMass;
  float  fEdep;
  float  fEtot;
  float  fLife;
  float  fX0;
  double fTrackNumber;
  int    fPDG;
  int    fPid;
  int    fTrType;
  int    fKaze;
  int    fKazePrev;
  int    fCase;

  Double_t fSumRadlen;
  Double_t fSumAbslen;
  
  TString       fParName;
  TString       fCasName;
  TString       fKazName;
  TGeoNode     *fNode;
  TGeoVolume   *fVolume;
  TGeoMedium   *fMedium;
  TGeoNavigator *fNavigator;

  TGeoMaterial *fMaterial;

  ClassDef(StMCStepping,0) // Extended TParticle
};

#endif //STMC_STEPPING_H   
   

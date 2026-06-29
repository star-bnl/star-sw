/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The Classes related to holding the data in the Pi0TTree in #StMuFcsPi0TreeMaker

  DESCRIPTION
  Contains the classes #FcsEventInfo, #FcsPhotonCandidate, and #FcsPairCandidate that is used to store information for the Pi0 Transverse Single Spin Asymmetry Analysis (TSSA). The #TTree is created and used in #StMuFcsPi0TreeMaker. Putting these classes in a separate folder was done so that the library can be loaded without loading other STAR libraries. This greatly simplifies the rootmap file needed as well. 

  LOG
  @[September 20, 2024] > Copied from *StMuFcsPi0TreeMaker*

  @[September 21, 2024] > Changed the comment style from Doxygen friendly to ROOT friendly so that the variables show up in the dictionary. This means changing __//!<__  to __///<__ according to [here](https://root.cern/for_developers/doxygen/)

  @[September 26, 2024] > Added #BlueSpin() and #YellowSpin() to #FcsEventInfo to correctly get the blue and yellow beam polarization from the #mSpin. Implemented #FcsEventInfo::Clear() and #FcsEventInfo::Print() as well as #FcsPhotonCandidate::Clear(), #FcsPhotonCandidate::Print(), #FcsPi0Candidate::Clear(), and #FcsPi0Candidate::Print(). Also added comparison functions to #FcsPi0Candidate so that they can be sorted in the #TClonesArray in #StMuFcsPi0TreeMaker. They will be sorted by their distance to the known pi0 mass; e.g. a pi0 candidate with mass 0.14 is less than a candidate with mass 0.1 because 0.14 is closer to the knwon pi0 mass. Added static const #FcsPi0Candidate::Pi0Mass() to just return the mass of the pi0 particle.

  @[September 30, 2024] > Added #FcsPhotonCandidate::IsSortable() and #FcsPhotonCandidate::IsEqual() and #FcsPhotonCandidate::Compare() to be able to sort photon candidates by cluster and energy. Also #FcsPhotonCandidate::Print() now prints #FcsPhotonCandidate::mEpdHitNmip value.

  @[October 3, 2024] > Added #mFromPh to #FcsPi0Candidate to distinguish pi0s with and without an epd cut.

  @[October 11, 2024] > Changed how comparison works for #FcsPhotonCandidate so a #TClonesArray of #FcsPhotonCandidate is sorted in descending order of energy rather than ascending order.
  @[October 21, 2024] > Changed #mEpdHitNmip in #FcsPhotonCandidate to default to -1. This is so that I can have an additional check for if the candidate intersected any EPD tile even if it was not hit
  @[October 31, 2024] > Changed Double_t to Float_t for many values to conserve on disk space for the trees
  @[November 22, 2024] > In #FcsPi0Candidate made all the functions related to kinematic variables const and added the Pt value to the printout so that I can check it against the Pt trigger threshold more easily.
  @[January 29, 2025] > In #FcsPi0Candidate added #lv() which returns a #TLorentzVector of the Pi0 candidate based on its px,py,pz,energy
  @[February 5, 2025] > Clarified a comment
  @[September 4, 2025] > Implemented a copy function for #FcsPhotonCandidate
  @[November 25, 2025] > Inside #FcsPhotonCandidate; added an array to hold EPD matches for different classes of intersections of EPD tiles; also added accessor functions to the EPD array that holds these matches and fixed the #FcsPhotonCandidate::Clear() and FcsPhotonCandidate::Copy() to clear and copy from array. Iterated #FcsPhotonCandidate to new version because of this change.
  @[January 26, 2026] > Added primary vertex information and #mFoundVertex to #FcsEventInfo. Also, changed #BlueSpin() and #YellowSpin() to be static functions that now take an argument of a 4 bit spin value and return "blue up(1)/down(-1)", or "yellow up(1)/down(-1)".
  @[June 8, 2026] > Changed 'FcsPi0Candidate' to #FcsPairCandidate. Added two new variables #FcsPhotonCandidate::mEpdHitNmipSum and #FcsPhotonCandidate::mEpdHitAdjMax to help with the discrimation of #FcsPhotonCandidate into charged or neutral category
  @[June 11, 2026] > Changed #mFoundVertex from Char_t to Short_t as it's a more appropriate type for this variable. Added 'epd' as print option to #FcsPhotonCandidate::Print()
  @[June 17, 2026] > Added #FcsPhotonCandidate::mFoundRegion to indicate in which region of the EPD matched tile the photon candidate lies. Changed comment of #FcsPhotonCandidate::mEpdHitNmip to better match new Epd matching algorithm. Added comments to explain new meaning of #FcsPairCandidate::mFromPh which can now be set using the new #FcsPairCandidate::DiscriminateCharge(). #FcsPairCandidate::DiscriminateCharge() will fit #FcsPairCandidate::mFromPh into 1 of 4 categories as described in the comment
*/

#ifndef STMUFCSPI0DATA_HH
#define STMUFCSPI0DATA_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
#include "TObject.h"
#include "TString.h"
#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TH1F.h"
#include "TLorentzVector.h"
#include "TClonesArray.h"

class FcsEventInfo : public TObject
{
public:
  FcsEventInfo();
  ~FcsEventInfo();

  Int_t mRunTime = -1;       ///< Time of the run (actually time of event)
  Int_t mRunNum = -1;        ///< Run number for event
  UInt_t mFill = 0;          ///< Fill number for event
  UInt_t mEvent = -1;        ///< STAR Event Id
  Int_t mBx48Id = -1;        ///< 48 bit bunch Id for event
  Int_t mBx7Id = -1;         ///< 7 bit bunch Id for event
  UShort_t mSpin = 0;        ///< [Spin bit, this is source polarization](https://drupal.star.bnl.gov/STAR/blog/oleg/spin-patterns-and-polarization-direction)
  //Short_t spinFrom4BitSpin(); ///< Correctly accounts for the spin flip when working with STAR data
  static Short_t BlueSpin(Int_t spinbit);        ///< Blue beam polarization at STAR +1 for B+ and -1 for B-
  static Short_t YellowSpin(Int_t spinbit);      ///< Yelllow beam polarization at STAR +1 for Y+ and -1 for Y-

  Int_t mTofMultiplicity = -1; ///< TOF Multiplicity

  Int_t mPrimVertRanking = -1;  ///< Ranking of primary vertex
  Double_t mPrimVx = -999;     ///< x position of found primary vertex
  Double_t mPrimVy = -999;     ///< y position of found primary vertex
  Double_t mPrimVz = -999;     ///< z position of found primary vertex
  Double_t mVpdVz = -999;      ///< VPD z Vertex
  Double_t mBbcVz = -999;      ///< BBC z Vertex
  Double_t mBbcTacDiff = 0; ///< BBC TAC difference
  Double_t mEpdTacEarlyW = 0;  ///< Earliest EPD TAC for West with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdTacEarlyE = 0;  ///< Earliest EPD TAC for East with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdAvgW = 0;    ///< Average EPD TAC for West with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdAvgE = 0;    ///< Average EPD TAC for East with cuts 1<adcnmip<15 && TAC>50
  //Double_t EpdTacDiffEarly();
  //Double_t EpdTacDiffAvg();
  Double_t mEpdVz = -999;      ///< EPD z Vertex
  Double_t mZdcVz = -999;      ///< ZDC z Vertex
  Short_t mFoundVertex = 0;    ///< Bit vector encoding for which vertex was best; 0 means no vertex, 1=Primary Vertex found,2=Vpd,3=Epd,4=Bbc

  //This will be used to indicate how many clusters are in the #TClonesArray of #FcsPhotonCandidate. Everything from this number to the size of the array will be points for a given detector Id. I did it this way so I don't have to create a separate branch holding these two numbers and there should only be one #FcsEventInfo object. Also, didn't want a seperate class for clusters and points since they will store the same information. It is kind of a hack since I know that I am only looping up to detector id 2.
  UShort_t mClusterSize = 0;       ///< Size of clusters in #mPhArr in #StMuFcsPi0TreeMaker. This means 0 to <#mClusterSize is cluster photon candidates

  virtual void Clear(Option_t* opt="");          ///< Resets all variables to default
  virtual void Print(Option_t* opt="") const;    ///< Prints all values no options

  ClassDef( FcsEventInfo, 1 )
};

//Class to hold basic particle info from which pi0s can be reconstructed
class FcsPhotonCandidate : public TObject
{
public:
  FcsPhotonCandidate();
  ~FcsPhotonCandidate();

  bool mFromCluster = false;  ///< True if from an FCS cluster
  Short_t mDetId = -1;        ///< Detector Id where candidate was found

  Float_t mX = 0;           ///< STAR global x coordinate
  Float_t mY = 0;           ///< STAR global y coordinate
  Float_t mZ = 0;           ///< STAR global z coordinate

  Float_t mEn = 0;          ///< Energy
  Float_t mPxRaw = 0;       ///< X momentum assuming 0,0,0 vertex
  Float_t mPyRaw = 0;       ///< Y momentum assuming 0,0,0 vertex
  Float_t mPzRaw = 0;       ///< Z momentum assuming 0,0,0 vertex

  Float_t mPxVert = 0;       ///< X momentum using best found vertex
  Float_t mPyVert = 0;       ///< Y momentum using best found vertex
  Float_t mPzVert = 0;       ///< Z momentum using best found vertex

  Short_t mEpdFoundRegion = -2;  ///< Found Region in EPD tile where photon candidate exists, -1=intile, 0=outer, 1=outerccw, 2=ccw, 3=innerccw, 4=inner, 5=innercw, 6=cw, 7=outercw
  Float_t mEpdHitNmip[5];   ///< NMIP value from EPD hit (-1 means could not project to valid tile, 0 means projected to valid tile but no hit, >0 value from EPD hit. first entry is always the "best" match, entry 1 is the intersected tile and the rest depend on found region
  Short_t mEpdMatch[5];       ///< Special key for knowing which Tile key (100*pp+tt) had an intersection, Note that "0" is not a valid key since pp goes from 1 to 12, and tt goes from 1 to 31. The index here matches #FcsPhotonCandidate::mEpdHitNmip
  Float_t mEpdHitNmipSum = 0;   ///< Take the best matched EPD intersected tile and add up its nmip value to its 8 adjacent tiles as defined in #StMuFcsAnaEpdMatch
  Float_t mEpdHitAdjMax = 0;    ///< The maximum nmip whecn checking the best matched EPD intersected tile and its 8 adjacent tiles as defined in #StMuFcsAnaEpdMatch

  TLorentzVector lvRaw();        ///< TLorentz vector for this condidate with 0,0,0 vertex momentum
  TLorentzVector lvVert();       ///< TLorentz vector for this candidate with vertex momentum
  Float_t magPosition();         ///< Magnitude of postiion vector i.e. sqrt(#mX^2+#mY^2+#mZ^2)

  virtual Bool_t IsSortable() const {return kTRUE; }  ///< I guess this is flag to indicate to ROOT that object is sortable
  virtual Bool_t IsEqual(const TObject* obj) const;   ///< if comparing clusters to points return false. otherwise if comparing clusters to clusters (or points to points) check if energy matches
  virtual Int_t Compare(const TObject* obj) const;    ///< Returns 1 if comparing clusters to points (i.e. clusters are less than points). If comparing clusters to clusters (or points to points) if this->en<other->en return 1 so it does descending energy sort; -1 if this->en>other->en

  virtual void Copy(TObject& object) const;            ///< Copy this candidate to object
  virtual void Clear(Option_t* opt="");               ///< Resets all variables to defaults
  virtual void Print(Option_t* opt="") const;         ///< Print all variables no options
  static void ConvertEpdKeyToPpTt(Short_t key, Short_t &pp, Short_t &tt);   ///< pp=key/100 (integer divide), then tt=key-100*pp

  ClassDef( FcsPhotonCandidate, 4 )
};

//Class to hold basic info for reconstructed pair candidates
class FcsPairCandidate : public TObject
{
public:
  FcsPairCandidate();
  ~FcsPairCandidate();
  
  Bool_t mFromCluster = false;  ///< Pi0 reconstructed from clusters or points
  Short_t mFromPh = -1;          ///< Flag to indicate if pi0 constructed using an epd photon cut or not (0=both neutral (<=nmip), 1=ph1<=nmip&&ph2>nmip, 2=ph2<=nmip&&ph1>nmip, 3=both charged (ph1>nmip && ph2>nmip), -2 means failure, -1 means not set)
  UShort_t mPhoton1Idx = -1;    ///< Index in #TClonesArray of #FcsPhotonCandidate 1 that was used to reconstruct this Pi0
  UShort_t mPhoton2Idx = -1;    ///< Index in #TClonesArray of #FcsPhotonCandidate 2 that was used to reconstruct this Pi0

  //#StMuFcsPi0TreeMaker will only store information for the Lorentz vector, and other data from the best vertex. To switch to 0,0,0 vertex; use the photon index
  Float_t mPx = 0;              ///< X-Momentum from Lorentz vector of two reconstructed candidates
  Float_t mPy = 0;              ///< Y-Momentum from Lorentz vector of two reconstructed candidates
  Float_t mPz = 0;              ///< Z-Momentum from Lorentz vector of two reconstructed candidates
  Float_t mEn = 0;              ///< Energy from Lorentz vector of two reconstructed candidates
  TLorentzVector lv();          ///< TLorentz vector for this condidate constructed from #mPx, #mPy, #mPz, and #mEn
  
  Float_t mEta = -1;            ///< Pseudorapidity from the lorentz vector
  Float_t eta() const;          ///< Needed since in simulations the stored eta was not there when reconstructing from simulated photons
  Float_t phi() const;          ///< Angle in x,y plane
  Float_t pt() const;           ///< Transverse momentum
  Float_t ptot() const;         ///< Total momentum
  Float_t theta() const;        ///< azimuthal angle (angle from z-axis to x-y plane)
  Float_t mass() const;         ///< Invariant mass of the Pi0
  void DiscriminateCharge(TClonesArray* photonarr, Double_t epdnmipcut);   ///< Set a pair mFromPh using two photon candidates and an epdnmip cut

  //Need to project using momentum
  //Float_t mStarX = 0;           ///< Global STAR x postion from best vertex
  //Float_t mStarY = 0;           ///< Global STAR y postion from best vertex
  //Float_t mStarZ = 0;           ///< Global STAR z postion from best vertex
  Float_t mDgg = 0;             ///< Euclidean Distance between the two particles (cm) 
  Float_t mZgg = 0;             ///< Energy Asymmetry |E1-E2|/(E1+E2) of pi0
  Float_t mAlpha = 0;           ///< Opening angle of pi0
  Float_t mInvMass = -1;        ///< invariant mass using best vertex as a variable to make it easier for analysis
  
  static Float_t zgg(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2);        ///< Energy asymmetry of pi0
  static Float_t dgg(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2);        ///< Distance between the two particles (cm)
  static Float_t alpha(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2);      ///< Opening angle of the two photons

  static const Double_t Pi0Mass(){ return 0.13498; }

  //@[September 26, 2024] > [Need these three functions so that TClonesArray can sort your object](https://root-forum.cern.ch/t/sort-a-tclonesarray/38056)
  virtual Bool_t IsSortable() const {return kTRUE; }  ///< I guess this is flag to indicate to ROOT that object is sortable
  virtual Bool_t IsEqual(const TObject* obj) const;   ///< if both are equal to pi0 mass then return true otherwise false 
  virtual Int_t Compare(const TObject* obj) const;    ///< -1 if distance to pi0 mass of this object is less than the other's distance, 1 if it is greater than, 0 otherwise

  virtual void Clear(Option_t* option="");            ///< Resets all variables to defaults
  virtual void Print(Option_t* option="") const;      ///< Print all variables no options

  ClassDef( FcsPairCandidate, 1 )
};

#endif

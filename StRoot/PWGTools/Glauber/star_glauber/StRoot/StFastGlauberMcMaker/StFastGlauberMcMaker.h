/******************************************************************************
 * $Id: StFastGlauberMcMaker.h,v 1.4 2013/02/05 22:49:50 hmasui Exp $
 * $Log: StFastGlauberMcMaker.h,v $
 * Revision 1.4  2013/02/05 22:49:50  hmasui
 * Added Pb and Cu nuclei
 *
 * Revision 1.3  2012/04/25 05:22:42  hmasui
 * Added deformation parameters, higher order participant eccentricity with r^2 weight
 *
 * Revision 1.2  2010/11/20 19:03:11  hmasui
 * Mode mode flag to StCentralityMaker. Use STAR logger rather than STD iostream
 *
******************************************************************************/
//====================================================================================================
//  StFastGlauberMcMaker class 
//    Fast MC Glauber simulation
//
//   In order to run the MC glauber simulation, you can do
//   root4star -b
//   [0] .L doFastGlauberMcMaker.C
//   [1] doFastGlauberMcMaker("glauber.root", 1000, "AuAu", 200, "default", kFALSE);
//   where 1st argument is output ROOT file name, 2nd is number of events, 
//   3rd is the system, 4th is the center of mass energy
//   and 5th is type. The available type can be checked by
//     StFastGlauberMcMaker::Print("type");
//
//   - The 5th argument is the flag of deformation. kFALSE (or false) gives 
//     spherical nuclei (default), kTRUE (or true) gives deformed nuclei.
//     Deformation parameters are defined inside the class up to 4th order deformation.
//
//   You can see the debugging messages by
//     StFastGlauberMcMaker::DebugOn()
//   in case you need further checks of outputs
//
//----------------------------------------------------------------------------------------------------
//  Hiroshi Masui (HMasui@lbl.gov)
//====================================================================================================

#ifndef __StFastGlauberMcMaker_h__
#define __StFastGlauberMcMaker_h__

class TF1 ;
class TF2 ;
class TF3 ;
class TGraph ;
class TH1 ;
class TTree ;
class Nucleon ;
class StCentralityMaker ;
class StGlauberTree ;
#include <vector>
#include "TString.h"

//____________________________________________________________________________________________________
// Class StFastGlauberMcMaker: Fast Glauber Monte Carlo
class StFastGlauberMcMaker {
  public:
    // Default constructor (spherical Au+Au), and output file "fastglaubermc.root"
    StFastGlauberMcMaker();

    /// User friendly constructor
    // Current available system:
    //     AuAu
    //
    // Current available type
    //     default
    //     large              Large R(+2%), small d(-10%)
    //     small              Small R(-2%), large d(+10%)
    //     largeXsec          Large inelastic NN cross section (+1mb)
    //     smallXsec          Small inelastic NN cross section (-1mb)
    //     gauss              Use gaussian collision profile
    StFastGlauberMcMaker(
        const TString outputFileName, // Output fileName
        const TString system, // System name (e.x. AuAu)
        const Double_t energy, // energy (GeV)
        const TString type,  // type (see above)
        const Bool_t isDeformed = kFALSE // Deformation flag
        );

    /// Constructor for symmetric collisions
    StFastGlauberMcMaker(
        const TString outputFileName,
        const UInt_t massNumber,  // Mass number of nucleus
        const Double_t radius,    // Radius of nucleus
        const Double_t skinDepth, // Skin depth of nucleus
        const Double_t beta2,     // 2nd order deformation parameter
        const Double_t beta4,     // 4th order deformation parameter
        const Double_t inelasticCrossSection, // Inelastic NN cross section
        const Double_t energy  // sqrt(sNN)
        ); /// Default constructor

    /// Constructor for asymmetric collisions
    StFastGlauberMcMaker(
        const TString outputFileName,
        const UInt_t massNumberA,  // Mass number of nucleus for nucleus A
        const Double_t radiusA,    // Radius of nucleus for nucleus A
        const Double_t skinDepthA, // Skin depth of nucleus for nucleus A
        const Double_t beta2A,     // 2nd order deformation parameter for nucleus A
        const Double_t beta4A,     // 4th order deformation parameter for nucleus A
        const UInt_t massNumberB,  // Mass number of nucleus for nucleus B
        const Double_t radiusB,    // Radius of nucleus for nucleus B
        const Double_t skinDepthB, // Skin depth of nucleus for nucleus B
        const Double_t beta2B,     // 2nd order deformation parameter for nucleus B
        const Double_t beta4B,     // 4th order deformation parameter for nucleus B
        const Double_t inelasticCrossSection, // Inelastic NN cross section
        const Double_t energy  // sqrt(sNN)
        ); /// Default constructor

    virtual ~StFastGlauberMcMaker(); /// Default destructor

    /// Set repulsion of nucleons (default is 0fm)
    void SetRepulsionDistance(const Double_t repulsionDistance);

    Int_t Make() ; /// Make one event
    Int_t Run(const UInt_t nevents) ; /// Run Make() by nevents
    Int_t Finish() ; /// Finish maker

    /// Hard-core smearing by sigmaNN
    void DoHardCoreSmearing() ; /// Default is OFF

    /// Gaussian smearing by width = 0.79/sqrt(3) from CPC180, 69, 2009
    void DoGaussianSmearing() ; /// Default is OFF

    /// Collision profiles
    void DoHardCoreCollision() ; /// Hard-core collision (default)
    void DoGaussianCollision() ; /// Gaussion profile collision

    /// Print info.
    //  option=type   print all available types
    void Print(const TString option="") const ;

    void DebugOn() ; /// Debug Mode ON
    UInt_t Version() const ; /// Return version 

  private:
    // Functions
    Int_t Clear() ; /// Clear all data members in tree

    /// Initialize ROOT tree
    Int_t InitTree() ;

    /// Initialization of nucleus and nucleons, and output ROOT file
    Int_t Init(
        const UInt_t massNumberA,  // Mass number of nucleus for nucleus A
        const Double_t radiusA,    // Radius of nucleus for nucleus A
        const Double_t skinDepthA, // Skin depth of nucleus for nucleus A
        const Double_t beta2A,     // 2nd order deformation parameter for nucleus A
        const Double_t beta4A,     // 4th order deformation parameter for nucleus A
        const UInt_t massNumberB,  // Mass number of nucleus for nucleus B
        const Double_t radiusB,    // Radius of nucleus for nucleus B
        const Double_t skinDepthB, // Skin depth of nucleus for nucleus B
        const Double_t beta2B,     // 2nd order deformation parameter for nucleus B
        const Double_t beta4B,      // 4th order deformation parameter for nucleus B
        const TString type = "default"
        );

    /// Initialization for specific collisions
    Int_t InitAuAu(const TString type) ;
    Int_t InitSmSm(const TString type) ;
    Int_t InitUU(const TString type) ;
    Int_t InitPbPb(const TString type) ;
    Int_t InitCuCu(const TString type) ;
    Int_t InitZrZr(const TString type, int Case) ;
    Int_t InitRuRu(const TString type, int Case) ;

    /// Nucleon-Nucleon collision
    Bool_t IsCollision(Nucleon* nucleon0, Nucleon* nucleon1) const ;

    /// Utilities
    const Char_t* GetName(const UInt_t massNumber) const ; /// Mass number -> nucleus name
    Double_t GetInelasticNNCrossSection(const Double_t energy, const TString type) const ;
    void GetRThetaPhi(const UInt_t inucleus,
        Double_t& r, Double_t& theta, Double_t& phi) const ; /// Arguments are outputs, not inputs

    /// Do smearing (either hard-core or gaussian depending on the switch)
    void Smearing(Double_t& r, Double_t& theta, Double_t& phi) const ; /// NOTE: (r,theta,phi) is output

    /// Participant eccentricity: eps_{part;n} = sqrt(q_{x;n}^2 + q_{y;n}^2)/r_t^2
    /// q_{x;n} = <r_T^2 * cos(n * phi_part)>
    /// q_{y;n} = <r_T^2 * sin(n * phi_part)>
    /// r_T = sqrt(x_{part}^2 + y_{part}^2)
    /// phi_part is the azimth of nucleons with respect to the participant plane
    //  
    //  TGraph::GetY()[0]    <r_T^2 * cos(n*phi)>
    //  TGraph::GetY()[1]    <r_T^2 * sin(n*phi)>
    //  TGraph::GetY()[2]    n * Psi = tan^{-1}(q_{y;n}, q_{x;n})
    //  TGraph::GetY()[3]    eps_{part;n}
    TGraph* GetParticipantEccentricity(const Double_t order, const Double_t sumx, const Double_t sumy,
        const Double_t sumw, const UInt_t weightId) const ;

    //____________________________________________________________________________________________________
    // Data members

    // For collision profile
    enum {
      mkHardCoreProfile = 0,
      mkGaussianProfile = 1
    };

    // Version control
    static const UInt_t mVersion ;

    const Double_t mEnergy ; /// sqrt(sNN)
    const TString mOutputFileName ; /// Output filename
    Double_t mInelasticNNCrossSection ; /// Inelastic NN cross section

    UInt_t mDebug ; /// Debug flag

    // Switches
    Double_t mRepulsionDistance ; /// Repulsion distance between nucleons (default is 0fm)

    // NOTE: Only one smearing method can be applied
    //       Another option will be switched off when one turn on one of them
    Bool_t mHardCoreSmearing    ; /// true  -> smear nucleons by step function with sigmaNN
                                  /// false -> Use original position (default)
    Bool_t mGaussianSmearing    ; /// true  -> smear nucleons by gaussian distribution with fixed sigma
                                  /// false -> Use original position (default)
    UInt_t mCollisionProfile    ; /// Collision profile flag (mkHardCoreProfile=0, mkGaussianProfile=1)

    UInt_t mMode ; /// 0:default, 1:large npp (small x), 2:small npp (large x)
    Bool_t mIsDeformed[2] ; /// Deformation flag (default is false, spherical nuclei) 

    StCentralityMaker* mCentralityMaker ; /// For multiplicity
    std::vector<Nucleon*> mNucleons[2] ; /// Nucleons
    TF1* mfWoodsSaxon[2]   ; /// Woods-saxon density profile for spherical nuclei
    TF2* mfWoodsSaxon2D[2] ; /// Woods-saxon density profile for deformed nuclei

    UInt_t mNeventsThrow  ; /// Number of all events
    UInt_t mNeventsAccept ; /// Number of accepted events (Ncoll>0)
    TTree* mHeader        ; /// Output ROOT tree (store constant info., R, d, sigma etc)

    // Output data in tree
    StGlauberTree* mGlauberTree ; /// MC glauber tree class

    // QA histograms
    TH1* mhWoodsSaxon[4] ; /// Woods-saxon checks. First 2 histograms = all nucleons, Last 2 histograms = for replusive nucleons
                           /// If repulsion distance is 0fm, then First and Last two should be identical

    // Random number generator
    TF1* mfB        ; /// Impact parameter distribtion p(b) ~ b
    TF3* mfHardCore ; /// Hard-core smearing for (x,y,z) of nucleons
    TF3* mfGaussian ; /// Gaussian smearing for (x,y,z) of nucleons

    ClassDef(StFastGlauberMcMaker, 1)
};

#endif


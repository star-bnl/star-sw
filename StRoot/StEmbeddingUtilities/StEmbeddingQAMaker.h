//****************************************************************************************************
//  Class StEmbeddingQAMaker
//
//    Produce QA histograms for embedding and real data
//    - For embedding outputs, we currently include the histograms for 
//      MC, MATCHED, GHOST, CONTAM, MATGLOB pairs 
//      (NOTE: MATGLOB pairs don't have a function to get the number of tracks
//       we have to implement that function in StMiniMcEvent later
//       in order to obtain the QA histograms)
//
//    - For real data outputs, both primary and global tracks will be analyzed
//
//
//    To get the QA histograms, you can run the macro "doEmbeddingQAMaker.C" under StRoot/macros/embedding
//
//    For example,
//    root4star -b -q doEmbeddingQAMaker.C'(2007, "P08ic", "PiPlus", "minimc.list", "embedding.root")'
//    
//    or
//
//    roo4star -b
//    > .L doEmbeddingQAMaker.C
//    > doEmbeddingQA(2007, "P08ic", "PiPlus", "minimc.list");
//    > ...
//    > .q
//
//    The details of arguments can be found in the doEmbeddingQAMaker.C
//
//    NOTE:
//     - Particle name (3rd argument, in this case "PiPlus") is case INSENSITIVE. 
//       You can use PiPlus, piPlus, piplus or whatever you want. 
//       Please look at the StRoot/macros/embedding/StEmbeddingQAUtilities.cxx 
//       for more details about particle name.
//
//     - Output file name (5th argument, in this case "embedding.root") will be 
//       automatically detemined according to the "year", "production" 
//       and "particle name" if you leave it blank.
//----------------------------------------------------------------------------------------------------
//   Revised history
//
//   Oct/23/2009 : Add pid cuts (nsigma <2) for e/pi/K/p for real data
//   Sep/20/2009 : Fix title for Nhit histograms. Wider eta/y range (-2.5,2.5)
//   Sep/18/2009 : Added QA histograms for decay daughters (H. Masui)
//   Sep/09/2009 : 1st version checked in   (H. Masui)
//****************************************************************************************************

#ifndef __StEmbeddingQAMaker_h__
#define __StEmbeddingQAMaker_h__

#include <map>

#include "TMath.h"
#include "TString.h"

#include "StEmbeddingQAUtilities.h"

class TFile ;
class TH1 ;
class TH2 ;
class TH3 ;
class TObject ;

class StContamPair ;
class StMiniMcEvent ;
class StEmbeddingQAPair ;
class StEmbeddingQATrack ;
class StMuDstMaker ;
class StMuTrack ;
class StTinyMcTrack ;

//____________________________________________________________________________________________________
//  Analyze either minimc trees for embedding QA or microDST for real data QA
class StEmbeddingQAMaker {
  public:
    /// Default constructor (default argument is 2007, P08ic)
    StEmbeddingQAMaker() ;

    /// Specify year, production (isSimulation=kTRUE --> do embedding QA)
    StEmbeddingQAMaker(const Int_t year, const TString production, const Bool_t isSimulation = kTRUE);

    /// Destructor
    virtual ~StEmbeddingQAMaker();

    /// Book histograms
    /// Default output is 'ana_{type}_{year}_{production}_{particleId}.root'
    /// if you don't put any words in the argument (i.e. whitespace)
    /// {type} is either embedding or real
    Bool_t book(const TString outputFileName = "");
    Bool_t make(const TString inputFileName, const Bool_t isSimulation = kTRUE);

    Bool_t run(const TString inputFileList) ; // Either RunRealData or RunEmbedding according to the kIsSimulation flag
    Bool_t runRealData(const TString inputFileList) ;
    Bool_t runEmbedding(const TString inputFileList) ;
    Bool_t end() const; // Close output file

    void setZVertexCut(const Float_t vz) ; /// set z-vertex cut (default is |vz|<30cm unless otherwise specified)

  private:
    const Int_t mYear ;               /// Year
    const TString mProduction ;       /// Production
    const Bool_t mIsSimulation ;      /// kTRUE : embedding QA,  kFALSE : real data QA

    void clear() ; /// Clear all histograms
    void init() ;  /// Initialization

    // Fill histograms
    Bool_t fillEmbedding(const TString inputFileName) ; // Fill embedding histograms
    Bool_t fillRealData(const TString inputFileName) ;  // Fill real data histograms

    /// Fill embedding tracks
    void fillEmbeddingTracks(const StMiniMcEvent& mcevent, const Int_t categoryid, const Int_t itrk) ;

    /// Get StEmbeddingQATrack from different minimc branches
    StEmbeddingQATrack* getEmbeddingQATrack(const StMiniMcEvent& mcevent, const Int_t categoryid, const Int_t itrk) ;

    /// Fill real data tracks
    void fillRealTracks(const StMuTrack& track, const Int_t categoryid, const Int_t itrk);

    /// Fill histograms for trakcs from both embedding and real tracks
    void fillHistograms(const StEmbeddingQATrack& track, const Int_t categoryid);

    /// Expand histograms if a new geantid is found in either MC or reconstructed track
    void expandHistograms(const Int_t categoryid, const Short_t geantid, const Short_t parentid);

    /// Push back a new geant id in mGeantId array
    Bool_t pushBackGeantId(const Int_t categoryid, const Short_t geantid) ;

    /// Z-vertex cut
    Bool_t isZVertexOk(const StMiniMcEvent& mcevent) const ;

    /// Number of tracks
    Int_t getNtrack(const Int_t categoryid, const StMiniMcEvent& mcevent) const ;

    /// Check geant id in StParticleTable
    Bool_t isGeantIdOk(const StTinyMcTrack& track) const ;

    StMuDstMaker* mMuDstMaker ;
    Float_t mVertexCut ; /// z-vertex cut (Default is 30 cm)

    // Output histograms
    TFile* mOutput ;

    // Event-wise histograms
    Double_t mVz ;
    TH1* mhVz ;         // z-vertex
    TH1* mhVzAccepted ; // z-vertex (with z-vertex cut)

    TH2* mhVyVx ; // y vs x vertices
    TH1* mhdVx ; // vx(real) - vx(MC)
    TH1* mhdVy ; // vy(real) - vy(MC)
    TH1* mhdVz ; // vz(real) - vz(MC)

    /// Track-wise histograms
    ///   Fill all available MC and reconstructed tracks
    //
    //  - Use MC momentum instead of reconstructed momentum (Update on Nov/13/2009)
    //  - Add p (reco) vs p (MC) (Update on Nov/13/2009)
    std::vector<Short_t> mGeantId[StEmbeddingQAConst::mNCategory] ; /// Geant id in both MC tracks and reconstructed pairs
    TH1* mhGeantId[StEmbeddingQAConst::mNCategory];                               /// Geant id
    TH1* mhParentGeantId ;                                                            /// Parent geantid (only for Contaminated pairs)
    std::map<Int_t, TH3*> mhNHit[StEmbeddingQAConst::mNCategory] ;                /// Nhit distribution vs eta vs pt
    std::map<Int_t, TH3*> mhDca[StEmbeddingQAConst::mNCategory] ;                 /// Dca vs eta vs pt
    std::map<Int_t, TH2*> mhPtVsEta[StEmbeddingQAConst::mNCategory] ;             /// pt vs pseudo-rapidity
    std::map<Int_t, TH2*> mhPtVsY[StEmbeddingQAConst::mNCategory] ;               /// pt vs rapidity
    std::map<Int_t, TH2*> mhPtVsPhi[StEmbeddingQAConst::mNCategory] ;             /// pt vs phi
    std::map<Int_t, TH2*> mhPtVsMom[StEmbeddingQAConst::mNCategory] ;             /// pt vs momentum
    std::map<Int_t, TH2*> mhdPtVsPt[StEmbeddingQAConst::mNCategory] ;             /// pt - pt(MC) vs pt
    std::map<Int_t, TH2*> mhMomVsEta[StEmbeddingQAConst::mNCategory] ;            /// momentum vs eta
    std::map<Int_t, TH2*> mhdEdxVsMomMc[StEmbeddingQAConst::mNCategory] ;         /// dE/dx vs MC momentum (no PID cut)
    std::map<Int_t, TH2*> mhdEdxVsMomMcPidCut[StEmbeddingQAConst::mNCategory] ;   /// dE/dx vs MC momentum (with PID cut, 2 sigma)
    std::map<Int_t, TH2*> mhdEdxVsMomReco[StEmbeddingQAConst::mNCategory] ;       /// dE/dx vs reconstructed momentum (no PID cut)
    std::map<Int_t, TH2*> mhdEdxVsMomRecoPidCut[StEmbeddingQAConst::mNCategory] ; /// dE/dx vs reconstructed momentum (with PID cut, 2 sigma)
    std::map<Int_t, TH2*> mhRecoPVsMcP[StEmbeddingQAConst::mNCategory] ;          /// Reconstructed momentum vs MC momentum
    std::map<Int_t, TH2*> mhNCommonHitVsNHit[StEmbeddingQAConst::mNCategory] ;      /// Ncommon hit vs Nhit

    std::map<Int_t, TH2*> mhEtaVsPhi[StEmbeddingQAConst::mNCategory] ;  /// pseudo-rapidity vs phi
    std::map<Int_t, TH2*> mhEtaVsVz[StEmbeddingQAConst::mNCategory] ;   /// pseudo-rapidity vs vz
    std::map<Int_t, TH2*> mhYVsVz[StEmbeddingQAConst::mNCategory] ;     /// rapidity vs vz

    ClassDef(StEmbeddingQAMaker, 1);
};

inline void StEmbeddingQAMaker::setZVertexCut(const Float_t vz) { mVertexCut = vz ; }

#endif


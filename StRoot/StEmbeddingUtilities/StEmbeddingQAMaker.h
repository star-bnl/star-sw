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
//   Sep/18/2009 : Added QA histograms for decay daughters (H. Masui)
//   Sep/09/2009 : 1st version checked in   (H. Masui)
//****************************************************************************************************

#ifndef __StEmbeddingQAMaker_h__
#define __StEmbeddingQAMaker_h__

#include "TMath.h"
#include "TString.h"

#include <vector>

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

class StEmbeddingQAParticleCollection ;

//____________________________________________________________________________________________________
//  Analyze either minimc trees for embedding QA or microDST for real data QA
class StEmbeddingQAMaker {
  public:
    StEmbeddingQAMaker() ; // Default is 2007, P08ic, pi+
    StEmbeddingQAMaker(const Int_t year, const TString production, const Int_t particleId, 
        const Bool_t isSimulation = kTRUE); // specify year, production, particle id (default is embedding QA)
    StEmbeddingQAMaker(const Int_t year, const TString production, const TString name,
        const Bool_t isSimulation = kTRUE); // specify year, production, particle name (default is embedding QA)
    virtual ~StEmbeddingQAMaker();

    Bool_t Book(const TString outputFileName = ""); // Default output is "ana_embedding_{year}_{production}_{particleId}.root" for embedding QA
    Bool_t Make(const TString inputFileName, const Bool_t isSimulation = kTRUE);

    Bool_t Run(const TString inputFileList) ; // Either RunRealData or RunEmbedding according to the kIsSimulation flag
    Bool_t RunRealData(const TString inputFileList) ;
    Bool_t RunEmbedding(const TString inputFileList) ;
    Bool_t End(); // Close output file

    void SetDebug(const Int_t val) ;

  private:
    static const Float_t kVertexCut ; // z-vertex cut (Default is 30 cm)
    const Int_t kYear ;               // Year
    const TString kProduction ;       // Production
    const Int_t kParticleId ;         // Input particle id
    const Bool_t kIsSimulation ;      // kTRUE : embedding QA,  kFALSE : real data QA

    // Initialization
    void Init() ;

    // Fill
    Bool_t FillEmbedding(const TString inputFileName) ; // Fill embedding histograms
    Bool_t FillRealData(const TString inputFileName) ;  // Fill real data histograms

    void FillMcTracks(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk);
    void FillMatchedPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk);
    void FillGhostPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk);
    void FillContamPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk);
    void FillMatGlobPairs(const StMiniMcEvent& mcevent, const Int_t trackid, const Int_t itrk);
    void FillRealTracks(const StMuTrack& track, const Int_t trackid, const Int_t itrk);
    void FillHistograms(const StEmbeddingQATrack& track, const Int_t trackid, const Int_t iparticle);
    void FillPair() ;
    Bool_t GetTrackSelectionForDaughters(const StEmbeddingQATrack& track) const;

    // Cuts
    Bool_t isZVertexOk(const StMiniMcEvent& mcevent, const Float_t vertexCut = 30.0) const ;

    Int_t mDebug ;
    StMuDstMaker* mMuDstMaker ;

    StEmbeddingQAParticleCollection* mParticles ;

    // Daughter's array
    std::vector<StEmbeddingQATrack*> mDaughterPositive ; // Positive charged track
    std::vector<StEmbeddingQATrack*> mDaughterNegative ; // Negative charged track
    std::vector<StEmbeddingQATrack*> mDaughterNeutral ;  // Neutral charged track

    // Output histograms
    TFile* mOutput ;

    // Event-wise histograms
    Double_t mVz ;
    TH1* hVz ;         // z-vertex
    TH1* hVzAccepted ; // z-vertex (with z-vertex cut)

    TH2* hVyVx ; // y vs x vertices
    TH1* hdVx ; // vx(real) - vx(MC)
    TH1* hdVy ; // vy(real) - vy(MC)
    TH1* hdVz ; // vz(real) - vz(MC)

    // Tracks
    //  Fill Daughters for contaminated pairs
    TH1** hGeantId[StEmbeddingQAUtilities::kNCategory];   // Geant id
    TH3** hNHit[StEmbeddingQAUtilities::kNCategory];      // Nhit distribution vs eta vs pt
    TH3** hDca[StEmbeddingQAUtilities::kNCategory];       // Dca vs eta vs pt
    TH2** hPtVsEta[StEmbeddingQAUtilities::kNCategory];   // pt vs pseudo-rapidity
    TH2** hPtVsY[StEmbeddingQAUtilities::kNCategory];     // pt vs rapidity
    TH2** hPtVsPhi[StEmbeddingQAUtilities::kNCategory];   // pt vs phi
    TH2** hPtVsMom[StEmbeddingQAUtilities::kNCategory];   // pt vs momentum
    TH2** hdPtVsPt[StEmbeddingQAUtilities::kNCategory];   // pt - pt(MC) vs pt
    TH2** hdEdxVsMom[StEmbeddingQAUtilities::kNCategory]; // dE/dx vs momentum

    TH2** hEtaVsPhi[StEmbeddingQAUtilities::kNCategory];  // pseudo-rapidity vs phi
    TH2** hEtaVsVz[StEmbeddingQAUtilities::kNCategory];   // pseudo-rapidity vs vz
    TH2** hYVsVz[StEmbeddingQAUtilities::kNCategory];     // rapidity vs vz

    // Pairs
    TH1** hPtReco;         // pt distribution for decay daughters (with similar track selection as real data)
    TH2* hInvMassVsPt[2] ; // Invariant mass vs pt (0:Unlike sign pair, 1: Like sign pair, only for contaiminated pairs)
    
    ClassDef(StEmbeddingQAMaker, 1);
};

inline void StEmbeddingQAMaker::SetDebug(const Int_t val) { mDebug = val ; }

#endif


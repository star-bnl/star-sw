//****************************************************************************************************
//  Class StEmbeddingQA
//
//  See http://drupal.star.bnl.gov/STAR/comp/embedding/embedding-procedures/qa-documentation
//  for instructions
//****************************************************************************************************
/****************************************************************************************************
 * $Id: StEmbeddingQA.h,v 1.1 2009/12/22 21:41:17 hmasui Exp $
 * $Log: StEmbeddingQA.h,v $
 * Revision 1.1  2009/12/22 21:41:17  hmasui
 * Change class name from StEmbeddingQAMaker to StEmbeddingQA
 *
 ****************************************************************************************************/

#ifndef __StEmbeddingQA_h__
#define __StEmbeddingQA_h__

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
class StEmbeddingQA {
  public:
    /// Default constructor (default argument is 2007, P08ic)
    StEmbeddingQA() ;

    /// Specify year, production (isSimulation=kTRUE --> do embedding QA)
    StEmbeddingQA(const Int_t year, const TString production, const Bool_t isSimulation = kTRUE);

    /// Destructor
    virtual ~StEmbeddingQA();

    /// Book histograms
    /// Default output is 'ana_{type}_{year}_{production}_{particleId}.root'
    /// if you don't put any words in the argument (i.e. whitespace)
    /// {type} is either embedding or real
    Bool_t book(const TString outputFileName = "");

    /// Either fillEmbedding or fillRealData according to the isSimulation flag
    Bool_t make(const TString inputFileName, const Bool_t isSimulation = kTRUE);

    /// Either RunRealData or RunEmbedding according to the kIsSimulation flag
    Bool_t run(const TString inputFileList) ;

    /// Analyzer real data
    Bool_t runRealData(const TString inputFileList) ;

    /// Analyzer embedding data
    Bool_t runEmbedding(const TString inputFileList) ;

    /// Close output file
    Bool_t end() const;

    /// set z-vertex cut (default is |vz|<30cm unless otherwise specified)
    void setZVertexCut(const Float_t vz) ;

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

    StMuDstMaker* mMuDstMaker ; /// Pointer to the StMuDstMaker
    Float_t mVertexCut ; /// z-vertex cut (Default is 30 cm)

    TFile* mOutput ; /// Output histograms

    // Event-wise histograms
    Double_t mVz ;      /// z-vertex
    TH1* mhVz ;         /// z-vertex (histogram)
    TH1* mhVzAccepted ; /// z-vertex (with z-vertex cut)

    TH2* mhVyVx ; /// y vs x vertices
    TH1* mhdVx ; /// vx(real) - vx(MC)
    TH1* mhdVy ; /// vy(real) - vy(MC)
    TH1* mhdVz ; /// vz(real) - vz(MC)

    /// Track-wise histograms
    ///   Fill all available MC and reconstructed tracks
    //
    //  - Use MC momentum instead of reconstructed momentum (Update on Nov/13/2009)
    //  - Add p (reco) vs p (MC) (Update on Nov/13/2009)
    std::vector<Short_t> mGeantId[StEmbeddingQAConst::mNCategory] ; /// Geant id in both MC tracks and reconstructed pairs
    TH1* mhGeantId[StEmbeddingQAConst::mNCategory];                               /// Geant id
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

    ClassDef(StEmbeddingQA, 1);
};

inline void StEmbeddingQA::setZVertexCut(const Float_t vz) { mVertexCut = vz ; }

#endif




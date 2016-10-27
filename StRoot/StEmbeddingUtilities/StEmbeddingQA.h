//****************************************************************************************************
//  Class StEmbeddingQA
//
//  See http://drupal.star.bnl.gov/STAR/comp/embedding/embedding-procedures/qa-documentation
//  for instructions
//****************************************************************************************************
/****************************************************************************************************
 * $Id: StEmbeddingQA.h,v 1.12 2016/10/27 15:50:06 zhux Exp $
 * $Log: StEmbeddingQA.h,v $
 * Revision 1.12  2016/10/27 15:50:06  zhux
 * added an option to set the maximum pT cut, by Zachariah Miller
 *
 * Revision 1.11  2012/03/05 10:32:29  cpowell
 * Functions added to cut on refMult
 *
 * Revision 1.10  2011/04/01 05:05:47  hmasui
 * Track selections by StEmbeddingQAUtilities. Added 1/pt(RC)-1/pt(MC) vs pt, and pt dependent Ncommon vs NhitFit histograms
 *
 * Revision 1.9  2011/02/11 03:55:44  hmasui
 * Change geantid type to integer
 *
 * Revision 1.8  2011/01/31 21:32:10  hmasui
 * Modify histogram keys to TString to take into account parent geantid
 *
 * Revision 1.7  2010/07/12 21:29:40  hmasui
 * Move isGeantIdOk() function into StEmbeddingQAUtilities
 *
 * Revision 1.6  2010/05/14 19:50:11  hmasui
 * Add rapidity and trigger cuts.
 *
 * Revision 1.5  2010/04/24 20:21:18  hmasui
 * Add geant process check for contaminated pairs
 *
 * Revision 1.4  2010/02/16 02:13:34  hmasui
 * Add parent-parent geant id in the histogram name
 *
 * Revision 1.3  2010/01/28 21:50:37  hmasui
 * Add Vx vs Vz and Vy vs Vz histograms.
 *
 * Revision 1.2  2010/01/26 17:46:31  hmasui
 * Add histograms for eventid, runnumber, and number of particles
 *
 * Revision 1.1  2009/12/22 21:41:17  hmasui
 * Change class name from StEmbeddingQAMaker to StEmbeddingQA
 *
 ****************************************************************************************************/

#ifndef __StEmbeddingQA_h__
#define __StEmbeddingQA_h__

#include <map>
#include <vector>

#include "TMath.h"
#include "TString.h"

#include "StEmbeddingQAUtilities.h"

class TFile ;
class TH1 ;
class TH2 ;
class TH3 ;
class TObject ;
//class TTree ;

class StContamPair ;
class StEmbeddingQAPair ;
class StEmbeddingQATrack ;
class StMiniMcEvent ;
class StMuDstMaker ;
class StMuEvent ;
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

    void init() ;  /// Initialization 

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
    /// Moved to StEmbeddingQAUtilities but keep the function for backward compatibility
    void setZVertexCut(const Float_t vz) ;
    void setRefMultMinCut(const Int_t refMultMin) ;
    void setRefMultMaxCut(const Int_t refMultMax) ;

    /// Add trigger id cut (default is no trigger id selections). Multiple trigger can be added
    /// Moved to StEmbeddingQAUtilities but keep the function for backward compatibility
    void addTriggerIdCut(const UInt_t id) ;

    /// Set rapidity cut (default is |y|<10, i.e. no rapidity cut)
    /// Moved to StEmbeddingQAUtilities but keep the function for backward compatibility
    void setRapidityCut(const Float_t ycut) ;

    /// Set Maximum Range of pT histograms; binning = 10*ptmax
    void setPtMax(Float_t ptmax) ; 

  private:
    const Int_t mYear ;               /// Year
    const TString mProduction ;       /// Production
    const Bool_t mIsSimulation ;      /// kTRUE : embedding QA,  kFALSE : real data QA
    Float_t mPtMax ;                  /// Sets maximum ptRange of Histograms

    void clear() ; /// Clear all histograms

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
    void expandHistograms(const Int_t categoryid, const Int_t geantid, const Int_t parentid,
        const Int_t parentparentid, const Int_t geantprocess);

    /// Push back a new geant id in mGeantId array
    Bool_t pushBackGeantId(const Int_t categoryid, const Int_t geantid, const Int_t parentid,
        const Int_t parentparentid, const Int_t geantprocess) ;

    /// Z-vertex cut
    Bool_t isZVertexOk(const StMiniMcEvent& mcevent) const ;

    /// RefMult cut
    Bool_t isRefMultOk(const StMiniMcEvent& mcevent) const ;

    /// Trigger id cut for real data. Return true if no trigger id is found
    Bool_t isTriggerOk(StMuEvent* event) const ;

    /// Number of tracks
    Int_t getNtrack(const Int_t categoryid, const StMiniMcEvent& mcevent) const ;

    /// Get combination string of geantid, parent and parent-parent id
    /// The format is ("%d_%d_%d", geantid, parentid, parentparentid)
    TString getIdCollection(const Int_t geantid, const Int_t parentid, const Int_t parentparentid) const ;

    StMuDstMaker* mMuDstMaker ; /// Pointer to the StMuDstMaker

    TFile* mOutput ; /// Output histograms

    // Event-wise histograms
    Double_t mVz ;      /// z-vertex
    TH1* mhVz ;         /// z-vertex (histogram)
    TH1* mhVzAccepted ; /// z-vertex (with z-vertex cut)
    TH1* mhRef ;         /// refMult (histogram)
    TH1* mhRefAccepted ; /// refMult (histogram)

    TH2* mhVyVx ; /// y vs x vertices
    TH2* mhVxVz ; /// x vs z vertices
    TH2* mhVyVz ; /// y vs z vertices
    TH1* mhdVx ; /// vx(real) - vx(MC)
    TH1* mhdVy ; /// vy(real) - vy(MC)
    TH1* mhdVz ; /// vz(real) - vz(MC)

    TH1* mhEventId ; /// Event id
    TH1* mhRunNumber ; /// Run number (see StEmbeddingQAUtilities for the definition of run number)
    TH1* mhNParticles[StEmbeddingQAConst::mNCategory] ; /// Number of particles

    /// Track-wise histograms
    ///   Fill all available MC and reconstructed tracks
    //
    //  - Use MC momentum instead of reconstructed momentum (Update on Nov/13/2009)
    //  - Add p (reco) vs p (MC) (Update on Nov/13/2009)
    std::vector<Int_t> mGeantId[StEmbeddingQAConst::mNCategory] ; /// Geant id in both MC tracks and reconstructed pairs
    std::vector<TString> mGeantIdCollection ;                       /// Array of (parent-parent id, parent id, geantid)
                                                                    /// for Contaminated pairs only

    TH1* mhGeantId[StEmbeddingQAConst::mNCategory];                               /// Geant id
    std::map<TString, TH3*> mhNHit[StEmbeddingQAConst::mNCategory] ;                /// Nhit distribution vs eta vs pt
    std::map<TString, TH3*> mhDca[StEmbeddingQAConst::mNCategory] ;                 /// Dca vs eta vs pt
    std::map<TString, TH2*> mhPtVsEta[StEmbeddingQAConst::mNCategory] ;             /// pt vs pseudo-rapidity
    std::map<TString, TH2*> mhPtVsY[StEmbeddingQAConst::mNCategory] ;               /// pt vs rapidity
    std::map<TString, TH2*> mhPtVsPhi[StEmbeddingQAConst::mNCategory] ;             /// pt vs phi
    std::map<TString, TH2*> mhPtVsMom[StEmbeddingQAConst::mNCategory] ;             /// pt vs momentum
    std::map<TString, TH2*> mhdPtVsPt[StEmbeddingQAConst::mNCategory] ;             /// pt(RC) - pt(MC) vs pt
    std::map<TString, TH2*> mhdInvPtVsPt[StEmbeddingQAConst::mNCategory] ;          /// 1/pt(RC) - 1/pt(MC) vs pt
    std::map<TString, TH2*> mhMomVsEta[StEmbeddingQAConst::mNCategory] ;            /// momentum vs eta
    std::map<TString, TH2*> mhdEdxVsMomMc[StEmbeddingQAConst::mNCategory] ;         /// dE/dx vs MC momentum (no PID cut)
    std::map<TString, TH2*> mhdEdxVsMomMcPidCut[StEmbeddingQAConst::mNCategory] ;   /// dE/dx vs MC momentum (with PID cut, 2 sigma)
    std::map<TString, TH2*> mhdEdxVsMomReco[StEmbeddingQAConst::mNCategory] ;       /// dE/dx vs reconstructed momentum (no PID cut)
    std::map<TString, TH2*> mhdEdxVsMomRecoPidCut[StEmbeddingQAConst::mNCategory] ; /// dE/dx vs reconstructed momentum (with PID cut, 2 sigma)
    std::map<TString, TH2*> mhRecoPVsMcP[StEmbeddingQAConst::mNCategory] ;          /// Reconstructed momentum vs MC momentum
    std::map<TString, TH3*> mhNCommonHitVsNHit[StEmbeddingQAConst::mNCategory] ;      /// Ncommon hit vs Nhit vs pT

    std::map<TString, TH2*> mhEtaVsPhi[StEmbeddingQAConst::mNCategory] ;  /// pseudo-rapidity vs phi
    std::map<TString, TH2*> mhEtaVsVz[StEmbeddingQAConst::mNCategory] ;   /// pseudo-rapidity vs vz
    std::map<TString, TH2*> mhYVsVz[StEmbeddingQAConst::mNCategory] ;     /// rapidity vs vz

    ClassDef(StEmbeddingQA, 1);
};

#endif




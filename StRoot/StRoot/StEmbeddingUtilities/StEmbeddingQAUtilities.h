//----------------------------------------------------------------------------------------------------
//  Class StEmbeddingQAUtilities
//    - Provide category id, such as 'MC' track in the minimc tree
//----------------------------------------------------------------------------------------------------
/****************************************************************************************************
 * $Id: StEmbeddingQAUtilities.h,v 1.13 2019/07/10 05:46:53 zhux Exp $
 * $Log: StEmbeddingQAUtilities.h,v $
 * Revision 1.13  2019/07/10 05:46:53  zhux
 * added option for btof pid for primary real tracks
 *
 * Revision 1.12  2012/03/05 10:32:50  cpowell
 * Functions added to cut on refMult
 *
 * Revision 1.11  2011/04/26 20:27:22  hmasui
 * Add isGamma function
 *
 * Revision 1.10  2011/04/05 23:12:35  hmasui
 * Added getGeantId() function
 *
 * Revision 1.9  2011/04/01 05:02:48  hmasui
 * Implement track selections (moved from StEmbeddingQATrack)
 *
 * Revision 1.8  2010/08/13 21:54:35  hmasui
 * Separate charge for pi/K/p
 *
 * Revision 1.7  2010/07/12 21:27:29  hmasui
 * Added StParticleTable & StParticleDefinition utilities
 *
 * Revision 1.6  2010/01/26 17:45:06  hmasui
 * Add runid functions
 *
 * Revision 1.5  2009/12/22 21:37:55  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/

#ifndef __StEmbeddingQAUtilities_h__
#define __StEmbeddingQAUtilities_h__

class StParticleDefinition ;
class TH1 ;
#include <map>
#include <vector>
#include "TString.h"
#include "StMiniMcEvent/StMiniMcEvent.h"

//____________________________________________________________________________________________________
namespace StEmbeddingQAConst 
{
  //----------------------------------------------------------------------------------------------------
  //  id       node           description
  //----------------------------------------------------------------------------------------------------
  //  0         MC             MC tracks (embedding)
  //  1         MATCHED        Matched pairs (embedding)
  //  2         GHOST          Ghost pairs (embedding)
  //  3         CONTAM         Contaminated pairs (embedding)
  //  4         MATGLOB        Matched global pairs (embedding)
  //----------------------------------------------------------------------------------------------------
  //  5         PRIMARY        Primary tracks (real)
  //  6         GLOBAL         Global tracks (real)
  //----------------------------------------------------------------------------------------------------
  enum {
    mNEmbedding = 5,
    mNReal      = 2,
    mNCategory  = mNEmbedding + mNReal
  };
}

//____________________________________________________________________________________________________
class StEmbeddingQAUtilities {
  public:
    /// Get instance
    static StEmbeddingQAUtilities* instance();

    /// Destructor
    virtual ~StEmbeddingQAUtilities();

    // Track category for minimc nodes
    Category getCategory(const UInt_t id) const ;      /// Category from category id
    TString getCategoryName(const UInt_t id) const ;   /// Category name from category id
    TString getCategoryTitle(const UInt_t id) const ;  /// Category title from category id
    Int_t getCategoryId(const TString name) const ;    /// Category id from category name

    // Input string is case insensitive
    //  - For example, MC, mc, mC and Mc are ok for isMc()
    Bool_t isMc(const TString name) const ;            /// Check whether the track is MC track or not   
    Bool_t isMatched(const TString name) const ;       /// Check whether the track is Matched pair or not   
    Bool_t isGhost(const TString name) const ;         /// Check whether the track is Ghost pair or not   
    Bool_t isContaminated(const TString name) const ;  /// Check whether the track is Contaminated pair or not   
    Bool_t isMatchedGlobal(const TString name) const ; /// Check whether the track is Contaminated pair or not   
    Bool_t isPrimary(const TString name) const ;       /// Check whether the track is primary track or not
    Bool_t isGlobal(const TString name) const ;        /// Check whether the track is global track or not
    Bool_t isEmbedding(const TString name) const ;     /// Check whether the track is embedding pair or not
    Bool_t isReal(const TString name) const ;          /// Check whether the track is real track or not

    // Check geantid
    Bool_t isElectron(const Int_t geantid) const ;     /// Check the input geantid is e-
    Bool_t isPositron(const Int_t geantid) const ;     /// Check the input geantid is e+
    Bool_t isPiPlus(const Int_t geantid) const ;       /// Check the input geantid is pi+
    Bool_t isPiMinus(const Int_t geantid) const ;      /// Check the input geantid is pi-
    Bool_t isKPlus(const Int_t geantid) const ;        /// Check the input geantid is K+
    Bool_t isKMinus(const Int_t geantid) const ;       /// Check the input geantid is K-
    Bool_t isProton(const Int_t geantid) const ;       /// Check the input geantid is p
    Bool_t isPBar(const Int_t geantid) const ;         /// Check the input geantid is pbar
    Bool_t isElectrons(const Int_t geantid) const ;    /// Check the input geantid is electrons
    Bool_t isPions(const Int_t geantid) const ;        /// Check the input geantid is pions
    Bool_t isKaons(const Int_t geantid) const ;        /// Check the input geantid is kaons
    Bool_t isProtons(const Int_t geantid) const ;      /// Check the input geantid is protons
    Bool_t isEPiKP(const Int_t geantid) const ;        /// Check the input geantid is e/pi/K/p
    Bool_t isGamma(const Int_t geantid) const ;       /// Check the input geantid is gamma

    // Histogram style
    void setStyle() const ;       /// Set overall styles
    void setStyle(TH1* h) const ; /// Set font, title and label styles

    // Run id
    Int_t getRunId(const Int_t runnumber, const Int_t year) const ; /// get runid from runnumber
    Int_t getRunNumber(const Int_t runid, const Int_t year) const ; /// runnumber = runid - (year - 2000 + 1) * 10^6

    /// Get StParticleDefinition from geantid
    StParticleDefinition* getParticleDefinition(const UInt_t geantid) const ;

    /// Check geant id is defined in StParticleTable or not
    Bool_t isGeantIdOk(const UInt_t geantid) const ;

    /// Return geant id
    Int_t getGeantId(const UInt_t geantid) const ;

    /// Get track and event selections
    Float_t getPtMinCut() const ;
    Float_t getPtMaxCut() const ;
    Float_t getEtaCut() const ;
    Short_t getNHitCut() const ;
    Float_t getNHitToNPossCut() const ;
    Float_t getDcaCut() const ;
    Double_t getNSigmaCut() const ;
    Bool_t getBTofPid() const ;
    Float_t getRapidityCut() const ;
    Float_t getZVertexCut() const ;
    Int_t 	getRefMultMinCut() const ;
    Int_t 	getRefMultMaxCut() const ;
    std::vector<UInt_t> getTriggerIdCut() const ;

    /// Set track and event selections, return new value
    Float_t setPtMinCut(const Float_t val) ;
    Float_t setPtMaxCut(const Float_t val) ;
    Float_t setEtaCut(const Float_t val) ;
    Short_t setNHitCut(const Short_t val) ;
    Float_t setNHitToNPossCut(const Float_t val) ;
    Float_t setDcaCut(const Float_t val) ;
    Double_t setNSigmaCut(const Double_t val) ;
    Bool_t setBTofPid(const Bool_t val) ;
    Float_t setRapidityCut(const Float_t val) ;
    Float_t setZVertexCut(const Float_t val) ;
    Int_t 	setRefMultMinCut(const Int_t val) ;
    Int_t 	setRefMultMaxCut(const Int_t val) ;
    void addTriggerIdCut(const UInt_t val) ;

    /// Track and event cuts
    Bool_t isPtOk(const Float_t pt) const ;
    Bool_t isEtaOk(const Float_t eta) const ;
    Bool_t isNHitsFitOk(const Float_t nHitsFit) const ;
    Bool_t isNHitToNPossOk(const Float_t ratio) const ;
    Bool_t isDcaOk(const Float_t dca) const ;
    Bool_t isNSigmaOk(const Float_t nsigma) const ;
    Bool_t isRapidityOk(const Float_t y) const ;
    Bool_t isZVertexOk(const Float_t vz) const ;
    Bool_t isRefMultOk(const Int_t refMult) const ;
    Bool_t isTriggerOk(const UInt_t trigger) const ;

    /// Print all track selections
    void PrintCuts() const ;

  private:
    /// Default constructor
    StEmbeddingQAUtilities();

    /// Single StEmbeddingQAUtilities object
    static StEmbeddingQAUtilities* mInstance ;

    /// Utility function to compare two TString
    Bool_t CompareString(const TString s0, const TString s1, const Bool_t isExact=kFALSE) const ;

    // Data members
    Category mCategory[StEmbeddingQAConst::mNEmbedding] ;    /// Category of minimc nodes
    TString mCategoryName[StEmbeddingQAConst::mNCategory] ;  /// Category name of minimc nodes
    TString mCategoryTitle[StEmbeddingQAConst::mNCategory] ; /// Category title of minimc nodes
    std::map<const TString, const UInt_t> mCategoryId ;      /// Pair of category name and category id

    // Moved track selections from StEmbeddingQATrack
    // To change the track selections, use Set...() function, for example
    //   StEmbeddingQAUtilities::Instance()->SetPtMinCut(0.2);
    // The cut values will be displayed in the pdf file
    Float_t mPtMinCut                 ;  /// Minimum pt cut (default pt > 0.1 GeV/c)
    Float_t mPtMaxCut                 ;  /// Minimum pt cut (default pt < 10 GeV/c)
    Float_t mEtaCut                   ;  /// Eta cut (default |eta| < 1.5)
    Short_t mNHitCut                  ;  /// Minimum Nfit cut (default Nfit >= 10)
    Float_t mNHitToNPossCut           ;  /// Minimum Nfit cut (default NHitFit/NHitPoss > 0.51)
    Float_t mDcaCut                   ;  /// Global dca cut (default |dca_{gl}| < 3 cm)
    Double_t mNSigmaCut               ;  /// Nsigma cut (default |Nsigma| < 2)
    Bool_t mBTofPid                   ;  /// Nsigma cut (default |Nsigma| < 2) using BTof Pid
    Float_t mRapidityCut              ;  /// Rapidity cut (default |y| < 10, i.e. basically no cut)
    Float_t mZVertexCut               ;  /// z-vertex cut (default is |vz| < 30cm)
    Int_t 	mRefMultMinCut            ;  /// refMult minimum cut (default is refMult >= 0)
    Int_t 	mRefMultMaxCut            ;  /// refMult maximum cut (default is refMult < 1000)
    std::vector<UInt_t> mTriggerIdCut ;  /// Trigger id cut (default is no cut)


    // Run id
    Int_t getYearNumber(const Int_t year) const ; // (year - 2000 + 1) * 10^6

    /// Convert special geant id in pams/sim/gstar/gstar_part.g to pre-defined GEANT id
    // See details in the source code
    UInt_t convertGeantId(const UInt_t geantid) const ;

    ClassDef(StEmbeddingQAUtilities, 1)
};

//____________________________________________________________________________________________________
inline Int_t StEmbeddingQAUtilities::getYearNumber(const Int_t year) const { return (year - 1999) * 1000000 ; }

//____________________________________________________________________________________________________
inline Int_t StEmbeddingQAUtilities::getRunId(const Int_t runnumber, const Int_t year) const
{
  return runnumber + getYearNumber(year) ;
}

//____________________________________________________________________________________________________
inline Int_t StEmbeddingQAUtilities::getRunNumber(const Int_t runid, const Int_t year) const
{
  return runid - getYearNumber(year) ;
}

#endif



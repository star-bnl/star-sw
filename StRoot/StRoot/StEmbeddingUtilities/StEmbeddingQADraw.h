//----------------------------------------------------------------------------------------------------
//   Draw QA histograms
//     - Comparison of the results between embedding and real data
//     - Print only one PDF file by default. 
//
//----------------------------------------------------------------------------------------------------
//  NOTE: These functions are not used for the latest QA codes.
//       file           function
//       png            setPNGOn()
//       gif            setGIFOn()
//       jpg            setJPGOn()
//       eps            setEPSOn()
//       ps             setPSOn()
//----------------------------------------------------------------------------------------------------
//
//  You can make the QA plots by drawEmbeddingQA.C under StRoot/macros/embedding
//  Suppose you have output ROOT files: qa_embedding_2007_P08ic.root,
//  and qa_real_2007_P08ic.root (the output file format is automatically 
//  determined if you leave the output filename blank in StEmbeddingQAMaker)
//  Suppose you have embedded single pion+ (geantid=8) in the output files.
//
//    > root4star -b -q drawEmbeddingQA.C'("./", "qa_embedding_2007_P08ic.root", "qa_real_2007_P08ic.root", 8)'
//
//  First argument is the directory where the output figures are printed.
//  The default output directory is the current directory.
//  The 6th argument is the input geantid
//
//  If you name the output ROOT files by hand, you need to put the year,
//  production and particle name by yourself since those are used 
//  for the output figure name and to print those informations in 
//  the legend for each QA plot.
//  Suppose you have output ROOT files: qa_embedding.root and qa_real.root
//
//    > root4star -b -q drawEmbeddingQA.C'("./", "qa_embedding.root", "qa_real.root", 2005, "P08ic", 8)'
//
//
//  If you just want ot check the embedding output only, you can do 
//
//    > root4star -b -q drawEmbeddingQA.C'("./", "qa_embedding_2007_P08ic.root", "qa_real_2007_P08ic.root", 8, kTRUE)'
//
//  where the last argument is the switch to skip drawing the real data if it's true (default is false)
//
//----------------------------------------------------------------------------------------------------
/****************************************************************************************************
 * $Id: StEmbeddingQADraw.h,v 1.16 2011/04/12 03:01:07 hmasui Exp $
 * $Log: StEmbeddingQADraw.h,v $
 * Revision 1.16  2011/04/12 03:01:07  hmasui
 * Fix isMatchedPairOk() to properly process particles with decay daughters
 *
 * Revision 1.15  2011/02/11 03:44:57  hmasui
 * Draw error messages in pdf if histogram is missing. Add error check for Ncommon histogram
 *
 * Revision 1.14  2011/01/31 21:33:51  hmasui
 * Add setParentGeantId() function to allow the multiple decays
 *
 * Revision 1.13  2010/06/22 16:31:17  hmasui
 * Separate 2D and 1D QA for MC tracks. Add pol0 fit for MC eta, y and phi distributions.
 *
 * Revision 1.12  2010/06/10 14:51:01  hmasui
 * Added particle name functions
 *
 * Revision 1.11  2010/04/07 19:45:11  hmasui
 * Use box option for dE/dx vs p to reduce the pdf file size
 *
 * Revision 1.10  2010/03/15 21:05:23  hmasui
 * Separate MC vertices QA into 2 pages. Added constraint on z-vertex cut for vx(vy) vs vz histograms.
 *
 * Revision 1.9  2010/02/24 18:11:47  hmasui
 * Added isMC flag in drawProjection2D() to switch real or MC tracks
 *
 * Revision 1.8  2010/02/23 16:56:37  hmasui
 * Add phi distributions QA (MC vs reconstructed)
 *
 * Revision 1.7  2010/02/16 02:14:03  hmasui
 * Print PDF file only for all QA plots
 *
 * Revision 1.6  2009/12/22 21:40:11  hmasui
 * Add comments for functions and members
 *
 ****************************************************************************************************/


#ifndef __StEmbeddingQADraw_h__
#define __StEmbeddingQADraw_h__

#include <vector>

class TCanvas ;
class TH1 ;
class TFile ;
class TObject ;
class TPad ;
class TPaveText ;
class TPDF ;
#include "TString.h"

class StEmbeddingQADraw {
  public:
    /// Input embedding/real data file names made by StEmbeddingQAAnalyzer
    /// Added 'isEmbeddingOnly' argument
    ///   true            --> draw QA only for the embedding
    ///   false (default) --> draw QA both embedding and real data
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile, const Int_t geantid,
        const Bool_t isEmbeddingOnly = kFALSE);

    /// Specify year, production and particle geantid if you put you own input embedding/real data file names
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,
        const Int_t year, const TString production, const Int_t geantid, const Bool_t isEmbeddingOnly = kFALSE);
    virtual ~StEmbeddingQADraw();

    /// Initialization
    void init() ;

    /// Set parent geant id (default is 0)
    void setParentGeantId(const Int_t parentgeantid) ;

    /// Default is current directory
    void setOutputDirectory(const TString name = "./") ;

    Bool_t draw() const ;       /// Draw all histograms (event and track histograms).

    // Event-wise
    Bool_t drawEvent() const;     /// Draw all event-wise histograms

    // Track-wise
    Bool_t drawMcTrack() const;    /// Draw MC track histograms
    Bool_t drawTrack() const ;     /// Draw Reconstructed track histograms

    Bool_t drawGeantId() const;    /// Geant id
    Bool_t drawPhi() const ;       /// Azimuthal angle (phi) distributions
    Bool_t drawRapidity() const;   /// (pseudo-)rapidity in different eta bins
    Bool_t drawPt() const;         /// pt (|eta|<2, 0.5 eta increment)
    Bool_t drawMomentum() const;   /// momentum (|eta|<2, 0.5 eta increment)
    Bool_t drawdEdx() const;       /// dE/dx (2D) and projections
    Bool_t drawDca() const;        /// Dca vs (pt, eta)
    Bool_t drawNHit() const;       /// NHit vs (pt, eta)

    Bool_t finish() ;               /// Finish QA

    void setPNGOn() ;        /// Set true for png file flag (default is false)
    void setGIFOn() ;        /// Set true for gif file flag (default is false)
    void setJPGOn() ;        /// Set true for jpg file flag (default is false)
    void setEPSOn() ;        /// Set true for eps file flag (default is false)
    void setPSOn() ;         /// Set true for ps file flag (default is false)

    Int_t getYear() const ;               /// Get year
    const Char_t* getProduction() const ; /// Get production
    Int_t getGeantId() const ;            /// Get geant id

    /// Get particle name from geant id
    //   Default id = -1, returns particle name from mGeantId
    const Char_t* getParticleName(const Int_t geantid = -1) const ;

    void setPtMax(const Double_t ptmax) ; /// Set maximum pt (draw results up to this pt or momentum)

  private:
    /// Open input files
    //  return true only if both embedding and real data files are opened properly
    Bool_t open(const TString embeddingFile, const TString realDataFile) ;

    /// Print figures (png only by default)
    void print(const TCanvas& canvas, const TString name) const;

    /// Check input geantid
    //   if we don't find geantid in the MC histogram, replace mGeantId to that found in the MC histogram
    void checkInputGeantId() ;

    /// Do QA if input files are opened properly & put the correct geantid
    Bool_t isOpen() const ;

    /// Check # of mathced pairs > 0
    Bool_t isMatchedPairOk() const ;

    /// Extract daughter geantid from the histogram
    void setDaughterGeantId() ;

    /// Get number of events in the embedding file
    Int_t getEntries() const ;

    /// Check input particles have decay daughters.
    Bool_t isDecay() const ;

    /// geant id for the real data, will be e/pi/K/p.
    // if input geantid (or daughter geantid) is not e/pi/K/p, use pi+(=8)
    Int_t getGeantIdReal(const Int_t daughter) const ;

    /// Get category id
    //    For embedding
    //      stable particles (# of daughters = 0) --> matched pairs
    //      unstable particles (# of daughters > 0) --> contaminated pairs
    //    For real data  --> primary tracks
    Int_t  getCategoryId(const Bool_t isEmbedding = kTRUE) const ;

    /// Get histogram and check pointer
    TObject* getHistogram(const TString name, const Bool_t isEmbedding = kTRUE) const ;

    /// Get histogram for either embedding or real data
    ///   Added parent-parent id (default is 0)
    TObject* getHistogram(const TString name, const UInt_t daughter, const Bool_t isEmbedding,
        const UInt_t parentparentid = 0) const ;

    /// Get histogram name (part of codes moved from getHistogram function)
    const Char_t* getHistogramName(const TString name, const UInt_t daughter, const Bool_t isEmbedding,
        const UInt_t parentparentid = 0) const ;


    /// (1/Ntrk) * (1/bin)
    Double_t getNormalization(const TH1& h) const ;

    /// Get ${year}_${production}_${particle name}
    const Char_t* getBaseName() const ;

    /// Number of events, year, production and particle name
    Bool_t drawStatistics(const Double_t x1=0.1, const Double_t y1=0.2, const Double_t x2=0.9, const Double_t y2=0.8,
        const Double_t textSize = 0.05) const;

    /// Header for each QA (title for QA plots, daughter particle name etc)
    TPaveText* drawHeader(const TString description,
        const Double_t x1=0.0, const Double_t y1=0.9, const Double_t x2=1.0, const Double_t y2=0.95, 
        const Double_t textSize = 0.032) const;

    /// Draw legend
    void drawLegend(const UInt_t id, TH1* hembed, TH1* hreal, const Option_t* option="L",
        const Bool_t doSplit=kFALSE) const ;

    /// Draw error messages if histogram doesn't exist
    void drawErrorMessages(const TString histogramName) const ;

    /// Event-wise informations
    Bool_t drawVertices() const;          /// Draw vertices
    Bool_t drawRunEventId() const;        /// Draw run and event id
    Bool_t drawNumberOfParticles() const; /// Draw number of particles per event

    /// dE/dx vs p, projections, mean/sigma vs momentum
    Bool_t drawdEdxVsMomentum(const Bool_t isMcMomentum=kTRUE) const ;

    // Added isMC flag
    //  isMC = kTRUE            --> Compare reconstructed embedding tracks with MC tracks
    //  isMC = kFALSE (default) --> Compare reconstructed embedding tracks with real data
    Bool_t drawProjection2D(const TString name, const Bool_t isMC = kFALSE) const; /// (pseudo-)rapidity, momentum, pt and phi
    Bool_t drawProjection3D(const TString name) const; /// For dca and nhit projections in (pt, eta) space

    /// Canvas/Pad initialization
    // Header for current QA, division of (x,y), default is (0,0), no division
    TPaveText* initCanvas(const TString header, const Int_t nx=0, const Int_t ny=0) const;

    /// Get accepted minimum/maximum vz from histogram
    Double_t getVzAcceptedMinimum() const ; /// Minimum vz
    Double_t getVzAcceptedMaximum() const ; /// Maximum vz

    /// Get input MC particle name
    const Char_t* getMcParticleName() const ; 

    /// Get reconstructed embedding particle name
    const Char_t* getEmbeddingParticleName(const UInt_t id, const Bool_t doSplit=kFALSE) const ; 

    /// Get real data particle name
    const Char_t* getRealParticleName(const UInt_t id, const Bool_t doSplit=kFALSE) const ; 

    // Data members
    static UInt_t mCanvasId ; /// Canvas id

    TCanvas* mCanvas ;        /// Canvas for all QA histograms
    TPad* mMainPad ;          /// Main pad to draw histograms
    TPDF* mPDF ;              /// Output QA in 1 pdf file

    const Bool_t mIsEmbeddingOnly ;  /// Flag to draw QA for embedding or embedding + real data

    Bool_t mIsOpen ;          /// Flag for input files (true:successfully opened both embedding and real input files)

    Bool_t mIsPNGOn ;         /// Print *.png file if true (default is false)
    Bool_t mIsGIFOn ;         /// Print *.gif file if true (default is false)
    Bool_t mIsJPGOn ;         /// Print *.jpg file if true (default is false)
    Bool_t mIsEPSOn ;         /// Print *.eps file if true (default is false)
    Bool_t mIsPSOn ;          /// Print *.ps file if true (default is false)

    Int_t mYear ;             /// year
    TString mProduction ;     /// production
    Int_t mGeantId ;          /// input geant id
    Double_t mPtMax ;         /// Draw up to maximum pt (or momentum). Default is 5 GeV/c

    TFile* mInputEmbedding ; /// Input ROOT file (embedding)
    TFile* mInputRealData ;  /// Input ROOT file (real data)

    TString mOutputFigureDirectory ; /// Figure directory (default is current directory)

    std::vector<Int_t> mDaughterGeantId ; /// Daughter geant id
    std::vector<Int_t> mParentGeantId ;   /// Parent geant id (only relevant for unstable particles)
    std::vector<Int_t> mParentParentGeantId ; /// Parent-parent geant id (only relevant for unstable particles)
    std::vector<Int_t> mMcGeantId ;       /// MC geant id
    Int_t mInputParentGeantId ; /// Input parent geant id

    ClassDef(StEmbeddingQADraw, 1)
};

inline Int_t StEmbeddingQADraw::getYear()               const { return mYear ; }
inline const Char_t* StEmbeddingQADraw::getProduction() const { return mProduction.Data() ; }
inline Int_t StEmbeddingQADraw::getGeantId()            const { return mGeantId ; }

#endif


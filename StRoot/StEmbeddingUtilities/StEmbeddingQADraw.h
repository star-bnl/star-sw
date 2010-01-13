//----------------------------------------------------------------------------------------------------
//   Draw QA histograms
//     - Comparison of the results between embedding and real data
//     - Print only png file by default. You can print other file format 
//       by using the function "setXXXOn()"
//       Current available file formats:
//
//       file           function
//       gif            setGIFOn()
//       jpg            setJPGOn()
//       eps            setEPSOn()
//       ps             setPSOn()
//        
//      NOTE : 
//      1. Dca and NHit distributions are printed only for pdf file format.
//      2. drawDca(), drawNHit() will be processed by the batch mode automatically,
//         i.e. you will not see the canvas.
//
//  You can make the QA plots by drawEmbeddingQA.C under StRoot/macros/embedding
//  Suppose you have output ROOT files: qa_embedding_2007_P08ic.root,
//  and qa_real_2007_P08ic.root (the output file format is automatically 
//  determined if you leave the output filename blank in StEmbeddingQAMaker)
//
//    > root4star -l drawEmbeddingQA.C'("./", "qa_embedding_2007_P08ic.root", "qa_real_2007_P08ic.root")'
//
//  First argument is the directory where the output figures are printed.
//  The default output directory is the current directory.
//
//  If you name the output ROOT files by hand, you need to put the year,
//  production and particle name by yourself since those are used 
//  for the output figure name and to print those informations in 
//  the legend for each QA plot.
//  Suppose you have output ROOT files: qa_embedding.root and qa_real.root
//  and the input particle is pi+ (geantid=8)
//
//    > root4star -l drawEmbeddingQA.C'("./", "qa_embedding.root", "qa_real.root", 2005, "P08ic", 8)'
//
//  where the 6th argument is the input geantid
//
//    NOTE: Output figures can be printed by batch mode so you can run the macro
//          by "root4star -b ..." rather than "root4star -l ..."
//          if you don't want to see a bunch of canvas.
//
//
//----------------------------------------------------------------------------------------------------
/****************************************************************************************************
 * $Id: StEmbeddingQADraw.h,v 1.6 2009/12/22 21:40:11 hmasui Exp $
 * $Log: StEmbeddingQADraw.h,v $
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
#include "TString.h"

class StEmbeddingQADraw {
  public:
    /// Input embedding/real data file names made by StEmbeddingQAAnalyzer
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile, const Int_t geantid);

    /// Specify year, production and particle geantid if you put you own input embedding/real data file names
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,
        const Int_t year, const TString production, const Int_t geantid);
    virtual ~StEmbeddingQADraw();

    /// Default is current directory
    void setOutputDirectory(const TString name = "./") ;

    Bool_t draw() const ;       /// Draw all histograms (event and track histograms).
    Bool_t drawEvent() const;   /// Draw event-wise histograms

    // Track-wise
    Bool_t drawMcTrack() const;    /// Draw MC track histograms
    Bool_t drawTrack() const ;     /// Draw Reconstructed track histograms

    Bool_t drawGeantId() const;    /// Geant id
    Bool_t drawRapidity() const;   /// (pseudo-)rapidity in different eta bins
    Bool_t drawPt() const;         /// pt (|eta|<2, 0.5 eta increment)
    Bool_t drawMomentum() const;   /// momentum (|eta|<2, 0.5 eta increment)
    Bool_t drawdEdx() const;       /// dE/dx (2D) and projections
    Bool_t drawDca() const;        /// Dca vs (pt, eta)
    Bool_t drawNHit() const;       /// NHit vs (pt, eta)

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

    /// Initialization
    void init() ;

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
    TObject* getHistogram(const TString name, const UInt_t daughter, const Bool_t isEmbedding) const ;

    /// (1/Ntrk) * (1/bin)
    Double_t getNormalization(const TH1& h) const ;

    /// Get ${year}_${production}_${particle name}
    const Char_t* getBaseName() const ;

    /// Number of events, year, production and particle name
    Bool_t drawStatistics(const Double_t x1=0.1, const Double_t y1=0.2, const Double_t x2=0.9, const Double_t y2=0.8,
        const Double_t textSize = 0.05) const;

    /// dE/dx vs p, projections, mean/sigma vs momentum
    Bool_t drawdEdxVsMomentum(const Bool_t isMcMomentum=kTRUE) const ;

    Bool_t drawProjection2D(const TString name) const; /// (pseudo-)rapidity, momentum, pt
    Bool_t drawProjection3D(const TString name) const; /// For dca and nhit projections in (pt, eta) space

    // Data members
    static UInt_t mCanvasId ; /// Canvas id

    Bool_t mIsOpen ;          /// Flag for input files (true:successfully opened both embedding and real input files)

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

    ClassDef(StEmbeddingQADraw, 1)
};

inline void StEmbeddingQADraw::setGIFOn() { mIsGIFOn = kTRUE ; }
inline void StEmbeddingQADraw::setJPGOn() { mIsJPGOn = kTRUE ; }
inline void StEmbeddingQADraw::setEPSOn() { mIsEPSOn = kTRUE ; }
inline void StEmbeddingQADraw::setPSOn()  { mIsPSOn = kTRUE ; }

inline Int_t StEmbeddingQADraw::getYear()               const { return mYear ; }
inline const Char_t* StEmbeddingQADraw::getProduction() const { return mProduction.Data() ; }
inline Int_t StEmbeddingQADraw::getGeantId()            const { return mGeantId ; }

inline void StEmbeddingQADraw::setPtMax(const Double_t ptmax) { mPtMax = ptmax ; }

#endif


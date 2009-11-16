//----------------------------------------------------------------------------------------------------
//   Draw QA histograms
//     - Comparison of the results between embedding and real data
//     - Print only png file by default. You can print other file format 
//       by using the function "SetXXXOn()"
//       Current available file formats:
//
//       file           function
//       gif            SetGIFOn()
//       jpg            SetJPGOn()
//       eps            SetEPSOn()
//       ps             SetPSOn()
//        
//      NOTE : 
//      1. Dca and NHit distributions are printed only for pdf file format.
//      2. DrawDca(), DrawNHit() will be processed by the batch mode automatically,
//         i.e. you will not see the canvas.
//
//  You can make the QA plots by drawEmbeddingQA.C under StRoot/macros/embedding
//  Suppose you have output ROOT files: qa_embedding_2007_P08ic_PiPlus.root,
//  and qa_real_2007_P08ic_PiPlus.root (the output file format is automatically 
//  determined if you leave the output filename blank in StEmbeddingQAMaker)
//
//    > root4star -l drawEmbeddingQA.C'("./", "qa_embedding_2007_P08ic_PiPlus.root", "qa_real_2007_P08ic_PiPlus.root")'
//
//  First argument is the directory where the output figures are printed.
//  The default output directory is the current directory.
//
//  If you name the output ROOT files by hand, you need to put the year,
//  production and particle name by yourself since those are used 
//  for the output figure name and to print those informations in 
//  the legend for each QA plot.
//  Suppose you have output ROOT files: qa_embedding.root and qa_real.root
//
//    > root4star -l drawEmbeddingQA.C'("./", "qa_embedding.root", "qa.root", 2005, "P08ic", "PiPlus")'
//
//    NOTE: Output figures can be printed by batch mode so you can run the macro
//          by "root4star -b ..." rather than "root4star -l ..."
//          if you don't want to see a bunch of canvas.
//
//
//----------------------------------------------------------------------------------------------------
//   Revised history
//
//   Oct/23/2009 : 
//     - eta and rapidity comparison with different pt bins
//     - pt and momentum comparison with different eta bins
//     - Add mean and sigma of dE/dx as a function of momentum
//       from gaussian fit
//----------------------------------------------------------------------------------------------------

#ifndef __StEmbeddingQADraw_h__
#define __StEmbeddingQADraw_h__

class TCanvas ;
class TH1 ;
class TFile ;
class TObject ;
#include "TString.h"

class StEmbeddingQADraw {
  public:
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile);  // input embedding file name made by StEmbeddingQAAnalyzer
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,   // specify year, production and particle name
        const Int_t year, const TString production, const TString particleName); // if you put you own input embedding file name
    virtual ~StEmbeddingQADraw();

    void setOutputDirectory(const TString name = "./"); // default is current directory

    Bool_t draw();        // Draw all histograms (event and track histograms).
    Bool_t drawEvent();   // Draw event-wise histograms

    // Track-wise
    Bool_t drawMcTrack();        // Draw MC track histograms
    Bool_t drawTrack();          // Draw Reconstructed track histograms

    Bool_t drawGeantId();        // Geant id
    Bool_t drawRapidity();       // (pseudo-)rapidity in different eta bins
    Bool_t drawPt();             // pt (|eta|<2, 0.5 eta increment)
    Bool_t drawMomentum();       // momentum (|eta|<2, 0.5 eta increment)
    Bool_t drawdEdx();           // dE/dx (2D) and projections
    Bool_t drawDca();            // Dca vs (pt, eta)
    Bool_t drawNHit();           // NHit vs (pt, eta)

  private:
    Bool_t isGIFOn ;        // Print *.gif file if true (default is false)
    Bool_t isJPGOn ;        // Print *.jpg file if true (default is false)
    Bool_t isEPSOn ;        // Print *.eps file if true (default is false)
    Bool_t isPSOn ;         // Print *.ps file if true (default is false)

    Int_t mYear ;           // year
    TString mProduction ;   // production
    TString mParticleName ; // particle name

    TFile* mInputEmbedding ; // Input ROOT file (embedding)
    TFile* mInputRealData ;  // Input ROOT file (real data)

    TString mOutputFigureDirectory ; // Figure directory (default is current directory)

    void open(const TString embeddingFile, const TString realDataFile) ; // open input files
    void print(const TCanvas& canvas, const TString name) const; // Print figures (png only by default)

    void setStyle() ;      // Set overall styles
    void setStyle(TH1* h); // Set font, title and label styles

    Int_t         getEntries() const ;    // Get number of events in the embedding file
    Bool_t        isDecay() const ;      // Decay daughters
    Int_t         getCategoryId(const Bool_t isEmbedding = kTRUE) const ; // Get category id
    Int_t         getNDaughters() const ; // Get number of daughters (if 0, return 1)
    TObject*      getHistogram(const TString name, const Int_t daughter, const Bool_t isEmbedding=kTRUE); // Get histograms in embedding file
    Double_t      getNormalization(const TH1& h) const ; // (1/Ntrk) * (1/bin)
    const Char_t* getBaseName() const ; // Get ${year}_${production}_${particle name}
    Bool_t        drawStatistics(const Double_t x1=0.1, const Double_t y1=0.2, 
        const Double_t x2=0.9, const Double_t y2=0.8,
        const Double_t textSize = 0.05); // Number of events, year, production and particle name

    Bool_t drawdEdxVsMomentum(const Bool_t isMcMomentum=kTRUE) ; // dE/dx vs p, projections, mean/sigma vs momentum

    Bool_t drawProjection2D(const TString name); // (pseudo-)rapidity, momentum, pt
    Bool_t drawProjection3D(const TString name); // For dca and nhit projections in (pt, eta) space

    ClassDef(StEmbeddingQADraw, 1)
};

#endif


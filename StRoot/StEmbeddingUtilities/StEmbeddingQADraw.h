

#ifndef __StEmbeddingQADraw_h__
#define __StEmbeddingQADraw_h__

class TCanvas ;
class TH1 ;
class TFile ;
class TObject ;
#include "TString.h"

//____________________________________________________________________________________________________
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
class StEmbeddingQADraw {
  public:
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile);  // input embedding file name made by StEmbeddingQAAnalyzer
    StEmbeddingQADraw(const TString embeddingFile, const TString realDataFile,   // specify year, production and particle name
        const Int_t year, const TString production, const TString particleName); // if you put you own input embedding file name
    virtual ~StEmbeddingQADraw();

    void SetOutputDirectory(const TString name = "./"); // default is current directory

    Bool_t Draw();        // Draw all histograms (event and track histograms).
    Bool_t DrawEvent();   // Draw event-wise histograms

    // Track-wise
    Bool_t DrawMcTrack();        // Draw MC track histograms
    Bool_t DrawTrack();          // Draw Reconstructed track histograms
    Bool_t DrawGeantId();        // Geant id
    Bool_t DrawRapidity();       // (pseudo-)rapidity
    Bool_t DrawMomentumAndPt();  // momentum and pt
    Bool_t DrawdEdx();           // dE/dx (2D) and projections
    Bool_t DrawDca();            // Dca vs (pt, eta)
    Bool_t DrawNHit();           // NHit vs (pt, eta)

  private:
    Bool_t isGIFOn ;        // Print *.gif file if true (default is false)
    Bool_t isJPGOn ;        // Print *.jpg file if true (default is false)
    Bool_t isEPSOn ;        // Print *.eps file if true (default is false)
    Bool_t isPSOn ;         // Print *.ps file if true (default is false)

    Int_t kYear ;           // year
    TString kProduction ;   // production
    TString kParticleName ; // particle name

    TFile* mInputEmbedding ; // Input ROOT file (embedding)
    TFile* mInputRealData ;  // Input ROOT file (real data)

    TString mOutputFigureDirectory ; // Figure directory (default is current directory)

    void Open(const TString embeddingFile, const TString realDataFile) ; // open input files
    void Print(const TCanvas& canvas, const TString name); // Print figures (png only by default)

    void SetStyle() ;      // Set overall styles
    void SetStyle(TH1* h); // Set font, title and label styles

    Int_t         GetEntries() const ;    // Get number of events in the embedding file
    Bool_t        IsDecay() const ;      // Decay daughters
    Int_t         GetCategoryId(const Bool_t isEmbedding = kTRUE) const ; // Get category id
    Int_t         GetNDaughters() const ; // Get number of daughters (if 0, return 1)
    TObject*      GetHistogram(const TString name, const Int_t daughter, const Bool_t isEmbedding=kTRUE); // Get histograms in embedding file
    Double_t      GetNormalization(const TH1& h) const ; // (1/Ntrk) * (1/bin)
    const Char_t* GetBaseName() const ; // Get ${year}_${production}_${particle name}
    Bool_t        DrawStatistics(const Double_t x1=0.1, const Double_t y1=0.2, 
        const Double_t x2=0.9, const Double_t y2=0.8,
        const Double_t textSize = 0.05); // Number of events, year, production and particle name

    Bool_t DrawProjection3D(const TString name); // For dca and nhit projections in (pt, eta) space

    ClassDef(StEmbeddingQADraw, 1)
};

#endif


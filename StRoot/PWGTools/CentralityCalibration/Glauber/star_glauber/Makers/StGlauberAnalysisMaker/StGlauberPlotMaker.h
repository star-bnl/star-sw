/******************************************************************************
 * $Id: StGlauberPlotMaker.h,v 1.2 2012/04/25 05:02:16 hmasui Exp $
 * $Log: StGlauberPlotMaker.h,v $
 * Revision 1.2  2012/04/25 05:02:16  hmasui
 * 5% increment for centrality bins. Added 3rd harmonic eccentricity
 *
******************************************************************************/

#ifndef __StGlauberPlotMaker_h__
#define __StGlauberPlotMaker_h__

class TGraph ;
class TGraphErrors ;
#include <vector>
#include "TString.h"

//____________________________________________________________________________________________________
// Class StGlauberPlotMaker: Plot average quantity as a function of centrality and evaluate systematic error
class StGlauberPlotMaker {
  public:
    StGlauberPlotMaker(const TString name="Npart"); /// Default constructor
    virtual ~StGlauberPlotMaker(); /// Default destructor

    /// Read text file to get graphs
    Int_t Read(const TString filename, const TString type) ;

    /// Draw and evaluate systematic errors. Write table in the current directory
    /// table name will be: table_{mName}_vs_centrality_systematicerror.txt
    /// mode
    /// 0:    evaluate errors by assuming flat probability distribution (error = RMS/sqrt(12)) (default)
    /// 1:    evaluate errors from maximum difference between the data points
    void Draw(const UInt_t mode = 0) ;

  private:
    // Functions
    Double_t GetYMinimum() const ; /// Y-axis minimum
    Double_t GetYMaximum() const ; /// Y-axis maximum
    TString GetYTitle() const    ; /// Y-axis title

    /// Divide g0 by g1
    TGraphErrors* Divide(const TGraphErrors& g0, const TGraphErrors& g1) const ;

    /// Evaluate sysetmatic errors from all graphs, return systeamtic error vs centrality (graph)
    TGraphErrors* SystematicErrors(const UInt_t mode) ;

    // Data members
    enum {
      mNCentrality = 16  // 16 centrality bins in 0-80% (5% increment)
    };
    const TString mName                   ; /// Name of average quantity
    std::vector<TGraphErrors*> mGraph     ; /// Collection of graphs (all centrality)
    std::vector<TGraphErrors*> mGraphDraw ; /// Collection of graphs (0-80% for drawing)

    TGraph* mSystematicError        ; /// Systematic error graph (not for draw)
    static UInt_t mCanvasId ; /// Unique canvas id
    static UInt_t mGraphId ;  /// Unique graph id

    ClassDef(StGlauberPlotMaker, 0)
};
#endif


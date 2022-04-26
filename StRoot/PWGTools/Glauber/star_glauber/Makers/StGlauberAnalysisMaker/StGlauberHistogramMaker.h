/******************************************************************************
 * $Id: StGlauberHistogramMaker.h,v 1.2 2012/04/25 05:03:06 hmasui Exp $
 * $Log: StGlauberHistogramMaker.h,v $
 * Revision 1.2  2012/04/25 05:03:06  hmasui
 * Use StCentralityMaker. Added weight for 2D fill. Use STAR logger instead of iostream
 *
******************************************************************************/

#ifndef __StGlauberHistogramMaker_h__
#define __StGlauberHistogramMaker_h__

class TH1 ;
class TH2 ;
class TProfile ;
class StGlauberTree ;
class StCentralityMaker;
#include "TString.h"
#include <vector>

//____________________________________________________________________________________________________
// Class StGlauberHistogramMaker: Histogram maker
class StGlauberHistogramMaker {
  public:
    StGlauberHistogramMaker() ; /// Default constructor

    // Fixed bin width
    StGlauberHistogramMaker(const TString name, 
        const TString title, const TString ytitle,
        const Int_t ybin, const Double_t ymin, const Double_t ymax,
        const Bool_t isUnitWeight=kTRUE);

    ~StGlauberHistogramMaker() ; /// Default destructor

    /// Set directory for output table (default is current directory)
    void SetTableDirectory(const TString directory) ;

    virtual void Init();   /// Initialize histograms

    void SetXaxis(const StGlauberTree& tree, const StCentralityMaker& centralityMaker, const TString type); /// Set X-axis variable
    virtual void Fill(const Double_t y, const Double_t weight); /// Fill histogram 'y' value with 'weight'

    /// Do correction for weight, and write table in the current directory
    /// table name will be: table_{mName}_vs_centrality.txt
    /// table contains
    /// <centrality bin>  <minimum centrality>  <maximum centrality>  <value>  <stat. error>
    virtual void Finish(const TString type) ;

    const TString GetName() const ; /// Get histogram name
    void DebugOn() ; /// Print debug messages

  private:
    // Data members

    const TString mName             ; /// Histogram name
    const TString mTitle            ; /// Histogram title
    const TString mYTitle           ; /// y-axis title
    const Int_t mYbin               ; /// Histogram bin in y-axis
    const Double_t mYmin            ; /// Histogram minimum value in y-axis
    const Double_t mYmax            ; /// Histogram maximum value in y-axis
    const Bool_t mIsUnitWeight      ; /// true:unit weight, false:some weight

    enum {
      mNXaxis = 4 /// Number of x-axis
    };

    static const TString mXAxisName[mNXaxis]  ; /// x-axis name
    static const TString mXAxisTitle[mNXaxis] ; /// x-axis title
    static const Int_t mXAxisBin[mNXaxis]     ; /// x-axis bin
    static const Double_t mXAxisMax[mNXaxis]  ; /// x-axis maximum (minimum is 0)

    std::vector<Double_t> mXaxis    ; /// x-axis definition

    std::vector<TH1*> mHistogram1D  ; /// 1D histogram collection (Profile/Weight)
    std::vector<TH2*> mHistogram2D  ; /// 2D histogram collection
    std::vector<TProfile*> mProfile ; /// Profile histogram collection
    std::vector<TProfile*> mWeight  ; /// Weight histogram

    TString mTableDirectory ; /// Output directory for table

    UInt_t mDebug ; /// Debug flag

  protected:
    // Functions
    virtual void Reset();  /// Reset all data members

    Bool_t IsXaxisOk() const  ; /// Check xAxis has been filled, abort if empty
    void Fill2D(std::vector<TH2*> collection, const Double_t y, const Double_t weight) ; 
    void FillProfile(std::vector<TProfile*> collection, const Double_t y) ;
    void Fill(const TString type, const Double_t y, const Double_t weight) ;

    /// Calculate sum(w*val)/sum(w) for each profile histogram
    void DoWeightCorrection(std::vector<TH1*> collection1d, std::vector<TProfile*> collectionp) ;

    /// Write text table, average quantity vs centrality (table: table_{mName}_vs_centrality.txt)
    void WriteTable(std::vector<TH1*> collection, const TString name) ;

    /// Write TGraphErrors (vs centrality, 0-80%)
    void WriteGraphs(std::vector<TH1*> collection) ;

    const TString GetHistogramName(const TString name, const UInt_t ix) const ; // histogram name
    TH1* GetTH1D(const TString name, const UInt_t ix)                         ; // 1D histogram
    TH2* GetTH2D(const TString name, const UInt_t ix)                         ; // 2D histogram
    TProfile* GetTProfile(const TString name, const UInt_t ix, const TString title="Profile") ; // Profile histogram

    /// Naxis
    UInt_t GetNXaxis() const ;

    ClassDef(StGlauberHistogramMaker, 0)
};

inline UInt_t StGlauberHistogramMaker::GetNXaxis() const { return mNXaxis ; }

#endif


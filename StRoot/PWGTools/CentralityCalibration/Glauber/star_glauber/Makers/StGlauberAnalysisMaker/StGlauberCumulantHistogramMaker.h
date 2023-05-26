/******************************************************************************
 * $Id: StGlauberCumulantHistogramMaker.h,v 1.2 2012/04/25 05:03:31 hmasui Exp $
 * $Log: StGlauberCumulantHistogramMaker.h,v $
 * Revision 1.2  2012/04/25 05:03:31  hmasui
 * Use STAR logger
 *
******************************************************************************/

#ifndef __StGlauberCumulantHistogramMaker_h__
#define __StGlauberCumulantHistogramMaker_h__

#include <vector>
#include "StGlauberHistogramMaker.h"

//____________________________________________________________________________________________________
// Class StGlauberCumulantHistogramMaker: Cumulant histogram maker, mainly for eccentricity
class StGlauberCumulantHistogramMaker : public StGlauberHistogramMaker {
  public:
    // Fixed bin width, make histograms up to 6th order cumulant
    StGlauberCumulantHistogramMaker(const TString name, 
        const TString title, const TString ytitle,
        const Int_t ybin, const Double_t ymin, const Double_t ymax,
        const Bool_t isUnitWeight=kTRUE);

    ~StGlauberCumulantHistogramMaker() ; /// Default destructor

    void Init() ;
    void Fill(const Double_t y, const Double_t weight) ;

    /// Do correction for weight, and write table in the current directory
    /// and calculate cumulants of the stored quantity
    /// table name will be: table_{mName}_vs_centrality.txt
    /// table contains
    /// <centrality bin>  <minimum centrality>  <maximum centrality>  <value>  <stat. error>
    void Finish(const TString type) ;

  private:
    // Functions
    void Reset() ;

    UInt_t   GetOrder(const UInt_t io) const ;
    Double_t Get4thOrderCumulant(const Double_t c2, const Double_t c4) const ;
    Double_t Get6thOrderCumulant(const Double_t c2, const Double_t c4, const Double_t c6) const ;

    Double_t GetNthOrderCumulantError(const Double_t order, const Double_t val, const Double_t err) const ;
    Double_t Get2ndOrderCumulantError(const Double_t c2, const Double_t c2error) const ;
    Double_t Get4thOrderCumulantError(const Double_t c2, const Double_t c4, const Double_t c2error, const Double_t c4error) const ;
    Double_t Get6thOrderCumulantError(const Double_t c2, const Double_t c4, const Double_t c6,
        const Double_t c2error, const Double_t c4error, const Double_t c6error) const ;
    
    Double_t GetCumulant(const UInt_t order, const Double_t* val) const ; /// Get n-th order (mOrder) cumulant of val
    Double_t GetCumulantError(const UInt_t order, const Double_t* val, const Double_t* err) const ; /// Get n-th order (mOrder) cumulant error

    // Data members
    enum {
      mNOrder = 3  /// Up to 6-th order cumulant
    };

    std::vector<TProfile*> mProfileCumulant[mNOrder] ; /// Cumulant profile histogram
    std::vector<TH1*> mHistogram1DCumulant[mNOrder]  ; /// Cumulant 1D histogram

    ClassDef(StGlauberCumulantHistogramMaker, 0)
};
#endif


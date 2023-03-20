/******************************************************************************
 * $Id: StNegativeBinomial.h,v 1.2 2012/04/25 05:13:24 hmasui Exp $
 * $Log: StNegativeBinomial.h,v $
 * Revision 1.2  2012/04/25 05:13:24  hmasui
 * Use STAR logger. Take into account additional constant inefficiency by using the trigger bias parameter
 *
******************************************************************************/

#ifndef __StNegativeBinomial_h__
#define __StNegativeBinomial_h__

class TH1 ;
#include "Rtypes.h"

//____________________________________________________________________________________________________
//  StNegativeBinomial class: Generate negative binomial distribution
class StNegativeBinomial {
  public:
    /// Default constructor
    //  npp: mean multiplicity in p+p
    //  k:   1/k deviation from poisson (1/k -> 0 poisson)
    //  x:   fraction of hard component
    //  efficiency: detector efficiency
    //  triggerbias: trigger bias
    //  IsConstEfficiency: true-> constant efficiencty, false-> multiplicity dependent efficiency
    StNegativeBinomial(const Double_t npp = 2.38, const Double_t k = 2.00, const Double_t x = 0.13,
        const Double_t efficiency = 1.0, const Double_t triggerbias = 1.0, const Bool_t isConstEfficiency=kTRUE);

    /// Default destructor
    virtual ~StNegativeBinomial();

    /// (1-x)*npart/2 + x*ncoll
    Double_t GetTwoComponentMultiplicity(const Double_t npart, const Double_t ncoll) const ;

    /// Get multiplcity by convoluting NBD
    Int_t GetMultiplicity(const Double_t npart, const Double_t ncoll) const ;

    /// Get multiplicity by scaled NBD, with npp*mult and k*mult
    TH1* GetMultiplicity(const Double_t npart, const Double_t ncoll,
        const Double_t weight) const ;

    Double_t GetNegativeBinomial(const Int_t n) const ; /// Get NBD(npp, k; n)
    Double_t GetNegativeBinomial(const Int_t n, const Double_t m) const ; /// Get NBD(npp*m, k*m; n)

    /// Set (npp, k, x)
    void SetParameters(const Double_t npp, const Double_t k, const Double_t x = -1.0)  ;

    // Getter
    Double_t GetNpp() const ; /// Get npp parameter
    Double_t GetK()   const ; /// Get k parameter
    Double_t GetX()   const ; /// Get x parameter
    Double_t GetEfficiency() const ; /// Get mEfficiency (CAUTION: value has different meaning between constant and multiplicity dep. modes)
    Bool_t IsConstEfficiency()   const ; /// Get flag for efficiency
    void DrawNbd() const ;    /// Draw nbd distribution

  private:
    // Functions
    Double_t GetEfficiency(const Int_t mult) const ;
    void InitHistogram() ; /// Initialize histogram (mhNbd)

    // Data members
    const Double_t mEfficiency      ; /// Efficiency
    const Double_t mTriggerBias     ; /// Trigger bias
    Double_t mNpp                   ; /// Mean multiplicity in p+p
    Double_t mK                     ; /// 1/k deviation from poisson
    Double_t mX                     ; /// Fraction of hard component
    const Bool_t mIsConstEfficiency ; /// Flag for efficiency
    static UInt_t mCounter          ; /// Unique histogram id for StNegativeBinomial

    TH1* mhNbd    ; /// Histogram to store negative binomial distribution

    ClassDef(StNegativeBinomial, 0)
};

inline Double_t StNegativeBinomial::GetNpp() const { return mNpp ; }
inline Double_t StNegativeBinomial::GetK()  const { return mK ; }
inline Double_t StNegativeBinomial::GetX()  const { return mX ; }
inline Double_t StNegativeBinomial::GetEfficiency() const { return mEfficiency ; }
inline Bool_t StNegativeBinomial::IsConstEfficiency() const { return mIsConstEfficiency ; }

#endif


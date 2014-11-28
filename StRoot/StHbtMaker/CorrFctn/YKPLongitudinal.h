/***************************************************************************
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description:
 *   Do YKP parametrization in longitudinal direction in kt-Y-bins
 *
 *
 **************************************************************************/

#ifndef YKPLongitudinal_hh
#define YKPLongitudinal_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
#include "StHbtMaker/Base/StHbtPairCut.h"

class YKPLongitudinal : public StHbtCorrFctn {

 public:
    // constructors
    YKPLongitudinal(TString ctype, TString frame, const int& nbins, const float& QLo, const float& QHi,
		    const int& nKtbins, const double& ktMin, const double& ktMax,
		    const int& nYbins, const double& YMin, const double& YMax,
		    const int& nbinsQINV = 100, const float& QLoQINV = 0.0, const float& QHiQINV = 0.2 ) ;
    virtual ~YKPLongitudinal() ;
    // mandatory report
    virtual StHbtString Report() ;
    // mandatory : filling histos
    virtual void AddRealPair(const StHbtPair*) ;
    virtual void AddMixedPair(const StHbtPair*) ;
    // mandatory : called at end
    virtual void Finish() ;

    // get histos
    StHbt3DHisto* Numerator(const int ktBin, const int yBin ) ;
    StHbt3DHisto* Denominator(const int ktBin, const int yBin ) ;
    StHbt3DHisto* Ratio(const int ktBin, const int yBin ) ;
    StHbt1DHisto* QinvNumerator(const int ktBin, const int yBin ) ;
    StHbt1DHisto* QinvDenominator(const int ktBin, const int yBin ) ;
    StHbt1DHisto* QinvRatio(const int ktBin, const int yBin ) ;

    // here are get and set for the range over which the correlation function 
    // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
    // by default, but the Set's below override this
    // note : there are other ways to get the normalization
    void SetNormRangeLo(float qLo) ;
    void SetNormRangeHi(float qHi) ;
    float GetNormRangeLo() ;
    float GetNormRangeHi() ;
    double GetNorm(const int ktBin, const int yBin ) ; 

    // connect to the coulomb correction object
    void SetCoulombCorrection(StHbtCoulomb* Correction) ;
        

 private:
    // the histo arrays
    StHbt3DHisto* mNumerator ;
    StHbt3DHisto* mDenominator ;
    StHbt3DHisto* mRatio ;
    StHbt1DHisto* mQinvNumerator ;
    StHbt1DHisto* mQinvDenominator ;
    StHbt1DHisto* mQinvRatio ;
    // the kt Y ranges
    int mNumberKtBins ;
    int mNumberYBins ;
    int mNumberBins ;
    double*  mktBinsMin ;
    double*  mktBinsMax ;
    double*  mYBinsMin ;
    double*  mYBinsMax ;

    // the frame we calculate the correlationfuntion in
    TString mFrame ;
    TString mCtype ;

    // upper and lower bounds of Qinv region where to do normalization
    float mQinvNormLo ;
    float mQinvNormHi ;
    
    // and here are the number of pairs in that region...
    unsigned long int* mNumRealsNorm ;
    unsigned long int* mNumMixedNorm ;
    
    // coulomb correction
    StHbtCoulomb* mCorrection; //!
    
    
#ifdef __ROOT__
    ClassDef(YKPLongitudinal, 1)
#endif
};

// histos
inline  StHbt3DHisto* YKPLongitudinal::Numerator(const int ktBin, const int yBin)            {return &mNumerator[(ktBin-1)+(yBin-1)*mNumberKtBins];}
inline  StHbt3DHisto* YKPLongitudinal::Denominator(const int ktBin, const int yBin)        {return &mDenominator[(ktBin-1)+(yBin-1)*mNumberKtBins];}
inline  StHbt3DHisto* YKPLongitudinal::Ratio(const int ktBin, const int yBin)                    {return &mRatio[(ktBin-1)+(yBin-1)*mNumberKtBins];}
inline  StHbt1DHisto* YKPLongitudinal::QinvNumerator(const int ktBin, const int yBin)    {return &mQinvNumerator[(ktBin-1)+(yBin-1)*mNumberKtBins];}
inline  StHbt1DHisto* YKPLongitudinal::QinvDenominator(const int ktBin, const int yBin){return &mQinvDenominator[(ktBin-1)+(yBin-1)*mNumberKtBins];}
inline  StHbt1DHisto* YKPLongitudinal::QinvRatio(const int ktBin, const int yBin)            {return &mQinvRatio[(ktBin-1)+(yBin-1)*mNumberKtBins];}

// ranges
inline  void YKPLongitudinal::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void YKPLongitudinal::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float YKPLongitudinal::GetNormRangeLo(){return mQinvNormLo;}
inline  float YKPLongitudinal::GetNormRangeHi(){return mQinvNormHi;}
inline  double YKPLongitudinal::GetNorm(const int ktBin, const int yBin){return (double)mNumRealsNorm[ktBin+yBin*mNumberKtBins]/(double)mNumMixedNorm[ktBin+yBin*mNumberKtBins];}
inline  void YKPLongitudinal::SetCoulombCorrection(StHbtCoulomb* Correction){mCorrection = Correction;}
#endif

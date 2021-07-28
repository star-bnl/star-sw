#ifndef QinvEbyECorrFctn_hh
#define QinvEbyECorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTagWriter.hh"

struct twoFit {
    float lambda;
    float lambdaErr;
    float radius;
    float radiusErr;
    float chi2;
    float ndf;
};

struct threeFit {
    float constant;
    float constantErr;
    float lambda;
    float lambdaErr;
    float radius;
    float radiusErr;
    float chi2;
    float ndf;
};

class QinvEbyECorrFctn : public StHbtCorrFctn {
public:

  // Basics
  QinvEbyECorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QinvEbyECorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  virtual void EventEnd(const StHbtEvent*);
  virtual void EventBegin(const StHbtEvent*);
  virtual void Finish();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* Denominator();
  StHbt1DHisto* Ratio();
  StHbt1DHisto* Correction();
  
  // 'Integrated over all events' histos
  StHbt1DHisto* IntNumerator();
  StHbt1DHisto* IntDenominator();
  StHbt1DHisto* IntRatio();
  
  // EbyE histos
  StHbt1DHisto* threeFitLambda();  // Histogram Radius out of 3 param fit 
  StHbt1DHisto* threeFitRadius();  // Histogram Lambda out of 3 param fit 
  StHbt1DHisto* twoFitLambda();    // Histogram Lambda out of 2 param fit
  StHbt1DHisto* twoFitRadius();    // Histogram Radius out of 2 param fit 
  StHbt1DHisto* rmsByHandMeV();    // Histogram width of bump in Mev
  StHbt1DHisto* rmsByHandFm();     // Histogram width of bump in fm 

  // Extract Parameters
  void Rms_by_hand(double& Width_MeV, double& Width_fm) ;     // Calculate rms of the bump 
  void Three_param_fit(threeFit&,StHbt1DHisto*); // Fit correlation function with 3 params
  void Two_param_fit(twoFit&) ;   // Fit correlation function after normalization with 2 params
  
  // Normalization
  double Norm_by_integral(double,double) ; // Norm correlation function by using integral of background and signal
  double Norm_by_fit(double,double) ; // Norm correlation function by fitting flat part of the correlation function
  
  // Fill histos artificially
  void Fill_ratio_artificial();
  void Fill_ratio_artificial_random();
 
  // TagDb
  void SetTagMeans( const char* );
  void SetTagSigmas( const char* );

  // Stuff
  void SetCorrection( const StHbt1DHisto* );

private:
  char* mTagMeans;
  char* mTagSigmas;

  // Basics
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;
  StHbt1DHisto* mCorrection;

  // 'Integrated over all events' histos
  StHbt1DHisto* mIntNumerator;
  StHbt1DHisto* mIntDenominator;
  StHbt1DHisto* mIntRatio;

  // EbyE histos
  StHbt1DHisto* mThreeFitLambda; 
  StHbt1DHisto* mThreeFitRadius; 
  StHbt1DHisto* mTwoFitLambda; 
  StHbt1DHisto* mTwoFitRadius; 
  StHbt1DHisto* mRmsByHandMeV;
  StHbt1DHisto* mRmsByHandFm;
  
  // Debug
  int m_Debug_ebye ; // Set (1) or Unset (0) debug option 

  //StHbtTagWriter* mTagWriter ;  //! <-- this is a singleton

#ifdef __ROOT__
  ClassDef(QinvEbyECorrFctn, 0)
#endif
};

// Return all histos by inline
inline  void QinvEbyECorrFctn::SetTagMeans( const char* c){ mTagMeans=(char*)c;}
inline  void QinvEbyECorrFctn::SetTagSigmas( const char* c){ mTagSigmas=(char*)c;}
// Basics
inline  StHbt1DHisto* QinvEbyECorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Ratio(){return mRatio;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Correction(){return mCorrection;}
// 'Integrated over all events' histos
inline  StHbt1DHisto* QinvEbyECorrFctn::IntNumerator(){return mIntNumerator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::IntDenominator(){return mIntDenominator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::IntRatio(){return mIntRatio;}
// Ebye Parameters
inline  StHbt1DHisto* QinvEbyECorrFctn::threeFitLambda(){return mThreeFitLambda;}
inline  StHbt1DHisto* QinvEbyECorrFctn::threeFitRadius(){return mThreeFitRadius;}
inline  StHbt1DHisto* QinvEbyECorrFctn::twoFitLambda(){return mTwoFitLambda;}
inline  StHbt1DHisto* QinvEbyECorrFctn::twoFitRadius(){return mTwoFitRadius;}
inline  StHbt1DHisto* QinvEbyECorrFctn::rmsByHandMeV(){return mRmsByHandMeV;}
inline  StHbt1DHisto* QinvEbyECorrFctn::rmsByHandFm(){return mRmsByHandFm;}

#endif

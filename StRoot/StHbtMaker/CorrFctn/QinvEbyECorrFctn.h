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

  

  ////
  // ebye stuff
  ////
  StHbt1DHisto* threeFitLambda();  // Histogram Radius out of 3 param fit 
  StHbt1DHisto* threeFitRadius();  // Histogram Lambda out of 3 param fit 
  StHbt1DHisto* twoFitLambda();    // Histogram Lambda out of 2 param fit
  StHbt1DHisto* twoFitRadius();    // Histogram Radius out of 2 param fit 
  StHbt1DHisto* rmsByHandMeV();    // Histogram width of bump in Mev
  StHbt1DHisto* rmsByHandFm();     // Histogram width of bump in fm 

  void Rms_by_hand(double& Width_MeV, double& Width_fm) ;     // Calculate rms of the bump 
  void Three_param_fit(threeFit&); // Fit correlation function with 3 params
  void Two_param_fit(twoFit&) ;   // Fit correlation function after normalization with 2 params
  
  double Norm_by_integral() ; // Norm correlation function by using integral of background and signal
  double Norm_by_fit() ; // Norm correlation function by fitting flat part of the correlation function
  
  void Fill_ratio_artificial();
  void Fill_ratio_artificial_random();
 
  void SetTagMeans( const char* );
  void SetTagSigmas( const char* );

private:
  char* mTagMeans;
  char* mTagSigmas;

  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;
  
  // EbyE histos
  StHbt1DHisto* mThreeFitLambda; 
  StHbt1DHisto* mThreeFitRadius; 
  StHbt1DHisto* mTwoFitLambda; 
  StHbt1DHisto* mTwoFitRadius; 
  StHbt1DHisto* mRmsByHandMeV;
  StHbt1DHisto* mRmsByHandFm;
  
  // Fit functions
  StHbtTF1* m_corrfunc_3param ; // 3 param fit to unnormalized correlation function
  StHbtTF1* m_corrfunc_2param ; // 2 param fit to normalized correlation function
  StHbtTF1* m_line ; // straight line fit to the flat part of the correlation function
 
  // Debug
  int m_Debug_ebye ; // Set (1) or Unset (0) debug option 

  StHbtTagWriter* mTagWriter ;  //! <-- this is a singleton

#ifdef __ROOT__
  ClassDef(QinvEbyECorrFctn, 0)
#endif
};

inline  void QinvEbyECorrFctn::SetTagMeans( const char* c){ mTagMeans=(char*)c;}
inline  void QinvEbyECorrFctn::SetTagSigmas( const char* c){ mTagSigmas=(char*)c;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvEbyECorrFctn::Ratio(){return mRatio;}

inline  StHbt1DHisto* QinvEbyECorrFctn::threeFitLambda(){return mThreeFitLambda;}
inline  StHbt1DHisto* QinvEbyECorrFctn::threeFitRadius(){return mThreeFitRadius;}
inline  StHbt1DHisto* QinvEbyECorrFctn::twoFitLambda(){return mTwoFitLambda;}
inline  StHbt1DHisto* QinvEbyECorrFctn::twoFitRadius(){return mTwoFitRadius;}
inline  StHbt1DHisto* QinvEbyECorrFctn::rmsByHandMeV(){return mRmsByHandMeV;}
inline  StHbt1DHisto* QinvEbyECorrFctn::rmsByHandFm(){return mRmsByHandFm;}

#endif

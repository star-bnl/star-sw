#ifndef StHbtV0Track_hh
#define StHbtV0Track_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtV0Track{
public:
  StHbtV0Track(){/* no-op*/};
  ~StHbtV0Track(){/* no-op*/};

  float NSigmaLambda() const;
  float NSigmaLambdaBar() const;
  float NSigmaK0() const;
  float NSigmaGamma() const;
  float DCAofTrackPair() const;
  float ImpactParameter() const;
  StHbtThreeVector P() const;
  StHbtThreeVector SecondaryVertex() const;

  void SetNSigmaLambda(const float&);
  void SetNSigmaLambdaBar(const float&);
  void SetNSigmaK0(const float&);
  void SetNSigmaGamma(const float&);
  void SetDCAofTrackPair(const float&);
  void SetImpactParameter(const float&);
  void SetP(const StHbtThreeVector&);
  void SetSecondaryVertex(const StHbtThreeVector&);


private:
  float mNSigmaLambda;
  float mNSigmaLambdaBar;
  float mNSigmaK0;
  float mNSigmaGamma;
  float mDCA;
  float mImpactParameter;
  StHbtThreeVector mP;
  StHbtThreeVector mV;
};

inline void StHbtV0Track::SetNSigmaLambda(const float& x){mNSigmaLambda = x;}
inline void StHbtV0Track::SetNSigmaLambdaBar(const float& x){
mNSigmaLambdaBar = x;}
inline void StHbtV0Track::SetNSigmaK0(const float& x){mNSigmaK0 = x;}
inline void StHbtV0Track::SetNSigmaGamma(const float& x){mNSigmaGamma = x;}
inline void StHbtV0Track::SetDCAofTrackPair(const float& x){mDCA = x;}
inline void StHbtV0Track::SetImpactParameter(const float& x){
mImpactParameter = x;}
inline void StHbtV0Track::SetP(const StHbtThreeVector& p){mP = p;}
inline void StHbtV0Track::SetSecondaryVertex(const StHbtThreeVector& p){
mV = p;} 

inline float StHbtV0Track::NSigmaLambda() const {return mNSigmaLambda;}
inline float StHbtV0Track::NSigmaLambdaBar() const {return mNSigmaLambdaBar;}
inline float StHbtV0Track::NSigmaK0() const {return mNSigmaK0;}
inline float StHbtV0Track::NSigmaGamma() const {return mNSigmaGamma;}
inline float StHbtV0Track::DCAofTrackPair() const {return mDCA;}
inline float StHbtV0Track::ImpactParameter() const {return mImpactParameter;}
inline StHbtThreeVector StHbtV0Track::P() const {return mP;}
inline StHbtThreeVector StHbtV0Track::SecondaryVertex() const {return mV;}

#endif

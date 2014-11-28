/***************************************************************************
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple 2-D Qinvtor-QualityFactor correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef QinvQualAvSepCorrFctn_hh
#define QinvQualAvSepCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class QinvQualAvSepCorrFctn : public StHbtCorrFctn {
public:
  QinvQualAvSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		   const int& nbinQual, const float& QualLo, const float& QualHi,
		   const int& nbinSep, const float& SepLo, const float& SepHi);
  virtual ~QinvQualAvSepCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt3DHisto* Numerator3D();
  StHbt3DHisto* Denominator3D();
  StHbt3DHisto* Ratio3D();

private:

  StHbt3DHisto* mNumerator3D;
  StHbt3DHisto* mDenominator3D;
  StHbt3DHisto* mRatio3D;

#ifdef __ROOT__
  ClassDef(QinvQualAvSepCorrFctn, 1)
#endif
};

inline  StHbt3DHisto* QinvQualAvSepCorrFctn::Numerator3D(){return mNumerator3D;}
inline  StHbt3DHisto* QinvQualAvSepCorrFctn::Denominator3D(){return mDenominator3D;}
inline  StHbt3DHisto* QinvQualAvSepCorrFctn::Ratio3D(){return mRatio3D;}


#endif


/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple 2-D Qvector-QualityFactor correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef QvecQualCorrFctn_hh
#define QvecQualCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class QvecQualCorrFctn : public StHbtCorrFctn {
public:
  QvecQualCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinQual, const float& QualLo, const float& QualHi);
  virtual ~QvecQualCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt2DHisto* Numerator2D();
  StHbt2DHisto* Denominator2D();
  StHbt2DHisto* Ratio2D();

private:

  StHbt2DHisto* mNumerator2D;
  StHbt2DHisto* mDenominator2D;
  StHbt2DHisto* mRatio2D;

#ifdef __ROOT__
  ClassDef(QvecQualCorrFctn, 1)
#endif
};

inline  StHbt2DHisto* QvecQualCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* QvecQualCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* QvecQualCorrFctn::Ratio2D(){return mRatio2D;}


#endif


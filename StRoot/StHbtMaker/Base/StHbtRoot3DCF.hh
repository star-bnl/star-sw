/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : same as StHbtRoot1DCF, but for 3D Corr Fctn
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/
#ifndef StHbtRoot3DCF_hh
#define StHbtRoot3DCF_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtNamed.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtRoot3DCF : public virtual StHbtCorrFctn, public virtual StHbtNamed {

public:


  StHbtRoot3DCF(char* aTitle, int aNBinsx, double aHLox, double aHHix,
		int aNBinsy, double aHLoy, double aHHiy,
		int aNBinsz, double aHLoz, double aHHiz);

  StHbtRoot3DCF(const StHbtRoot3DCF&);

  virtual ~StHbtRoot3DCF()  ;

  virtual void Finish();
  virtual StHbtString Report();

  virtual void SetName( const char* aName);

  virtual StHbt3DHisto* Numerator() const ;
  virtual StHbt3DHisto* Denominator() const ;
  virtual StHbt3DHisto* Ratio() const ;
  virtual void Write() ;



protected:
  double mHLo;
  double mHHi;
  double mHLoY;
  double mHHiY;
  double mHLoZ;
  double mHHiZ;

  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;


  StHbtRoot3DCF(): StHbtCorrFctn(),StHbtNamed(),mHLo(0),mHHi(0),mHLoY(0),mHHiY(0),mHLoZ(0),mHHiZ(0),mNumerator(0),mDenominator(0),mRatio(0){};

#ifdef __ROOT__
ClassDef(StHbtRoot3DCF,1)
#endif
};
inline StHbt3DHisto* StHbtRoot3DCF::Numerator() const {return mNumerator;};
inline StHbt3DHisto* StHbtRoot3DCF::Denominator() const {return mDenominator;};
inline StHbt3DHisto* StHbtRoot3DCF::Ratio() const {return mRatio;};
inline void StHbtRoot3DCF::Write()  {mNumerator->Write();mDenominator->Write();mRatio->Write();};

#endif

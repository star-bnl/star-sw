/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : same as StHbtRoot1DCF, but for 2D Corr Fctn
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/
#ifndef StHbtRoot2DCF_hh
#define StHbtRoot2DCF_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtNamed.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtRoot2DCF : public virtual StHbtCorrFctn, public virtual StHbtNamed {

public:


  StHbtRoot2DCF(char* aTitle, int aNBinsx, double aHLox, double aHHix,
		int aNBinsy, double aHLoy, double aHHiy);

  StHbtRoot2DCF(const StHbtRoot2DCF&);

  virtual ~StHbtRoot2DCF()  ;

  virtual void Finish();
  virtual StHbtString Report();

  virtual void SetName( const char* aName);

  virtual StHbt2DHisto* Numerator() const ;
  virtual StHbt2DHisto* Denominator() const ;
  virtual StHbt2DHisto* Ratio() const ;
  virtual void Write() ;



protected:
  double mHLo;
  double mHHi;
  double mHLoY;
  double mHHiY;

  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mDenominator;
  StHbt2DHisto* mRatio;


  StHbtRoot2DCF(): StHbtCorrFctn(),StHbtNamed(),mHLo(0),mHHi(0),mHLoY(0),mHHiY(0),mNumerator(0),mDenominator(0),mRatio(0){};

#ifdef __ROOT__
ClassDef(StHbtRoot2DCF,1)
#endif
};
inline StHbt2DHisto* StHbtRoot2DCF::Numerator() const {return mNumerator;};
inline StHbt2DHisto* StHbtRoot2DCF::Denominator() const {return mDenominator;};
inline StHbt2DHisto* StHbtRoot2DCF::Ratio() const {return mRatio;};
inline void StHbtRoot2DCF::Write()  {mNumerator->Write();mDenominator->Write();mRatio->Write();};

#endif

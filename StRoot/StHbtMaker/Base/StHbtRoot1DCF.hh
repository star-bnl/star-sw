/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : pure virtual class to manage specific Root part of 1D
 * CorrFctn. to write one new Corr Fctn, user shoud write a class which
 * inherit from StHbtRoot1DCD. then he just have
 * to write AddRealPair and AddMixedPair.
 * if user want to write a ThCorrFctn, class shoul ineherit virtually from
 * both StHbtThCorrFctn and StHbtRoot1DCF
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/
#ifndef StHbtRoot1DCF_hh
#define StHbtRoot1DCF_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtNamed.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtRoot1DCF : public virtual StHbtCorrFctn, public virtual StHbtNamed{

public:
// --- Constructor

  StHbtRoot1DCF(char* aTitle, int aNBins, 
	     double aHLo, double aHHi);
  StHbtRoot1DCF (const StHbtRoot1DCF&);

  virtual ~StHbtRoot1DCF();

  virtual void Finish();
  virtual StHbtString Report();

  virtual void SetName( const char* aName);

  virtual StHbt1DHisto* Numerator() const ;
  virtual StHbt1DHisto* Denominator() const ;
  virtual StHbt1DHisto* Ratio() const ;
  virtual void Write() ;


protected:

  StHbtRoot1DCF() : StHbtCorrFctn(),StHbtNamed(),mHLo(0),mHHi(0),mNumerator(0),mDenominator(0),mRatio(0){ };

  double mHLo;
  double mHHi;

  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;

#ifdef __ROOT__
ClassDef(StHbtRoot1DCF,1)
#endif
};

inline StHbt1DHisto* StHbtRoot1DCF::Numerator() const { cout << "Return Ratio" << endl; return mNumerator;};
inline StHbt1DHisto* StHbtRoot1DCF::Denominator() const {cout << "Return Denimnatior" << endl; return mDenominator;};
inline StHbt1DHisto* StHbtRoot1DCF::Ratio() const {cout << "Return Numerator" << endl; return mRatio;};
inline void StHbtRoot1DCF::Write()  {mNumerator->Write();mDenominator->Write();mRatio->Write();};


#endif

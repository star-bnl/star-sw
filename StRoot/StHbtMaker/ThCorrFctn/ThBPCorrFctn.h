/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Calculate the theoretical QInv correlation function 
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ThBPCorrFctn_hh
#define ThBPCorrFctn_hh

#include "StHbtMaker/Base/StHbtThCorrFctn.hh"

class StHbtThPair;

class ThBPCorrFctn :  public virtual StHbtThCorrFctn {
 public:
  ThBPCorrFctn(char* aTitle, int aNBins, 
	       double aHLo, double aHHi, int addHistos);
  ThBPCorrFctn(const ThBPCorrFctn& ThCf);
  
  virtual ~ThBPCorrFctn();

  void AddNum(StHbtThPair*);
  void AddDen(StHbtThPair*);
  
  StHbtThCorrFctn* ThClone() const ;

  virtual StHbt3DHisto* Numerator() const ;
  virtual StHbt3DHisto* Denominator() const ;
  virtual StHbt3DHisto* Ratio() const ;
  virtual void Write() ;
  virtual void Finish();
  StHbtString Report();

 private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;
  StHbt3DHisto* mQinvHisto;

  // upper and lower bounds of Qinv region where to do normalization
  float mQinvNormLo;
  float mQinvNormHi;

  // and here are the number of pairs in that region...
  unsigned long int mNumRealsNorm;
  unsigned long int mNumMixedNorm;

  StHbt2DHisto** add2DHistos; //!
  Int_t numAdd2DHistos;
  Int_t addedHistos;

#ifdef __ROOT__
ClassDef(ThBPCorrFctn, 1)
#endif
};

#endif

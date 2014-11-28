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

#ifndef ThQinvCorrFctn_hh
#define ThQinvCorrFctn_hh

#include "StHbtMaker/Base/StHbtRoot1DCF.hh"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"

class StHbtThPair;

class ThQinvCorrFctn :  public virtual StHbtThCorrFctn ,public virtual StHbtRoot1DCF  {
 public:
  ThQinvCorrFctn(char* aTitle, int aNBins, 
		 double aHLo, double aHHi);
  ThQinvCorrFctn(const ThQinvCorrFctn& ThCf);
  
  virtual ~ThQinvCorrFctn();

  void AddNum(StHbtThPair*);
  void AddDen(StHbtThPair*);
  
  StHbtThCorrFctn* ThClone() const ;

  virtual StHbt1DHisto* Numerator() const ;
  virtual StHbt1DHisto* Denominator() const ;
  virtual StHbt1DHisto* Ratio() const ;
  virtual void Write() ;
  virtual void Finish();

#ifdef __ROOT__
ClassDef(ThQinvCorrFctn, 1)
#endif
};

#endif

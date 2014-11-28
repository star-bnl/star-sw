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

#ifndef Th3DQCorrFctn_hh
#define Th3DQCorrFctn_hh

#include "StHbtMaker/Base/StHbtRoot1DCF.hh"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"

class StHbtThPair;

class Th3DQCorrFctn :  public virtual StHbtThCorrFctn ,public virtual StHbtRoot1DCF  {
 public:
  Th3DQCorrFctn(char* aTitle, int aNBins, 
		double aHLo, double aHHi);
  Th3DQCorrFctn(const Th3DQCorrFctn& ThCf);
  
  virtual ~Th3DQCorrFctn();

  void AddNum(StHbtThPair*);
  void AddDen(StHbtThPair*);
  
  virtual StHbtThCorrFctn* ThClone() const ;

  virtual StHbt1DHisto* Numerator() const ;
  virtual StHbt1DHisto* Denominator() const ;
  virtual StHbt1DHisto* Ratio() const ;
  virtual void Write() ;
  virtual void Finish();

 private:
  
  StHbt1DHisto* qOutdist;
  StHbt1DHisto* qOutSdist;
  StHbt1DHisto* DeltaqOutdist;
  
  StHbt1DHisto* qSidedist;
  StHbt1DHisto* qSideSdist;
  StHbt1DHisto* DeltaqSidedist;
  
  StHbt1DHisto* qLongdist;
  StHbt1DHisto* qLongSdist;
  StHbt1DHisto* DeltaqLongdist;
  

#ifdef __ROOT__
ClassDef(Th3DQCorrFctn, 1)
#endif
};

#endif

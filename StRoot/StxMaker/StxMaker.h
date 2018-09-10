// $Id: StxMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $
#ifndef STAR_StHTLCAMaker
#define STAR_StHTLCAMaker

/*!
 *                                                                     
 * \class  StxMaker
 * \author Yuri Fisyak
 * \date   2013/08/12
 * \brief  
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TVector3.h"
#include "TMatrixDSym.h"
class AliHLTTPCCAGBTrack;
class StxMaker : public StMaker {
 public: 
 StxMaker(const char *name="Stx") : StMaker(name) {}
  virtual       ~StxMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t FitTrack(const AliHLTTPCCAGBTrack &tr);
  static Double_t ConvertCA2XYZ(const AliHLTTPCCAGBTrack &tr, TVector3 &pos, TVector3 &mom, TMatrixDSym &covM);
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StxMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
 private:
  
  ClassDef(StxMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StxMaker.h,v $

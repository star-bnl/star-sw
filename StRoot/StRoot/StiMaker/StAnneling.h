#ifndef __StAnneling_h__
#define __StAnneling_h__
// $Id: StAnneling.h,v 2.1 2012/05/07 14:56:14 fisyak Exp $
#include "TMath.h"
#include "TObject.h"
//________________________________________________________________________________
class StAnneling : public TObject {
public:
  static void      SetTemperature(Double_t  Temperature=1)   {fTemperature = Temperature;}
  static Double_t  Temperature() {return fTemperature;}
  static void      SetChi2Cut    (Double_t chi2Cut=12.25)  {fChi2Cut = chi2Cut;}
  static Double_t  Chi2Cut()     {return fChi2Cut;}
  static Double_t  Weight()   {
    return TMath::Exp(Chi2Cut()/(2*Temperature())) + 
      Temperature()*TMath::Log(1 + TMath::Exp(Chi2Cut()/(2*Temperature())));
  }
private:
  static Double_t  fTemperature;
  static Double_t  fChi2Cut;
  ClassDef(StAnneling,1)
};
// $Log: StAnneling.h,v $
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.3  2012/02/07 19:38:26  fisyak
// Repackage
//
#endif

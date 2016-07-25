#ifndef KpT09_16NMDF_hh
#define KpT09_16NMDF_hh
class KpT09_16N {
 public:
  Int_t    fgNVariables;
  Int_t    fgNCoefficients;
  Double_t fgDMean; 
  Double_t fgXMean[];       //[fgNVariables] 
  Double_t fgXMin[];        //[fgNVariables] 
  Double_t fgXMax[];        //[fgNVariables] 
  Double_t fgCoefficient[]; //[fgNCoeffficents] 
  Int_t    fgPower[];       //[fgNCoeffficents*fgNVariables] 
  Double_t MDF(Double_t *x);
  ClassDef(KpT09_16N)
}

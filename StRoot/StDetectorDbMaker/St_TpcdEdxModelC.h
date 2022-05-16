#ifndef St_TpcdEdxModelC_h
#define St_TpcdEdxModelC_h
#include "St_tpcCorrectionC.h"
#include "TF1.h"
class St_TpcdEdxModelC : public St_tpcCorrectionC {
 public:
  static St_TpcdEdxModelC* 	instance();
  static Double_t MostProbablenE(Double_t nP);
  static Double_t dE(Double_t ne) {return 21.19e-9*ne;}
  static Double_t nE(Double_t de) {return de/21.19e-9;}
  static Double_t MostProbabledE(Double_t nP) {return dE(MostProbablenE(nP));} // GeV
  static TF1* Prob();
  static Double_t funcProb(Double_t *x, Double_t *p);
 protected:
  St_TpcdEdxModelC(St_tpcCorrection *table=0) : St_tpcCorrectionC(table) {}
  virtual ~St_TpcdEdxModelC() {fgInstance = 0;}
 private:
  static St_TpcdEdxModelC* fgInstance;
  static TF1* fProb;
  ClassDef(St_TpcdEdxModelC,1)
};
#endif

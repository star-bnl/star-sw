#ifndef St_TpcdEdxModelC_h
#define St_TpcdEdxModelC_h
#include "St_tpcCorrectionC.h"
#include "TF1.h"
class St_TpcdEdxModelC : public St_tpcCorrectionC {
 public:
  static St_TpcdEdxModelC* 	instance();
  static Double_t E(Double_t ne) {return 24.95e-9*ne;} // deposited energy GeV from no. conduction electrons
  static Double_t n(Double_t e) {return e/24.95e-9;} // no of conducting electrons from energy (GeV)
#if 0
  static Double_t MostProbableN(Double_t N);         // N no. of primary clusters
  static Double_t MostProbableE(Double_t N) {return E(MostProbableN(N));} // GeV
#endif
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

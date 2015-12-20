#ifndef __StVertexP_h__
#define __StVertexP_h__

#include "TObject.h"
#include "StVertexT.h"
class StVertexP : public TObject { // Vertex pair
public:
  StVertexP() : fI(0), fJ(0), fChi2(0) {}
  StVertexP(Int_t i, Int_t j, StVertexT &VI, StVertexT &VJ, Double_t chi2) : fI(i), fJ(j), fChi2(chi2), fVI(VI), fVJ(VJ) {}
  virtual ~StVertexP() {}
private:
  Int_t   fI;
  Int_t   fJ;
  Double_t fChi2;
  StVertexT fVI;
  StVertexT fVJ;
  ClassDef(StVertexP,1)
}; // for pair vertices
#endif

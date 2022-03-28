// Author: Yuri Fisyak   03/27/22
#ifndef ROOT_THnSparseProject
#define ROOT_THnSparseProject
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// THnSparseProject                                                     //
// Generate mapped list of Projection of THnSparse to the last variable //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "THashList.h"
#include "TArrayI.h"
#include "THnSparse.h"
#include "TAxis.h"
#include "TH1.h"
#include "TArrayF.h"
class THnSparseProject: public TObject {
 public:
  THnSparseProject(const THnSparse *hs = 0);
  ~THnSparseProject() {SafeDelete(fProjMap);}
  static void SetDebug(Int_t k = 0) {_debug = k;}
  TH1D    *Next();
  Int_t   *GetBins() {return fBins.GetArray();}
  Float_t *GetVars() {return fX.GetArray();}
  Int_t    Ndim()    {return fNdim;}
 private:
  const THnSparse *fHs;
  Int_t    fNdim;
  TArrayI  fBins;
  TArrayI  fCoord;
  TArrayF  fX;
  TAxis   *fAxis;
  THashList *fProjMap;
  static Int_t _debug; 
  ClassDef(THnSparseProject,1)
};
#endif //ROOT_THnSparseProject

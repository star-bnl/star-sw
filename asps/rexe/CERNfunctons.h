#ifndef CERNfunctions_H
#define CERNfunctions_H

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

extern "C" {
  void     hlimit_(Int_t *limit);
  void     hsetpr_ (Char_t *, Float_t *, Int_t);
  void     hparmn_(Float_t *xy, Float_t *vex, Float_t *ex, 
		   Int_t *np, Int_t *ndim, Int_t *ic, Float_t *r2mini,
		   Int_t *mx, Double_t *Coef, Int_t *Iterm, Int_t *nCo);
  Double_t hrval_ (Float_t *x);
  Float_t  fint_(Int_t *narg, Float_t *arg, Int_t *nent, Float_t *ent, Float_t *table);
}
#if 0
   ClassDef(CERNfunctions,0)
#endif
#endif

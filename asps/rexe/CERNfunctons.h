#ifndef CERNfunctions_H
#define CERNfunctions_H

#ifndef ROOT_Rtypes
#include "Rtypes.h"
#endif

extern "C" {
  void     hlimit_(Int_t *limit);
  void     hsetpr_ (Char_t *, Float_t *, Int_t);
  void     hparam_(Float_t *xy, Float_t *vex, Float_t *ex, 
		   Int_t *np, Int_t *ndim, Int_t *ic,
		   Double_t *Coef, Int_t *Iterm, Int_t *nCo);
}
#if 0
   ClassDef(CERNfunctions,0)
#endif
#endif

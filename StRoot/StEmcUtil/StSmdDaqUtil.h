//////////////////////////////////////////////////////////////////////////
//
// Author: Aleksei Pavlinov, WSU 24-may-2001
//
// StSmdDaqUtil => Util for online decoding for SMD.
//
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StSmdDaqUtil
#define STAR_StSmdDaqUtil
#include "TArrayI.h"

class StSmdDaqUtil {
private:

public:
  StSmdDaqUtil();
  virtual ~StSmdDaqUtil();

  static Bool_t getBsmdeCell(const Int_t , const Int_t, Int_t&);
  static Bool_t getBsmdpCell(const Int_t , const Int_t, Int_t&, Int_t&);
  static Bool_t checkBound(const Int_t, const Int_t, const Int_t);

  ClassDef(StSmdDaqUtil,1)  // Util for online decoding
};
#endif

// $Id: Load.C,v 1.19 2006/10/17 19:38:00 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void load(Char_t *Chain="MuDst") {
  gROOT->LoadMacro("bfc.C");
  TString C(Chain);
  C += ",nodefault";
  bfc(-1,C);
}

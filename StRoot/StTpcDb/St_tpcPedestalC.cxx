#include "St_tpcPedestalC.h"
ClassImp(St_tpcPedestalC);
//________________________________________________________________________________
Float_t St_tpcPedestalC::Pedestal(Int_t sector, Int_t row, Int_t pad) {
  tpcPedestal_st *t = ((St_tpcPedestal *) Table())->GetTable() + sector - 1;
  return t ? t->Pedestal[row-1][pad-1] : 0;
}
//________________________________________________________________________________
Float_t St_tpcPedestalC::Rms(Int_t sector, Int_t row, Int_t pad) {
  tpcPedestal_st *t = ((St_tpcPedestal *) Table())->GetTable() + sector - 1;
  return t ? t->Rms[row-1][pad-1] : 0;

}

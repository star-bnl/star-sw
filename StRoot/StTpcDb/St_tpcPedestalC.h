#ifndef St_tpcPedestalC_h
#define St_tpcPedestalC_h
#include "TChair.h"
#include "tables/St_tpcPedestal_Table.h"

class St_tpcPedestalC : public TChair {
 public:
    St_tpcPedestalC (St_tpcPedestal *table=0) : TChair(table) {}
    Float_t Pedestal(Int_t sector, Int_t row, Int_t pad);
    Float_t Rms(Int_t sector, Int_t row, Int_t pad);
    ClassDefChair(St_tpcPedestal, tpcPedestal_st )
    ClassDef(St_tpcPedestalC,1) //C++ TChair for tpcPedestal table class
};
#endif

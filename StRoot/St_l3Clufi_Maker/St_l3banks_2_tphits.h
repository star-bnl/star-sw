#ifndef St_l3banks_2_tphits_hh
#define St_l3banks_2_tphits_hh
#include <Rtypes.h>
#include <StObject.h>
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_hitarray_Table.h"

class St_l3banks_2_tphits: public StObject {

private:    
    St_tcl_tphit* mytclhits;
    St_hitarray* mybank;

public:
    St_l3banks_2_tphits(St_tcl_tphit*, St_hitarray*);
    virtual ~St_l3banks_2_tphits();
    
    // Memberfunctions
    Int_t Filltclpoints();

    // Root connection
     ClassDef(St_l3banks_2_tphits, 0)
};

#endif // St_l3banks_2_tphits

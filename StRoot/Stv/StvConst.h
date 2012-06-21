#ifndef __StvConst_h_
#define __StvConst_h_
#include "TNamed.h"
#include "tables/St_StvKonst_Table.h"

class StvConst : public TNamed 
{
public:	
StvConst();
static const StvKonst_st *Inst() {return mgConst;}	

private:
static const StvKonst_st *mgConst;
ClassDef(StvConst,0)//
};
#endif //__StvConst_h_



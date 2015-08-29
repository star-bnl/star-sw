#ifndef __StvConst_h_
#define __StvConst_h_
#include "TNamed.h"
#include "tables/St_StvKonst_Table.h"

class StvConst : public StvKonst_st
{
public:	
StvConst();
virtual ~StvConst(){;}
static const StvConst *Inst(){ return mgConst;}

private:
static const StvConst *mgConst;
ClassDef(StvConst,0)//
};
#endif //__StvConst_h_



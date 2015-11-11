#ifndef __StvConst_h_
#define __StvConst_h_
#include "TNamed.h"
#include "tables/St_StvKonst_Table.h"

class StvConst : public StvKonst_st
{
public:	
StvConst();
virtual ~StvConst(){mFw=0;}
static const StvConst *Inst(){ return mgConst;}
const StvKonst_st *At(int idx) const;
private:

mutable StvKonst_st *mFw;
static const StvConst *mgConst;
ClassDef(StvConst,0)//
};
#endif //__StvConst_h_



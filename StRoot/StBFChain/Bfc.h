#ifndef __Bfc_st__
#define __Bfc_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
#define STR_OBSOLETE "WARNING *** Option is OBSOLETE ***"
struct Bfc_st {
  Char_t       Key[63];      /* nick name */
  Char_t       Name[63];     /* maker name */
  Char_t       Chain[63];    /* its chain */
  Char_t       Opts[257];    /* required options */
  Char_t       Maker[63];    /* required Makers */
  Char_t       Libs[127];    /* libraries to be loaded */
  Char_t       Comment[257];  
  Char_t       Flag;         /* F/T to use it in chain */
};
class St_Bfc : public TTable {
 public:
  ClassDefTable(St_Bfc,Bfc_st)
  ClassDef(St_Bfc,1) //C++ container for chain/makers status 
};
#endif /* __Bfc_st__ */

#ifndef __Bfc_st__
#define __Bfc_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
#define STR_OBSOLETE "WARNING *** Option is OBSOLETE ***"
#define USE_BFCTIMESTAMP
struct Bfc_st {
  Char_t       Key[63];      /* nick name */
  Char_t       Name[63];     /* maker name */
  Char_t       Chain[63];    /* its chain */
  Char_t       Opts[255];    /* required options */
  Char_t       Maker[63];    /* required Makers */
  Char_t       Libs[255];    /* libraries to be loaded */
  Char_t       Comment[255];  
  Char_t       Flag;         /* F/T to use it in chain */
};
#ifdef USE_BFCTIMESTAMP
struct BFCTimeStamp {
  Int_t     Type;     //< 1 for DBV and 2 for SDT
  Int_t     Date;     //< A date in YYYYMMDD format
  Int_t     Time;     //< A time in HHmmss format
  TString   Detector; //< This detector tag needs to follow the DB branch
  TString   Realm;    //< Realm is Calibrations or Geometry or ...
};
typedef std::vector<BFCTimeStamp> StVecBFCTS;
#endif /*  USE_BFCTIMESTAMP */
class St_Bfc : public TTable {
 public:
  ClassDefTable(St_Bfc,Bfc_st)
  ClassDef(St_Bfc,1) //C++ container for chain/makers status 
};

#endif /* __Bfc_st__ */

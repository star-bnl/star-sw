#ifndef St_tpcCorrect192C_h
#define St_tpcCorrect192C_h

#include "TChair.h"
#include "tables/St_tpcCorrect192_Table.h"
#include "St_tpcCorrectionC.h"
class St_tpcCorrect192C : public St_tpcCorrectionC  {
 public:
 St_tpcCorrect192C(St_tpcCorrect192 *table=0) : St_tpcCorrectionC((St_tpcCorrection *)table) {}
  virtual ~St_tpcCorrect192C() {}
  tpcCorrection_st 	*Struct(Int_t i = 0) 	const {return (tpcCorrection_st *) ((St_tpcCorrect192*) Table())->GetTable()+i;}
  ClassDefChair(St_tpcCorrect192, tpcCorrect192_st )
  ClassDef(St_tpcCorrect192C,1) //C++ TChair for tpcCorrect192 table class
};
#endif

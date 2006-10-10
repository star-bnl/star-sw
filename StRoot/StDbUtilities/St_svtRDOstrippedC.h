#ifndef St_svtRDOstrippedC_h
#define St_svtRDOstrippedC_h
#include "TChair.h"
#include "TArrayI.h"
#include "tables/St_svtRDOstripped_Table.h"

class St_svtRDOstrippedC : public TChair {
 public:
  St_svtRDOstrippedC (St_svtRDOstripped *table=0) : TChair(table), fDate(0), fDelay(0), fSwitchedOff(0) {
			       if (fgsvtRDOstrippedC) delete fgsvtRDOstrippedC; fgsvtRDOstrippedC = this;}
  virtual           ~St_svtRDOstrippedC() {fgsvtRDOstrippedC = 0;}

  static St_svtRDOstrippedC *instance()  {return fgsvtRDOstrippedC;}
  Int_t                      svtRDOstrippedStatus(Int_t barrel, Int_t ladder, Int_t wafer);
  svtRDOstripped_st         *pRDO(Int_t barrel, Int_t ladder, Int_t wafer);
  //                                                           1 hour                   3 hours
  void                       SetDate(UInt_t date, UInt_t delay=3600, UInt_t switchOff = 10800) 
  {fDate = date; fDelay = delay; fSwitchedOff = switchOff;}
  static void                PrintRDOmap();
 private:
  UInt_t                     fDate;
  UInt_t                     fDelay;
  UInt_t                     fSwitchedOff;
  static St_svtRDOstrippedC *fgsvtRDOstrippedC;

  ClassDefChair(St_svtRDOstripped, svtRDOstripped_st )
  ClassDef(St_svtRDOstrippedC,1) //C++ TChair for svtRDOstripped table class
};
#endif

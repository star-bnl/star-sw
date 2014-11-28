#ifndef St_svtRDOstrippedC_h
#define St_svtRDOstrippedC_h
#include "TChair.h"
#include "TArrayI.h"
#include "tables/St_svtRDOstripped_Table.h"

class St_svtRDOstrippedC : public TChair {
 public:
  St_svtRDOstrippedC (St_svtRDOstripped *table=0);
  virtual           ~St_svtRDOstrippedC() {fgsvtRDOstrippedC = 0;}
  
  static St_svtRDOstrippedC *instance()  {return fgsvtRDOstrippedC;}
  Int_t                      svtRDOstrippedStatus(Int_t barrel, Int_t ladder, Int_t wafer);
  svtRDOstripped_st         *pRDO(Int_t barrel, Int_t ladder, Int_t wafer);
  //                                                           1 hour                   3 hours
  void                       SetDate(Int_t date, Int_t delay=3600, Int_t switchOff = 10800) 
  {fDate = date; fDelay = delay; fSwitchedOff = switchOff;}
  static void                PrintRDOmap();
  void                       Init();
 private:
  Int_t                     fDate;
  Int_t                     fDelay;
  Int_t                     fSwitchedOff;
  static St_svtRDOstrippedC *fgsvtRDOstrippedC;

  ClassDefChair(St_svtRDOstripped, svtRDOstripped_st )
  ClassDef(St_svtRDOstrippedC,1) //C++ TChair for svtRDOstripped table class
};
#endif

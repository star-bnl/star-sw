// Author: Wei-Ming Zhang 
// $Id: StEEmcPrint.h,v 1.1 2006/12/12 20:29:17 balewski Exp $
//  

#ifndef STAR_StEEmcPrint
#define STAR_StEEmcPrint
#include <TObject.h>

class StEmcCollection;

class StEEmcPrint {

 public: 
 Int_t  mPrint;                             //! print mode
  StEEmcPrint();
  virtual ~StEEmcPrint();
  void setMode(Int_t mode) {mPrint = mode;} //bits: 1:Tower, 2:Pre, 4:SmdU, 8:SmdV , 15:all

  void print(StEmcCollection * emcCol);
  void  printChange(StEmcCollection * ecolA,StEmcCollection * ecolB,char *comm);
  ClassDef(StEEmcPrint,1);
};

#endif


// StRefMultCorr headers
#include "Param.h"

//________________
const string getParamX_ShapeWeight( const int irun, const int ivz) {
  string temstr;
  temstr = RefmultShapeRatio[irun][ivz];
  return temstr;
}

//________________
const string getParamX( const int x, const int y1, const int y2 ) {
  string str;
  switch(x) {
    case 0:  str = mParamStr_gref[y1][y2]; break;
    case 1:  str = mParamStr_ref1[y1][y2]; break;
    case 2:  str = mParamStr_ref2[y1][y2]; break;
    case 3:  str = mParamStr_ref3[y1][y2]; break;
    case 4:  str = mParamStr_ref4[y1][y2]; break;
    default: str = "0";
  }
  return str;
}
#include <TObject.h>
#include <TString.h>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
struct EEmcStrip2StripMapItem {
  const Char_t *strip;
  Int_t   iMin;   // minimum orthogonal strip
  Int_t   iMax;   // maximum orthogonal strip
};

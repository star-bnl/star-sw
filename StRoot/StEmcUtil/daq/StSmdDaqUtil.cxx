#include "StSmdDaqUtil.h"

ClassImp(StSmdDaqUtil)

StSmdDaqUtil::StSmdDaqUtil()  { };

StSmdDaqUtil::~StSmdDaqUtil() { };

Bool_t
StSmdDaqUtil::getBsmdeCell(const Int_t connector, const Int_t pin, Int_t& e)
{
  // connector - input connector of FEE card (change from 1 to 3)
  // pin       - pin number (from 1 to 50)
  // e         - eta division in EMC offline numbering scheme;
  // s         - phi division in EMC offline numbering scheme;

  static Int_t connectorShift[3]={151, 101, 51};
  if(checkBound(connector, 1, 3) && checkBound(pin, 1, 50)){ 
    e = connectorShift[connector-1] - pin;
    return kTRUE;
  }
  else {
    printf(" Bsmde : Wrong connector number %i or pin number %i\n" , connector, pin);  
    return kFALSE;
  }
}

Bool_t
StSmdDaqUtil::getBsmdpCell(const Int_t connector, const Int_t pin, Int_t& e, Int_t& s)
{
  static Int_t connectorShift[3]={15, 10, 5};
  static Int_t rem;
  if(checkBound(connector, 1, 3) && checkBound(pin, 1, 50)){ 
    rem = pin%10;
    e   = (rem==0)?10:rem;
    s   = (connectorShift[connector-1] - (pin-1)/10);
    return kTRUE;
  }
  else {
    printf(" Bsmdp : Wrong connector number %i or pin number %i\n" , connector, pin);  
    return kFALSE;
  }
}

Bool_t
StSmdDaqUtil::checkBound(const Int_t i, const Int_t min, const Int_t max)
{
  if(i>=min && i<=max) return kTRUE;
  else                 return kFALSE; 
}

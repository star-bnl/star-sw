//
// $Id: BetheBloch.h,v 1.3 2001/03/21 16:25:50 fisyak Exp $
//
// Description:
// Function to return the expected dE/dx as a function of
// beta*gamma for the TPC
// 
// $Log: BetheBloch.h,v $
// Revision 1.3  2001/03/21 16:25:50  fisyak
// New BetheBloch curve
//
// Revision 1.2  2000/12/21 00:59:51  fisyak
// Add parameteriation for Bethe Bloch depending on track length
//
// Revision 1.1  2000/07/04 17:35:08  calderon
// Initial Revision
// Bethe Bloch curve for the TPC with initial preliminary tuning
//
//
//

#ifndef BetheBloch_hh
#define BetheBloch_hh
#include "StObject.h"

class BetheBloch : public StObject {
 public:
  BetheBloch() {};
  virtual ~BetheBloch() {};
  Double_t   operator() (Double_t poverm) 
    {return 1.e-6*Sirrf(poverm);}
  Double_t   operator() (Double_t poverm, Double_t Length,Int_t k) 
    {return 1.e-6*Sirrf(poverm,Length,k);}
  static   Double_t Sirrf(Double_t poverm, Double_t Length=60., Int_t k=0);
  ClassDef(BetheBloch,1)
};
#endif

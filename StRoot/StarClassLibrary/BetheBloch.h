//
// $Id: BetheBloch.h,v 1.7 2004/03/18 18:57:43 fisyak Exp $
//
// Description:
// Function to return the expected dE/dx as a function of
// beta*gamma for the TPC
// 
// $Log: BetheBloch.h,v $
// Revision 1.7  2004/03/18 18:57:43  fisyak
// Make only warning about BetheBloch::BetheBloch
//
// Revision 1.6  2003/05/22 21:01:01  perev
// Remove redundant dependency
//
// Revision 1.5  2003/02/15 02:24:57  fisyak
// Add static function Girrf for GEANT3 prediction for dE/dx
//
// Revision 1.4  2001/04/24 15:36:30  fisyak
// Restore Manuel's parameterization, move scale of dE/dx from 2.4 to 1.4 keV/cm for MIP
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
#include "Rtypes.h"
#include <map>
#ifndef ST_NO_NAMESPACES
using std::map;
#endif

class BetheBloch {
    map<double, double> mMap; //!
public:
    BetheBloch();
    virtual ~BetheBloch();
    static   int noWarn;
    double   operator() (double);
    static   Double_t Sirrf(Double_t poverm, Double_t Length=60., Int_t k=0);
    static   Double_t Girrf(Double_t poverm, Double_t Tmin=1.e-3, Int_t k=0);
};
#endif

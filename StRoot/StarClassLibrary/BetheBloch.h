//
// $Id: BetheBloch.h,v 1.1 2000/07/04 17:35:08 calderon Exp $
//
// Description:
// Function to return the expected dE/dx as a function of
// beta*gamma for the TPC
// 
// $Log: BetheBloch.h,v $
// Revision 1.1  2000/07/04 17:35:08  calderon
// Initial Revision
// Bethe Bloch curve for the TPC with initial preliminary tuning
//
//
//

#ifndef BetheBloch_hh
#define BetheBloch_hh

#include <map>
#include "StObject.h"
#ifndef ST_NO_NAMESPACES
using std::map;
#endif

class BetheBloch : public StObject {
    map<double, double> mMap; //!
public:
    BetheBloch();
    virtual ~BetheBloch();
    double   operator() (double);
    ClassDef(BetheBloch,1)
};
#endif

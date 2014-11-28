//
// $Id: myBetheBloch.h,v 1.1 2001/04/23 21:46:47 meissner Exp $
//
// Description:
// Function to return the expected dE/dx as a function of
// beta*gamma for the TPC
// 
// $Log: myBetheBloch.h,v $
// Revision 1.1  2001/04/23 21:46:47  meissner
// Copy of old (P00hm) version of BB function, temp solution, take this out after next production, meissner
//
// Revision 1.1  2000/07/04 17:35:08  calderon
// Initial Revision
// Bethe Bloch curve for the TPC with initial preliminary tuning
//
//
//

#ifndef myBetheBloch_hh
#define myBetheBloch_hh

#include <map>
#include "StObject.h"
#ifndef ST_NO_NAMESPACES
using std::map;
#endif

class myBetheBloch : public StObject {
    map<double, double> mMap; //!
public:
    myBetheBloch();
    virtual ~myBetheBloch();
    double   operator() (double);
    ClassDef(myBetheBloch,1)
};
#endif

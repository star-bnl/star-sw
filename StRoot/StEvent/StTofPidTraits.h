/***************************************************************************
 *
 * $Id: StTofPidTraits.h,v 2.2 2000/12/09 02:13:23 perev Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.h,v $
 * Revision 2.2  2000/12/09 02:13:23  perev
 * default StObject::clone() used
 *
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#ifndef StTofPidTraits_hh
#define StTofPidTraits_hh

#include "StTrackPidTraits.h"

class StTofPidTraits : public StTrackPidTraits {
public:
    StTofPidTraits();
    ~StTofPidTraits();
    
    //StTofPidTraits(const StTofPidTraits&) {/* nopt */}
    //StTofPidTraits& operator=(const StTofPidTraits&) {/* nopt */}
        
private:
//VP    StObject* clone();
    ClassDef(StTofPidTraits,1)
};
#endif

/***************************************************************************
 *
 * $Id: StTofPidTraits.h,v 2.1 2000/12/08 03:52:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.h,v $
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
    StObject* clone();
    ClassDef(StTofPidTraits,1)
};
#endif

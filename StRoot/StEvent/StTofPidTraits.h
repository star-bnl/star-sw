/*!
 * \class StTofPidTraits 
 * \author Thomas Ullrich, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofPidTraits.h,v 2.4 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofPidTraits.h,v $
 * Revision 2.4  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.2  2000/12/09 02:13:23  perev
 * default StObject::clone() const used
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
//VP    StObject* clone() const;
    ClassDef(StTofPidTraits,1)
};
#endif

/*!
 * \class StPrimaryTrack 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StPrimaryTrack.h,v 2.12 2013/07/16 14:29:04 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.h,v $
 * Revision 2.12  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.10  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
 *
 * Revision 2.9  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.8  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.7  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.6  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.5  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.3  2001/03/24 03:34:53  perev
 * clone() -> clone() const
 *
 * Revision 2.2  1999/10/28 22:26:13  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPrimaryTrack_hh
#define StPrimaryTrack_hh

#include "StTrack.h"

class StPrimaryVertex;
class StPrimaryTrack;

ostream&  operator<<(ostream& os,  const StPrimaryTrack& t);

class StPrimaryTrack : public StTrack {
public:
    StPrimaryTrack()  {/* noop */}
    ~StPrimaryTrack() {/* noop */}
    
    StTrackType type() const {return primary; }
    void Print(Option_t *option="") const {cout << option << *this << endl; }
    
    ClassDef(StPrimaryTrack,3)
};
#endif

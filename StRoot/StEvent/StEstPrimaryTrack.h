/*!
 * \class StEstPrimaryTrack 
 * \author Thomas Ullrich, Mar 2002
 */
/***************************************************************************
 *
 * $Id: StEstPrimaryTrack.h,v 2.2 2003/10/30 20:07:32 perev Exp $
 *
 * Author: Thomas Ullrich, Mar 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEstPrimaryTrack.h,v $
 * Revision 2.2  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.1  2002/04/19 13:28:34  jeromel
 * New include for SVT 2 tables scheme support. Forgot about it yesterday ...
 *
 **************************************************************************/
#ifndef StEstPrimaryTrack_hh
#define StEstPrimaryTrack_hh

#include "StPrimaryTrack.h"
class StPrimaryVertex;

class StEstPrimaryTrack : public StPrimaryTrack {
public:
    StEstPrimaryTrack();
    StEstPrimaryTrack(const dst_track_st&);
    StEstPrimaryTrack(const StEstPrimaryTrack&);
    StEstPrimaryTrack& operator=(const StEstPrimaryTrack&);
    ~StEstPrimaryTrack();

    StTrackType type() const;

protected:
    StObject*   clone() const;
    
private:
    ClassDef(StEstPrimaryTrack,2)
};
#endif

/***************************************************************************
 *
 * $Id: StVpdSummary.hh,v 1.3 1999/03/17 22:22:19 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.hh,v $
 * Revision 1.3  1999/03/17 22:22:19  ullrich
 * Some cosmetics
 *
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVpdSummary_hh
#define StVpdSummary_hh
#include "StEvent/StEnumerations.hh"

class StVpdSummary {
public:
    StVpdSummary();
    StVpdSummary(float te, float tw, float z);
    ~StVpdSummary();
    // StVpdSummary(const StVpdSummary &right);
    // const StVpdSummary & operator=(const StVpdSummary &right);

    float vertexZ() const;
    float minimumTime(StBeamDirection) const;

    void setMinimumTime(StBeamDirection, float); 
    void setVertexZ(float);     
    
protected:
    float mMinimumTimeEast;
    float mMinimumTimeWest;
    float mVertexZ;        
};

inline float StVpdSummary::vertexZ() const { return mVertexZ; }

inline float StVpdSummary::minimumTime(StBeamDirection dir) const
{
    if (dir == east)
	return mMinimumTimeEast;
    else
	return mMinimumTimeWest;
}

#endif

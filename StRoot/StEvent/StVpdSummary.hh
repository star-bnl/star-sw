/***************************************************************************
 *
 * $Id: StVpdSummary.hh,v 1.2 1999/01/15 22:54:26 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.hh,v $
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
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
    StVpdSummary(float ae, float aw, float s);
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

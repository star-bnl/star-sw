/***************************************************************************
 *
 * $Id: StV0Vertex.hh,v 1.4 1999/01/30 23:03:18 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StV0Vertex.hh,v $
 * Revision 1.4  1999/01/30 23:03:18  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.5  1999/02/18 15:41:42  ullrich
 * Momemtum of daughter tracks added.
 *
 * Revision 1.4  1999/01/30 23:03:18  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/27 13:04:50  ullrich
 * Renamed data member and access functions: xxxToV0 into xxxToPrimaryVertex.
 * This is the right meaning according to P. Jones.
 *
 * Revision 1.2  1999/01/15 22:54:16  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#include "StEvent/StVertex.hh"
#include "StEvent/StEnumerations.hh"
#include "tables/dst_vertex.h"
#include "tables/dst_v0_vertex.h"
#include <float.h>

class StV0Vertex : public StVertex {
    StV0Vertex(dst_v0_vertex_st*);
    StV0Vertex();
    ~StV0Vertex();
    StV0Vertex(dst_v0_vertex_st*,dst_vertex_st*);
    float dcaDaughterToPrimaryVertex(unsigned int i) const;
    // const StV0Vertex & operator=(const StV0Vertex&);

    float dcaDaughters() const;
    void setDcaDaughtersToPrimaryVertex(unsigned int, float);

    void setDcaDaughterToPrimaryVertex(StTrackSign sign, float);
    void setMomentumOfDaughter(StTrackSign sign, const StThreeVector<float>&);
    void setDcaDaughters(float);
    void setDcaParentToPrimaryVertex(float);
    
    float mDcaDaughtersToPrimaryVertex[2];
    float mDcaDaughters;
    float mDcaParentToPrimaryVertex;
    StThreeVector<float> mMomentumOfDaughters[2];
    float                mDcaDaughters;
inline float StV0Vertex::dcaDaughterToPrimaryVertex (unsigned int i) const
};
    if (i < 2)
	return mDcaDaughtersToPrimaryVertex[i];
    else
	return FLT_MAX;
StV0Vertex::momentumOfDaughter(StTrackSign sign) const
{
    return mMomentumOfDaughters[sign];
}

inline float StV0Vertex::dcaDaughters() const { return mDcaDaughters; }

inline float StV0Vertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

#endif

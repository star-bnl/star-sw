/***************************************************************************
 *
 * $Id: StV0Vertex.hh,v 1.8 1999/04/09 19:34:04 genevb Exp $
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
 * Revision 1.8  1999/04/09 19:34:04  genevb
 * Added vertex daughter functionality
 *
 * Revision 1.7  1999/03/23 21:47:48  ullrich
 * Member function made virtual
 *
 * Revision 1.6  1999/02/21 20:32:48  genevb
 * Improve StV0Vertex code
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
#ifndef StV0Vertex_hh
#define StV0Vertex_hh
#include "StEvent/StVertex.hh"
#include "StEvent/StEnumerations.hh"
#include "StEvent/StGlobalTrack.hh"
#include "tables/dst_vertex.h"
#include "tables/dst_v0_vertex.h"
#include <float.h>

class StV0Vertex : public StVertex {
public:
    StV0Vertex();
    ~StV0Vertex();
    StV0Vertex(dst_v0_vertex_st*,dst_vertex_st*);
    // StV0Vertex(const StV0Vertex&);        use default
    // const StV0Vertex & operator=(const StV0Vertex&);

    virtual StGlobalTrack* daughter(StTrackSign sign, double B);
    virtual float dcaDaughterToPrimaryVertex(StTrackSign sign) const;
    virtual float dcaDaughters() const;
    virtual float dcaParentToPrimaryVertex() const;
    virtual const StThreeVector<float>& momentumOfDaughter(StTrackSign sign) const;

    virtual void setDcaDaughterToPrimaryVertex(StTrackSign sign, float);
    virtual void setMomentumOfDaughter(StTrackSign sign, const StThreeVector<float>&);
    virtual void setDcaDaughters(float);
    virtual void setDcaParentToPrimaryVertex(float);
    
    void setType(StVertexType);     // overwrite from base class          

protected:
    float                mDcaDaughtersToPrimaryVertex[2];
    StThreeVector<float> mMomentumOfDaughters[2];
    float                mDcaDaughters;
    float                mDcaParentToPrimaryVertex;
};

inline float StV0Vertex::dcaDaughterToPrimaryVertex (StTrackSign sign) const
{
    return mDcaDaughtersToPrimaryVertex[sign];
}

inline const StThreeVector<float>&
StV0Vertex::momentumOfDaughter(StTrackSign sign) const
{
    return mMomentumOfDaughters[sign];
}

inline float StV0Vertex::dcaDaughters() const { return mDcaDaughters; }

inline float StV0Vertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

#endif

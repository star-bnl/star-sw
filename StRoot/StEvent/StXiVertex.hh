/***************************************************************************
 *
 * $Id: StXiVertex.hh,v 1.8 1999/04/19 15:54:10 genevb Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 *
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.hh,v $
 * Revision 1.8  1999/04/19 15:54:10  genevb
 * Added momentum() to vertex classes
 *
 * Revision 1.7  1999/04/14 22:04:30  genevb
 * Fixed a memory leak
 *
 * Revision 1.6  1999/04/13 23:27:21  genevb
 * Slightly refined vertex code, updated V0, Xi vertex documentation
 *
 * Revision 1.5  1999/04/09 20:02:12  genevb
 * Change constancy of new functions
 *
 * Revision 1.4  1999/04/09 19:34:06  genevb
 * Added vertex daughter functionality
 *
 * Revision 1.3  1999/02/23 16:13:26  genevb
 * Add v0 pointers for xi's outside constructor
 *
 * Revision 1.2  1999/02/23 13:50:59  genevb
 * Fixed some typos
 *
 * Revision 1.1  1999/02/23 13:46:46  genevb
 * Add new StXiVertex
 *
 *
 **************************************************************************/
#ifndef StXiVertex_hh
#define StXiVertex_hh
#include "StEvent/StVertex.hh"
#include "StEvent/StV0Vertex.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StEnumerations.hh"
#include "tables/dst_vertex.h"
#include "tables/dst_xi_vertex.h"
#include <float.h>

class StXiVertex : public StVertex {
public:
    StXiVertex();
    ~StXiVertex();
    StXiVertex(dst_xi_vertex_st*,dst_vertex_st*);
    // StXiVertex(const StXiVertex&);        use default
    // const StXiVertex & operator=(const StXiVertex&);

    float dcaBachelorToPrimaryVertex() const;
    float dcaV0ToPrimaryVertex() const;
    float dcaDaughters() const;
    float dcaParentToPrimaryVertex() const;
    const StThreeVector<float>& momentumOfBachelor() const;
    StThreeVector<float> momentumOfV0() const;
    StThreeVector<float> momentum() const;
    StV0Vertex* v0Vertex() const;
    StGlobalTrack* bachelor();
    double chargeOfBachelor(double B);

    void setDcaBachelorToPrimaryVertex(float);
    void setMomentumOfBachelor(const StThreeVector<float>&);
    void setDcaDaughters(float);
    void setDcaParentToPrimaryVertex(float);
    
    void setType(StVertexType);     // overwrite from base class          
    void setV0Vertex(StV0Vertex*);

protected:
    float                mDcaBachelorToPrimaryVertex;
    StThreeVector<float> mMomentumOfBachelor;
    float                mDcaDaughters;
    float                mDcaParentToPrimaryVertex;
    StV0Vertex*          mV0Vertex;          // Pointer to V0 daughter
};

inline float StXiVertex::dcaBachelorToPrimaryVertex () const
{
    return mDcaBachelorToPrimaryVertex;
}

inline const StThreeVector<float>&
StXiVertex::momentumOfBachelor() const { return mMomentumOfBachelor; }

inline StThreeVector<float> StXiVertex::momentum() const
{
    return (mMomentumOfBachelor + momentumOfV0());
}

inline float StXiVertex::dcaDaughters() const { return mDcaDaughters; }

inline float StXiVertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

inline StV0Vertex* StXiVertex::v0Vertex() const { return mV0Vertex; }

inline StGlobalTrack* StXiVertex::bachelor() { return StVertex::daughter(0); }

#endif

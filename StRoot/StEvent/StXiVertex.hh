/***************************************************************************
 *
 * $Id: StXiVertex.hh,v 1.1 1999/02/23 13:46:46 genevb Exp $
 *
 * Author: Gene Vab Buren, Feb 1999
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.hh,v $
 * Revision 1.1  1999/02/23 13:46:46  genevb
 * Add new StXiVertex
 *
 *
 **************************************************************************/
#ifndef StXiVertex_hh
#define StXiVertex_hh
#include "StEvent/StVertex.hh"
#include "StEvent/StV0Vertex.hh"
#include "StEvent/StEnumerations.hh"
#include "tables/dst_vertex.h"
#include "tables/dst_xi_vertex.h"
#include <float.h>

class StXiVertex : public StVertex {
public:
    StXiVertex();
    ~StXiVertex();
    StXiVertex(dst_xi_vertex_st*,dst_vertex_st*,StV0Vertex*);
    // StXiVertex(const StXiVertex&);        use default
    // const StXiVertex & operator=(const StXiVertex&);

    float dcaBachelorToPrimaryVertex() const;
    float dcaV0ToPrimaryVertex() const;
    float dcaDaughters() const;
    float dcaParentToPrimaryVertex() const;
    const StThreeVector<float>& momentumOfBachelor() const;
    StThreeVector<float>& momentumOfV0() const;
    StV0Vertex* v0Vertex() const;

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
StXiVertex::momentumOfBachelor() const
{
    return mMomentumOfBachelor;
}

inline float StXiVertex::dcaDaughters() const { return mDcaDaughters; }

inline float StXiVertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }

inline StV0Vertex* StXiVertex::v0Vertex() const { return mV0Vertex; }

#endif

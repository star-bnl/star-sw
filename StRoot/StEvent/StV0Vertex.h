/***************************************************************************
 *
 * $Id: StV0Vertex.h,v 2.1 1999/10/28 22:28:04 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StV0Vertex.h,v $
 * Revision 2.1  1999/10/28 22:28:04  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:26  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StV0Vertex_hh
#define StV0Vertex_hh
#include "StVertex.h"
#include "StContainers.h"

class dst_v0_vertex_st;

class StV0Vertex : public StVertex {
public:
    StV0Vertex();
    StV0Vertex(const dst_vertex_st&, const dst_v0_vertex_st&);
    // StV0Vertex(const StV0Vertex&);            use default
    // StV0Vertex& operator=(const StV0Vertex&); use default
    ~StV0Vertex();
    
    StVertexId            type() const;
    UInt_t                numberOfDaughters() const;
    StTrack*              daughter(StChargeSign sign);
    const StTrack*        daughter(StChargeSign sign) const;
    StTrack*              daughter(UInt_t);
    const StTrack*        daughter(UInt_t) const;
    StPtrVecTrack         daughters(StTrackFilter&);
    void                  addDaughter(StTrack*);
    void                  removeDaughter(StTrack*);

    Float_t               dcaDaughterToPrimaryVertex(StChargeSign sign) const;
    Float_t               dcaDaughters() const;
    Float_t               dcaParentToPrimaryVertex() const;
    const StThreeVectorF& momentumOfDaughter(StChargeSign sign) const;
    StThreeVectorF        momentum() const;
    
    void setDcaDaughterToPrimaryVertex(StChargeSign, Float_t);
    void setMomentumOfDaughter(StChargeSign, const StThreeVectorF&);
    void setDcaDaughters(Float_t);
    void setDcaParentToPrimaryVertex(Float_t);

private:
    StPtrVecTrack    mDaughters;
    Float_t          mDcaDaughtersToPrimaryVertex[2];
    StThreeVectorF   mMomentumOfDaughters[2];
    Float_t          mDcaDaughters;
    Float_t          mDcaParentToPrimaryVertex;

    StObject* clone();
    ClassDef(StV0Vertex,1)
};
#endif

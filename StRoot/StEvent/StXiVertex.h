/***************************************************************************
 *
 * $Id: StXiVertex.h,v 2.0 1999/10/12 18:43:37 ullrich Exp $
 *
 * Author: Gene Van Buren, Feb 1999, revised Thomas Ullrich Sep 99
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.h,v $
 * Revision 2.0  1999/10/12 18:43:37  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.1  1999/10/28 22:28:18  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:37  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StXiVertex_hh
#define StXiVertex_hh
#include "StVertex.h"
#include "StEnumerations.h"

class dst_vertex_st;
class dst_xi_vertex_st;
class StV0Vertex;

class StXiVertex : public StVertex {
public:
    StXiVertex();
    StXiVertex(const dst_xi_vertex_st&, const dst_vertex_st&);
    // StXiVertex(const StXiVertex&);            use default
    // StXiVertex& operator=(const StXiVertex&); use default
    ~StXiVertex();

    StVertexId            type() const;
    UInt_t                numberOfDaughters() const;
    StTrack*              daughter(UInt_t = 0);
    const StTrack*        daughter(UInt_t = 0) const;
    StPtrVecTrack         daughters(StTrackFilter&);

    Float_t               dcaBachelorToPrimaryVertex() const;
    Float_t               dcaV0ToPrimaryVertex() const;
    Float_t               dcaDaughters() const;
    Float_t               dcaParentToPrimaryVertex() const;
    const StThreeVectorF& momentumOfBachelor() const;
    StThreeVectorF        momentumOfV0() const;
    StThreeVectorF        momentum() const;
    StV0Vertex*           v0Vertex() const;
    StTrack*              bachelor();
    Double_t              chargeOfBachelor();

    void setDcaBachelorToPrimaryVertex(Float_t);
    void setMomentumOfBachelor(const StThreeVectorF&);
    void setDcaDaughters(Float_t);
    void setDcaParentToPrimaryVertex(Float_t);
    void setV0Vertex(StV0Vertex*);
    void addDaughter(StTrack*);
    void removeDaughter(StTrack*);

protected:
    StTrack*               mDaughter;                   //$LINK
    Float_t                mDcaBachelorToPrimaryVertex;
    StThreeVectorF         mMomentumOfBachelor;
    Float_t                mDcaDaughters;
    StV0Vertex*            mV0Vertex;                   //$LINK

    StObject* clone();
    ClassDef(StXiVertex,1)
};
#endif

/***************************************************************************
 *
 * $Id: StXiVertex.h,v 1.1 1999/04/27 01:24:32 fisyak Exp $
 *
 * Author: Gene Van Buren, Feb 1999
 *
 ***************************************************************************
 *
 * Description: vertex class for cascades
 *
 ***************************************************************************
 *
 * $Log: StXiVertex.h,v $
 * Revision 1.1  1999/04/27 01:24:32  fisyak
 * Fix intermidaiate version with pointer instead of referencies
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
#ifdef __ROOT__
#include "TObject.h"
#endif
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:37  ullrich
#include "tables/dst_vertex.h"
#include "tables/dst_xi_vertex.h"
#include "dst_vertex.h"

#ifndef __ROOT__
#include <float.h>
#endif
#include "StEnumerations.h"

class dst_vertex_st;
class StXiVertex : public StVertex {
    StXiVertex(dst_xi_vertex_st*,dst_vertex_st*);
    // StXiVertex(const StXiVertex&);        use default
    // const StXiVertex & operator=(const StXiVertex&);

    Float_t dcaBachelorToPrimaryVertex() const;
    Float_t dcaV0ToPrimaryVertex() const;
    StThreeVectorF& momentumOfV0() const;
    StPtrVecTrack         daughters(StTrackFilter&);
    StV0Vertex* v0Vertex() const;
    StGlobalTrack* bachelor();
    Double_t chargeOfBachelor(Double_t B);
    const StThreeVectorF& momentumOfBachelor() const;
    StThreeVectorF        momentumOfV0() const;
    StThreeVectorF        momentum() const;
    StV0Vertex*           v0Vertex() const;
    StTrack*              bachelor();
    
    void setType(StVertexType);     // overwrite from base class          
    Double_t              chargeOfBachelor();
    void setMomentumOfBachelor(const StThreeVectorF&);
    void setDcaDaughters(Float_t);
    void setV0Vertex(StV0Vertex*);
    StThreeVectorF mMomentumOfBachelor;
#ifdef __ROOT__
	ClassDef(StXiVertex,1)  //StXiVertex structure
#endif

    StV0Vertex*          mV0Vertex;          // Pointer to V0 daughter
  ClassDef(StXiVertex,1)  //StXiVertex structure
    StThreeVectorF         mMomentumOfBachelor;

inline Float_t StXiVertex::dcaBachelorToPrimaryVertex () const
{
    return mDcaBachelorToPrimaryVertex;
StXiVertex::momentumOfBachelor() const
StXiVertex::momentumOfBachelor() const { return mMomentumOfBachelor; }
    return mMomentumOfBachelor;
inline StThreeVectorF StXiVertex::momentum() const
{
    return (mMomentumOfBachelor + momentumOfV0());
}

inline Float_t StXiVertex::dcaDaughters() const { return mDcaDaughters; }

inline StV0Vertex* StXiVertex::v0Vertex() const { return mV0Vertex; }

inline StGlobalTrack* StXiVertex::bachelor() { return StVertex::daughter(0); }

    Float_t                mDcaDaughters;
    StV0Vertex*            mV0Vertex;                   //$LINK

    StObject* clone();
    ClassDef(StXiVertex,1)
};
#endif

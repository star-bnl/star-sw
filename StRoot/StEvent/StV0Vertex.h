/***************************************************************************
 *
 * $Id: StV0Vertex.h,v 1.7 1999/09/24 01:23:02 fisyak Exp $
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
 * $Log: StV0Vertex.h,v $
 * Revision 1.7  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/04/30 13:16:30  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/28 22:27:39  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.9  1999/04/19 15:54:10  genevb
 * Added momentum() to vertex classes
 *
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
 * Revision 2.0  1999/10/12 18:43:26  ullrich
 * Completely Revised for New Version
 *
#include "StObject.h"
 **************************************************************************/
#include "StEnumerations.h"
#include "StGlobalTrack.h"
#include "dst_vertex.h"
#include "dst_v0_vertex.h"
#ifndef __ROOT__
#include <float.h>
#endif

class dst_v0_vertex_st;

    StV0Vertex(const dst_vertex_st&, const dst_v0_vertex_st&);
    StV0Vertex(dst_v0_vertex_st*,dst_vertex_st*);
    // StV0Vertex(const StV0Vertex&);        use default
    // const StV0Vertex & operator=(const StV0Vertex&);

    virtual StGlobalTrack* daughter(StTrackSign sign, Double_t B);
    virtual Float_t dcaDaughterToPrimaryVertex(StTrackSign sign) const;
    virtual Float_t dcaDaughters() const;
    virtual Float_t dcaParentToPrimaryVertex() const;
    virtual const StThreeVectorF& momentumOfDaughter(StTrackSign sign) const;
    virtual StThreeVectorF momentum() const;

    virtual void setDcaDaughterToPrimaryVertex(StTrackSign sign, Float_t);
    virtual void setMomentumOfDaughter(StTrackSign sign, const StThreeVectorF&);
    virtual void setDcaDaughters(Float_t);
    virtual void setDcaParentToPrimaryVertex(Float_t);
    // StV0Vertex(const StV0Vertex&);            use default
    void setType(StVertexType);     // overwrite from base class          
    StThreeVectorF   mMomentumOfDaughters[2];
protected:
    Float_t                mDcaDaughtersToPrimaryVertex[2];
    StThreeVectorF mMomentumOfDaughters[2];
    Float_t                mDcaDaughters;
    Float_t                mDcaParentToPrimaryVertex;
  ClassDef(StV0Vertex,1)  //StV0Vertex structure
    Float_t          mDcaParentToPrimaryVertex;

inline Float_t StV0Vertex::dcaDaughterToPrimaryVertex (StTrackSign sign) const
{
    return mDcaDaughtersToPrimaryVertex[sign];
}

inline const StThreeVectorF&
StV0Vertex::momentumOfDaughter(StTrackSign sign) const
{
    return mMomentumOfDaughters[sign];
}

inline StThreeVectorF StV0Vertex::momentum() const
{
    return (mMomentumOfDaughters[negativeTrack] +
            mMomentumOfDaughters[positiveTrack]);
}

inline Float_t StV0Vertex::dcaDaughters() const { return mDcaDaughters; }

inline Float_t StV0Vertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }


    ClassDef(StV0Vertex,1)
};
#endif

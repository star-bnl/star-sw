/***************************************************************************
 *
 * $Id: StV0Vertex.h,v 1.2 1999/02/09 19:53:53 fisyak Exp $
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
 * Revision 1.2  1999/02/09 19:53:53  fisyak
 * Import new Torre staff
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
#ifdef __ROOT__
#include "TObject.h"
#endif
 * Completely Revised for New Version
#include "tables/dst_v0_vertex.h"
#include "dst_vertex.h"

#ifndef __ROOT__
#include <float.h>
#endif

    StV0Vertex(dst_v0_vertex_st*);

    StV0Vertex(const dst_vertex_st&, const dst_v0_vertex_st&);
    StV0Vertex(dst_v0_vertex_st*,dst_vertex_st*);
    Float_t dcaDaughterToPrimaryVertex(UInt_t i) const;
    Float_t dcaDaughters() const;
    Float_t dcaParentToPrimaryVertex() const;

    void setDcaDaughtersToPrimaryVertex(UInt_t, Float_t);
    void setDcaDaughters(Float_t);
    void setDcaParentToPrimaryVertex(Float_t);
    virtual void setMomentumOfDaughter(StTrackSign sign, const StThreeVectorF&);
    virtual void setDcaDaughters(Float_t);
    virtual void setDcaParentToPrimaryVertex(Float_t);
    // StV0Vertex(const StV0Vertex&);            use default
    Float_t mDcaDaughtersToPrimaryVertex[2];
    Float_t mDcaDaughters;
    Float_t mDcaParentToPrimaryVertex;
#ifdef __ROOT__
	ClassDef(StV0Vertex,1)  //StV0Vertex structure
#endif
    Float_t                mDcaDaughters;

inline Float_t StV0Vertex::dcaDaughterToPrimaryVertex (UInt_t i) const

    if (i < 2)
	return mDcaDaughtersToPrimaryVertex[i];
    else
	return FLT_MAX;
{
    return (mMomentumOfDaughters[negativeTrack] +
            mMomentumOfDaughters[positiveTrack]);
}



inline Float_t StV0Vertex::dcaParentToPrimaryVertex() const { return mDcaParentToPrimaryVertex; }


    ClassDef(StV0Vertex,1)
};
#endif

/***************************************************************************
 *
 * $Id: StKinkVertex.h,v 2.2 1999/10/28 22:25:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StKinkVertex.h,v $
 * Revision 2.2  1999/10/28 22:25:56  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.2  1999/10/28 22:25:56  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StKinkVertex_hh
#define StKinkVertex_hh
#include "StVertex.h"

class dst_tkf_vertex_st;
class StParticleDefinition;

class StKinkVertex : public StVertex {
public:
    StKinkVertex();
    StKinkVertex(const dst_vertex_st&, const dst_tkf_vertex_st&);
    // StKinkVertex(const StKinkVertex&);            use default
    // StKinkVertex& operator=(const StKinkVertex&); use default
    ~StKinkVertex();
    
    StVertexId     type() const;
    UInt_t         numberOfDaughters() const;
    StTrack*       daughter(UInt_t = 0);
    const StTrack* daughter(UInt_t = 0) const;
    StPtrVecTrack  daughters(StTrackFilter&);

    StParticleDefinition* pidParent() const;
    StParticleDefinition* pidDaughter() const;
    UShort_t              geantIdParent() const;
    UShort_t              geantIdDaughter() const;
    Float_t               dcaParentDaughter() const;
    Float_t               dcaDaughterPrimaryVertex() const;
    Float_t               dcaParentPrimaryVertex() const;
    Float_t               hitDistanceParentDaughter() const;
    Float_t               hitDistanceParentVertex() const;
    Float_t               dE(UInt_t i) const;
    Float_t               decayAngle() const;
    Float_t               decayAngleCM() const;
    const StThreeVectorF& parentMomentum() const;
    StThreeVectorF&       parentMomentum();
    const StThreeVectorF& daughterMomentum() const;
    StThreeVectorF&       daughterMomentum();

    void setGeantIdParent(UShort_t);
    void setGeantIdDaughter(UShort_t);
    void setDcaParentDaughter(Float_t);
    void setDcaDaughterPrimaryVertex(Float_t);
    void setDcaParentPrimaryVertex(Float_t);
    void setHitDistanceParentDaughter(Float_t);
    void setHitDistanceParentVertex(Float_t);
    void setdE(UInt_t, Float_t);
    void setDecayAngle(Float_t);
    void setDecayAngleCM(Float_t);
    void setParentMomentum(const StThreeVectorF&);
    void setDaughterMomentum(const StThreeVectorF&);
    void addDaughter(StTrack*);
    void removeDaughter(StTrack*);

protected:
    StTrack*       mDaughter;         //$LINK
    UShort_t       mParentGeantId;
    UShort_t       mDaughterGeantId;
    Float_t        mDcaParentDaughter;
    Float_t        mDcaDaughterPrimaryVertex;
    Float_t        mDcaParentPrimaryVertex;
    Float_t        mHitDistanceParentDaughter;
    Float_t        mHitDistanceParentVertex;
    Float_t        mDeltaEnergy[3];
    Float_t        mDecayAngle;
    Float_t        mDecayAngleCM;
    StThreeVectorF mParentMomentum;
    StThreeVectorF mDaughterMomentum;

    StObject* clone();
    ClassDef(StVertex,1)
};
#endif

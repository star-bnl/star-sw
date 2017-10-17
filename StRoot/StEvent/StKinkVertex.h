/*!
 * \class StKinkVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StKinkVertex.h,v 2.9 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StKinkVertex.h,v $
 * Revision 2.9  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.8  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.7  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.5  2001/04/05 04:00:38  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:34:52  perev
 * clone() -> clone() const
 *
 * Revision 2.3  1999/11/04 16:33:26  ullrich
 * Fixed wrong option to ClassDef macro.
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

class StParticleDefinition;

class StKinkVertex : public StVertex {
public:
    StKinkVertex();
    // StKinkVertex(const StKinkVertex&);            use default
    // StKinkVertex& operator=(const StKinkVertex&); use default
    ~StKinkVertex();
    
    StVertexId     type() const;
    unsigned int   numberOfDaughters() const;
    StTrack*       daughter(unsigned int = 0);
    const StTrack* daughter(unsigned int = 0) const;
    StPtrVecTrack  daughters(StTrackFilter&);

    StParticleDefinition* pidParent() const;
    StParticleDefinition* pidDaughter() const;
    unsigned short        geantIdParent() const;
    unsigned short        geantIdDaughter() const;
    float                 dcaParentDaughter() const;
    float                 dcaDaughterPrimaryVertex() const;
    float                 dcaParentPrimaryVertex() const;
    float                 hitDistanceParentDaughter() const;
    float                 hitDistanceParentVertex() const;
    float                 dE(unsigned int i) const;
    float                 decayAngle() const;
    float                 decayAngleCM() const;
    const StThreeVectorF& parentMomentum() const;
    StThreeVectorF&       parentMomentum();
    const StThreeVectorF& daughterMomentum() const;
    StThreeVectorF&       daughterMomentum();

    void setGeantIdParent(unsigned short);
    void setGeantIdDaughter(unsigned short);
    void setDcaParentDaughter(float);
    void setDcaDaughterPrimaryVertex(float);
    void setDcaParentPrimaryVertex(float);
    void setHitDistanceParentDaughter(float);
    void setHitDistanceParentVertex(float);
    void setdE(unsigned int, float);
    void setDecayAngle(float);
    void setDecayAngleCM(float);
    void setParentMomentum(const StThreeVectorF&);
    void setDaughterMomentum(const StThreeVectorF&);
    void addDaughter(StTrack*);
    void removeDaughter(StTrack*);

protected:
//    StTrack*       mDaughter;         //$LINK
#ifdef __CINT__
    StObjLink        mDaughter;		
#else
    StLink<StTrack>  mDaughter;		
#endif //__CINT__
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

    ClassDef(StKinkVertex,2)
};
#endif

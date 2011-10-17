/*!
 * \class StVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StVertex.h,v 2.12 2011/10/17 00:13:49 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.h,v $
 * Revision 2.12  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.11  2011/03/31 19:29:01  fisyak
 * Add IdTruth information for tracks and vertices
 *
 * Revision 2.10  2009/11/23 16:34:08  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.9  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.8  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.7  2001/05/30 17:45:55  perev
 * StEvent branching
 *
 * Revision 2.6  2001/04/05 04:00:46  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2000/03/08 14:29:56  ullrich
 * New method probChiSquared() added.
 *
 * Revision 2.4  2000/02/10 18:49:08  ullrich
 * Fixed typo introduced at last check-in.
 *
 * Revision 2.3  2000/02/10 16:32:21  ullrich
 * flag changed from unsigned to signed long
 *
 * Revision 2.2  2000/01/11 19:22:14  ullrich
 * Added non-const parent() method.
 *
 * Revision 2.1  1999/10/28 22:28:09  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:32  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StVertex_hh
#define StVertex_hh
#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"
#include "StContainers.h"

class StTrack;
class StTrackFilter;

class StVertex : public StMeasuredPoint {
public:
    StVertex();
    // StVertex(const StVertex&);            use default
    // StVertex& operator=(const StVertex&); use default
    virtual ~StVertex();
    
    int operator==(const StVertex&) const;
    int operator!=(const StVertex&) const;

    virtual StVertexId     type() const = 0;
    int                    flag() const;
    float                  chiSquared() const;
    float                  probChiSquared() const;
    StMatrixF              covariantMatrix() const;  // overwrite inherited
    StThreeVectorF         positionError() const;    // overwrite inherited
    StTrack*               parent();
    const StTrack*         parent() const;
    virtual unsigned int   numberOfDaughters() const = 0;
    virtual StTrack*       daughter(unsigned int) = 0;
    virtual const StTrack* daughter(unsigned int) const = 0;
    virtual StPtrVecTrack  daughters(StTrackFilter&) = 0;

    virtual void setFlag(int);
    virtual void setCovariantMatrix(float[6]);
    virtual void setChiSquared(float);
    virtual void setProbChiSquared(float);
    virtual void setParent(StTrack*);
    virtual void addDaughter(StTrack*) = 0;
    virtual void removeDaughter(StTrack*) = 0;
    Int_t            idTruth() const { return mIdTruth;}
    Int_t            qaTruth() const { return mQuality; }
    Int_t            idParent() const { return mIdParent;}
    void          setIdTruth(Int_t idtru,Int_t qatru=0) {mIdTruth = (UShort_t) idtru; mQuality = (UShort_t) qatru;}
    void          setIdParent(Int_t id) {mIdParent = id;}
    void          setIdTruth(); 				//setting on track info
protected:
    StVertexId    mType;
    Int_t         mFlag;
    Float_t       mCovariantMatrix[6];
    Float_t       mChiSquared;
    Float_t       mProbChiSquared;
    UShort_t      mIdTruth; // MC vertex id if any 
    UShort_t      mQuality; // quality of this information (percentage of tracks coming the above MC Vertex)
    Int_t         mIdParent;// Id of MC parent track
//  StTrack*      mParent;           	//$LINK
#ifdef __CINT__
    StObjLink     mParent;            
#else
    StLink<StTrack> mParent;            
#endif //__CINT__

    ClassDef(StVertex,4)
};
#endif

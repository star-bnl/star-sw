/*!
 * \class StVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StVertex.h,v 2.20 2017/06/01 23:46:27 smirnovd Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.h,v $
 * Revision 2.20  2017/06/01 23:46:27  smirnovd
 * StVertex: Convenience getter for covariance matrix as array of doubles
 *
 * Revision 2.19  2017/05/04 23:52:35  smirnovd
 * StVertex: Convenience setter for covariance matrix from array of doubles
 *
 * Revision 2.18  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.17  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.15  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.13  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
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
#include "Riostream.h"
#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"
#include "StContainers.h"
#include "StEnumerations.h"
class StTrack;
class StTrackFilter;

class StVertex : public StMeasuredPoint {
public:
    StVertex();
    virtual ~StVertex() {}
    
    int operator==(const StVertex&) const;
    int operator!=(const StVertex&) const;

    virtual StVertexId     type() const {return mType; }
    int                    flag() const {return mFlag; }
    float                  chiSquared() const { return mChiSquared; }
    float                  probChiSquared() const { return mProbChiSquared; }
    StMatrixF              covariantMatrix() const;  // overwrite inherited
    void                   covarianceMatrix(double covM[6]) const { std::copy(mCovariantMatrix, mCovariantMatrix + 6, covM); }
    StThreeVectorF         positionError() const;    // overwrite inherited
    StTrack*               parent()        { return mParent; }
    const StTrack*         parent() const  { return mParent; }
    virtual unsigned int   numberOfDaughters()    const {NotImplemented("numberOfDaughters"); return 0;}
    virtual unsigned int   numberOfGoodTracks()   const {NotImplemented("numberOfGoodTracks"); return 0;}
    virtual StTrack*       daughter(unsigned int)       {NotImplemented("daughter"); return 0;}
    virtual const StTrack* daughter(unsigned int) const {NotImplemented("daughter"); return 0;}
    virtual StPtrVecTrack  daughters(StTrackFilter&)    {NotImplemented("daughters"); return 0;}

    virtual void setFlag(int val) { mFlag = val; }
    virtual void setCovariantMatrix(float[6]);
            void setCovariantMatrix(const double val[6]) { std::copy(val, val+6, mCovariantMatrix); }
    virtual void setChiSquared(float val) { mChiSquared = val; }
    virtual void setProbChiSquared(float val) { mProbChiSquared = val; }
    virtual void setParent(StTrack*);
    virtual void addDaughter(StTrack*) = 0;
    virtual void removeDaughter(StTrack*) = 0;
    int          idTruth() const { return mIdTruth;}
    int          qaTruth() const { return mQuality; }
    int          idParent() const { return mIdParent;}
    void         setIdTruth(int idtru, int qatru=0) {mIdTruth = idtru; mQuality = static_cast<unsigned short>(qatru);}
    void         setIdParent(Int_t id) {mIdParent = id;}
    void         setIdTruth(); 				//setting on track info

    virtual void setPrimaryVtx()      {SETBIT(mFlag,kPrimaryVtxId);} 
    virtual void setV0Vtx()           {SETBIT(mFlag,kV0VtxId);}	   
    virtual void setXiVtx()           {SETBIT(mFlag,kXiVtxId);}	   
    virtual void setKinkVertex()      {SETBIT(mFlag,kKinkVtxId);}    
    virtual void setBeamConstrained() {SETBIT(mFlag,kBEAMConstrVtxId);}
    virtual void setRejected()        {SETBIT(mFlag,kRejectedVtxId);}

    bool        isPrimaryVtx()      const {return TESTBIT(mFlag,kPrimaryVtxId);}
    bool        isV0Vtx()           const {return TESTBIT(mFlag,kV0VtxId);}
    bool        isXiVtx()           const {return TESTBIT(mFlag,kXiVtxId);}
    bool        isKinkVertex()      const {return TESTBIT(mFlag,kKinkVtxId);}
    bool        isBeamConstrained() const {return TESTBIT(mFlag,kBEAMConstrVtxId);}
    bool        isRejected()        const {return TESTBIT(mFlag,kRejectedVtxId);}
    void Print(Option_t *option="") const {cout << option << *this << endl; }
    static void   SetNoFitPointCutForGoodTrack(UInt_t val) {fgNoFitPointCutForGoodTrack = val;}
    static UInt_t NoFitPointCutForGoodTrack() {return fgNoFitPointCutForGoodTrack;}
    
protected:
    void          NotImplemented(const char *method) const;

protected:
    StVertexId    mType;
    Char_t        mBeg[1]; //!
    Int_t         mFlag;
    Float_t       mCovariantMatrix[6];
    Float_t       mChiSquared;
    Float_t       mProbChiSquared;
    Int_t         mIdTruth; // MC vertex id if any
    UShort_t      mQuality; // quality of this information (percentage of tracks coming the above MC Vertex)
    Int_t         mIdParent;// Id of MC parent track
    Char_t        mEnd[1]; //!
    static UInt_t fgNoFitPointCutForGoodTrack;
#ifdef __CINT__
    StObjLink     mParent;            
#else
    StLink<StTrack> mParent;            
#endif //__CINT__

    ClassDef(StVertex,5)
};
#endif

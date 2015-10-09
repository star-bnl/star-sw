/*!
 * \class StVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StVertex.h,v 2.18 2015/10/09 17:46:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.h,v $
 * Revision 2.18  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.17  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 *
 * Revision 2.16  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.14  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
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
#include "StTrackMassFit.h"
class StTrack;
class StTrackFilter;
class StVertex;
ostream&  operator<<(ostream& os,  const StVertex& v);

class StVertex : public StMeasuredPoint {
public:
    StVertex();
    virtual ~StVertex() {}
    
    int operator==(const StVertex&) const;
    int operator!=(const StVertex&) const;

    virtual StVertexId     type() const {return mType; }
    virtual Int_t          key()  const {return mKey;}
    Int_t                  flag() const {return mFlag; }
    Float_t                chiSquared() const { return mChiSquared; }
    Float_t                probChiSquared() const { return mProbChiSquared; }
    StMatrixF              covariantMatrix() const;  // overwrite inherited
    StThreeVectorF         positionError() const;    // overwrite inherited
    StTrackMassFit*        parent()        { return mParent; }
    const StTrackMassFit*  parent() const  { return mParent; }
    StTrackMassFit*        parentMF()        { return parent(); }
    const StTrackMassFit*  parentMF() const  { return parent(); }
    virtual UInt_t         numberOfDaughters()    const {NotImplemented("numberOfDaughters"); return 0;}
    virtual UInt_t         numberOfFittedTracks() const {NotImplemented("numberOfFittedTracks"); return 0;}
    virtual UInt_t         numberOfGoodTracks()   const {NotImplemented("numberOfGoodTracks"); return 0;}
    virtual StTrack*       daughter(UInt_t)       {NotImplemented("daughter"); return 0;}
    virtual const StTrack* daughter(UInt_t) const {NotImplemented("daughter"); return 0;}
    virtual UInt_t         numberOfMassFits()    const {return mMassFits.size();}
    virtual StTrackMassFit       *massFit(UInt_t);
    virtual const StTrackMassFit *massFit(UInt_t) const;
    virtual StPtrVecTrackMassFit  massFits(StTrackFilter&);
    //?    virtual const StPtrVecTrackMassFit&  massFits() const  {return *&mMassFits;}

    virtual void setKey(Int_t key) {mKey = key;}
    virtual void setFlag(Int_t val) { mFlag = val; }
    virtual void setCovariantMatrix(Float_t[6]);
    virtual void setChiSquared(Float_t val) { mChiSquared = val; }
    virtual void setProbChiSquared(Float_t val) { mProbChiSquared = val; }
    virtual void setParent(StTrackMassFit*);
    virtual void addDaughter(StTrack*) {NotImplemented("addDaughter");}
    virtual void addMassFit(StTrackMassFit*);
    virtual void removeMassFit(StTrackMassFit*);
    Int_t            idTruth() const { return mIdTruth;}
    Int_t            qaTruth() const { return mQuality; }
    Int_t            idParent() const { return mIdParent;}
    void          setIdTruth(Int_t idtru,Int_t qatru=0) {mIdTruth = idtru; mQuality = (UShort_t) qatru;}
    void          setIdParent(Int_t id) {mIdParent = id;}
    void          setIdTruth(); 				//setting on track info

    virtual void setPrimaryVtx()      {SETBIT(mFlag,kPrimaryVtxId);} 
    virtual void setV0Vtx()           {SETBIT(mFlag,kV0VtxId);}	   
    virtual void setXiVtx()           {SETBIT(mFlag,kXiVtxId);}	   
    virtual void setKinkVertex()      {SETBIT(mFlag,kKinkVtxId);}    
    virtual void setBeamConstrained() {SETBIT(mFlag,kBEAMConstrVtxId);}
    virtual void setRejected()        {SETBIT(mFlag,kRejectedVtxId);}
    
    Bool_t        isPrimaryVtx()      const {return TESTBIT(mFlag,kPrimaryVtxId);} 
    Bool_t        isV0Vtx()           const {return TESTBIT(mFlag,kV0VtxId);}	   
    Bool_t        isXiVtx()           const {return TESTBIT(mFlag,kXiVtxId);}	   
    Bool_t        isKinkVertex()      const {return TESTBIT(mFlag,kKinkVtxId);}    
    Bool_t        isBeamConstrained() const {return TESTBIT(mFlag,kBEAMConstrVtxId);}
    Bool_t        isRejected()        const {return TESTBIT(mFlag,kRejectedVtxId);}
    virtual  void Print(Option_t *option="") const {cout << option << *this << endl; }
    static void   SetNoFitPointCutForGoodTrack(UInt_t val) {fgNoFitPointCutForGoodTrack = val;}
    static UInt_t NoFitPointCutForGoodTrack() {return fgNoFitPointCutForGoodTrack;}

protected:
    void          NotImplemented(const Char_t *method) const;
    StVertexId    mType;
    Char_t        mBeg[1]; //!
    Int_t         mKey;
    Int_t         mFlag;
    Float_t       mCovariantMatrix[6];
    Float_t       mChiSquared;
    Float_t       mProbChiSquared;
    Int_t         mIdTruth; // MC vertex id if any
    UShort_t      mQuality; // quality of this information (percentage of tracks coming the above MC Vertex)
    Int_t         mIdParent;// Id of MC parent track
    StTrackMassFit*mParent;// 
    Char_t        mEnd[1]; //!
    static        UInt_t fgNoFitPointCutForGoodTrack;
protected:
    StSPtrVecTrackMassFit    mMassFits;
    
    ClassDef(StVertex,6)
};
#endif

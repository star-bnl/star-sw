/*!
 * \class StPrimaryVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StPrimaryVertex.h,v 2.13 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.h,v $
 * Revision 2.13  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.12  2006/04/07 18:21:28  ullrich
 * Added data member mMeanDip incl. access functions (Marco).
 *
 * Revision 2.11  2005/07/15 20:17:35  ullrich
 * Corrected spelling in membrane
 *
 * Revision 2.10  2005/06/15 21:54:34  ullrich
 * Added members and methods to identify used vertex finder and store vertex quality.
 *
 * Revision 2.9  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2002/04/18 23:38:21  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.7  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/04/05 04:00:39  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:34:54  perev
 * clone() -> clone() const
 *
 * Revision 2.4  1999/11/09 15:44:22  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.3  1999/11/04 20:36:17  ullrich
 * New method to obtain daughter container directly
 *
 * Revision 2.2  1999/10/28 22:26:19  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPrimaryVertex_hh
#define StPrimaryVertex_hh
#include "StVertex.h"

class StPrimaryVertex : public StVertex {
public:
    StPrimaryVertex();
    // StPrimaryVertex(const StPrimaryVertex&);            use default
    // StPrimaryVertex& operator=(const StPrimaryVertex&); use default
    ~StPrimaryVertex();
    
    StVertexId                   type() const;
    unsigned int                 numberOfDaughters() const;
    unsigned int                 numberOfDaughters(StTrackType) const;      // remove when EST becomes standard
    StTrack*                     daughter(unsigned int);
    const StTrack*               daughter(unsigned int) const;
    StTrack*                     daughter(unsigned int, StTrackType);       // remove when EST becomes standard
    const StTrack*               daughter(unsigned int, StTrackType) const; // remove when EST becomes standard
    StPtrVecTrack                daughters(StTrackFilter&);
    StPtrVecTrack                daughters(StTrackFilter&, StTrackType);    // remove when EST becomes standard
    StSPtrVecPrimaryTrack&       daughters(StTrackType = primary);          // change when EST becomes standard
    const StSPtrVecPrimaryTrack& daughters(StTrackType = primary) const;    // change when EST becomes standard
    void                         addDaughter(StTrack*);
    void                         removeDaughter(StTrack*);

    void setParent(StTrack*);     // overwrite inherited

    //
    //  Vertex finder specifics
    //
    StVertexFinderId vertexFinderId() const; 
    unsigned short  numTracksUsedInFinder() const; 
    unsigned short  numMatchesWithCTB() const; 
    unsigned short  numMatchesWithBEMC() const; 
    unsigned short  numMatchesWithEEMC() const; 
    unsigned short  numTracksCrossingCentralMembrane() const;  
    float           meanDip() const;  
    float           sumOfTrackPt() const; 
    float           ranking() const;
    
    void setVertexFinderId(StVertexFinderId);
    void setNumTracksUsedInFinder(unsigned short);
    void setNumMatchesWithCTB(unsigned short);
    void setNumMatchesWithBEMC(unsigned short);
    void setNumMatchesWithEEMC(unsigned short);
    void setNumTracksCrossingCentralMembrane(unsigned short);
    void setMeanDip(float);
    void setSumOfTrackPt(float);
    void setRanking(float);

private:
    void init();
    
protected:
    StSPtrVecPrimaryTrack    mDaughters;
    StSPtrVecPrimaryTrack    mEstDaughters;  // remove when EST becomes standard

private:
    StVertexFinderId mVertexFinderId; 
    UShort_t         mNumTracksUsedInFinder;
    UShort_t         mNumMatchesWithCTB; 
    UShort_t         mNumMatchesWithBEMC;
    UShort_t         mNumMatchesWithEEMC;
    UShort_t         mNumTracksCrossingCentralMembrane; 
    Float_t          mMeanDip;
    Float_t          mSumOfTrackPt;
    Float_t          mRanking;    

    ClassDef(StPrimaryVertex,4)
};

inline StVertexFinderId StPrimaryVertex::vertexFinderId() const {return mVertexFinderId;}
inline unsigned short StPrimaryVertex::numTracksUsedInFinder() const {return mNumTracksUsedInFinder;}
inline unsigned short StPrimaryVertex::numMatchesWithCTB() const {return mNumMatchesWithCTB;} 
inline unsigned short StPrimaryVertex::numMatchesWithBEMC() const {return mNumMatchesWithBEMC;} 
inline unsigned short StPrimaryVertex::numMatchesWithEEMC() const {return mNumMatchesWithEEMC;} 
inline unsigned short StPrimaryVertex::numTracksCrossingCentralMembrane() const {return mNumTracksCrossingCentralMembrane;} 
inline float StPrimaryVertex::meanDip() const {return mMeanDip;}
inline float StPrimaryVertex::sumOfTrackPt() const {return mSumOfTrackPt;}
inline float StPrimaryVertex::ranking() const {return mRanking;}
inline void StPrimaryVertex::setVertexFinderId(StVertexFinderId val) {mVertexFinderId = val;}
inline void StPrimaryVertex::setNumTracksUsedInFinder(unsigned short val) {mNumTracksUsedInFinder = val;}
inline void StPrimaryVertex::setNumMatchesWithCTB(unsigned short val) {mNumMatchesWithCTB = val;}
inline void StPrimaryVertex::setNumMatchesWithBEMC(unsigned short val) {mNumMatchesWithBEMC = val;}
inline void StPrimaryVertex::setNumMatchesWithEEMC(unsigned short val) {mNumMatchesWithEEMC = val;}
inline void StPrimaryVertex::setNumTracksCrossingCentralMembrane(unsigned short val) {mNumTracksCrossingCentralMembrane = val;}
inline void StPrimaryVertex::setMeanDip(float val) {mMeanDip = val;}
inline void StPrimaryVertex::setSumOfTrackPt(float val) {mSumOfTrackPt = val;}
inline void StPrimaryVertex::setRanking(float val) {mRanking = val;}

#endif

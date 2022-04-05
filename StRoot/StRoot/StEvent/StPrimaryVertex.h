/*!
 * \class StPrimaryVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StPrimaryVertex.h,v 2.16 2012/09/16 21:37:13 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.h,v $
 * Revision 2.16  2012/09/16 21:37:13  fisyak
 * Add no. of Tpc West Only and East only tracks
 *
 * Revision 2.15  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.14  2009/11/23 22:25:21  ullrich
 * Added new member mNumMatchesWithBTOF and related access fcts.
 *
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
class StPrimaryVertex;
ostream&  operator<<(ostream& os,  const StPrimaryVertex& v);
class StPrimaryVertex : public StVertex {
public:
    StPrimaryVertex();
    // StPrimaryVertex(const StPrimaryVertex&);            use default
    // StPrimaryVertex& operator=(const StPrimaryVertex&); use default
    ~StPrimaryVertex();
    
    StVertexId                   type() const;
    UInt_t                       numberOfDaughters() const;
    UInt_t                       numberOfGoodTracks() const; 
    StTrack*                     daughter(UInt_t); 
    const StTrack*               daughter(UInt_t) const;
    StSPtrVecPrimaryTrack       &daughters()       {return mDaughters;}
    const StSPtrVecPrimaryTrack &daughters() const {return mDaughters;}
    StPtrVecTrack                daughters(StTrackFilter&);
    void                         addDaughter(StTrack*);
    void                         removeDaughter(StTrack*);
    void setParent(StTrack*);     // overwrite inherited
    //
    //  Vertex finder specifics
    //
    StVertexFinderId vertexFinderId()           const {return mVertexFinderId;}
    UShort_t numTracksUsedInFinder()            const {return mNumTracksUsedInFinder;} 		
    UShort_t numMatchesWithCTB()     		const {return mNumMatchesWithCTB;}    
    UShort_t numMatchesWithTOF()     		const {return mNumMatchesWithTOF;}    
    UShort_t numMatchesWithBTOF()    		const {return mNumMatchesWithTOF;}    
    UShort_t numMatchesWithBEMC()    		const {return mNumMatchesWithBEMC;}   
    UShort_t numMatchesWithEEMC()    		const {return mNumMatchesWithEEMC;}   
    UShort_t numNotMatchesWithCTB()  		const {return mNumNotMatchesWithCTB;} 
    UShort_t numNotMatchesWithTOF()  		const {return mNumNotMatchesWithTOF;} 
    UShort_t numNotMatchesWithBTOF() 		const {return mNumNotMatchesWithTOF;} 
    UShort_t numNotMatchesWithBEMC()  		const {return mNumNotMatchesWithBEMC;}
    UShort_t numNotMatchesWithEEMC()  		const {return mNumNotMatchesWithEEMC;}
    UShort_t numTracksCrossingCentralMembrane() const {return mNumTracksCrossingCentralMembrane;} 
    Float_t  meanDip()                          const {return mMeanDip;}
    Float_t  sumOfTrackPt()                     const {return mSumOfTrackPt;}
    Float_t  ranking()                          const {return mRanking;}
    UShort_t numTracksWithPromptHit()           const {return mNumTracksWithPromptHit;}
    UShort_t numPostXTracks()            const {return mNumPostXTracks;}
    UShort_t numTracksTpcWestOnly() const {return mNumTracksTpcWestOnly;} 
    UShort_t numTracksTpcEastOnly() const {return mNumTracksTpcEastOnly;} 
    void setTrackNumbers();
    void setNumMatchesWithCTB(UShort_t val)     {mNumMatchesWithCTB  = val;} 
    void setNumMatchesWithTOF(UShort_t val)  	{mNumMatchesWithTOF  = val;} 
    void setNumMatchesWithBTOF(UShort_t val) 	{mNumMatchesWithTOF  = val;} 
    void setNumMatchesWithBEMC(UShort_t val) 	{mNumMatchesWithBEMC = val;}
    void setNumMatchesWithEEMC(UShort_t val) 	{mNumMatchesWithEEMC = val;}
    void setNumNotMatchesWithCTB(UShort_t val)  {mNumNotMatchesWithCTB  = val;}
    void setNumNotMatchesWithTOF(UShort_t val)  {mNumNotMatchesWithTOF  = val;}
    void setNumNotMatchesWithBTOF(UShort_t val) {mNumNotMatchesWithTOF  = val;}
    void setNumNotMatchesWithBEMC(UShort_t val) {mNumNotMatchesWithBEMC = val;}
    void setNumNotMatchesWithEEMC(UShort_t val) {mNumNotMatchesWithEEMC = val;}
    void setVertexFinderId(StVertexFinderId val)           {mVertexFinderId = val;}
    void setNumTracksUsedInFinder(UShort_t val)            {mNumTracksUsedInFinder = val;}
    void setNumTracksCrossingCentralMembrane(UShort_t val) {mNumTracksCrossingCentralMembrane = val;}
    void setMeanDip(Float_t val)                           {mMeanDip = val;}
    void setSumOfTrackPt(Float_t val)                      {mSumOfTrackPt = val;}
    void setRanking(Float_t val)                           {mRanking = val;}
    void setNumTracksWithPromptHit(UShort_t p)             {mNumTracksWithPromptHit = p;}
    void setNumPostXTracks(UShort_t p)              {mNumPostXTracks = p;}
    void setNumTracksTpcWestOnly(UShort_t val) {mNumTracksTpcWestOnly = val;}
    void setNumTracksTpcEastOnly(UShort_t val) {mNumTracksTpcEastOnly = val;}
    void Print(Option_t *option="") const {cout << option << *this << endl; }
private:
    void init();
protected:
    StSPtrVecPrimaryTrack    mDaughters;

private:
    StVertexFinderId mVertexFinderId; 
    Char_t           mBeg[1]; //! 
    UShort_t         mNumTracksUsedInFinder;
    UShort_t         mNumMatchesWithCTB; 
    UShort_t         mNumMatchesWithTOF; 
    UShort_t         mNumMatchesWithBEMC;
    UShort_t         mNumMatchesWithEEMC;
    UShort_t         mNumNotMatchesWithCTB; 
    UShort_t         mNumNotMatchesWithTOF; 
    UShort_t         mNumNotMatchesWithBEMC;
    UShort_t         mNumNotMatchesWithEEMC;
    UShort_t         mNumTracksCrossingCentralMembrane; 
    Float_t          mMeanDip;
    Float_t          mSumOfTrackPt;
    Float_t          mRanking;
    UShort_t         mNumTracksWithPromptHit;
    UShort_t         mNumPostXTracks;
    UShort_t         mNumTracksTpcWestOnly; 
    UShort_t         mNumTracksTpcEastOnly; 
    Char_t           mEnd[1]; //!
    ClassDef(StPrimaryVertex,7)
};
#endif

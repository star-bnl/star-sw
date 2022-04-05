/*!
 * \class StTrack 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrack.h,v 2.39 2020/01/27 21:28:31 genevb Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 * Note the following: with the arrival of ITTF it is now possible to
 * store the numberOfPossiblePoints for every detector individually. Before
 * that and because of the way the tables were defined TPC and FTPC were
 * one and the same. This caused confusion. However, since we have to
 * stay backwards compatible the "old way" is still working.
 *    mNumberOfPossiblePoints ==>  seedQuality 
 *    mNumberOfPossiblePointsTpc
 *    mNumberOfPossiblePointsFtpcWest 
 *    mNumberOfPossiblePointsFtpcEast 
 *    mNumberOfPossiblePointsSvt 
 *    mNumberOfPossiblePointsSsd 
 *    are the ones that count
 * --------------------------------------------------------------------------
 *  The track flag (mFlag accessed via flag() method) definitions with ITTF 
 *  (flag definition in EGR era can be found at
 *   http://www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html)
 *
 *  mFlag= zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
 *                      x indicates the detectors included in the fit and 
 *                     yy indicates the status of the fit. 
 *  Positive mFlag values are good fits, negative values are bad fits. 
 *
 *  The first digit indicates which detectors were used in the refit: 
 *
 *      x=1 -> TPC only 
 *      x=3 -> TPC       + primary vertex 
 *      x=5 -> SVT + TPC 
 *      x=6 -> SVT + TPC + primary vertex 
 *      x=7 -> FTPC only 
 *      x=8 -> FTPC      + primary 
 *      x=9 -> TPC beam background tracks            
 *
 *  The last two digits indicate the status of the refit: 
 *       = +x01 -> good track 
 *
 *      = -x01 -> Bad fit, outlier removal eliminated too many points 
 *      = -x02 -> Bad fit, not enough points to fit 
 *      = -x03 -> Bad fit, too many fit iterations 
 *      = -x04 -> Bad Fit, too many outlier removal iterations 
 *      = -x06 -> Bad fit, outlier could not be identified 
 *      = -x10 -> Bad fit, not enough points to start 
 *
 *      = +x11 -> Short track pointing to EEMC
 *      = +x12 -> Short track pointing to ETOF
 *
 ***************************************************************************
 *
 * $Log: StTrack.h,v $
 * Revision 2.39  2020/01/27 21:28:31  genevb
 * Add short tracks toward ETOF
 *
 * Revision 2.38  2017/05/04 00:59:36  perev
 * Add Fts
 *
 * Revision 2.37  2016/11/28 21:00:24  ullrich
 * Added StExtGeometry features.
 *
 * Revision 2.36  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.35  2015/05/13 17:06:14  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.34  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.32  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.30  2013/01/15 23:21:06  fisyak
 * improve printouts
 *
 * Revision 2.29  2012/09/16 21:36:09  fisyak
 * Handlers for Tpc Only West and Only East bits
 *
 * Revision 2.28  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.27  2011/10/13 21:25:27  perev
 * setting IdTruth from the hits is added
 *
 * Revision 2.26  2011/04/26 21:41:29  fisyak
 * Make mKey Int_t instead of UShort_t (no. of tracks might be more that 64k)
 *
 * Revision 2.25  2011/03/31 19:29:01  fisyak
 * Add IdTruth information for tracks and vertices
 *
 * Revision 2.24  2010/08/31 20:00:09  fisyak
 * Clean up, add mSeedQuality
 *
 * Revision 2.23  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.22  2008/08/26 12:47:38  fisyak
 * add track pule up flag description
 *
 * Revision 2.21  2007/10/11 21:51:40  ullrich
 * Added member to handle number of possible points fpr PXL and IST.
 *
 * Revision 2.20  2006/08/28 17:03:54  fisyak
 * Add track flag definitions from EGR (we suppose that this will be primary information from now)
 *
 * Revision 2.19  2004/08/05 22:24:51  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.18  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.17  2003/10/31 16:00:04  ullrich
 * Added setKey() method.
 *
 * Revision 2.16  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.15  2002/03/14 17:42:15  ullrich
 * Added method to set mNumberOfPossiblePoints.
 *
 * Revision 2.14  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.13  2001/09/28 22:20:50  ullrich
 * Added helix geometry at last point.
 *
 * Revision 2.12  2001/05/30 17:45:55  perev
 * StEvent branching
 *
 * Revision 2.11  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.10  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.9  2001/03/16 20:56:46  ullrich
 * Added non-const version of fitTraits().
 *
 * Revision 2.8  1999/12/01 15:58:10  ullrich
 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.7  1999/11/30 23:20:32  didenko
 * temporary solution to get library compiled
 *
 * Revision 2.6  1999/11/29 17:32:45  ullrich
 * Added non-const method pidTraits().
 *
 * Revision 2.5  1999/11/15 18:48:22  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.4  1999/11/05 15:27:07  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.3  1999/11/04 13:32:03  ullrich
 * Added non-const versions of some methods
 *
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.1  1999/10/28 22:27:24  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
 *
 * 
 **************************************************************************/
#ifndef StTrack_hh
#define StTrack_hh
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StTrackTopologyMap.h"
#include "StFunctional.h"
#include "StTrackFitTraits.h"
class StTrack;
ostream&  operator<<(ostream& os,  const StTrack& t);

class StParticleDefinition;
class StVertex;
class StTrackGeometry;
class StExtGeometry;
class StTrackDetectorInfo;
class StTrackPidTraits;
class StTrackNode;

class StTrack : public StObject {
public:
    StTrack();
    StTrack(const StTrack&);
    StTrack & operator=(const StTrack&);
    virtual ~StTrack();

    virtual StTrackType            type() const = 0;
    virtual const StVertex*        vertex() const = 0;
    virtual int                    key() const { return mKey; }
    short                          flag() const;
    unsigned int                   flagExtension() const { return mFlagExtension; }
    unsigned short                 encodedMethod() const;
    bool                           finderMethod(StTrackFinderMethod) const;
    StTrackFittingMethod           fittingMethod() const;
    float                          impactParameter() const;
    float                          length() const;
    unsigned short                 numberOfPossiblePoints() const;
    unsigned short                 numberOfPossiblePoints(StDetectorId) const;
    const StTrackTopologyMap&      topologyMap() const;
    StTrackGeometry*               geometry();
    const StTrackGeometry*         geometry() const;
    StTrackGeometry*               outerGeometry();
    const StTrackGeometry*         outerGeometry() const;
    StExtGeometry*                 extGeometry()	 {return mExtGeometry;}
    const StExtGeometry*           extGeometry()   const {return mExtGeometry;}
    StTrackDetectorInfo*           detectorInfo();
    const StTrackDetectorInfo*     detectorInfo() const;
    StTrackFitTraits&              fitTraits();
    const StTrackFitTraits&        fitTraits() const;
    const StSPtrVecTrackPidTraits& pidTraits() const;
    StSPtrVecTrackPidTraits&       pidTraits();
    StPtrVecTrackPidTraits         pidTraits(StDetectorId) const;
    const StParticleDefinition*    pidTraits(StPidAlgorithm&) const;
    StTrackNode*                   node();
    const StTrackNode*             node() const;
    
    unsigned short                 seedQuality() const {return mSeedQuality;}
    bool       isCtbMatched()      const {return testBit(kCtbMatched);}
    bool       isToFMatched()  	   const {return testBit(kToFMatched);}   
    bool       isBToFMatched()     const {return testBit(kToFMatched);}
    bool       isBemcMatched() 	   const {return testBit(kBemcMatched);}  
    bool       isEemcMatched() 	   const {return testBit(kEemcMatched);}  

    bool       isCtbNotMatched()       const {return testBit(kCtbNotMatched);}
    bool       isToFNotMatched()  	   const {return testBit(kToFNotMatched);}   
    bool       isBToFNotMatched()  	   const {return testBit(kToFNotMatched);}   
    bool       isBemcNotMatched() 	   const {return testBit(kBemcNotMatched);}  
    bool       isEemcNotMatched() 	   const {return testBit(kEemcNotMatched);}  

    bool       isDecayTrack()  	   const {return testBit(kDecayTrack);}   
    bool       isPromptTrack() 	   const {return testBit(kPromptTrack);}       
    bool       isPostXTrack()            const {return testBit(kPostXTrack);} 
    bool       isMembraneCrossingTrack() const {return testBit(kXMembrane);} 
    bool       isShortTrack2EMC()        const {return testBit(kShortTrack2EMC);}
    bool       isRejected()              const {return testBit(kRejectedTrack);}
    bool       isWestTpcOnly()           const {return testBit(kWestTpcOnlyTrack);}
    bool       isEastTpcOnly()           const {return testBit(kEastTpcOnlyTrack);}

    virtual void setCtbMatched()       {setBit(kCtbMatched);}
    virtual void setToFMatched()  	   {setBit(kToFMatched);}   
    virtual void setBToFMatched()  	   {setBit(kToFMatched);}   
    virtual void setBemcMatched() 	   {setBit(kBemcMatched);}  
    virtual void setEemcMatched() 	   {setBit(kEemcMatched);}  

    virtual void setCtbNotMatched()        {setBit(kCtbNotMatched);}   
    virtual void setToFNotMatched()  	   {setBit(kToFNotMatched);}   
    virtual void setBToFNotMatched()  	   {setBit(kToFNotMatched);}   
    virtual void setBemcNotMatched() 	   {setBit(kBemcNotMatched);}  
    virtual void setEemcNotMatched() 	   {setBit(kEemcNotMatched);}  
    virtual void setDecayTrack()  	       {setBit(kDecayTrack);}
    virtual void setPromptTrack() 	       {setBit(kPromptTrack);}       
    virtual void setPostCrossingTrack()    {setBit(kPostXTrack);} 
    virtual void setMembraneCrossingTrack(){setBit(kXMembrane);} 
    virtual void setShortTrack2EMC()       {reSetBit(kRejectedTrack); setBit(kShortTrack2EMC);}
    virtual void setShortTrack2ETOF()      {reSetBit(kRejectedTrack); setBit(kShortTrack2ETOF);}
    virtual void setRejected()             {setBit(kRejectedTrack);}
    virtual void setWestTpcOnly()          {setBit(kWestTpcOnlyTrack);}
    virtual void setEastTpcOnly()          {setBit(kEastTpcOnlyTrack);}
    virtual void setFlagExtension(unsigned int i){mFlagExtension = i;}
    void         setFlag(short);
    void         setKey(int val) { mKey = val; }
    void         setEncodedMethod(UShort_t);
    void         setImpactParameter(float);
    void         setLength(float);
    void         setTopologyMap(const StTrackTopologyMap&);
    void         setGeometry(StTrackGeometry*);
    void         setOuterGeometry(StTrackGeometry*);
    void         addExtGeometry(StExtGeometry* extGeo);
    void         setFitTraits(const StTrackFitTraits&);
    void         addPidTraits(StTrackPidTraits*);
    void         setDetectorInfo(StTrackDetectorInfo*);
    void         setNode(StTrackNode*);
    int          bad() const;
    void         setNumberOfPossiblePoints(unsigned char, StDetectorId);
    void         setSeedQuality(UShort_t qa) 		{mSeedQuality = qa;}
    int          idTruth() const 			{ return mIdTruth;}
    int          qaTruth() const 			{ return mQuality; }
    int          idParentVx() const {return mIdParentVx;}
    void         setIdTruth(int idtru,int qatru=0) 	{mIdTruth = idtru; mQuality = static_cast<unsigned short>(qatru);}
    void         setIdTruth(); 				//setting on hits info
    void         setIdParentVx(int id) {mIdParentVx = id;}
   //----- bit manipulation
    void         setBit(unsigned int f, bool set) {(set) ? setBit(f) : reSetBit(f);}
    void         setBit(unsigned int f) { mFlagExtension |= f; }
    void         reSetBit(unsigned int f) { mFlagExtension &= ~(f); }
    bool         testBit(unsigned int f) const { return (bool) ((mFlagExtension & f) != 0); }
    int          testBits(unsigned int f) const { return (int) (mFlagExtension & f); }
    void         invertBit(unsigned int f) { mFlagExtension ^= f; }
    void Print(Option_t *option="") const {cout << option << *this << endl; }
    
protected:
    void         setNumberOfPossiblePoints(UShort_t); // obsolete
    Char_t                  mBeg[1]; //!
    Int_t                   mKey;
    Short_t                 mFlag;
    UInt_t                  mFlagExtension; // bit wise fast detector matching status
    UShort_t                mEncodedMethod;
    UShort_t                mSeedQuality;   // ITTF: this is seed quality
    UChar_t                 mNumberOfPossiblePointsTpc;
    UChar_t                 mNumberOfPossiblePointsFtpcWest;
    UChar_t                 mNumberOfPossiblePointsFtpcEast;
    UChar_t                 mNumberOfPossiblePointsSvt;
    UChar_t                 mNumberOfPossiblePointsSsd;
    UChar_t                 mNumberOfPossiblePointsSst;
    UChar_t                 mNumberOfPossiblePointsPxl;
    UChar_t                 mNumberOfPossiblePointsIst;
    UChar_t                 mNumberOfPossiblePointsFts;
    Float_t                 mImpactParameter;
    Float_t                 mLength;
    StTrackGeometry*        mGeometry;
    StTrackGeometry*        mOuterGeometry;
    StExtGeometry*          mExtGeometry;
    Int_t                   mIdTruth; // MC track id
    UShort_t                mQuality; // quality of this information (percentage of hits coming from the above MC track)
    Int_t                   mIdParentVx; // MC Parent vertex Id
    Char_t                  mEnd[1]; //!
    StTrackTopologyMap      mTopologyMap;
    StTrackFitTraits        mFitTraits;
    
//  StTrackDetectorInfo         *mDetectorInfo;         //$LINK
//  StTrackNode                 *mNode;                 //$LINK
#ifdef __CINT__
    StObjLink                mDetectorInfo;         
    StObjLink      	         mNode;                 	
#else
    StLink<StTrackDetectorInfo>  mDetectorInfo;         
    StLink<StTrackNode>          mNode;                 	
#endif //__CINT__

    StSPtrVecTrackPidTraits mPidTraitsVec;

    ClassDef(StTrack,12)
};
#endif

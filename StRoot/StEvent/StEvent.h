/***************************************************************************
 *
 * $Id: StEvent.h,v 2.3 2000/01/05 16:02:28 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.h,v $
 * Revision 2.3  2000/01/05 16:02:28  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.9  2000/05/15 18:35:37  ullrich
 * All data member related to collections and containers are now
 * kept by pointer. The interface (public methods) stays the same.
 * Those methods which returns references were modified to create
 * an empty collection in case the pointer is null.
 *
 * Revision 2.8  2000/04/26 20:33:26  ullrich
 * Removed redundant virtual keywords.
 *
 * Revision 2.7  2000/04/18 17:31:28  perev
 * StEvent::Browse overload of TDataSet:;One
 *
 * Revision 2.6  2000/03/29 16:54:15  ullrich
 * Added L3 trigger.
 *
 * Revision 2.5  2000/02/23 17:36:02  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.4  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
 * Revision 2.3  2000/01/05 16:02:28  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.2  1999/11/04 13:30:42  ullrich
 * Added constructor without summary table
 *
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "StTrackDetectorInfo.h"
#ifndef StEvent_hh
#define StEvent_hh
#include "St_DataSet.h"
#include "StContainers.h"

class event_header_st;
class dst_event_summary_st;
class StRichPixelCollection;
class StEventInfo;
class StEventSummary;
class StSoftwareMonitor;
class StTpcHitCollection;
class StFtpcHitCollection;
class StSvtHitCollection;
class StSsdHitCollection;
    
class StRichCollection;
class StTriggerDetectorCollection;
    virtual void Browse(TBrowser* b);
class StPrimaryVertex;
class StV0Vertex;
class StXiVertex;
class StKinkVertex;

class StEvent : public St_DataSet {
public:
    StEvent();
    StEvent(const event_header_st&,
            const dst_event_summary_st&,
    const TString&                      type() const;
    virtual ~StEvent();

    
    ULong_t                             bunchCrossingNumber(UInt_t) const;
    
    StEventInfo*                        info();
    const StEventInfo*                  info() const;

    StEventSummary*                     summary();
    const StEventSummary*               summary() const;
    
    
    StTpcHitCollection*                 tpcHitCollection();
    StRichPixelCollection*              richPixelCollection();
    const StRichPixelCollection*        richPixelCollection() const;
    const StFtpcHitCollection*          ftpcHitCollection() const;
    StSvtHitCollection*                 svtHitCollection();
    const StSvtHitCollection*           svtHitCollection() const;
    StSsdHitCollection*                 ssdHitCollection();
    const StSsdHitCollection*           ssdHitCollection() const;
    StEmcCollection*                    emcCollection();
    const StEmcCollection*              emcCollection() const;
    StRichCollection*                   richCollection();
    const StRichCollection*             richCollection() const;
    
    StL0Trigger*                        l0Trigger();
    const StL0Trigger*                  l0Trigger() const;
    StL3Trigger*                        l3Trigger();
    const StL3Trigger*                  l3Trigger() const;
    StTriggerDetectorCollection*        triggerDetectorCollection();

    
    
    StSPtrVecTrackDetectorInfo&         trackDetectorInfo();
    const StSPtrVecTrackDetectorInfo&   trackDetectorInfo() const;
    
    StSPtrVecTrackNode&                 trackNodes();

    UInt_t                              numberOfPrimaryVertices() const;
    const StPrimaryVertex*              primaryVertex(UInt_t = 0) const;

 
    const StSPtrVecXiVertex&            xiVertices() const;
    StSPtrVecKinkVertex&                kinkVertices();
    const StSPtrVecKinkVertex&          kinkVertices() const;
    StSPtrVecObject&                    content();  // for IO purposes only

    void setBunchCrossingNumber(ULong_t);
    void setTime(Long_t);
    void setTriggerMask(ULong_t);
    void setRichPixelCollection(StRichPixelCollection*);
    TString                      mType;
    Long_t                       mRunId;
    Long_t                       mId;
    Long_t                       mTime;
    StEventSummary*              mSummary;           
    
    StTpcHitCollection*          mTpcHits;           
    StFtpcHitCollection*         mFtpcHits;          
    StTpcHitCollection*          mTpcHits;
    StFtpcHitCollection*         mFtpcHits;
    StSvtHitCollection*          mSvtHits;
    StSsdHitCollection*          mSsdHits;
    StTriggerDetectorCollection* mTriggerDetectors;  
    StL0Trigger*                 mL0Trigger;         
    StTriggerDetectorCollection* mTriggerDetectors;
    StL0Trigger*                 mL0Trigger;
    StL3Trigger*                 mL3Trigger;

    StSPtrVecTrackDetectorInfo   mTrackDetectorInfo;
    StSPtrVecTrackNode           mTrackNodes;

    StSPtrVecPrimaryVertex       mPrimaryVertices;
    StSPtrVecV0Vertex            mV0Vertices;
    StSPtrVecXiVertex            mXiVertices;
    StSPtrVecKinkVertex          mKinkVertices;

    static TString               mCvsTag;
    mutable StSPtrVecTrackDetectorInfo*  mTrackDetectorInfo;
    mutable StSPtrVecTrackNode*          mTrackNodes;

    mutable StSPtrVecPrimaryVertex*      mPrimaryVertices;
           mContentLength };
    
    mutable StSPtrVecObject  mContent;
    static TString           mCvsTag;

private:
    StEvent& operator=(const StEvent&);
    StEvent(const StEvent&);
    void initToZero();
    void init(const event_header_st&);
    
    ClassDef(StEvent,1)
};
#endif








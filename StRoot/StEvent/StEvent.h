/***************************************************************************
 *
 * $Id: StEvent.h,v 2.10 2000/05/22 21:47:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.h,v $
 * Revision 2.10  2000/05/22 21:47:15  ullrich
 * Added RICH collection and related methods.
 *
 * Revision 2.12  2000/06/19 01:32:15  perev
 *  Thomas StEvent branches added
 *
 * Revision 2.11  2000/05/24 15:46:10  ullrich
 * Added setSummary() method.
 *
 * Revision 2.10  2000/05/22 21:47:15  ullrich
 * Added RICH collection and related methods.
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
 * Revision 2.1  1999/10/28 22:25:10  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:41:58  ullrich
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
class dst_summary_param_st;
class StEventInfo;
class StEventSummary;
class StSoftwareMonitor;
class StTpcHitCollection;
class StFtpcHitCollection;
class StSvtHitCollection;
class StSsdHitCollection;
class StEmcCollection;
class StRichCollection;
class StTriggerDetectorCollection;
class StL0Trigger;
class StL3Trigger;
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

    void                                Browse(TBrowser*);
    static const TString&               cvsTag();
    
    TString                             type() const;
    ULong_t                             triggerMask() const;
    ULong_t                             bunchCrossingNumber(UInt_t) const;
    
    StEventInfo*                        info();
    const StEventInfo*                  info() const;

    StEventSummary*                     summary();
    const StEventSummary*               summary() const;
    
    StSoftwareMonitor*                  softwareMonitor();
    const StSoftwareMonitor*            softwareMonitor() const;
    
    StTpcHitCollection*                 tpcHitCollection();
    const StTpcHitCollection*           tpcHitCollection() const;
    StFtpcHitCollection*                ftpcHitCollection();
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
    const StTriggerDetectorCollection*  triggerDetectorCollection() const;
    
    StSPtrVecTrackDetectorInfo&         trackDetectorInfo();
    const StSPtrVecTrackDetectorInfo&   trackDetectorInfo() const;
    
    StSPtrVecTrackNode&                 trackNodes();
    const StSPtrVecTrackNode&           trackNodes() const;

    UInt_t                              numberOfPrimaryVertices() const;
    StPrimaryVertex*                    primaryVertex(UInt_t = 0);
    const StPrimaryVertex*              primaryVertex(UInt_t = 0) const;

 
    const StSPtrVecXiVertex&            xiVertices() const;
    StSPtrVecKinkVertex&                kinkVertices();
    const StSPtrVecKinkVertex&          kinkVertices() const;

    StSPtrVecObject&                    content();  // for IO purposes only

    void setBunchCrossingNumber(ULong_t);
    void setTime(Long_t);
    void setTriggerMask(ULong_t);
    void setBunchCrossingNumber(ULong_t, UInt_t);
    void setInfo(StEventInfo*);
    void setSummary(StEventSummary*);
    void setSoftwareMonitor(StSoftwareMonitor*);
    void setTpcHitCollection(StTpcHitCollection*);
    void setFtpcHitCollection(StFtpcHitCollection*);
    void setSvtHitCollection(StSvtHitCollection*);
    void setSsdHitCollection(StSsdHitCollection*);
    void setEmcCollection(StEmcCollection*);
    void setRichCollection(StRichCollection*);
    TString                              mType;
    Long_t                               mRunId;
    Long_t                               mId;
    Long_t                               mTime;
    ULong_t                              mTriggerMask;
    ULong_t                              mBunchCrossingNumber;
                                             
    StEventSummary*                      mSummary;
    StSoftwareMonitor*                   mSoftwareMonitor;
                                         
    StTpcHitCollection*                  mTpcHits;
    StFtpcHitCollection*                 mFtpcHits;
    StSvtHitCollection*                  mSvtHits;
    StSsdHitCollection*                  mSsdHits;
    StRichCollection*                    mRichCollection;
    StEmcCollection*                     mEmcCollection;
    
    StTriggerDetectorCollection*         mTriggerDetectors;
    StL0Trigger*                         mL0Trigger;
    StL3Trigger*                         mL3Trigger;

    mutable StSPtrVecTrackDetectorInfo*  mTrackDetectorInfo;
    mutable StSPtrVecTrackNode*          mTrackNodes;

    mutable StSPtrVecPrimaryVertex*      mPrimaryVertices;
    mutable StSPtrVecV0Vertex*           mV0Vertices;
    mutable StSPtrVecXiVertex*           mXiVertices;
    mutable StSPtrVecKinkVertex*         mKinkVertices;
	   mTrackNodes,	
    static TString                       mCvsTag;
	   mXiVertices,
	   mKinkVertices,
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








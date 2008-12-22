/*!
 * \class StEvent 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StEvent.h,v 2.33 2008/12/22 20:36:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.h,v $
 * Revision 2.33  2008/12/22 20:36:53  ullrich
 * Added hooks for new ToF (BTof)
 *
 * Revision 2.32  2006/04/25 23:21:25  ullrich
 * Modified addPrimaryVertex(). New 2nd arg: StPrimaryVertexOrder.
 *
 * Revision 2.31  2006/01/19 21:48:21  ullrich
 * Add RnD collection.
 *
 * Revision 2.30  2003/04/16 17:48:32  ullrich
 * Added StTriggerData and inherited classe(s).
 *
 * Revision 2.29  2003/01/30 18:36:31  ullrich
 * Added hooks for StTriggerIdCollection.
 *
 * Revision 2.28  2002/12/20 22:41:30  ullrich
 * Added PMD.
 *
 * Revision 2.27  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.26  2002/01/03 20:59:33  ullrich
 * Added BBC and FPD.
 *
 * Revision 2.25  2001/12/01 15:40:48  ullrich
 * Added StDetectorState access function.
 *
 * Revision 2.24  2001/11/10 23:53:23  ullrich
 * Added calibration vertices.
 *
 * Revision 2.23  2001/11/07 21:19:42  ullrich
 * Added L1 trigger.
 *
 * Revision 2.22  2001/09/18 00:15:25  ullrich
 * Added StRunInfo and access functions.
 *
 * Revision 2.21  2001/05/30 17:45:53  perev
 * StEvent branching
 *
 * Revision 2.20  2001/05/17 22:56:33  ullrich
 * Removed all usage of dst_summary_param.
 *
 * Revision 2.19  2001/04/23 19:28:14  ullrich
 * Added StClusteringHints and methods to access it.
 *
 * Revision 2.18  2001/04/05 04:00:36  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.17  2001/03/14 02:35:43  ullrich
 * Added container and methods to handle PSDs.
 *
 * Revision 2.16  2001/03/09 05:24:01  ullrich
 * Added new method statistics().
 *
 * Revision 2.15  2000/12/08 03:53:41  ullrich
 * Prepared hooks for ToF.
 *
 * Revision 2.14  2000/09/25 14:21:30  ullrich
 * Removed enums for content vector. Replaced by lookup function.
 *
 * Revision 2.13  2000/09/06 22:34:17  ullrich
 * Changed mBunchCrossingNumber from scalar to array to hold all 64 bits.
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
#ifndef StEvent_hh
#define StEvent_hh

#include "St_DataSet.h"
#include "TString.h"
#include "StContainers.h"
#include "StEnumerations.h"

class event_header_st;
class dst_event_summary_st;
class StCalibrationVertex;
class StDetectorState;
class StEventClusteringHints;
class StEventInfo;
class StEventSummary;
class StFpdCollection;
class StPhmdCollection;
class StSoftwareMonitor;
class StTpcHitCollection;
class StFtpcHitCollection;
class StSvtHitCollection;
class StSsdHitCollection;
class StEmcCollection;
class StRichCollection;
class StTofCollection;
class StBTofCollection;
class StTriggerDetectorCollection;
class StTriggerIdCollection;
class StTriggerData;
class StL0Trigger;
class StL1Trigger;
class StL3Trigger;
class StPrimaryVertex;
class StRunInfo;
class StV0Vertex;
class StXiVertex;
class StKinkVertex;
class StPsd;
class StRnDHitCollection;

class StEvent : public StXRefMain {
public:
    StEvent();
    StEvent(const event_header_st&,
            const dst_event_summary_st&);
    StEvent(const event_header_st&);
    virtual ~StEvent();

    void                                Browse(TBrowser*);
    static const TString&               cvsTag();
    
    TString                             type() const;
    int                                 id() const;
    int                                 runId() const;
    int                                 time() const;
    unsigned int                        triggerMask() const;
    unsigned int                        bunchCrossingNumber(unsigned int) const;
    
    StEventInfo*                        info();
    const StEventInfo*                  info() const;

    StRunInfo*                          runInfo();
    const StRunInfo*                    runInfo() const;

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
    StTofCollection*                    tofCollection();
    const StTofCollection*              tofCollection() const;
    StBTofCollection*                   btofCollection();
    const StBTofCollection*             btofCollection() const;
    StFpdCollection*                    fpdCollection();
    const StFpdCollection*              fpdCollection() const;
    StPhmdCollection*                   phmdCollection();
    const StPhmdCollection*             phmdCollection() const;
    StRnDHitCollection*                 rndHitCollection();
    const StRnDHitCollection*           rndHitCollection() const;
    
    StL0Trigger*                        l0Trigger();
    const StL0Trigger*                  l0Trigger() const;
    StL1Trigger*                        l1Trigger();
    const StL1Trigger*                  l1Trigger() const;
    StL3Trigger*                        l3Trigger();
    const StL3Trigger*                  l3Trigger() const;
    StTriggerDetectorCollection*        triggerDetectorCollection();
    const StTriggerDetectorCollection*  triggerDetectorCollection() const;
    StTriggerIdCollection*              triggerIdCollection();
    const StTriggerIdCollection*        triggerIdCollection() const;
    StTriggerData*                      triggerData();
    const StTriggerData*                triggerData() const;
    
    StSPtrVecTrackDetectorInfo&         trackDetectorInfo();
    const StSPtrVecTrackDetectorInfo&   trackDetectorInfo() const;
    
    StSPtrVecTrackNode&                 trackNodes();
    const StSPtrVecTrackNode&           trackNodes() const;

    unsigned int                        numberOfPrimaryVertices() const;
    StPrimaryVertex*                    primaryVertex(unsigned int = 0);
    const StPrimaryVertex*              primaryVertex(unsigned int = 0) const;

    unsigned int                        numberOfCalibrationVertices() const;
    StCalibrationVertex*                calibrationVertex(unsigned int);
    const StCalibrationVertex*          calibrationVertex(unsigned int) const;

    StSPtrVecV0Vertex&                  v0Vertices();
    const StSPtrVecV0Vertex&            v0Vertices() const;
    StSPtrVecXiVertex&                  xiVertices();
    const StSPtrVecXiVertex&            xiVertices() const;
    StSPtrVecKinkVertex&                kinkVertices();
    const StSPtrVecKinkVertex&          kinkVertices() const;

    StDetectorState*                    detectorState(StDetectorId);
    const StDetectorState*              detectorState(StDetectorId) const;
    
    StPsd*                              psd(StPwg, int);
    const StPsd*                        psd(StPwg, int) const;
    unsigned int                        numberOfPsds() const;
    unsigned int                        numberOfPsds(StPwg) const;
    
    StSPtrVecObject&                    content();               // for IO purposes only

    const StEventClusteringHints*       clusteringHints() const; // for IO purposes only
    StEventClusteringHints*             clusteringHints();       // for IO purposes only
    
    void                                statistics();            // *MENU*

    void setType(const char*);
    void setRunId(int);
    void setId(int);
    void setTime(int);
    void setTriggerMask(unsigned int);
    void setBunchCrossingNumber(unsigned int, unsigned int);
    void setInfo(StEventInfo*);
    void setRunInfo(StRunInfo*);
    void setSummary(StEventSummary*);
    void setSoftwareMonitor(StSoftwareMonitor*);
    void setTpcHitCollection(StTpcHitCollection*);
    void setRnDHitCollection(StRnDHitCollection*);
    void setFtpcHitCollection(StFtpcHitCollection*);
    void setSvtHitCollection(StSvtHitCollection*);
    void setSsdHitCollection(StSsdHitCollection*);
    void setEmcCollection(StEmcCollection*);
    void setRichCollection(StRichCollection*);
    void setTofCollection(StTofCollection*);
    void setBTofCollection(StBTofCollection*);
    void setFpdCollection(StFpdCollection*);
    void setPhmdCollection(StPhmdCollection*);
    void setTriggerDetectorCollection(StTriggerDetectorCollection*);
    void setTriggerIdCollection(StTriggerIdCollection*);
    void setTriggerData(StTriggerData*);
    void setL0Trigger(StL0Trigger*);
    void setL1Trigger(StL1Trigger*);
    void setL3Trigger(StL3Trigger*);
    void addPrimaryVertex(StPrimaryVertex*, StPrimaryVertexOrder = orderByNumberOfDaughters);
    void addCalibrationVertex(StCalibrationVertex*);
    void addDetectorState(StDetectorState*);
    void addPsd(StPsd*);
    void removePsd(StPsd*);
    
    virtual Bool_t Notify();
    
protected:
    mutable StSPtrVecObject  mContent;
    static  TString          mCvsTag;
    void    Split();

private:
    StEvent& operator=(const StEvent&);
    StEvent(const StEvent&);
    void initToZero();
    void init(const event_header_st&);
    
    ClassDef(StEvent,4)
};
#endif








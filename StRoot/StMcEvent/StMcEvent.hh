/***************************************************************************
 *
 * $Id: StMcEvent.hh,v 2.6 2000/06/06 02:58:40 calderon Exp $
 * $Log: StMcEvent.hh,v $
 * Revision 2.6  2000/06/06 02:58:40  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.5  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.4  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:15  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:15  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.2  1999/07/28 20:27:32  calderon
 * Version with SL99f libraries
 *
 * 
 **************************************************************************/
#ifndef StMcEvent_hh
#define StMcEvent_hh

#include <iostream.h>

#include "StMcContainers.hh" 
#include "TString.h"

class StMcTpcHitCollection;
class StMcFtpcHitCollection;
class StMcRichHitCollection;
class StMcSvtHitCollection;
class StMcEmcHitCollection;

class StMcVertex;
class g2t_event_st;

class StMcEvent {
    
public:
    StMcEvent();  
    StMcEvent(g2t_event_st*);
    virtual ~StMcEvent();
    
    int operator==(const StMcEvent&) const;
    int operator!=(const StMcEvent&) const;
    
    void initToZero();


    //"Get" Methods
  
    // The following stuff will be read directly from g2t_event table
    static const TString&        cvsTag();   
    
    unsigned long                eventGeneratorEventLabel() const;
    unsigned long                eventNumber() const;
    unsigned long                runNumber() const;              
    unsigned long                type() const;
    unsigned long                zWest() const;
    unsigned long                nWest() const;
    unsigned long                zEast() const;
    unsigned long                nEast() const;
    unsigned long                numberOfPrimaryTracks() const;
    float                        impactParameter() const;
    float                        phiReactionPlane() const;
    float                        triggerTimeOffset() const;
    
    
    
    StMcVertex*                    primaryVertex();
    const StMcVertex*              primaryVertex() const;
    StSPtrVecMcVertex&             vertices();
    const StSPtrVecMcVertex&       vertices() const;
    StSPtrVecMcTrack&              tracks();
    const StSPtrVecMcTrack&        tracks() const;
    StMcTpcHitCollection*          tpcHitCollection();
    const StMcTpcHitCollection*    tpcHitCollection()const;
    StMcSvtHitCollection*          svtHitCollection();
    const StMcSvtHitCollection*    svtHitCollection() const;
    StMcFtpcHitCollection*         ftpcHitCollection();
    const StMcFtpcHitCollection*   ftpcHitCollection() const;
    StMcRichHitCollection*         richHitCollection();
    const StMcRichHitCollection*   richHitCollection() const;
    
    StMcEmcHitCollection*          bemcHitCollection();
    const StMcEmcHitCollection*    bemcHitCollection() const;
    StMcEmcHitCollection*          bprsHitCollection();
    const StMcEmcHitCollection*    bprsHitCollection() const;

    StMcEmcHitCollection*          bsmdeHitCollection();
    const StMcEmcHitCollection*    bsmdeHitCollection() const;
    StMcEmcHitCollection*          bsmdpHitCollection();
    const StMcEmcHitCollection*    bsmdpHitCollection() const;
    
    // "Set" Methods
    
    void setEventGeneratorEventLabel(unsigned long);
    void setEventNumber(unsigned long);
    void setRunNumber(unsigned long);
    void setType(unsigned long);
    void setZWest(unsigned long);
    void setNWest(unsigned long);
    void setZEast(unsigned long);
    void setNEast(unsigned long);
    void setNumberOfPrimaryTracks(unsigned long);
    void setImpactParameter(float);
    void setPhiReactionPlane(float);
    void setTriggerTimeOffset(float);
    void setPrimaryVertex(StMcVertex*);  
    void setTpcHitCollection(StMcTpcHitCollection*);               
    void setSvtHitCollection(StMcSvtHitCollection*);               
    void setFtpcHitCollection(StMcFtpcHitCollection*);              
    void setRichHitCollection(StMcRichHitCollection*);              
    void setBemcHitCollection(StMcEmcHitCollection*);              
    void setBprsHitCollection(StMcEmcHitCollection*);              
    void setBsmdeHitCollection(StMcEmcHitCollection*);              
    void setBsmdpHitCollection(StMcEmcHitCollection*);              
    
    
protected:
    unsigned long                  mEventGeneratorEventLabel;
    unsigned long                  mEventNumber;
    unsigned long                  mRunNumber;
    unsigned long                  mType;  
    unsigned long                  mZWest;
    unsigned long                  mNWest;
    unsigned long                  mZEast;
    unsigned long                  mNEast;
    unsigned long                  mPrimaryTracks;
    float                          mImpactParameter;
    float                          mPhiReactionPlane;
    float                          mTriggerTimeOffset;
    StMcVertex*                    mPrimaryVertex;
    StSPtrVecMcVertex              mVertices;
    StSPtrVecMcTrack               mTracks;
    StMcTpcHitCollection*          mTpcHits;
    StMcSvtHitCollection*          mSvtHits;
    StMcFtpcHitCollection*         mFtpcHits;
    StMcRichHitCollection*         mRichHits;
    StMcEmcHitCollection*          mBemcHits;
    StMcEmcHitCollection*          mBprsHits;
    StMcEmcHitCollection*          mBsmdeHits;
    StMcEmcHitCollection*          mBsmdpHits;
    static TString                 mCvsTag;
private:
    const StMcEvent& operator=(const StMcEvent&);
    StMcEvent(const StMcEvent&);
    
};

ostream&  operator<<(ostream& os, const StMcEvent&);

// Definition of "Get" methods

inline unsigned long StMcEvent::eventGeneratorEventLabel() const { return mEventGeneratorEventLabel; }

inline unsigned long StMcEvent::eventNumber() const { return mEventNumber; }

inline unsigned long StMcEvent::runNumber() const { return mRunNumber;}             

inline unsigned long StMcEvent::type() const { return mType;}             

inline unsigned long StMcEvent::zWest() const { return mZWest;}

inline unsigned long StMcEvent::nWest() const { return mNWest;}

inline unsigned long StMcEvent::zEast() const { return mZEast;}

inline unsigned long StMcEvent::nEast() const { return mNEast;}

inline unsigned long StMcEvent::numberOfPrimaryTracks() const { return mPrimaryTracks;}

inline float StMcEvent::impactParameter() const { return mImpactParameter; }

inline float StMcEvent::phiReactionPlane() const { return mPhiReactionPlane; }

inline float StMcEvent::triggerTimeOffset() const { return mTriggerTimeOffset;}

inline StMcVertex* StMcEvent::primaryVertex() { return mPrimaryVertex;}

inline const StMcVertex* StMcEvent::primaryVertex() const { return mPrimaryVertex;}

inline StSPtrVecMcVertex& StMcEvent::vertices() { return mVertices;}

inline const StSPtrVecMcVertex& StMcEvent::vertices() const { return mVertices;}

inline StSPtrVecMcTrack& StMcEvent::tracks() { return mTracks;}

inline const StSPtrVecMcTrack& StMcEvent::tracks() const { return mTracks;}

inline StMcTpcHitCollection* StMcEvent::tpcHitCollection() { return mTpcHits;}

inline const StMcTpcHitCollection* StMcEvent::tpcHitCollection() const { return mTpcHits;}

inline StMcSvtHitCollection* StMcEvent::svtHitCollection() { return mSvtHits;}

inline const StMcSvtHitCollection* StMcEvent::svtHitCollection() const { return mSvtHits;}

inline StMcFtpcHitCollection* StMcEvent::ftpcHitCollection() { return mFtpcHits;}

inline const StMcFtpcHitCollection* StMcEvent::ftpcHitCollection() const { return mFtpcHits;}

inline StMcRichHitCollection* StMcEvent::richHitCollection() { return mRichHits;}

inline const StMcRichHitCollection* StMcEvent::richHitCollection() const { return mRichHits;}

inline StMcEmcHitCollection* StMcEvent::bemcHitCollection() { return mBemcHits;}
inline const StMcEmcHitCollection* StMcEvent::bemcHitCollection() const {return mBemcHits;}
inline StMcEmcHitCollection* StMcEvent::bprsHitCollection() { return mBprsHits;}
inline const StMcEmcHitCollection* StMcEvent::bprsHitCollection() const {return mBprsHits;}
inline StMcEmcHitCollection* StMcEvent::bsmdeHitCollection() { return mBsmdeHits;}
inline const StMcEmcHitCollection* StMcEvent::bsmdeHitCollection() const {return mBsmdeHits;}
inline StMcEmcHitCollection* StMcEvent::bsmdpHitCollection() { return mBsmdpHits;}
inline const StMcEmcHitCollection* StMcEvent::bsmdpHitCollection() const {return mBsmdpHits;}

#endif


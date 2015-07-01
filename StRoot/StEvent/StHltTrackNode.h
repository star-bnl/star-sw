/***************************************************************************
 *
 * $Id: StHltTrackNode.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltTrackNode.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltTrackNode_hh
#define StHltTrackNode_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

#include "StHltTriggerReasonCapable.h"

class StHltTrack;
class StHltBTofHit;
class StHltBEmcTowerHit;


class StHltTrackNode : public StHltTriggerReasonCapable {
public:
    StHltTrackNode();
    ~StHltTrackNode();
    
    StHltTrack* globalTrack();
    const StHltTrack* globalTrack() const;
    
    StHltTrack* primaryTrack();
    const StHltTrack* primaryTrack() const;
    
    StHltBTofHit* bTofHit();
    const StHltBTofHit* bTofHit() const;
    
    StHltBEmcTowerHit* bEmcTowerHit();
    const StHltBEmcTowerHit* bEmcTowerHit() const;
    
    int globalTrackSN() const;
    int primaryTrackSN() const;
    int tofHitSN() const;
    int emcTowerSN() const;
    
    double bEmcMatchPhiDiff() const;
    double bEmcMatchZEdge() const;
    
    float bTofProjChannel() const;
    float bTofCellLocalY() const;
    float bTofCellLocalZ() const;
    float bTofPathLength() const;
    float beta() const;
    float tof() const;
    
    void setGlobalTrack(StHltTrack*);
    void setPrimaryTrack(StHltTrack*);
    void setBTofHit(StHltBTofHit*);
    void setBEmcTowerHit(StHltBEmcTowerHit*);
    void setGlobalTrackSN(int);
    void setPrimaryTrackSN(int);
    void setTofHitSN(int);
    void setEmcTowerSN(int);
    void setBEmcMatchPhiDiff(double);
    void setBEmcMatchZEdge(double);
    void setBTofProjChannel(float);
    void setBTofCellLocalY(float);
    void setBTofCellLocalZ(float);
    void setBTofPathLength(float);	
    void setBeta(float);
    void setTof(float);
    
private:
    
#ifdef __CINT__
    StObjLink mGlobalTrack;
    StObjLink mPrimaryTrack;
    StObjLink mBTofHit;
    StObjLink mBEmcTowerHit;
#else
    StLink<StHltTrack> mGlobalTrack;
    StLink<StHltTrack> mPrimaryTrack;
    StLink<StHltBTofHit> mBTofHit;
    StLink<StHltBEmcTowerHit> mBEmcTowerHit;
#endif //__CINT__
    
    double mBEmcMatchPhiDiff;
    double mBEmcMatchZEdge;
    
    int mGlobalTrackSN;
    int mPrimaryTrackSN;
    int mTofHitSN;
    int mEmcTowerSN;
    
    float mBTofProjChannel;
    float mBTofCellLocalY;
    float mBTofCellLocalZ;
    float mBTofPathLength;
    float mBeta;
    float mTof;
    
    ClassDef(StHltTrackNode,1)
};

ostream& operator<<(ostream&, const StHltTrackNode&);


#endif







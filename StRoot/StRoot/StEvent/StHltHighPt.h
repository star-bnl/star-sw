/***************************************************************************
 *
 * $Id: StHltHighPt.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltHighPt.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltHighPt_hh
#define StHltHighPt_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

#include "StHltTrack.h"
#include "StHltBTofHit.h"
#include "StHltBEmcTowerHit.h"
#include "StHltTriggerReasonCapable.h"

class StHltHighPt : public StHltTriggerReasonCapable {

public:    
    StHltHighPt();
    ~StHltHighPt();
    
    StHltTrack& primaryTrack();
    const StHltTrack& primaryTrack() const;
    StHltTrack& globalTrack();
    const StHltTrack& globalTrack() const;
    StHltBTofHit& bTofHit();
    const StHltBTofHit& bTofHit() const;
    StHltBEmcTowerHit& bEmcTowerHit();
    const StHltBEmcTowerHit& bEmcTowerHit() const;
    
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
    
    void setGlobalTrack(const StHltTrack &);
    void setPrimaryTrack(const StHltTrack &);
    void setBTofHit(const StHltBTofHit &);
    void setBEmcTowerHit(const StHltBEmcTowerHit &);
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
    
    StHltTrack mPrimaryTrack;         ///< primary track object of the highpt track
    StHltTrack mGlobalTrack;          ///< global track object of the highpt track
    StHltBTofHit mBTofHit;            ///< btof object of the highpt track
    StHltBEmcTowerHit mBEmcTowerHit;  ///< bemc object of the highpt track
    
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
    
    ClassDef(StHltHighPt,1)
};

ostream& operator<<(ostream&, const StHltHighPt&);


#endif



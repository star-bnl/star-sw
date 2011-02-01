/***************************************************************************
 *
 * $Id: StHltDiElectron.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltDiElectron.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltDiElectron_hh
#define StHltDiElectron_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

#include "StHltTrack.h"
#include "StHltBTofHit.h"
#include "StHltBEmcTowerHit.h"
#include "StHltTriggerReasonCapable.h"

class StHltDiElectron : public StHltTriggerReasonCapable {
public:
    StHltDiElectron();               ///< default constructor
    ~StHltDiElectron();
    
    float invariantMass() const;  ///< return invariant mass of di-electron pairs
    float pt() const;
    float psi() const;
    float tanl() const;
    
    StHltTrack& daughter1primaryTrack();                     ///< return object of StHltTrack
    const StHltTrack& daughter1primaryTrack() const;         
    StHltTrack& daughter1globalTrack();                      ///< return object of StHltTrack
    const StHltTrack& daughter1globalTrack() const;
    StHltBTofHit& daughter1bTofHit();                        ///< return object of StHltBTofHit
    const StHltBTofHit& daughter1bTofHit() const;
    StHltBEmcTowerHit& daughter1bEmcTowerHit();              ///< return object of StHltBEmcTowerHit
    const StHltBEmcTowerHit& daughter1bEmcTowerHit() const;
    int daughter1SelectionBit() const;
    int daughter1globalTrackSN() const;
    int daughter1primaryTrackSN() const;
    int daughter1tofHitSN() const;
    int daughter1emcTowerSN() const;
    double daughter1bEmcMatchPhiDiff() const;
    double daughter1bEmcMatchZEdge() const;
    float daughter1bTofProjChannel() const;
    float daughter1bTofCellLocalY() const;
    float daughter1bTofCellLocalZ() const;
    float daughter1bTofPathLength() const;
    float daughter1beta() const;
    float daughter1tof() const;
    
    StHltTrack& daughter2primaryTrack();                      ///< return object of StHltTrack
    const StHltTrack& daughter2primaryTrack() const;
    StHltTrack& daughter2globalTrack();                       ///< return object of StHltTrack
    const StHltTrack& daughter2globalTrack() const;
    StHltBTofHit& daughter2bTofHit();                         ///< return object of StHltBTofHit
    const StHltBTofHit& daughter2bTofHit() const;
    StHltBEmcTowerHit& daughter2bEmcTowerHit();               ///< return object of StHltBEmcTowerHit
    const StHltBEmcTowerHit& daughter2bEmcTowerHit() const;
    int daughter2SelectionBit() const;
    int daughter2globalTrackSN() const;
    int daughter2primaryTrackSN() const;
    int daughter2tofHitSN() const;
    int daughter2emcTowerSN() const;
    double daughter2bEmcMatchPhiDiff() const;
    double daughter2bEmcMatchZEdge() const;
    float daughter2bTofProjChannel() const;
    float daughter2bTofCellLocalY() const;
    float daughter2bTofCellLocalZ() const;
    float daughter2bTofPathLength() const;
    float daughter2beta() const;
    float daughter2tof() const;
    
    void setInvariantMass(float);	
    void setPt(float);
    void setPsi(float);
    void setTanl(float);
    
    void setDaughter1GlobalTrack(const StHltTrack &);
    void setDaughter1PrimaryTrack(const StHltTrack &);
    void setDaughter1BTofHit(const StHltBTofHit &);
    void setDaughter1BEmcTowerHit(const StHltBEmcTowerHit &);
    void setDaughter1SelectionBit(int);
    void setDaughter1GlobalTrackSN(int);
    void setDaughter1PrimaryTrackSN(int);
    void setDaughter1TofHitSN(int);
    void setDaughter1EmcTowerSN(int);
    void setDaughter1BEmcMatchPhiDiff(double);
    void setDaughter1BEmcMatchZEdge(double);
    void setDaughter1BTofProjChannel(float);
    void setDaughter1BTofCellLocalY(float);
    void setDaughter1BTofCellLocalZ(float);
    void setDaughter1BTofPathLength(float);	
    void setDaughter1Beta(float);
    void setDaughter1Tof(float);
    
    void setDaughter2GlobalTrack(const StHltTrack &);
    void setDaughter2PrimaryTrack(const StHltTrack &);
    void setDaughter2BTofHit(const StHltBTofHit &);
    void setDaughter2BEmcTowerHit(const StHltBEmcTowerHit &);
    void setDaughter2SelectionBit(int);
    void setDaughter2GlobalTrackSN(int);
    void setDaughter2PrimaryTrackSN(int);
    void setDaughter2TofHitSN(int);
    void setDaughter2EmcTowerSN(int);
    void setDaughter2BEmcMatchPhiDiff(double);
    void setDaughter2BEmcMatchZEdge(double);
    void setDaughter2BTofProjChannel(float);
    void setDaughter2BTofCellLocalY(float);
    void setDaughter2BTofCellLocalZ(float);
    void setDaughter2BTofPathLength(float);	
    void setDaughter2Beta(float);
    void setDaughter2Tof(float);
    
private:
    
    float mInvariantMass;
    float mPt;
    float mPsi;
    float mTanl;
    
    StHltTrack mDaughter1PrimaryTrack;         ///< primary object of daughter 1 track 
    StHltTrack mDaughter1GlobalTrack;          ///< global object of daughter 1 track
    StHltBTofHit mDaughter1BTofHit;            ///< btof object of daughter 1 track
    StHltBEmcTowerHit mDaughter1BEmcTowerHit;  ///< bemc object of daughter 1 track
    double mDaughter1BEmcMatchPhiDiff;
    double mDaughter1BEmcMatchZEdge;
    int mDaughter1SelectionBit;
    int mDaughter1GlobalTrackSN;
    int mDaughter1PrimaryTrackSN;
    int mDaughter1TofHitSN;
    int mDaughter1EmcTowerSN;
    float mDaughter1BTofProjChannel;
    float mDaughter1BTofCellLocalY;
    float mDaughter1BTofCellLocalZ;
    float mDaughter1BTofPathLength;
    float mDaughter1Beta;
    float mDaughter1Tof;
    
    StHltTrack mDaughter2PrimaryTrack;         ///< primary object of daughter 2 track
    StHltTrack mDaughter2GlobalTrack;          ///< global object of daughter 2 track
    StHltBTofHit mDaughter2BTofHit;            ///< btof object of daughter 2 track
    StHltBEmcTowerHit mDaughter2BEmcTowerHit;  ///< bemc object of daughter 2 track
    double mDaughter2BEmcMatchPhiDiff;
    double mDaughter2BEmcMatchZEdge;
    int mDaughter2SelectionBit;
    int mDaughter2GlobalTrackSN;
    int mDaughter2PrimaryTrackSN;
    int mDaughter2TofHitSN;
    int mDaughter2EmcTowerSN;
    float mDaughter2BTofProjChannel;
    float mDaughter2BTofCellLocalY;
    float mDaughter2BTofCellLocalZ;
    float mDaughter2BTofPathLength;
    float mDaughter2Beta;
    float mDaughter2Tof;
    
    ClassDef(StHltDiElectron,1);
    
};

inline float StHltDiElectron::invariantMass() const {return mInvariantMass;}
inline float StHltDiElectron::pt() const {return mPt; }
inline float StHltDiElectron::psi() const {return mPsi;}
inline float StHltDiElectron::tanl() const {return mTanl;}


ostream& operator<<(ostream&, const StHltDiElectron&);///< print operator

#endif





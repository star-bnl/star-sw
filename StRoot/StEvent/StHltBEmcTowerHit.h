/***************************************************************************
 *
 * $Id: StHltBEmcTowerHit.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltBEmcTowerHit.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltBEmcTowerHit_hh
#define StHltBEmcTowerHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

class StHltTrackNode;

class StHltBEmcTowerHit : public StObject {
public:
    StHltBEmcTowerHit();
    ~StHltBEmcTowerHit();
    
    int adc() const;
    float energy() const;
    float phi() const;
    float eta() const;
    float z() const;
    float softId() const;
    float daqId() const;
    
    StHltTrackNode* trackNode();
    const StHltTrackNode* trackNode() const;
    
    void setAdc(int);
    void setEnergy(float);
    void setPhi(float);
    void setEta(float);
    void setZ(float);
    void setSoftId(int);
    void setDaqId(int);
    
    void setTrackNode(StHltTrackNode*);
    
private:
    int mAdc;
    float mEnergy; ///< with online calibration	
    float mPhi;
    float mEta;
    float mZ;
    int mSoftId;
    int mDaqId;
    
#ifdef __CINT__
    StObjLink mTrackNode;
#else
    StLink<StHltTrackNode> mTrackNode;
#endif //__CINT__
    
    ClassDef(StHltBEmcTowerHit,1)
};

inline int StHltBEmcTowerHit::adc() const {return mAdc;}
inline float StHltBEmcTowerHit::energy() const {return mEnergy;}
inline float StHltBEmcTowerHit::phi() const {return mPhi;}
inline float StHltBEmcTowerHit::eta() const {return mEta;}
inline float StHltBEmcTowerHit::z() const {return mZ;}
inline float StHltBEmcTowerHit::softId() const {return mSoftId;}
inline float StHltBEmcTowerHit::daqId() const {return mDaqId;}

inline StHltTrackNode* StHltBEmcTowerHit::trackNode() {return mTrackNode;}
inline const StHltTrackNode* StHltBEmcTowerHit::trackNode() const {return mTrackNode;}

ostream& operator<<(ostream&, const StHltBEmcTowerHit&); ///< Printting operator

#endif



/***************************************************************************
 *
 * $Id: StHltBTofHit.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltBTofHit.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltBTofHit_hh
#define StHltBTofHit_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"

class StHltTrackNode;

class StHltBTofHit : public StObject {
public:
    StHltBTofHit();
    ~StHltBTofHit();
    
    short trayId() const;
    short channel() const; ///< module*6+cell
    float tdc() const;
    float tot() const;
    float tof() const;
    float triggerTime() const;
    short module() const;
    short cell() const;
    
    StHltTrackNode* trackNode();
    const StHltTrackNode* trackNode() const;
    
    void setTrayId(short);
    void setChannel(short);
    void setTdc(float);
    void setTot(float);
    void setTof(float);
    void setTriggerTime(float);
    
    void setTrackNode(StHltTrackNode*);
    
private:
    short mTrayId;
    short mChannel; ///< = nModule*6 + nCell
    float mTdc;
    float mTot;
    float mTof;
    float mTriggerTime;
    
#ifdef __CINT__
    StObjLink mTrackNode;
#else
    StLink<StHltTrackNode> mTrackNode;
#endif //__CINT__
    
    ClassDef(StHltBTofHit,1)
};

inline short StHltBTofHit::trayId() const {return mTrayId;}
inline short StHltBTofHit::channel() const {return mChannel;}
inline float StHltBTofHit::tdc() const {return mTdc;}
inline float StHltBTofHit::tot() const {return mTot;}
inline float StHltBTofHit::tof() const {return mTof;}
inline float StHltBTofHit::triggerTime() const {return mTriggerTime;}
inline short StHltBTofHit::module() const {return mChannel/6;}
inline short StHltBTofHit::cell() const {return mChannel%6;}

inline StHltTrackNode* StHltBTofHit::trackNode() {return mTrackNode;}
inline const StHltTrackNode* StHltBTofHit::trackNode() const {return mTrackNode;}

ostream& operator<<(ostream&, const StHltBTofHit&); ///< Printting operator

#endif


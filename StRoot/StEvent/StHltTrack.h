/***************************************************************************
 *
 * $Id: StHltTrack.h,v 2.1 2011/02/01 19:45:47 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltTrack.h,v $
 * Revision 2.1  2011/02/01 19:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StHltTrack_hh
#define StHltTrack_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StArray.h"
#include "StEnumerations.h"

class StHltTrackNode;

class StHltTrack : public StObject {
public:
    StHltTrack();
    virtual ~StHltTrack();
    
    StTrackType type() const;
    int id() const;
    unsigned int flag() const;
    int innerMostRow() const;
    int outerMostRow() const;
    unsigned int nHits() const;
    unsigned int ndedx() const;
    int q() const;
    float chi2(int) const;
    float dedx() const;
    float pt() const;
    float phi0() const;
    float psi() const;
    float r0() const;
    float tanl() const;
    float z0() const;
    float length() const;
    float dpt() const;
    float dpsi() const;
    float dz0() const;
    float dtanl() const;
    
    StHltTrackNode* trackNode();
    const StHltTrackNode* trackNode() const;
    
    void setType(StTrackType);
    void setId(int);
    void setFlag(unsigned short);
    void setInnerMostRow(char);
    void setOuterMostRow(char);
    void setNHits(unsigned char);
    void setNDedx(unsigned char);
    void setQ(char);
    void setChi2(int,float);
    void setDedx(float);
    void setPt(float);
    void setPhi0(float);
    void setPsi(float);
    void setR0(float);
    void setTanl(float);
    void setZ0(float);
    void setLength(float);
    void setDpt(float);
    void setDpsi(float);
    void setDz0(float);
    void setDtanl(float);
    
    void setTrackNode(StHltTrackNode*);
    
protected:
    StTrackType mType;  ///< track Type
    int mId;            ///< primary key
    unsigned short mFlag;
    char mInnerMostRow;
    char mOuterMostRow;
    unsigned char mNHits; 
    unsigned char mNDedx; 
    char mQ;           ///< charge
    float mChi2[2];    ///< chi squared of the momentum fit
    float mDedx;
    float mPt;         ///< pt at (r, phi,z)
    float mPhi0;       ///< azimuthal angle of point where parameters are given
    float mPsi;        ///< azimuthal angle of the momentum at (r,...
    float mR0;         ///< r (in cyl. coord.) for point where parameters are given
    float mTanl;       ///< tg of the dip angle at (r,phi,z)
    float mZ0;         ///< z coordinate of point where parameters are given
    float mLength;
    float mDpt;
    float mDpsi;
    float mDz0;
    float mDtanl;
    
#ifdef __CINT__
    StObjLink mTrackNode;
#else
    StLink<StHltTrackNode> mTrackNode;
#endif //__CINT__
    
    ClassDef(StHltTrack,1)
};

inline StTrackType StHltTrack::type() const {return mType;}
inline int StHltTrack::id() const {return mId;}
inline unsigned int StHltTrack::flag() const {return mFlag;}
inline int StHltTrack::innerMostRow() const {return mInnerMostRow;}
inline int StHltTrack::outerMostRow() const {return mOuterMostRow;}
inline unsigned int StHltTrack::nHits() const {return mNHits;}
inline unsigned int StHltTrack::ndedx() const {return mNDedx;}
inline int StHltTrack::q() const {return mQ;}
inline float StHltTrack::chi2(int i) const { if (i==0 || i==1) return mChi2[i]; else return 0;}
inline float StHltTrack::dedx() const {return mDedx;}
inline float StHltTrack::pt() const {return mPt;}
inline float StHltTrack::phi0() const {return mPhi0;}
inline float StHltTrack::psi() const {return mPsi;}
inline float StHltTrack::r0() const {return mR0;}
inline float StHltTrack::tanl() const {return mTanl;}
inline float StHltTrack::z0() const {return mZ0;}
inline float StHltTrack::length() const {return mLength;}
inline float StHltTrack::dpt() const {return mDpt;}
inline float StHltTrack::dpsi() const {return mDpsi;}
inline float StHltTrack::dz0() const {return mDz0;}
inline float StHltTrack::dtanl() const {return mDtanl;}

inline StHltTrackNode* StHltTrack::trackNode() {return mTrackNode;}
inline const StHltTrackNode* StHltTrack::trackNode() const {return mTrackNode;}


ostream& operator<<(ostream&, const StHltTrack&); ///< Printting operator

#endif








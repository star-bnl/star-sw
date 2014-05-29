/*!
 * \class StMtdPidTraits 
 * \author Frank Geurts, Feb 2013
 */
/***************************************************************************
 *
 * $Id: StMtdPidTraits.h,v 2.2 2014/05/29 16:58:06 ullrich Exp $
 *
 * Author: Frank Geurts (Rice)
 ***************************************************************************
 *
 * $Log: StMtdPidTraits.h,v $
 * Revision 2.2  2014/05/29 16:58:06  ullrich
 * Added new member mExpTimeOfFlight and referring access methods.
 *
 * Revision 1.2  2014/05/22 19:04:22  marr
 * locally backup /star/u/marr/data02/mtd/dev/StRoot/StEvent/StMtdPidTraits.h
 *
 * Revision 2.1  2013/02/21 00:23:09  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#ifndef StMtdPidTraits_hh
#define StMtdPidTraits_hh

#include "StTrackPidTraits.h"
#include "StMtdHit.h"
#include "StThreeVectorF.hh"

class StMtdPidTraits : public StTrackPidTraits {
public:
    StMtdPidTraits();
    ~StMtdPidTraits();
    
    StMtdHit*       mtdHit();
    const StMtdHit* mtdHit() const;
    
    /// Matching information
    unsigned char    matchFlag() const;
    float            yLocal() const;
    float            zLocal() const;
    float            thetaLocal() const;
    
    StThreeVectorF&         position();
    const StThreeVectorF&   position() const;
    
    /// timing for PID
    float   timeOfFlight() const;
    float   expTimeOfFlight() const;
    float   pathLength() const;
    float   beta() const;
    
    /// PID functions
    float   sigmaMuon() const;
    
    float   probMuon() const;
    
    ///
    void    setMtdHit(StMtdHit*);
    
    void    setMatchFlag(unsigned char);
    void    setYLocal(float);
    void    setZLocal(float);
    void    setThetaLocal(float);
    void    setPosition( const StThreeVectorF&);                            
    
    void    setTimeOfFlight(float);
    void    setExpTimeOfFlight(float);
    void    setPathLength(float);
    void    setBeta(float);
    
    void    setSigmaMuon(float);
    
    void    setProbMuon(float);
    
private:
#ifdef __CINT__
    StObjLink        mMtdHit;
#else
    StLink<StMtdHit>  mMtdHit;
#endif //__CINT__
    
    UChar_t   mMatchFlag;
    Float_t   mYLocal;
    Float_t   mZLocal;
    Float_t   mThetaLocal;
    StThreeVectorF  mPosition;
    
    Float_t   mTimeOfFlight;          // Measured time-of-flight
    Float_t   mPathLength;            // Path length obtained from track extrapolation
    Float_t   mBeta;                 
    
    Float_t   mSigmaMuon;
    
    Float_t   mProbMuon;
    Float_t   mExpTimeOfFlight;       // Expected time-of-flight obtained from track extrapolation
    
    ClassDef(StMtdPidTraits,2)
};

inline unsigned char StMtdPidTraits::matchFlag() const { return mMatchFlag; }
inline float StMtdPidTraits::yLocal() const { return mYLocal; }
inline float StMtdPidTraits::zLocal() const { return mZLocal; }
inline float StMtdPidTraits::thetaLocal() const { return mThetaLocal; }
inline float StMtdPidTraits::timeOfFlight() const { return mTimeOfFlight; }
inline float StMtdPidTraits::expTimeOfFlight() const { return mExpTimeOfFlight; }
inline float StMtdPidTraits::pathLength() const { return mPathLength; }
inline float StMtdPidTraits::beta() const { return mBeta; }
inline float StMtdPidTraits::sigmaMuon() const { return mSigmaMuon; }

inline void StMtdPidTraits::setMatchFlag(unsigned char flag) { mMatchFlag=flag; }
inline void StMtdPidTraits::setYLocal(float y) { mYLocal=y; }
inline void StMtdPidTraits::setZLocal(float z) { mZLocal=z; }
inline void StMtdPidTraits::setThetaLocal(float theta) { mThetaLocal=theta; }
inline void StMtdPidTraits::setTimeOfFlight(float t) { mTimeOfFlight=t; }
inline void StMtdPidTraits::setExpTimeOfFlight(float t) { mExpTimeOfFlight=t; }
inline void StMtdPidTraits::setPathLength(float s) { mPathLength=s; }
inline void StMtdPidTraits::setBeta(float beta) { mBeta=beta; }
inline void StMtdPidTraits::setSigmaMuon(float sigma) { mSigmaMuon=sigma; }
inline void StMtdPidTraits::setProbMuon(float prob) { mProbMuon=prob; }

#endif

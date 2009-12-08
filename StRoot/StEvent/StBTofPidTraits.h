/*!
 * \class StBTofPidTraits 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofPidTraits.h,v 2.2 2009/12/08 23:24:46 fine Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBTofPidTraits.h,v $
 * Revision 2.2  2009/12/08 23:24:46  fine
 * Fix issue  #1748 http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1748
 *
 * Revision 2.1  2008/12/22 20:31:00  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#ifndef StBTofPidTraits_hh
#define StBTofPidTraits_hh

#include "StTrackPidTraits.h"
#include "StBTofHit.h"
#include "StThreeVectorF.hh"

class StBTofPidTraits : public StTrackPidTraits {
public:
    StBTofPidTraits();
    ~StBTofPidTraits();
    
    StBTofHit*       tofHit();
    const StBTofHit* tofHit() const;

    /// Matching information
    unsigned char    matchFlag() const;
    float            yLocal() const;
    float            zLocal() const;
    float            thetaLocal() const;
    
    StThreeVectorF&         position();
    const StThreeVectorF&   position() const;

    /// timing for PID
    float   tot() const;
    float   timeOfFlight() const;
    float   pathLength() const;
    float   beta() const;
    
    /// PID functions
    float   sigmaElectron() const;
    float   sigmaPion() const;
    float   sigmaKaon() const;
    float   sigmaProton() const;
    
    float   probElectron() const;
    float   probPion() const;
    float   probKaon() const;
    float   probProton() const;

    ///
    void    setTofHit(StBTofHit*);
            
    void    setMatchFlag(unsigned char);
    void    setYLocal(float);
    void    setZLocal(float);
    void    setThetaLocal(float);
    void    setPosition( const StThreeVectorF&);                            

    void    setTimeOfFlight(float);
    void    setPathLength(float);
    void    setBeta(float);

    void    setSigmaElectron(float);
    void    setSigmaPion(float);
    void    setSigmaKaon(float);
    void    setSigmaProton(float);

    void    setProbElectron(float);
    void    setProbPion(float);
    void    setProbKaon(float);
    void    setProbProton(float);

private:
    //    StBTofHit *mBTofHit;   //$LINK
#ifdef __CINT__
    StObjLink        mBTofHit;
#else
    StLink<StBTofHit>  mBTofHit;
#endif //__CINT__

    UChar_t   mMatchFlag;
    Float_t   mYLocal;
    Float_t   mZLocal;
    Float_t   mThetaLocal;
    StThreeVectorF  mPosition;

    Float_t   mTimeOfFlight;
    Float_t   mPathLength;
    Float_t   mBeta;

    Float_t   mSigmaElectron;
    Float_t   mSigmaPion;
    Float_t   mSigmaKaon;
    Float_t   mSigmaProton;
    
    Float_t   mProbElectron;
    Float_t   mProbPion;
    Float_t   mProbKaon;
    Float_t   mProbProton;

    ClassDef(StBTofPidTraits,1)
};

inline unsigned char StBTofPidTraits::matchFlag() const { return mMatchFlag; }
inline float StBTofPidTraits::yLocal() const { return mYLocal; }
inline float StBTofPidTraits::zLocal() const { return mZLocal; }
inline float StBTofPidTraits::thetaLocal() const { return mThetaLocal; }
inline float StBTofPidTraits::timeOfFlight() const { return mTimeOfFlight; }
inline float StBTofPidTraits::pathLength() const { return mPathLength; }
inline float StBTofPidTraits::beta() const { return mBeta; }
inline float StBTofPidTraits::sigmaElectron() const { return mSigmaElectron; }
inline float StBTofPidTraits::sigmaPion() const { return mSigmaPion; }
inline float StBTofPidTraits::sigmaKaon() const { return mSigmaKaon; }
inline float StBTofPidTraits::sigmaProton() const { return mSigmaProton; }
inline float StBTofPidTraits::probElectron() const { return mProbElectron; }
inline float StBTofPidTraits::probPion() const { return mProbPion; }
inline float StBTofPidTraits::probKaon() const { return mProbKaon; }
inline float StBTofPidTraits::probProton() const { return mProbProton; }

inline void StBTofPidTraits::setMatchFlag(unsigned char flag) { mMatchFlag=flag; }
inline void StBTofPidTraits::setYLocal(float y) { mYLocal=y; }
inline void StBTofPidTraits::setZLocal(float z) { mZLocal=z; }
inline void StBTofPidTraits::setThetaLocal(float theta) { mThetaLocal=theta; }
inline void StBTofPidTraits::setTimeOfFlight(float t) { mTimeOfFlight=t; }
inline void StBTofPidTraits::setPathLength(float s) { mPathLength=s; }
inline void StBTofPidTraits::setBeta(float beta) { mBeta=beta; }
inline void StBTofPidTraits::setSigmaElectron(float sigma) { mSigmaElectron=sigma; }
inline void StBTofPidTraits::setSigmaPion(float sigma) { mSigmaPion=sigma; }
inline void StBTofPidTraits::setSigmaKaon(float sigma) { mSigmaKaon=sigma; }
inline void StBTofPidTraits::setSigmaProton(float sigma) { mSigmaProton=sigma; }
inline void StBTofPidTraits::setProbElectron(float prob) { mProbElectron=prob; }
inline void StBTofPidTraits::setProbPion(float prob) { mProbPion=prob; }
inline void StBTofPidTraits::setProbKaon(float prob) { mProbKaon=prob; }
inline void StBTofPidTraits::setProbProton(float prob) { mProbProton=prob; }

#endif

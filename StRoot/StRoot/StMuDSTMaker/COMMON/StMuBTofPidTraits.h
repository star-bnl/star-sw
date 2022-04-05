/*!
 * \class StMuBTofPidTraits 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StMuBTofPidTraits.h,v 1.6 2013/07/23 11:02:59 jeromel Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMuBTofPidTraits.h,v $
 * Revision 1.6  2013/07/23 11:02:59  jeromel
 * Undo changes (KF and other)
 *
 * Revision 1.4  2013/04/10 19:28:35  jeromel
 * Step back to 04/04 version (van aware) - previous changes may be recoverred
 *
 * Revision 1.2  2009/12/08 23:24:46  fine
 * Fix issue  #1748 http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1748
 *
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ***************************************************************************/
#ifndef StMuBTofPidTraits_hh
#define StMuBTofPidTraits_hh

class StBTofPidTraits;
#include "StThreeVectorF.hh"

class StMuBTofPidTraits : public TObject {
public:
    StMuBTofPidTraits();
    ~StMuBTofPidTraits();

   StBTofPidTraits* createBTofPidTraits() const;

    /// Matching information
    unsigned char    matchFlag() const;
    float            yLocal() const;
    float            zLocal() const;
    float            thetaLocal() const;
    
    StThreeVectorF&         position();
    const StThreeVectorF&   position() const;

    /// timing for PID
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

    void    setBTofPidTraits(const StBTofPidTraits*);

    void    setMatchFlag(unsigned char);
    void    setYLocal(float);
    void    setZLocal(float);
    void    setThetaLocal(float);
    void    setPosition(const StThreeVectorF&);                            

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

    ClassDef(StMuBTofPidTraits,1)
};

inline unsigned char StMuBTofPidTraits::matchFlag() const { return mMatchFlag; }
inline float StMuBTofPidTraits::yLocal() const { return mYLocal; }
inline float StMuBTofPidTraits::zLocal() const { return mZLocal; }
inline float StMuBTofPidTraits::thetaLocal() const { return mThetaLocal; }
inline float StMuBTofPidTraits::timeOfFlight() const { return mTimeOfFlight; }
inline float StMuBTofPidTraits::pathLength() const { return mPathLength; }
inline float StMuBTofPidTraits::beta() const { return mBeta; }
inline float StMuBTofPidTraits::sigmaElectron() const { return mSigmaElectron; }
inline float StMuBTofPidTraits::sigmaPion() const { return mSigmaPion; }
inline float StMuBTofPidTraits::sigmaKaon() const { return mSigmaKaon; }
inline float StMuBTofPidTraits::sigmaProton() const { return mSigmaProton; }
inline float StMuBTofPidTraits::probElectron() const { return mProbElectron; }
inline float StMuBTofPidTraits::probPion() const { return mProbPion; }
inline float StMuBTofPidTraits::probKaon() const { return mProbKaon; }
inline float StMuBTofPidTraits::probProton() const { return mProbProton; }

inline void StMuBTofPidTraits::setMatchFlag(unsigned char flag) { mMatchFlag=flag; }
inline void StMuBTofPidTraits::setYLocal(float y) { mYLocal=y; }
inline void StMuBTofPidTraits::setZLocal(float z) { mZLocal=z; }
inline void StMuBTofPidTraits::setThetaLocal(float theta) { mThetaLocal=theta; }
inline void StMuBTofPidTraits::setTimeOfFlight(float t) { mTimeOfFlight=t; }
inline void StMuBTofPidTraits::setPathLength(float s) { mPathLength=s; }
inline void StMuBTofPidTraits::setBeta(float beta) { mBeta=beta; }
inline void StMuBTofPidTraits::setSigmaElectron(float sigma) { mSigmaElectron=sigma; }
inline void StMuBTofPidTraits::setSigmaPion(float sigma) { mSigmaPion=sigma; }
inline void StMuBTofPidTraits::setSigmaKaon(float sigma) { mSigmaKaon=sigma; }
inline void StMuBTofPidTraits::setSigmaProton(float sigma) { mSigmaProton=sigma; }
inline void StMuBTofPidTraits::setProbElectron(float prob) { mProbElectron=prob; }
inline void StMuBTofPidTraits::setProbPion(float prob) { mProbPion=prob; }
inline void StMuBTofPidTraits::setProbKaon(float prob) { mProbKaon=prob; }
inline void StMuBTofPidTraits::setProbProton(float prob) { mProbProton=prob; }

#endif

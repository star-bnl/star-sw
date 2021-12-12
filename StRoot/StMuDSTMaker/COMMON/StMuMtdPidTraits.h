#ifndef StMuMtdPidTraits_hh
#define StMuMtdPidTraits_hh

#include "StTrackPidTraits.h"
#include "StThreeVectorF.hh"
class StMtdPidTraits;
class StMuMtdHit;

class StMuMtdPidTraits : public TObject {
public:
    StMuMtdPidTraits();
    ~StMuMtdPidTraits();
    
    StMtdPidTraits* createMtdPidTraits() const;
    
    /// Matching information
    unsigned char    matchFlag()  const;
    float            yLocal()     const;
    float            zLocal()     const;
    float            thetaLocal() const;
    float            deltaY()     const;
    float            deltaZ()     const;
    
    StThreeVectorF&         position();
    const StThreeVectorF&   position() const;
    
    /// timing for PID
    float   timeOfFlight() const;
    float   pathLength() const;
    float   beta() const;
    float   expTimeOfFlight() const;
    
    /// PID functions
    float   sigmaMuon() const;
    float   probMuon() const;
    
    /// Setters
    void    setMtdPidTraits(const StMtdPidTraits*);
    
    void    setMatchFlag(unsigned char);
    void    setYLocal(float);
    void    setZLocal(float);
    void    setThetaLocal(float);
    void    setPosition( const StThreeVectorF&);    
    void    setDeltaY(float);
    void    setDeltaZ(float);
    
    void    setTimeOfFlight(float);
    void    setPathLength(float);
    void    setBeta(float);
    
    void    setSigmaMuon(float);
    
    void    setProbMuon(float);
    
    void    setExpTimeOfFlight(float);
private:
   
    UChar_t         mMatchFlag;
    Float_t         mYLocal;
    Float_t         mZLocal;
    Float_t         mThetaLocal;
    StThreeVectorF  mPosition;
    
    Float_t         mTimeOfFlight;          // Measured time-of-flight
    Float_t         mPathLength;            // Path length obtained from track extrapolation
    Float_t         mBeta;
    
    Float_t         mSigmaMuon;
    Float_t         mProbMuon;

    Float_t         mExpTimeOfFlight;       // Expected time-of-flight obtained from track extrapolation
    Float_t         mDeltaY;                // DeltaY between matched track-hit pair
    Float_t         mDeltaZ;                // DeltaZ between matched track-hit pair
    
    ClassDef(StMuMtdPidTraits,3)
};

inline unsigned char StMuMtdPidTraits::matchFlag() const       { return mMatchFlag;      }
inline float StMuMtdPidTraits::yLocal() const                  { return mYLocal;         }
inline float StMuMtdPidTraits::zLocal() const                  { return mZLocal;         }
inline float StMuMtdPidTraits::deltaY() const                  { return mDeltaY;         }
inline float StMuMtdPidTraits::deltaZ() const                  { return mDeltaZ;         }
inline float StMuMtdPidTraits::thetaLocal() const              { return mThetaLocal;     }
inline float StMuMtdPidTraits::timeOfFlight() const            { return mTimeOfFlight;   }
inline float StMuMtdPidTraits::pathLength() const              { return mPathLength;     }
inline float StMuMtdPidTraits::beta() const                    { return mBeta;           }
inline float StMuMtdPidTraits::sigmaMuon() const               { return mSigmaMuon;      }
inline float StMuMtdPidTraits::probMuon() const                { return mProbMuon;       }
inline float StMuMtdPidTraits::expTimeOfFlight() const         {return mExpTimeOfFlight; }

inline void StMuMtdPidTraits::setMatchFlag(unsigned char flag) { mMatchFlag=flag;        }
inline void StMuMtdPidTraits::setYLocal(float y)               { mYLocal=y;              }
inline void StMuMtdPidTraits::setZLocal(float z)               { mZLocal=z;              }
inline void StMuMtdPidTraits::setDeltaY(float y)               { mDeltaY=y;              }
inline void StMuMtdPidTraits::setDeltaZ(float z)               { mDeltaZ=z;              }
inline void StMuMtdPidTraits::setThetaLocal(float theta)       { mThetaLocal=theta;      }
inline void StMuMtdPidTraits::setTimeOfFlight(float t)         { mTimeOfFlight=t;        }
inline void StMuMtdPidTraits::setPathLength(float s)           { mPathLength=s;          }
inline void StMuMtdPidTraits::setBeta(float beta)              { mBeta=beta;             }
inline void StMuMtdPidTraits::setSigmaMuon(float sigma)        { mSigmaMuon=sigma;       }
inline void StMuMtdPidTraits::setProbMuon(float prob)          { mProbMuon=prob;         }
inline void StMuMtdPidTraits::setExpTimeOfFlight(float time)   { mExpTimeOfFlight=time;  }
#endif

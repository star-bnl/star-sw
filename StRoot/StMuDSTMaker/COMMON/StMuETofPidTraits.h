/***************************************************************************
 *
 * $Id: StMuETofPidTraits.h,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, November 2018
 ***************************************************************************
 *
 * Description: Data class for storing eTOF PID information for tracks
 * matched to eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StMuETofPidTraits.h,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#ifndef STMUETOFPIDTRAITS_H
#define STMUETOFPIDTRAITS_H

#include "StThreeVectorF.hh"

class StETofPidTraits;


class StMuETofPidTraits : public TObject {
public:
    StMuETofPidTraits();
    ~StMuETofPidTraits();

    StETofPidTraits* createETofPidTraits() const;

    /// matching information
    unsigned short   matchFlag()  const;
    float            localX()     const;
    float            localY()     const;
    float            thetaLocal() const;
    float            deltaX()     const;
    float            deltaY()     const;
    
    StThreeVectorF&         position();
    const StThreeVectorF&   position() const;

    /// timing for PID
    float   timeOfFlight() const;
    float   pathLength() const;
    float   beta() const;
    
    /// PID functions -- to be added (?)


    /// setters
    void    setETofPidTraits( const StETofPidTraits* );

    void    setMatchFlag( const unsigned short& );

    void    setLocalX(     const float& );
    void    setLocalY(     const float& );
    void    setThetaLocal( const float& );
    void    setDeltaX(     const float& );
    void    setDeltaY(     const float& );
    
    void    setPosition( const StThreeVectorF& ); 

    void    setTimeOfFlight( const float& );
    void    setPathLength(   const float& );
    void    setBeta(         const float& );

private:
    UShort_t        mMatchFlag;
    Float_t         mLocalX;
    Float_t         mLocalY;
    Float_t         mThetaLocal;
    StThreeVectorF  mPosition;

    Float_t         mDeltaX;            // deltaX between matched track-hit pair
    Float_t         mDeltaY;            // deltaY between matched track-hit pair
    
    Float_t         mTimeOfFlight;      // measured time-of-flight
    Float_t         mPathLength;        // path length obtained from track extrapolation
    Float_t         mBeta;                 



    ClassDef( StMuETofPidTraits, 1 )
};

inline unsigned short StMuETofPidTraits::matchFlag()     const { return mMatchFlag;    }
inline float          StMuETofPidTraits::localX()        const { return mLocalX;       }
inline float          StMuETofPidTraits::localY()        const { return mLocalY;       }
inline float          StMuETofPidTraits::deltaX()        const { return mDeltaX;       }
inline float          StMuETofPidTraits::deltaY()        const { return mDeltaY;       }
inline float          StMuETofPidTraits::thetaLocal()    const { return mThetaLocal;   }
inline float          StMuETofPidTraits::timeOfFlight()  const { return mTimeOfFlight; }
inline float          StMuETofPidTraits::pathLength()    const { return mPathLength;   }
inline float          StMuETofPidTraits::beta()          const { return mBeta;         }

inline void StMuETofPidTraits::setMatchFlag(     const unsigned short& flag ) { mMatchFlag    = flag;    }
inline void StMuETofPidTraits::setLocalX(        const float& x )             { mLocalX       = x;       }
inline void StMuETofPidTraits::setLocalY(        const float& y )             { mLocalY       = y;       }
inline void StMuETofPidTraits::setDeltaX(        const float& x )             { mDeltaX       = x;       }
inline void StMuETofPidTraits::setDeltaY(        const float& y )             { mDeltaY       = y;       }
inline void StMuETofPidTraits::setThetaLocal(    const float& theta )         { mThetaLocal   = theta;   }
inline void StMuETofPidTraits::setTimeOfFlight(  const float& t )             { mTimeOfFlight = t;       }
inline void StMuETofPidTraits::setPathLength(    const float& s )             { mPathLength   = s;       }
inline void StMuETofPidTraits::setBeta(          const float& beta )          { mBeta         = beta;    }

#endif

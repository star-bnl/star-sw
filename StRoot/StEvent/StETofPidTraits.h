/***************************************************************************
 *
 * $Id: StETofPidTraits.h,v 2.1 2019/02/11 18:41:19 ullrich Exp $
 *
 * Author: Florian Seck, August 2018
 ***************************************************************************
 *
 * Description: Data class for storing eTOF PID information for tracks
 * matched to eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StETofPidTraits.h,v $
 * Revision 2.1  2019/02/11 18:41:19  ullrich
 * Initial Revision
 *
 *
 ***************************************************************************/ 
#ifndef STETOFPIDTRAITS_H
#define STETOFPIDTRAITS_H


#include "StTrackPidTraits.h"
#include "StETofHit.h"
#include "StThreeVectorF.hh"
#include "StEnumerations.h"



class StETofPidTraits : public StTrackPidTraits {
public:
    StETofPidTraits();
    ~StETofPidTraits();
    
    StETofHit*       etofHit();
    const StETofHit* etofHit() const;
    
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
    float   pathLength()   const;
    float   beta()         const;
    
    /// PID functions  --  to be added (?)


    /// setters
    void    setETofHit( StETofHit* hit );
    
    void    setMatchFlag( const unsigned short& flag );

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
    StETofHit* mETofHit;   //$LINK

    UShort_t        mMatchFlag;        // match flag: 
    Float_t         mLocalX;           // local X coordinate
    Float_t         mLocalY;           // local Y coordinate
    Float_t         mThetaLocal;       // angle of incident angle of track to volume normal
    Float_t         mDeltaX;           // delta X between matched track-hit pair
    Float_t         mDeltaY;           // delta Y between matched track-hit pair
    StThreeVectorF  mPosition;
   
    Float_t         mTimeOfFlight;     // measured time-of-flight
    Float_t         mPathLength;       // path length obtained from track extrapolation
    Float_t         mBeta;             // particle velocity    


    ClassDef( StETofPidTraits, 1 )
};

inline unsigned short StETofPidTraits::matchFlag()     const { return mMatchFlag;    }
inline float          StETofPidTraits::localX()        const { return mLocalX;       }
inline float          StETofPidTraits::localY()        const { return mLocalY;       }
inline float          StETofPidTraits::deltaX()        const { return mDeltaX;       }
inline float          StETofPidTraits::deltaY()        const { return mDeltaY;       }
inline float          StETofPidTraits::thetaLocal()    const { return mThetaLocal;   }
inline float          StETofPidTraits::timeOfFlight()  const { return mTimeOfFlight; }
inline float          StETofPidTraits::pathLength()    const { return mPathLength;   }
inline float          StETofPidTraits::beta()          const { return mBeta;         }

inline void StETofPidTraits::setMatchFlag(     const unsigned short& flag ) { mMatchFlag    = flag;    }
inline void StETofPidTraits::setLocalX(        const float& x )             { mLocalX       = x;       }
inline void StETofPidTraits::setLocalY(        const float& y )             { mLocalY       = y;       }
inline void StETofPidTraits::setDeltaX(        const float& x )             { mDeltaX       = x;       }
inline void StETofPidTraits::setDeltaY(        const float& y )             { mDeltaY       = y;       }
inline void StETofPidTraits::setThetaLocal(    const float& theta )         { mThetaLocal   = theta;   }
inline void StETofPidTraits::setTimeOfFlight(  const float& t )             { mTimeOfFlight = t;       }
inline void StETofPidTraits::setPathLength(    const float& s )             { mPathLength   = s;       }
inline void StETofPidTraits::setBeta(          const float& beta )          { mBeta         = beta;    }

#endif

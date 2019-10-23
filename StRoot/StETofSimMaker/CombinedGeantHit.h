#ifndef STETOFSIMMAKER_COMBINEDGEANTHIT_H
#define STETOFSIMMAKER_COMBINEDGEANTHIT_H
#include <vector>
#include "StThreeVectorD.hh"

struct g2t_ctf_hit_st;



class CombinedGeantHit : public TObject {

public:
    CombinedGeantHit();
    ~CombinedGeantHit();

    void addRawHit( const g2t_ctf_hit_st* hitIn, std::vector<int>& volumeIds );

    void averageRawHits();

    void log() const;

    size_t          nRawHits()      const;

    StThreeVectorD  mom()           const;
    StThreeVectorD  pos()           const;

    int             sector()        const;
    int             plane()         const;
    int             counter()       const;
    int             strip()         const;

    int             track_p()       const;

    double          pathLength()    const;
    double          time()          const;

    double          tot()           const;

    int             clusterSize()   const;

    std::vector< int >   volumeVec()     const;


    void            setMom( const double&, const double&, const double& );
    void            setPos( const double&, const double&, const double& );

    void            setNRawHits(    const int&    );
    
    void            setSector(      const int&    );
    void            setPlane(       const int&    );
    void            setCounter(     const int&    );
    void            setStrip(       const int&    );

    void            setTrack_p(     const int&    );

    void            setPathLength(  const double& );
    void            setTime(        const double& );
    void            setTot(         const double& );

    void            setClusterSize( const int&    );

protected:
    size_t          mNRawHits;

    double          mMomentum[ 3 ];
    double          mLocalPos[ 3 ];

    int             mSector;
    int             mPlane;
    int             mCounter;
    int             mStrip;

    int             mTrack_p;

    double          mPathLength;
    double          mTime;

    double          mTot;

    int             mClusterSize;


	ClassDef( CombinedGeantHit, 1 )
};


inline size_t           CombinedGeantHit::nRawHits()        const { return mNRawHits;    }

inline StThreeVectorD   CombinedGeantHit::mom()             const { return StThreeVectorD( mMomentum[ 0 ], mMomentum[ 1 ], mMomentum[ 2 ] ); }
inline StThreeVectorD   CombinedGeantHit::pos()             const { return StThreeVectorD( mLocalPos[ 0 ], mLocalPos[ 1 ], mLocalPos[ 2 ] ); }

inline int              CombinedGeantHit::sector()          const { return mSector;      }
inline int              CombinedGeantHit::plane()           const { return mPlane;       }
inline int              CombinedGeantHit::counter()         const { return mCounter;     }
inline int              CombinedGeantHit::strip()           const { return mStrip;       }

inline int              CombinedGeantHit::track_p()         const { return mTrack_p;     }

inline double           CombinedGeantHit::pathLength()      const { return mPathLength;  }
inline double           CombinedGeantHit::time()            const { return mTime;        }

inline double           CombinedGeantHit::tot()             const { return mTot;         }

inline int              CombinedGeantHit::clusterSize()     const { return mClusterSize; }


inline void     CombinedGeantHit::setMom( const double& x, const double& y, const double& z )  { mMomentum[ 0 ] = x; mMomentum[ 1 ] = y; mMomentum[ 2 ]= z; }
inline void     CombinedGeantHit::setPos( const double& x, const double& y, const double& z )  { mLocalPos[ 0 ] = x; mLocalPos[ 1 ] = y; mLocalPos[ 2 ]= z; }

inline void     CombinedGeantHit::setNRawHits(   const int& nRawHits       )     { mNRawHits    = nRawHits;    }

inline void     CombinedGeantHit::setSector(      const int& sector        )     { mSector      = sector;      }
inline void     CombinedGeantHit::setPlane(       const int& plane         )     { mPlane       = plane;       }
inline void     CombinedGeantHit::setCounter(     const int& counter       )     { mCounter     = counter;     }
inline void     CombinedGeantHit::setStrip(       const int& strip         )     { mStrip       = strip;       }

inline void     CombinedGeantHit::setTrack_p(     const int& track_p       )     { mTrack_p     = track_p;     }
inline void     CombinedGeantHit::setPathLength(  const double& pathLength )     { mPathLength  = pathLength;  }
inline void     CombinedGeantHit::setTime(        const double& time       )     { mTime        = time;        }
inline void     CombinedGeantHit::setTot(         const double& tot        )     { mTot         = tot;         }

inline void     CombinedGeantHit::setClusterSize( const int& clusterSize   )     { mClusterSize = clusterSize; }

#endif /* StETofSimMaker_CombinedGeantHit_h */

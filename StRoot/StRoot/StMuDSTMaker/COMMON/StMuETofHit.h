/***************************************************************************
 *
 * $Id: StMuETofHit.h,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information in MuDsts:
 * eTOF hits created out of 2 or more eTOF digis from different sides of the
 * MRPC counters 
 *
 ***************************************************************************
 *
 * $Log: StMuETofHit.h,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/
#ifndef STMUETOFHIT_H
#define STMUETOFHIT_H

#include "TObject.h"
#include "StMuDst.h"

class StETofHit;
class StMuTrack;


class StMuETofHit : public TObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StMuETofHit();


    /**
    ** @brief Constructor with detailled assignment.
    ** @param[in] sector        sector Id.
    ** @param[in] zPlane        eTof Z-Plane.
    ** @param[in] counter       counter Id.
    ** @param[in] time          absolute raw time [ns].
    ** @param[in] tot           raw Time Over Threshold [ns].
    ** @param[in] clusterSize   number of strips that are clustered in one hit
    ** @param[in] localX        local X position on detector [cm].
    ** @param[in] localY        local Y position on detector [cm].
    **/
    StMuETofHit( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
                 const double& time, const double& tot, const unsigned int clusterSize,
                 const double& localX, const double& localY );


    /**
    ** @brief Copy constructor.
    **/
    StMuETofHit( const StMuETofHit& );

    /**
    ** @brief Copy constructor from StEvent class to MuDst class.
    **/
    StMuETofHit( const StETofHit* );


    /**
    ** @brief Destructor.
    **/
    ~StMuETofHit();


    /**
    ** @brief Sector.
    **/
    unsigned int sector()      const;
    /**
    ** @brief ZPlane.
    **/
    unsigned int zPlane()      const;
    /**
    ** @brief Counter.
    **/
    unsigned int counter()     const;


    /**
    ** @brief Time.
    **/
    double time()              const;

    /**
    ** @brief Total Tot.
    **/
    double totalTot()          const;


    /**
    ** @brief Cluster size
    **/
    unsigned int clusterSize() const;


    /**
    ** @brief X-position.
    **/
    double localX()            const;
    /**
    ** @brief Y-position.
    **/
    double localY()            const;


    /**
    ** @brief id to the track which has been matched to this hit
    **/
    int associatedTrackId() const;

    int index2Primary() const;
    int index2Global()  const;
    
    StMuTrack* primaryTrack() const;
    StMuTrack* globalTrack()  const;

    /**
    ** @brief mc-true associated track id in simulation
    **/
    unsigned int idTruth()     const;
    /**
    ** @brief quality of this information (percentage of charge produced by mIdTruth)
    **/
    unsigned int qaTruth()     const;


    /**
    ** @brief Sorting using the time, assumes Digis are in same reference frame (e.g. same epoch).
    **/
    bool operator<( const StMuETofHit& rhs ) const; 
    
    int compare( const TObject*     obj )    const;
    int compare( const StMuETofHit* hit )    const;


    /** Modifiers **/
    void setHwAddress( const unsigned int iSector, const unsigned int iZPlane, const unsigned int iCounter );

    void setSector(  const unsigned int sector   );
    void setZPlane(  const unsigned int zPlane   );
    void setCounter( const unsigned int counter  );

    void setTotalTot   ( const double& tot   );
    void setTime       ( const double& time  );

    void setClusterSize( const unsigned int clustSize );

    void setLocalX     ( const double& X     );
    void setLocalY     ( const double& Y     );

    void setAssociatedTrackId( const int& id );
    void setIndex2Primary(     const int& id );
    void setIndex2Global(      const int& id );

    void setIdTruth( unsigned short idtruth, unsigned short qatruth=0 );


private:
    UInt_t      mSector;
    UInt_t      mZPlane;
    UInt_t      mCounter;
    
    Double_t    mTime;
    Double_t    mTotalTot;

    UInt_t      mClusterSize;

    Double_t    mLocalX;
    Double_t    mLocalY;

    Int_t       mAssociatedTrackId;
    Int_t       mIndex2Primary;
    Int_t       mIndex2Global;

    UShort_t     mIdTruth;
    UShort_t     mQuality;

    friend class StMuDst;



    ClassDef( StMuETofHit, 1 );
};



inline unsigned int StMuETofHit::sector()      const { return mSector;  }
inline unsigned int StMuETofHit::zPlane()      const { return mZPlane;  }
inline unsigned int StMuETofHit::counter()     const { return mCounter; }

inline double StMuETofHit::time()     const { return mTime;     }
inline double StMuETofHit::totalTot() const { return mTotalTot; }

inline unsigned int StMuETofHit::clusterSize() const { return mClusterSize; }

inline double StMuETofHit::localX()   const { return mLocalX; }
inline double StMuETofHit::localY()   const { return mLocalY; }

inline int  StMuETofHit::associatedTrackId()   const { return mAssociatedTrackId; }
inline int  StMuETofHit::index2Primary()       const { return mIndex2Primary;     }
inline int  StMuETofHit::index2Global()        const { return mIndex2Global;      }

inline StMuTrack* StMuETofHit::primaryTrack() const { return ( mIndex2Primary>=0 ) ? ( StMuTrack* ) StMuDst::array( muPrimary )->UncheckedAt( mIndex2Primary ) : 0; }
inline StMuTrack* StMuETofHit::globalTrack()  const { return ( mIndex2Global >=0 ) ? ( StMuTrack* ) StMuDst::array( muGlobal  )->UncheckedAt( mIndex2Global  ) : 0; }

inline unsigned int StMuETofHit::idTruth()    const { return mIdTruth; }
inline unsigned int StMuETofHit::qaTruth()    const { return mQuality; }


inline void StMuETofHit::setSector(  const unsigned int sector  )  { mSector   = sector;  }
inline void StMuETofHit::setZPlane(  const unsigned int zPlane  )  { mZPlane   = zPlane;  }
inline void StMuETofHit::setCounter( const unsigned int counter )  { mCounter  = counter; }

inline void StMuETofHit::setTotalTot   ( const double& tot   )  { mTotalTot    = tot;     }
inline void StMuETofHit::setTime       ( const double& time  )  { mTime        = time;    }

inline void StMuETofHit::setClusterSize( const unsigned int clustSize ) { mClusterSize = clustSize; }

inline void StMuETofHit::setLocalX     ( const double& X     )  { mLocalX = X; }
inline void StMuETofHit::setLocalY     ( const double& Y     )  { mLocalY = Y; }

inline void StMuETofHit::setAssociatedTrackId( const int& id ) { mAssociatedTrackId = id; }
inline void StMuETofHit::setIndex2Primary(     const int& id ) { mIndex2Primary     = id; }
inline void StMuETofHit::setIndex2Global(      const int& id ) { mIndex2Global      = id; }


#endif // STMUETOFHIT_H 

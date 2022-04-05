/***************************************************************************
 *
 * $Id: StETofHit.h,v 2.3 2019/02/11 18:53:09 ullrich Exp $
 *
 * Author: Philipp Weidenkaff, April 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information:
 * eTOF hits created out of 2 or more eTOF digis from different sides of the
 * MRPC counters 
 *
 ***************************************************************************
 *
 * $Log: StETofHit.h,v $
 * Revision 2.3  2019/02/11 18:53:09  ullrich
 * Added additional access functions to get the associated track & idTruth and qaTruth variables for simulated Hits.
 *
 * Revision 2.2  2018/07/13 14:55:09  ullrich
 * Added getter function for the associated hit (Florian)
 *
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#ifndef STETOFHIT_H
#define STETOFHIT_H

#include <Stiostream.h>
#include "StObject.h"

class StTrack;


class StETofHit : public StObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StETofHit();


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
    StETofHit( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
               const double& time, const double& tot, const unsigned int clusterSize,
               const double& localX, const double& localY );


    /**
    ** @brief Copy constructor.
    **/
    StETofHit( const StETofHit& );


    /**
    ** @brief Destructor.
    **/
    ~StETofHit();


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
    ** @brief pointer to the track which has been matched to this hit
    **/
    StTrack* associatedTrack();
    
    /**
    ** @brief pointer to the track which has been matched to this hit
    **/
    StTrack* associatedTrack() const;


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
    bool operator<( const StETofHit& rhs ) const; 
    
    int compare( const StObject*  obj )    const;
    int compare( const StETofHit* hit )    const;


    /** Modifiers **/
    void setHwAddress( const unsigned int iSector, const unsigned int iZPlane, const unsigned int iCounter);

    void setSector(  const unsigned int sector   );
    void setZPlane(  const unsigned int zPlane   );
    void setCounter( const unsigned int counter  );

    void setTotalTot   ( const double& tot   );
    void setTime       ( const double& time  );

    void setClusterSize( const unsigned int clustSize );

    void setLocalX     ( const double& X     );
    void setLocalY     ( const double& Y     );


    void setAssociatedTrack( StTrack* trk );

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

    StTrack*    mAssociatedTrack;  //$LINK

    UShort_t     mIdTruth;
    UShort_t     mQuality;



    ClassDef( StETofHit, 1 );
};

ostream& operator << ( ostream&, const StETofHit& hit ); // Printing operator


inline unsigned int StETofHit::sector()      const { return mSector;  };
inline unsigned int StETofHit::zPlane()      const { return mZPlane;  };
inline unsigned int StETofHit::counter()     const { return mCounter; };

inline double StETofHit::time()     const { return mTime;     }
inline double StETofHit::totalTot() const { return mTotalTot; };

inline unsigned int StETofHit::clusterSize() const { return mClusterSize; };

inline double StETofHit::localX()   const { return mLocalX; };
inline double StETofHit::localY()   const { return mLocalY; };

inline StTrack* StETofHit::associatedTrack()       { return mAssociatedTrack; };
inline StTrack* StETofHit::associatedTrack() const { return mAssociatedTrack; };

inline unsigned int StETofHit::idTruth()     const { return mIdTruth; }
inline unsigned int StETofHit::qaTruth()     const { return mQuality; }

inline void StETofHit::setSector(  const unsigned int sector  )  { mSector   = sector;  }
inline void StETofHit::setZPlane(  const unsigned int zPlane  )  { mZPlane   = zPlane;  }
inline void StETofHit::setCounter( const unsigned int counter )  { mCounter  = counter; }

inline void StETofHit::setTotalTot   ( const double& tot   )  { mTotalTot    = tot;     }
inline void StETofHit::setTime       ( const double& time  )  { mTime        = time;    }

inline void StETofHit::setClusterSize( const unsigned int clustSize ) { mClusterSize = clustSize; }

inline void StETofHit::setLocalX     ( const double& X     )  { mLocalX = X; }
inline void StETofHit::setLocalY     ( const double& Y     )  { mLocalY = Y; }

inline void StETofHit::setAssociatedTrack( StTrack* trk ) { mAssociatedTrack = trk; }


#endif // STETOFHIT_H 

/***************************************************************************
 *
 * $Id: StMuETofDigi.h,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information in MuDsts:
 * eTOF digis capture the electronic response of each side of the MRPC
 * counter read-out
 *
 ***************************************************************************
 *
 * $Log: StMuETofDigi.h,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#ifndef STMUETOFDIGI_H
#define STMUETOFDIGI_H

#include "TObject.h"

class StETofDigi;


class StMuETofDigi : public TObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StMuETofDigi();

    /**
    ** @brief Constructor with detailled assignment.
    ** @param[in] sector        sector Id.
    ** @param[in] zPlane        eTof Z-plane.
    ** @param[in] counter       counter Id.
    ** @param[in] strip         strip Id.
    ** @param[in] side          channel Side (optional, used for strips). (cf CbmTofAddress) 
    ** @param[in] time          absolute raw time [ns].
    ** @param[in] tot           raw Time Over Threshold [ns].
    **/
    StMuETofDigi( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
                  const unsigned int strip, const unsigned int side,
                  const double& time, const double& tot );

    /**
    ** @brief Constructor with detailled assignment.
    ** @param[in] rocId	        Readout Controller Id.
    ** @param[in] get4Id        Get4 Chip Id.
    ** @param[in] elChan        Get4 Channel Id (electronic channel).
    ** @param[in] time          Absolute raw time [ns].
    ** @param[in] tot           raw Time Over Threshold [ns].
    **/
    StMuETofDigi( const unsigned int rocId, const unsigned int get4Id, const unsigned int elChan,   
                  const double& time, const double& tot );

    /**
    ** @brief Copy constructor.
    **/
    StMuETofDigi( const StMuETofDigi& );


    /**
    ** @brief Copy constructor from StEvent class to MuDst class.
    **/
    StMuETofDigi( const StETofDigi* );
    

    /**
    ** @brief Copy constructor from StEvent class to MuDst class with assignment of associated hit index
    **/
    StMuETofDigi( const StETofDigi*, const int assocHitId );


    /**
    ** @brief Destructor.
    **/
    ~StMuETofDigi();

    /**
    ** @brief Raw Time.
    **/
    double rawTime()          const;

    /**
    ** @brief calibrated time
    **/
    double calibTime()        const;



    /**
    ** @brief Alias for GetRawTot.
    **/
    double rawCharge()        const;
    /**
    ** @brief Getter for uncalibrated Tot.
    **/
    double rawTot()           const; 
    /**
    ** @brief Alias for GetRawTot.
    **/
    double calibCharge()      const;
    /**
    ** @brief Getter for calibrated Tot.
    **/
    double calibTot()         const;



    /**
    ** @brief Sector.
    **/
    unsigned int sector()     const;
    /**
    ** @brief ZPlane.
    **/
    unsigned int zPlane()     const;
    /**
    ** @brief Counter.
    **/
    unsigned int counter()    const;
    /**
    ** @brief Strip.
    **/
    unsigned int strip()      const;
    /**
    ** @brief Alias for strip.
    **/
    unsigned int chan()       const;
    /**
    ** @brief Side.
    **/
    unsigned int side()       const;


    /**
    ** @brief electronic Channel.
    **/
    unsigned int elChan()     const;
    /**
    ** @brief get4Id.
    **/
    unsigned int get4Id()     const;
    /**
    ** @brief RocId.
    **/
    unsigned int rocId()      const;


    /**
    ** @id of the hit which has been reconstructed from this digi
    **/
    int associatedHitId()     const;



    /**
    ** @brief Sorting using the time, assumes Digis are in same reference frame (e.g. same epoch).
    **/
    bool operator < ( const StMuETofDigi& rhs ) const; //ordering operator

    int compare( const TObject*       obj  )    const;
    int compare( const StMuETofDigi*  digi )    const;
    

    /** Modifiers **/
    void setGeoAddress( const unsigned int iSector, const unsigned int iZPlane, const unsigned int iCounter,
                        const unsigned int iChannel, const unsigned int iSide );
    void setHwAddress(  const unsigned int iRocId, const unsigned int iGet4Id, const unsigned int iElChan );

    void setRawTime(   const double& time );  //ns
    void setRawTot(    const double& tot  );  //ns
    void setCalibTime( const double& time );
    void setCalibTot(  const double& tot  );
    
    void setAssociatedHitId( const int& id );


private:
    UInt_t      mSector;
    UInt_t      mZPlane;
    UInt_t      mCounter;
    UInt_t      mStrip;
    UInt_t      mSide;

    UInt_t      mRocId;
    UInt_t      mGet4Id;
    UInt_t      mElChan;

    Double_t    mRawTime;
    Double_t    mCalibTime;
    Double_t    mRawTot;
    Double_t    mCalibTot;

    Int_t       mAssociatedHitId;


    ClassDef( StMuETofDigi, 1 );
};

inline double StMuETofDigi::rawTime()      const { return mRawTime;   };
inline double StMuETofDigi::calibTime()    const { return mCalibTime; };
inline double StMuETofDigi::rawCharge()    const { return rawTot();   };
inline double StMuETofDigi::rawTot()       const { return mRawTot;    };
inline double StMuETofDigi::calibCharge()  const { return calibTot(); };
inline double StMuETofDigi::calibTot()     const { return mCalibTot;  };

inline unsigned int StMuETofDigi::sector()   const { return mSector;  };
inline unsigned int StMuETofDigi::zPlane()   const { return mZPlane;  };
inline unsigned int StMuETofDigi::counter()  const { return mCounter; };
inline unsigned int StMuETofDigi::strip()    const { return mStrip;   };
inline unsigned int StMuETofDigi::chan()     const { return strip();  };
inline unsigned int StMuETofDigi::side()     const { return mSide;    };
inline unsigned int StMuETofDigi::elChan()   const { return mElChan;  };
inline unsigned int StMuETofDigi::get4Id()   const { return mGet4Id;  };
inline unsigned int StMuETofDigi::rocId()    const { return mRocId;   };

inline int StMuETofDigi::associatedHitId()   const { return mAssociatedHitId; };

inline void StMuETofDigi::setRawTime(   const double& time )  { mRawTime   = time; };
inline void StMuETofDigi::setRawTot(    const double& tot  )  { mRawTot    = tot;  };
inline void StMuETofDigi::setCalibTime( const double& time )  { mCalibTime = time; };
inline void StMuETofDigi::setCalibTot(  const double& tot  )  { mCalibTot  = tot;  };

inline void StMuETofDigi::setAssociatedHitId( const int& id ) { mAssociatedHitId = id; };


#endif // STMUETOFDIGI_H

/***************************************************************************
 *
 * $Id: StETofDigi.h,v 2.3 2019/02/11 18:52:38 ullrich Exp $
 *
 * Author: Philipp Weidenkaff, April 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information:
 * eTOF digis capture the electronic response of each side of the MRPC
 * counter read-out
 *
 ***************************************************************************
 *
 * $Log: StETofDigi.h,v $
 * Revision 2.3  2019/02/11 18:52:38  ullrich
 * Added an additional access functions to get the associated hit.
 *
 * Revision 2.2  2018/07/13 14:55:09  ullrich
 * Added getter function for the associated hit (Florian)
 *
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/ 
#ifndef STETOFDIGI_H
#define STETOFDIGI_H

#include <Stiostream.h>
#include "StObject.h"

class StETofHit;


class StETofDigi : public StObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StETofDigi();

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
    StETofDigi( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
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
    StETofDigi( const unsigned int rocId, const unsigned int get4Id, const unsigned int elChan,   
                const double& time, const double& tot );

    /**
    ** @brief Copy constructor.
    **/
    StETofDigi( const StETofDigi& );

    /**
    ** @brief Destructor.
    **/
    ~StETofDigi();

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
    ** @brief pointer to the hit which has been reconstructed from this digi
    **/
    StETofHit* associatedHit();
    /**
    ** @brief pointer to the hit which has been reconstructed from this digi
    **/
    StETofHit* associatedHit() const;


    /**
    ** @brief Sorting using the time, assumes Digis are in same reference frame (e.g. same epoch).
    **/
    bool operator < ( const StETofDigi& rhs ) const; //ordering operator

    int compare( const StObject*    obj  )    const;
    int compare( const StETofDigi*  digi )    const;
    

    /** Modifiers **/
    void setGeoAddress( const unsigned int iSector, const unsigned int iZPlane, const unsigned int iCounter,
                        const unsigned int iChannel, const unsigned int iSide );
    void setHwAddress( const unsigned int iRocId, const unsigned int iGet4Id, const unsigned int iElChan );

    void setRawTime(   const double& time );  //ns
    void setRawTot(    const double& tot  );  //ns
    void setCalibTime( const double& time );
    void setCalibTot(  const double& tot  );
    
    void setAssociatedHit( StETofHit* hit );


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

    StETofHit*  mAssociatedHit; //$LINK


    ClassDef( StETofDigi, 1 );
};

ostream& operator << ( ostream&, const StETofDigi& digi ); // Printing operator


inline double StETofDigi::rawTime()      const { return mRawTime;   };
inline double StETofDigi::calibTime()    const { return mCalibTime; };
inline double StETofDigi::rawCharge()    const { return rawTot();   };
inline double StETofDigi::rawTot()       const { return mRawTot;    };
inline double StETofDigi::calibCharge()  const { return calibTot(); };
inline double StETofDigi::calibTot()     const { return mCalibTot;  };

inline unsigned int StETofDigi::sector()   const { return mSector;  };
inline unsigned int StETofDigi::zPlane()   const { return mZPlane;  };
inline unsigned int StETofDigi::counter()  const { return mCounter; };
inline unsigned int StETofDigi::strip()    const { return mStrip;   };
inline unsigned int StETofDigi::chan()     const { return strip();  };
inline unsigned int StETofDigi::side()     const { return mSide;    };
inline unsigned int StETofDigi::elChan()   const { return mElChan;  };
inline unsigned int StETofDigi::get4Id()   const { return mGet4Id;  };
inline unsigned int StETofDigi::rocId()    const { return mRocId;   };

inline StETofHit*   StETofDigi::associatedHit()         { return mAssociatedHit; };
inline StETofHit*   StETofDigi::associatedHit()  const  { return mAssociatedHit; };

inline void StETofDigi::setRawTime(   const double& time )  { mRawTime   = time; };
inline void StETofDigi::setRawTot(    const double& tot  )  { mRawTot    = tot;  };
inline void StETofDigi::setCalibTime( const double& time )  { mCalibTime = time; };
inline void StETofDigi::setCalibTot(  const double& tot  )  { mCalibTot  = tot;  };

inline void StETofDigi::setAssociatedHit( StETofHit* hit ) { mAssociatedHit = hit; };


#endif // STETOFDIGI_H

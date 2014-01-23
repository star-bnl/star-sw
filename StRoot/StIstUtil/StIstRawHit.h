/***************************************************************************
*
* $Id: StIstRawHit.h,v 1.1 2014/01/23 20:11:31 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* Data structure for individual IST pad (channel).
****************************************************************************
*
* $Log: StIstRawHit.h,v $
* Revision 1.1  2014/01/23 20:11:31  ypwang
* adding scripts
*
*
****************************************************************************
* StIstRawHit.h,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstRawHit_hh
#define StIstRawHit_hh

#include "StObject.h"
#include "StIstConsts.h"

class StIstRawHit : public StObject
{
public:
    // constructors
    StIstRawHit();
    StIstRawHit( const StIstRawHit& );
    StIstRawHit& operator=( const StIstRawHit& );
    //deconstructors
    ~StIstRawHit();

    // accessors
    int       	      getChannelId()  const; //!< 0-110591
    int		      getGeoId()      const; //!< 1-110592
    unsigned char     getLadder()     const; //!< 1-24
    unsigned char     getSensor()     const; //!< 1-6
    unsigned char     getRow()        const; //!< 1-64
    unsigned char     getColumn()     const; //!< 1-12
    float     	      getCharge(unsigned char tb = 0) 	 const;
    float             getChargeErr(unsigned char tb = 0) const;
    unsigned char     getMaxTimeBin() const;
    unsigned char     getRdo()        const; //!< 1-6
    unsigned char     getArm()        const; //!< 0-5
    unsigned char     getApv()        const; //!< 0-23
    unsigned char     getChannel()    const; //!< 0-127    
    unsigned char     getDefaultTimeBin()       	 const;
    unsigned short    getIdTruth()    const; //!< for embedding, 0 as background
   
    // modifiers
    void        setChannelId(int rChannelId) ;
    void	setGeoId(int rChannelId);
    void        setCharge(float charge, unsigned char tb = -1) ;
    void	setChargeErr(float chargeErr, unsigned char tb = -1) ;
    void        setMaxTimeBin(unsigned char tb) ;
    void        setDefaultTimeBin( unsigned char tb )  ;
    void        setIdTruth(unsigned short idTruth);
    
private:
    // data member
    Int_t       mChannelId;                 // channel Id, numbering from 0 to 110591
    Int_t       mGeoId;                     // geometry Id, numbering from 1 to 110592
    Float_t     mCharge[kIstNumTimeBins];   // raw ADC value saved in calibration mode; 
					    // charge value saved in non-calibration mode (pedestal subtracted and CMN corrected)
    Float_t     mChargeErr[kIstNumTimeBins];// charge error in all time bins
    UChar_t     mMaxTimeBin;                // the max ADC time bin index of the raw hit
    UShort_t    mIdTruth;           	    // !< for embedding, 0 as background 
    static UChar_t mDefaultTimeBin;
    
    ClassDef(StIstRawHit, 1)
};

// Function for sorting the raw hits in the pad Id order.
struct rawHitPtrLessThan {
    bool operator() (const StIstRawHit* rawHit1, const StIstRawHit* rawHit2) const;
};

//inline functions
//accessories
inline int       	 StIstRawHit::getChannelId()                         const  { return mChannelId;   };
inline int               StIstRawHit::getGeoId()                             const  { return mGeoId;   };
inline unsigned char     StIstRawHit::getLadder()                            const  { 
	return 1 + (mGeoId-1)/(kIstNumSensorsPerLadder*kIstNumPadsPerSensor);
};
inline unsigned char     StIstRawHit::getSensor()                            const  { 
	return 1 + ((mGeoId-1)%(kIstNumSensorsPerLadder*kIstNumPadsPerSensor))/kIstNumPadsPerSensor;   
};
inline unsigned char     StIstRawHit::getRow()                               const  { 
	short pad = ((mGeoId-1)%(kIstNumSensorsPerLadder*kIstNumPadsPerSensor))%kIstNumPadsPerSensor;
	return 1 + pad%kIstNumRowsPerSensor;
};
inline unsigned char     StIstRawHit::getColumn()                            const  { 
	short pad = ((mGeoId-1)%(kIstNumSensorsPerLadder*kIstNumPadsPerSensor))%kIstNumPadsPerSensor;
	return 1 + pad/kIstNumRowsPerSensor;
};
inline float     	 StIstRawHit::getCharge( unsigned char tb )                 const  {
	return mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ];
};
inline float     	 StIstRawHit::getChargeErr( unsigned char tb )                const  {
        return mChargeErr[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ];
};
inline unsigned char     StIstRawHit::getMaxTimeBin()                        const  { return mMaxTimeBin; };
inline unsigned char     StIstRawHit::getRdo()                               const  { 
        return 1 + mChannelId/(kIstNumArmsPerRdo*kIstNumChanPerArm); 
};
inline unsigned char     StIstRawHit::getArm()                               const  {
        return (mChannelId%(kIstNumArmsPerRdo*kIstNumChanPerArm))/kIstNumChanPerArm;        
};
inline unsigned char     StIstRawHit::getApv()                               const  {
	return ((mChannelId%(kIstNumArmsPerRdo*kIstNumChanPerArm))%kIstNumChanPerArm)/kIstNumApvChannels;
};
inline unsigned char     StIstRawHit::getChannel()                          const  {
	return ((mChannelId%(kIstNumArmsPerRdo*kIstNumChanPerArm))%kIstNumChanPerArm)%kIstNumApvChannels;
};
inline unsigned char     StIstRawHit::getDefaultTimeBin()                    const  { return mDefaultTimeBin; };
inline unsigned short    StIstRawHit::getIdTruth()                           const  { return mIdTruth; };

//modifiers
inline void        StIstRawHit::setChannelId(int rChannelId)              { mChannelId = rChannelId;     };
inline void        StIstRawHit::setGeoId(int rGeoId)              	  { mGeoId = rGeoId;     };
inline void        StIstRawHit::setCharge( float charge, unsigned char tb )      {
	mCharge[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = charge;
};
inline void        StIstRawHit::setChargeErr(float rChargeErr, unsigned char tb) {
        mChargeErr[ (tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb ] = rChargeErr;
};
inline void        StIstRawHit::setMaxTimeBin(unsigned char tb)                  {
    	mMaxTimeBin = ((tb < 0 || tb >= kIstNumTimeBins) ? mDefaultTimeBin : tb);
};
inline void        StIstRawHit::setDefaultTimeBin( unsigned char tb )             { mDefaultTimeBin = tb;      };
inline void        StIstRawHit::setIdTruth(unsigned short idTruth)                { mIdTruth = idTruth;        };

#endif

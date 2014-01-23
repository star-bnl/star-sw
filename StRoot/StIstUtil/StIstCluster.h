/***************************************************************************
*
* $Id: StIstCluster.h,v 1.1 2014/01/23 20:11:30 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* Data structure for IST cluster (neighboring pads fired by the same track).
****************************************************************************
*
* $Log: StIstCluster.h,v $
* Revision 1.1  2014/01/23 20:11:30  ypwang
* adding scripts
*
*
****************************************************************************
* StIstCluster.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstCluster_hh
#define StIstCluster_hh

#include <map>

#include "StObject.h"
#include "StIstRawHit.h"

typedef std::map< StIstRawHit*, float, rawHitPtrLessThan > rawHitMap_t;

class StIstCluster: public TObject {
 public:
    //constructor
    StIstCluster(int key=-1, unsigned char ladder=-1, unsigned char sensor=-1, float meanRow=-1, float meanColumn=-1, float totCharge=0, float totChargeErr=0, unsigned char clusteringType=-1);
    
    //deconstructor
    ~StIstCluster();
        
    // accessors/modifiers for the map
    rawHitMap_t& getRawHitMap();
    const rawHitMap_t& getRawHitMap() const;
    
    //accessories
    int		      	getKey()		const;
    unsigned char     	getLadder() 		const;//!< 1-24
    unsigned char     	getSensor() 		const;//!< 1-6
    float     		getMeanRow() 		const;
    float	     	getMeanColumn() 	const;  
    float     		getTotCharge() 		const;     
    float     		getTotChargeErr() 	const;  
    unsigned char     	getMaxTimeBin() 	const; 
    unsigned char     	getClusteringType() 	const;
    unsigned char     	getNRawHits() 		const;  
    unsigned char     	getNRawHitsRPhi() 	const; 
    unsigned char     	getNRawHitsZ() 		const;
    unsigned short    	getIdTruth()    	const;

    //modifiers
    void        setLadder(unsigned char ladder);
    void        setSensor(unsigned char sensor);
    void        setMeanRow(float meanRow);
    void        setMeanColumn(float meanColumn);
    void        setTotCharge(float totCharge);
    void        setTotChargeErr(float totChargeErr);
    void        setMaxTimeBin(unsigned char tb);
    void        setClusteringType(unsigned char clusteringType);    
    void        setNRawHits(unsigned char nRawHits);
    void        setNRawHitsRPhi(unsigned char nRawHitsRPhi);
    void        setNRawHitsZ(unsigned char nRawHitsZ);
    void        setIdTruth(unsigned short idTruth);
     
protected:
    Int_t	mKey;			    //cluster unique lable
    UChar_t     mLadderId;                  //ladder id the cluster belongs to
    UChar_t     mSensorId;                  //sensor id the cluster belongs to
    Float_t     mMeanRow, mMeanColumn;      //mean row and mean column
    Float_t     mTotCharge;                 //charge sum of the cluster
    Float_t     mTotChargeErr;              //RMS noise of the cluster
    UChar_t     mClusteringType;            //clustering algorithm type
    UChar_t     mMaxTimeBin;                //max ADC time bin index
    UChar_t     mNRawHits;		    //cluster size
    UChar_t     mNRawHitsRPhi;		    //cluster size in r-phi direction
    UChar_t     mNRawHitsZ;		    //cluster size in beam direction
    UShort_t    mIdTruth;		    //!< for embedding, 0 as background
    rawHitMap_t mRawHitMap;                 //map container to save raw hits who contribute to the cluster
    
private:
    ClassDef(StIstCluster, 1);
};

//inline function
inline rawHitMap_t& StIstCluster::getRawHitMap() 		  {    return mRawHitMap;  };
inline const rawHitMap_t& StIstCluster::getRawHitMap() 	   const  {    return mRawHitMap;  };
inline int	 	StIstCluster::getKey()		   const  {    return mKey;	      };
inline unsigned char   	StIstCluster::getLadder() 	   const  {    return mLadderId;      };
inline unsigned char   	StIstCluster::getSensor() 	   const  {    return mSensorId;      };
inline float   		StIstCluster::getMeanRow() 	   const  {    return mMeanRow;       };
inline float   		StIstCluster::getMeanColumn() 	   const  {    return mMeanColumn;    };
inline float	   	StIstCluster::getTotCharge() 	   const  {    return mTotCharge;     };
inline float   		StIstCluster::getTotChargeErr()    const  {    return mTotChargeErr;  };
inline unsigned char   	StIstCluster::getMaxTimeBin() 	   const  {    return mMaxTimeBin;    };
inline unsigned char   	StIstCluster::getClusteringType()  const  {    return mClusteringType;};
inline unsigned char   	StIstCluster::getNRawHits() 	   const  {    return mNRawHits;      };
inline unsigned char   	StIstCluster::getNRawHitsRPhi()    const  {    return mNRawHitsRPhi;  };
inline unsigned char   	StIstCluster::getNRawHitsZ() 	   const  {    return mNRawHitsZ;     };
inline unsigned short  	StIstCluster::getIdTruth()	   const  {    return mIdTruth;       };
//////////
inline void StIstCluster::setLadder(unsigned char ladder)            	{    mLadderId = ladder; };
inline void StIstCluster::setSensor(unsigned char sensor)            	{    mSensorId = sensor; };
inline void StIstCluster::setMeanRow(float meanRow)          		{    mMeanRow = meanRow; };
inline void StIstCluster::setMeanColumn(float meanColumn)    		{    mMeanColumn = meanColumn; };
inline void StIstCluster::setTotCharge(float totCharge)      		{    mTotCharge = totCharge;       };
inline void StIstCluster::setTotChargeErr(float totChargeErr)		{    mTotChargeErr = totChargeErr;       };
inline void StIstCluster::setMaxTimeBin(unsigned char tb)            	{    mMaxTimeBin = tb; };
inline void StIstCluster::setClusteringType(unsigned char clusteringType)         {    mClusteringType = clusteringType;   };
inline void StIstCluster::setNRawHits(unsigned char nRawHits)        	{    mNRawHits = nRawHits;  };
inline void StIstCluster::setNRawHitsRPhi(unsigned char nRawHitsRPhi)	{    mNRawHitsRPhi = nRawHitsRPhi;  };
inline void StIstCluster::setNRawHitsZ(unsigned char nRawHitsZ)      	{    mNRawHitsZ = nRawHitsZ;  };
inline void StIstCluster::setIdTruth(unsigned short idTruth)         	{    mIdTruth = idTruth;  };

#endif

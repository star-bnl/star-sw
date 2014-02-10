/***************************************************************************
*
* $Id: StIstCluster.h,v 1.6 2014/02/10 16:34:22 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* Data structure for IST cluster (neighboring pads fired by the same track).
****************************************************************************
*
* $Log: StIstCluster.h,v $
* Revision 1.6  2014/02/10 16:34:22  smirnovd
* Use constructor initialization list, other nit-picks
*
* Revision 1.5  2014/02/10 16:34:09  smirnovd
* Addressed minor doxygen and style issues
*
* Revision 1.4  2014/02/10 16:33:46  smirnovd
* Trimmed trailing spaces, expanded tabs to eight spaces
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
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


class StIstCluster: public TObject
{
public:

   StIstCluster(int key = -1, unsigned char ladder = -1, unsigned char sensor = -1,
      float meanRow = -1, float meanColumn = -1, float totCharge = 0,
      float totChargeErr = 0, unsigned char clusteringType = -1);
   ~StIstCluster();

   //accessors
   rawHitMap_t &getRawHitMap();
   const rawHitMap_t   &getRawHitMap() const;
   int                  getKey()            const;
   unsigned char        getLadder()         const; //!< 1-24
   unsigned char        getSensor()         const; //!< 1-6
   float                getMeanRow()        const;
   float                getMeanColumn()     const;
   float                getTotCharge()      const;
   float                getTotChargeErr()   const;
   unsigned char        getMaxTimeBin()     const;
   unsigned char        getClusteringType() const;
   unsigned char        getNRawHits()       const;
   unsigned char        getNRawHitsRPhi()   const;
   unsigned char        getNRawHitsZ()      const;
   unsigned short       getIdTruth()        const;

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
   Int_t       mKey;                    ///< Cluster unique label
   UChar_t     mLadderId;               ///< Ladder id the cluster belongs to
   UChar_t     mSensorId;               ///< Sensor id the cluster belongs to
   Float_t     mMeanRow;                ///< Cluster's mean row
   Float_t     mMeanColumn;             ///< Cluster's mean column
   Float_t     mTotCharge;              ///< Charge sum of the cluster
   Float_t     mTotChargeErr;           ///< rMS noise of the cluster
   UChar_t     mClusteringType;         ///< Clustering algorithm type
   UChar_t     mMaxTimeBin;             ///< Max ADC time bin index
   UChar_t     mNRawHits;               ///< Cluster size
   UChar_t     mNRawHitsRPhi;           ///< Cluster size in r-phi direction
   UChar_t     mNRawHitsZ;              ///< Cluster size in beam direction
   UShort_t    mIdTruth;                //!< For embedding, 0 as background
   rawHitMap_t mRawHitMap;              ///< Map container to save raw hits who contribute to the cluster

   ClassDef(StIstCluster, 1);
};
#endif

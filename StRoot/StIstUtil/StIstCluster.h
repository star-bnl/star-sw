// $Id: StIstCluster.h,v 1.12 2016/01/05 16:29:25 smirnovd Exp $

#ifndef StIstCluster_hh
#define StIstCluster_hh

#include <map>

#include "StObject.h"
#include "StIstRawHit.h"


/**
 * Data structure for IST cluster (neighboring pads fired by the same track).
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstCluster: public TObject
{
public:

   StIstCluster(int key = -1, int ladder = -1, int sensor = -1,
                float meanRow = -1, float meanColumn = -1, float totCharge = 0,
                float totChargeErr = 0, int clusteringType = -1);
   ~StIstCluster();

   //accessors
   vector<StIstRawHit *> &getRawHitVec();
   const vector<StIstRawHit *> &getRawHitVec() 	const;
   int                  getKey()            	const;
   unsigned char        getLadder()         	const; //!< 1-24
   unsigned char        getSensor()         	const; //!< 1-6
   float                getMeanRow()        	const;
   float                getMeanColumn()     	const;
   float                getTotCharge()      	const;
   float                getTotChargeErr()   	const;
   unsigned char        getMaxTimeBin()     	const;
   unsigned char        getClusteringType() 	const;
   unsigned char        getNRawHits()       	const;
   unsigned char        getNRawHitsRPhi()   	const;
   unsigned char        getNRawHitsZ()      	const;
   unsigned short       getIdTruth()        	const;

   void        setLadder(int ladder);
   void        setSensor(int sensor);
   void        setMeanRow(float meanRow);
   void        setMeanColumn(float meanColumn);
   void        setTotCharge(float totCharge);
   void        setTotChargeErr(float totChargeErr);
   void        setMaxTimeBin(int tb);
   void        setClusteringType(int clusteringType);
   void        setNRawHits(int nRawHits);
   void        setNRawHitsRPhi(int nRawHitsRPhi);
   void        setNRawHitsZ(int nRawHitsZ);
   void        setIdTruth(unsigned short idTruth);

   virtual void Print(Option_t *opt = "") const;

protected:
   Int_t       mKey;                    	///< Cluster unique label
   Float_t     mMeanRow;                	///< Cluster's mean row
   Float_t     mMeanColumn;             	///< Cluster's mean column
   Float_t     mTotCharge;              	///< Charge sum of the cluster
   Float_t     mTotChargeErr;           	///< rMS noise of the cluster
   UShort_t    mIdTruth;                        //!< For embedding, 0 as background
   UChar_t     mLadderId;                       ///< Ladder id the cluster belongs to
   UChar_t     mSensorId;                       ///< Sensor id the cluster belongs to
   UChar_t     mClusteringType;         	///< Clustering algorithm type
   UChar_t     mMaxTimeBin;             	///< Max ADC time bin index
   UChar_t     mNRawHits;               	///< Cluster size
   UChar_t     mNRawHitsRPhi;           	///< Cluster size in r-phi direction
   UChar_t     mNRawHitsZ;              	///< Cluster size in beam direction
   std::vector<StIstRawHit *> mRawHitVec;	///< Map container to save raw hits who contribute to the cluster

   ClassDef(StIstCluster, 1);
};

#endif

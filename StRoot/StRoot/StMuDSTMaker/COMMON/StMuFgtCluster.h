/***************************************************************************
 *
 * $Id: StMuFgtCluster.h,v 1.2 2012/12/12 00:36:03 sangalin Exp $
 * Author: S. Gliske, Jan. 2012
 *
 ***************************************************************************
 *
 * Description: data for a 1D cluster.  Contains subset of the
 * information in the StEvent/StMuFgtCluster class.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtCluster.h,v $
 * Revision 1.2  2012/12/12 00:36:03  sangalin
 * Merged in updated StMuFgtCluster class format from Anselm Vossen.
 *
 * Revision 1.5  2012/07/20 16:11:24  sgliske
 * Added StFgtStripAssociation, and removed all dynamically
 * allocated memory from StMuFgt* containers.
 * Also removed StMuFgtInfo
 *
 * Revision 1.4  2012/01/30 16:38:03  sgliske
 * updated class def number
 *
 * Revision 1.3  2012/01/28 10:47:12  sgliske
 * Added cluster charge uncertainty to containers
 *
 * Revision 1.2  2012/01/04 22:34:56  sgliske
 * Fixed some bugs
 *
 * Revision 1.1  2012/01/04 19:15:34  sgliske
 * Reintroduced support for the FGT in MuDst
 *
 *
 **************************************************************************/

#ifndef _ST_MU_FGT_CLUSTER_H_
#define _ST_MU_FGT_CLUSTER_H_

#include <TObject.h>
#include <TArrayI.h>
#include <TArrayF.h>

class StFgtHit;

class StMuFgtCluster : public TObject {
 public:
   // constructors
  StMuFgtCluster( Int_t centralStripGeoId = -1, Int_t firstStripAssociationIdx = -1, Int_t maxTimeBin=-1, Int_t maxAdc=-9999, Int_t numStrips = 0, 
                   Float_t charge = 0, Float_t chargeUncert = 1000,
                   Float_t r = 0,      Float_t errR = 1e10,
		  Float_t phi = 0,    Float_t errPhi = 1e10 , Float_t evenOddChargeAsy=-1);

   // converting from StFgtHit
   StMuFgtCluster( const StFgtHit& fgtHit );

   // defaults
   // StMuFgtCluster(const StMuFgtCluster&);             --> use default
   // StMuFgtCluster& operator=(const StMuFgtCluster&);  --> use default
   // ~StMuFgtCluster();                                 --> use default

   // accessors
   Int_t getCentralStripGeoId() const;
   Int_t getFirstStripAssociationIndex() const;
   Int_t getMaxTimeBin() const;
   Int_t getMaxAdc() const;
   Int_t getNumStrips() const;
   Float_t getCharge() const;
   Float_t getChargeUncert() const;
   Float_t getR() const;
   Float_t getErrR() const;
   Float_t getPhi() const;
   Float_t getErrPhi() const;
   Float_t getEvenOddChargeAsy() const;

   // modifiers
   void setCentralStripGeoId( Int_t geoId );
   void setCharge( Float_t val );
   void setChargeUncert( Float_t val );
   void setR( Float_t val );
   void setErrR( Float_t val );
   void setPhi( Float_t val );
   void setErrPhi( Float_t val );
   void setEvenOddChargeAsy(Float_t val);
   void setMaxTimeBin( Int_t val );
   void setMaxAdc( Int_t val );
   void setNumStrips( Int_t numStrips );

   void setFirstStripAssociationIndex( Int_t idx );


 protected:
   // data members
   Int_t mCentralStripGeoId;                     // obvious--also serves as a unique key, and
                                                 // identifies the physical quadrant of the cluster

   Int_t mFirstStripAssociationIdx;              // Index in the MuDst mFgtStripAssociation
                                                 // TClonesArray. The associated strips are from index
                                                 // mFirstStripAssociationIdx to
                                                 // mFirstStripAssociationIdx+mNumStrips-1
   Int_t mMaxTimeBin;                            // time bin for max adc
   Int_t mMaxAdc;                                // max adc in all strips and timebin

   Int_t mNumStrips;                             // number of strips associated with this cluster

   Float_t mCharge, mChargeUncert;               // associated charge for the cluster, and its uncertainty
   Float_t mR, mErrR, mPhi, mErrPhi;             // r, phi position and error
    Float_t mEvenOddChargeAsy;                    // (even-odd)/sum charge for phi layer

 private:   
   ClassDef( StMuFgtCluster, 4 );
}; 

// inline functions

inline Int_t   StMuFgtCluster::getCentralStripGeoId() const { return mCentralStripGeoId; };
inline Int_t   StMuFgtCluster::getNumStrips() const { return mNumStrips; };
inline Int_t   StMuFgtCluster::getMaxTimeBin() const { return mMaxTimeBin; };
inline Int_t   StMuFgtCluster::getMaxAdc() const { return mMaxAdc; };
inline Int_t   StMuFgtCluster::getFirstStripAssociationIndex() const { return mFirstStripAssociationIdx; };
inline Float_t StMuFgtCluster::getCharge() const { return mCharge; };
inline Float_t StMuFgtCluster::getChargeUncert() const { return mChargeUncert; };
inline Float_t StMuFgtCluster::getR() const { return mR; };
inline Float_t StMuFgtCluster::getErrR() const { return mErrR; };
inline Float_t StMuFgtCluster::getPhi() const { return mPhi; };
inline Float_t StMuFgtCluster::getErrPhi() const { return mErrPhi; };
inline Float_t   StMuFgtCluster::getEvenOddChargeAsy() const { return mEvenOddChargeAsy; };

inline void StMuFgtCluster::setCentralStripGeoId( Int_t val ){ mCentralStripGeoId = val; };
inline void StMuFgtCluster::setNumStrips( Int_t val ) { mNumStrips = val; };
inline void StMuFgtCluster::setMaxTimeBin( Int_t val ) { mMaxTimeBin = val; };
inline void StMuFgtCluster::setMaxAdc( Int_t val ) { mMaxAdc = val; };
inline void StMuFgtCluster::setFirstStripAssociationIndex( Int_t val ) { mFirstStripAssociationIdx = val; };
inline void StMuFgtCluster::setCharge( Float_t val ){ mCharge = val; };
inline void StMuFgtCluster::setChargeUncert( Float_t val ){ mChargeUncert = val; };
inline void StMuFgtCluster::setR( Float_t val ){ mR = val; };
inline void StMuFgtCluster::setErrR( Float_t val ){ mErrR = val; };
inline void StMuFgtCluster::setPhi( Float_t val ){ mPhi = val; };
inline void StMuFgtCluster::setErrPhi( Float_t val ){ mErrPhi = val; };
inline void StMuFgtCluster::setEvenOddChargeAsy( Float_t val ){ mEvenOddChargeAsy = val; };

#endif

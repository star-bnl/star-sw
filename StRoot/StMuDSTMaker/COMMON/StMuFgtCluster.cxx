/***************************************************************************
 *
 * $Id: StMuFgtCluster.cxx,v 1.3 2018/01/04 17:36:47 smirnovd Exp $
 * Author: S. Gliske, Jan. 2012
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtCluster.cxx,v $
 * Revision 1.3  2018/01/04 17:36:47  smirnovd
 * [Cosmetic] Remove StRoot/ from include path
 *
 * $STAR/StRoot is already in the default path search
 *
 * Revision 1.2  2012/12/12 00:36:03  sangalin
 * Merged in updated StMuFgtCluster class format from Anselm Vossen.
 *
 * Revision 1.3  2012/07/20 16:11:24  sgliske
 * Added StFgtStripAssociation, and removed all dynamically
 * allocated memory from StMuFgt* containers.
 * Also removed StMuFgtInfo
 *
 * Revision 1.2  2012/01/28 10:47:12  sgliske
 * Added cluster charge uncertainty to containers
 *
 * Revision 1.1  2012/01/04 19:15:34  sgliske
 * Reintroduced support for the FGT in MuDst
 *
 *
 **************************************************************************/

#include "StMuFgtCluster.h"
#include "StEvent/StFgtHit.h"

// constructor
StMuFgtCluster::StMuFgtCluster( Int_t centralStripGeoId, Int_t firstStripAssociationIdx, Int_t maxTimeBin, Int_t maxAdc,Int_t numStrips,  Float_t charge, Float_t chargeUncert, Float_t r, Float_t errR, Float_t phi, Float_t errPhi, Float_t evenOddChargeAsy ) :
   mCentralStripGeoId( centralStripGeoId ),
   mFirstStripAssociationIdx( firstStripAssociationIdx ),
   mMaxTimeBin( maxTimeBin ),
   mMaxAdc( maxAdc ),
   mNumStrips( numStrips ),
   mCharge( charge ),
   mChargeUncert( chargeUncert ),
   mR( r ),
   mErrR( errR ),
   mPhi( phi ),
   mErrPhi( errPhi ),
   mEvenOddChargeAsy(evenOddChargeAsy){ /* */ };


// converting from StFgtHit
StMuFgtCluster::StMuFgtCluster( const StFgtHit& fgtHit ) :
   mCentralStripGeoId( fgtHit.getCentralStripGeoId() ),
   mCharge( fgtHit.charge() ),
   mChargeUncert( fgtHit.getChargeUncert() ),
   mR( fgtHit.getPositionR() ),
   mErrR( fgtHit.getErrorR() ),
   mPhi( fgtHit.getPositionPhi() ),
   mErrPhi( fgtHit.getErrorPhi() ){
   mNumStrips = fgtHit.getStripWeightMap().size();
   mMaxTimeBin = fgtHit.getMaxTimeBin();
   mMaxAdc= fgtHit.getMaxAdc();
   mEvenOddChargeAsy = fgtHit.getEvenOddChargeAsy();
};

ClassImp( StMuFgtCluster );

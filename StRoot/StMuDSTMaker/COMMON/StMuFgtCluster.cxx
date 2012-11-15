/***************************************************************************
 *
 * $Id: StMuFgtCluster.cxx,v 1.1 2012/11/15 22:27:24 sangalin Exp $
 * Author: S. Gliske, Jan. 2012
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StMuFgtCluster.cxx,v $
 * Revision 1.1  2012/11/15 22:27:24  sangalin
 * Copied over from StFgtDevel.
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
#include "StRoot/StEvent/StFgtHit.h"

// constructor
StMuFgtCluster::StMuFgtCluster( Int_t centralStripGeoId, Int_t firstStripAssociationIdx, Int_t numStrips, Float_t charge, Float_t chargeUncert, Float_t r, Float_t errR, Float_t phi, Float_t errPhi ) :
   mCentralStripGeoId( centralStripGeoId ),
   mFirstStripAssociationIdx( firstStripAssociationIdx ),
   mNumStrips( numStrips ),
   mCharge( charge ),
   mChargeUncert( chargeUncert ),
   mR( r ),
   mErrR( errR ),
   mPhi( phi ),
   mErrPhi( errPhi ){ /* */ };


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
};

ClassImp( StMuFgtCluster );

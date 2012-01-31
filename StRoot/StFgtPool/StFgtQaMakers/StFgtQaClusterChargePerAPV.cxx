/*!
 * \class StFgtQaClusterChargePerAPV 
 * \author S. Gliske, Oct 2011
 */

/***************************************************************************
 *
 * $Id: StFgtQaClusterChargePerAPV.cxx,v 1.2 2012/01/31 16:48:34 wwitzke Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: Maker to make a histogram of the charge per cluster,
 * combined for all r (or phi) strips connected to single APV.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaClusterChargePerAPV.cxx,v $
 * Revision 1.2  2012/01/31 16:48:34  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2011/11/01 19:07:24  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.1  2011/10/10 17:40:36  sgliske
 * creation
 *
 *
 **************************************************************************/

#include "StFgtQaClusterChargePerAPV.h"

#include <string>
#include <TH2F.h>

#include "StFgtQaMaker.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"

#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"


StFgtQaClusterChargePerAPV::StFgtQaClusterChargePerAPV( const Char_t* name,
                                                        Short_t discId,
                                                        Short_t quadId,
                                                        Short_t apvId,
                                                        const Char_t* quadName ) :
   StFgtQaMaker( name, discId, quadId, quadName ),
   mNbins( 160 ), mChargeMin( -512 ), mChargeMax( 2048 ), mUnits("arb. units")
{
   //initialize hist pointers
   for( Int_t i=0; i<10; ++i ){
      mHistR[i] = 0;
      mHistPhi[i] = 0;
   };
};

StFgtQaClusterChargePerAPV::StFgtQaClusterChargePerAPV(const StFgtQaClusterChargePerAPV& other ) :
   StFgtQaMaker(other),
   mNbins( 160 ), mChargeMin( -512 ), mChargeMax( 2048 ), mUnits("arb. units") {

   //initialize hist pointers
   for( Int_t i=0; i<10; ++i ){
      mHistR[i] = 0;
      mHistPhi[i] = 0;

      if( other.mHistR[i] ){
         std::string name = std::string( other.mHistR[i]->GetName() ) + "_copy";
         mHistR[i] = static_cast< TH1F* >( other.mHistR[i]->Clone(name.data()) );
      };

      if( other.mHistPhi ){
         std::string name = std::string( other.mHistPhi[i]->GetName() ) + "_copy";
         mHistPhi[i] = static_cast< TH1F* >(  other.mHistPhi[i]->Clone( name.data() ) );
      };
   };
};

// deconstructor
StFgtQaClusterChargePerAPV::~StFgtQaClusterChargePerAPV(){
   for( Int_t i=0; i<10; ++i ){
      if( mHistR[i] )
         delete mHistR[i];
      if( mHistPhi[i] )
         delete mHistPhi[i];
   };
};

// equals operator
StFgtQaClusterChargePerAPV& StFgtQaClusterChargePerAPV::operator=(const StFgtQaClusterChargePerAPV& rhs ){
   StFgtQaMaker::operator=( rhs );

   for( Int_t i=0; i<10; ++i ){
      if( mHistR[i] )
         delete mHistR[i];
      if( mHistPhi[i] )
         delete mHistPhi[i];

      mHistR[i] = 0;
      mHistPhi[i] = 0;
   };

   // copy hist pointers
   for( Int_t i=0; i<10; ++i ){
      mHistR[i] = 0;
      mHistPhi[i] = 0;

      if( rhs.mHistR[i] ){
         std::string name = std::string( rhs.mHistR[i]->GetName() ) + "_copy";
         mHistR[i] = static_cast< TH1F* >( rhs.mHistR[i]->Clone(name.data()) );
      };

      if( rhs.mHistPhi ){
         std::string name = std::string( rhs.mHistPhi[i]->GetName() ) + "_copy";
         mHistPhi[i] = static_cast< TH1F* >(  rhs.mHistPhi[i]->Clone( name.data() ) );
      };
   };

   return *this;
};

Int_t StFgtQaClusterChargePerAPV::Init(){
   Int_t ierr = StFgtQaMaker::Init();

   if( !ierr ){
      // Take care of the histogram

      for( Int_t i=0; i<10; ++i ){
         if( mHistR[i] )
            delete mHistR[i];
         if( mHistPhi[i] )
            delete mHistPhi[i];

         mHistR[i] = 0;
         mHistPhi[i] = 0;

         std::string name;
         {
            std::stringstream ss;
            ss << GetName() << "_" << mDiscId << "_" << mQuadId << "_" << i;
            ss >> name;
         };

         mHistR[i]   = new TH1F( ( name + "_histR"  ).data(), "", mNbins, mChargeMin, mChargeMax );
         mHistPhi[i] = new TH1F( ( name + "_histPhi").data(), "", mNbins, mChargeMin, mChargeMax );

         std::string titleR = "R-strip Charge per Cluster,";
         std::string titlePhi = "#phi-strip Charge per Cluster,";

         std::string middle;
         {
            std::stringstream ss;
            ss << "Quad " << mQuadName << ", APV " << i;
            ss >> middle;
         };
         middle += "; Charge per strip [";
         middle += mUnits;
         middle += "];";


         titleR += middle;
         titlePhi += middle;

         titleR += "R-strip Id.";
         titlePhi += "#phi-strip Id.";

         mHistR[i]->SetTitle( titleR.data() );
         mHistPhi[i]->SetTitle( titlePhi.data() );
      };
   };

   return ierr;
};

Int_t StFgtQaClusterChargePerAPV::Make(){
   Int_t ierr = StFgtQaMaker::Make();

   StFgtHitCollection *hitCollectionPtr = 0;
   if( !ierr ){
      hitCollectionPtr = mFgtCollectionPtr->getHitCollection( mDiscId );
   };

   if( hitCollectionPtr ){
      const StSPtrVecFgtHit &hitVec = hitCollectionPtr->getHitVec();
      StSPtrVecFgtHitConstIterator hitIter;
      stripWeightMap_t::iterator stripMapIter;

      for( hitIter = hitVec.begin(); hitIter != hitVec.end(); ++hitIter ){
         stripWeightMap_t& stripWeightMap = (*hitIter)->getStripWeightMap();

         Float_t maxCharge = -99999;
         Int_t geoId4Max = 0;

         // find the geoId for the strip with the most charge
         // contributing to the cluster
         for( stripMapIter = stripWeightMap.begin(); stripMapIter != stripWeightMap.end(); ++stripMapIter ){
            const StFgtStrip *stripPtr = stripMapIter->first;
            Float_t w = stripMapIter->second;

            Float_t stripCharge = 0;
            if( stripPtr && w )
               stripCharge = w*stripPtr->getCharge();

            if( stripCharge > maxCharge ){
               geoId4Max = stripPtr->getGeoId();
               maxCharge = stripCharge;
            };
         };

         if( maxCharge > -99999 ){
            // now find the quadrant and apv
            Int_t rdo, arm, apv, channel;
            Short_t disc, quad, strip;
            Char_t layer;

            StFgtCosmicTestStandGeom::decodeGeoId( geoId4Max, disc, quad, layer, strip );
            if( quad == mQuadId && disc == mDiscId ){
               StFgtCosmicTestStandGeom::getNaiveElecCoordFromGeoId( geoId4Max, rdo, arm, apv, channel);

               apv %= 12;
               if( apv > -1 && apv < 10 )
                  ( layer == 'R' ? mHistR[apv] : mHistPhi[apv] )->Fill( maxCharge );
            };
         };
      };
   };

   return ierr;
};

Int_t StFgtQaClusterChargePerAPV::Finish(){
   // nothing for the moment.
   return kStOk;
};

ClassImp(StFgtQaClusterChargePerAPV);

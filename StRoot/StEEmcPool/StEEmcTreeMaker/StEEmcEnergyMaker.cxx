/*
 * Created by S. Gliske, May 2012
 *
 * Description: maker to fill the EEmcEnergy_t structure.
 *
 */

#include "StEEmcEnergyMaker.h"

#include "StRoot/StEEmcPool/./EEmcTreeContainers/EEmcEnergy.h"
#include "StRoot/StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"

/// constructor
StEEmcEnergyMaker_t::StEEmcEnergyMaker_t( const Char_t *myName, const Char_t *a2EMakerName ) : StMaker( myName ), mA2EMkrName( a2EMakerName ),
                                                                                               mNumTowers( 0 ), mTowerThres( 1 ), mStripThres( 0.0005 ) { /* */ };

/// deconstructor
StEEmcEnergyMaker_t::~StEEmcEnergyMaker_t(){ /* */ };

/// Initialize
Int_t StEEmcEnergyMaker_t::Init(){
   Int_t ierr = kStOk;

   mA2EMkr = static_cast< StEEmcA2EMaker* >( GetMaker( mA2EMkrName.data() ) );

   if( !mA2EMkr ){
      LOG_ERROR << "Cannot find maker with the name '" << mA2EMkrName << "'" << endm;
      ierr = kStFatal;
   };

   return ierr;
};

/// Build an event
Int_t StEEmcEnergyMaker_t::Make(){
   assert( mA2EMkr ); // already checked this in ::Init

   static ETowEnergy_t *etowLayerPtrArr[4] = { &mEEmcEnergy.eTow, 
                                               &mEEmcEnergy.ePre1,
                                               &mEEmcEnergy.ePre2,
                                               &mEEmcEnergy.ePost   };

   mEEmcEnergy.nTowers = 0;
   mEEmcEnergy.nStrips = 0;

#ifdef DEBUG
   cout << GetEventNumber() << " Number of towers/strips in A2CMaker? ";
   for( Int_t i=0; i<4; ++i )
      cout << mA2EMkr->towers( i ).size() << ' ';
   cout << "| ";
   for( Int_t layer = 0; layer < 2; ++layer ){
      for( Int_t sector = 0; sector < kEEmcNumSectors; ++sector )
         cout << mA2EMkr->strips( sector, layer ).size() << ' ';
      if( !layer ) 
         cout << "| ";
   };
   cout << endl;
#endif

   for( Int_t iLayer=0; iLayer<4; ++iLayer ){
      StEEmcTowerVec_t& towVec = mA2EMkr->towers( iLayer );
      ETowEnergy_t* etowEptr =   etowLayerPtrArr[iLayer];

      if( !towVec.empty() ){
         StEEmcTowerVec_t::iterator towIter;

         for( towIter = towVec.begin(); towIter != towVec.end(); ++towIter ){
            EEmcElement_t& element = etowEptr->getByIdx( towIter->index() );
            element.energy = towIter->energy();
            element.fail = towIter->fail() || towIter->stat();

            if( iLayer == 0 && element.energy > mTowerThres && !element.fail )
               ++mEEmcEnergy.nTowers;

#ifdef DEBUG2
            cout << "Tower Layer " << iLayer << ", idx " << towIter->index() << ", E = " << element.energy << ", num = " << mEEmcEnergy.nTowers << endl;
#endif
         };
      };

      if( iLayer == 0 && mEEmcEnergy.nTowers < mNumTowers )
         return kStOk;
   };

   for( Int_t sector = 0; sector < kEEmcNumSectors; ++sector ){
      for( Int_t layer = 0; layer < 2; ++layer ){
         StEEmcStripVec_t &stripVec = mA2EMkr->strips( sector, layer );

         if( !stripVec.empty() ){
            ESmdLayer_t& esmdLayer = mEEmcEnergy.eSmd.sec[ sector ].layer[ layer ];
            esmdLayer.nStrips = 0;
            StEEmcStripVec_t::iterator stripIter;

            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               EEmcElement_t& element = esmdLayer.strip[ stripIter->index() ];

               element.energy = stripIter->energy();
               element.fail = stripIter->fail() || stripIter->stat();

               if( element.energy > mStripThres && !element.fail ){
                  ++esmdLayer.nStrips;
                  ++mEEmcEnergy.nStrips;
               };

#ifdef DEBUG2
               cout << "SMD Sector/Layer " << sector << (layer ? 'v' : 'u' )
                    << ", idx " << stripIter->index() << ", E = " << element.energy << ", num = " << mEEmcEnergy.nStrips << endl;
#endif
            };
         };
      };
   };

   return kStOk;
};

/// Clear for next event
void StEEmcEnergyMaker_t::Clear(Option_t *opts ){
   mEEmcEnergy.Clear();
};

ClassImp( StEEmcEnergyMaker_t );

/*
 * $Id: StEEmcEnergyMaker.cxx,v 1.2 2013/02/21 21:59:02 sgliske Exp $
 * $Log: StEEmcEnergyMaker.cxx,v $
 * Revision 1.2  2013/02/21 21:59:02  sgliske
 * general update
 *
 * Revision 1.1  2012/11/26 19:06:10  sgliske
 * moved from offline/users/sgliske/StRoot/StEEmcPool/StEEmcTreeMaker to StRoot/StEEmcPool/StEEmcTreeMaker
 *
 * 
 */

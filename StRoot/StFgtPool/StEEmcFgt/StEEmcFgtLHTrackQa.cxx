/***************************************************************************
 *
 * $Id: StEEmcFgtLHTrackQa.cxx,v 1.2 2012/04/11 22:13:24 sgliske Exp $
 * Author: S. Gliske, April 2012
 *
 ***************************************************************************
 *
 * Description: See header.
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtLHTrackQa.cxx,v $
 * Revision 1.2  2012/04/11 22:13:24  sgliske
 * update
 *
 *
 **************************************************************************/

#include <vector>
#include <TVector3.h>
#include "StEEmcFgtLHTrackQa.h"
#include "StRoot/StFgtPool/StFgtTracking/StFgtLHTracking.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StRoot/StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StRoot/StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StRoot/StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"

StEEmcFgtLHTrackQa::StEEmcFgtLHTrackQa( const Char_t* name, const Char_t* a2eMkrName, const Char_t* fgtLHTkrName ) : StMaker( name ), mEnergyPerTrack(0), mEnergy(0) {
   mEEmcA2EMkr = static_cast< StEEmcA2EMaker* >( GetMaker( a2eMkrName ) );
   mFgtLHTkr = 
};

// deconstructor
StEEmcFgtLHTrackQa::~StEEmcFgtLHTrackQa(){ /* */ };

Int_t StEEmcFgtLHTrackQa::Init(){
   mEnergy = new TH1F( "hEnergy", "", 20, 0, 30 );
   mEnergyPerTrack = new TH1F( "hEnergyPerTrack", "", 20, 0, 30 );
   assert( mEEmcA2EMkr );

   return kStOK;
};

Int_t StEEmcFgtLHTrackQa::Make(){

   StFgtLHTrackData *data = static_cast< StFgtLHTrackData* >( GetData( "LHTracks" ) );
   assert( mEEmcA2EMkr );

   LOG_INFO << "Event " << GetEventNumber() << " data at " << data << endm;

   if( data ){
      const StFgtLHTrackVec& trackVec = data->getTrackVec();
      StFgtLHTrackVec::const_iterator trackVecIter;

      std::map< Int_t, Int_t > towMap;
      std::map< Int_t, Int_t >::iterator towMapIter;
      
      EEmcGeomSimple& eemcGeom = EEmcGeomSimple::Instance();
      Int_t sec, sub, etabin;

      for( trackVecIter = trackVec.begin(); trackVecIter != trackVec.end(); ++trackVecIter ){
         // get the x and y position at the endcap
         Double_t x = trackVecIter->line.bx + trackVecIter->line.mx*kEEmcZSMD;
         Double_t y = trackVecIter->line.by + trackVecIter->line.my*kEEmcZSMD;
         TVector3 pos( x, y, kEEmcZSMD );

         Bool_t found = eemcGeom.getTower( pos, sec, sub, etabin );
         if( found ){
            Int_t index = etabin + kEEmcNumEtas*( sub + kEEmcNumSubSectors*sec );

            towMapIter = towMap.find( index );
            if( towMapIter == towMap.end() )
               towMap[index] = 1;
            else
               ++(towMapIter->second);
         };
      };

      for( towMapIter = towMap.begin(); towMapIter != towMap.end(); ++towMapIter ){
         Int_t index = towMapIter->first;
         Int_t num = towMapIter->second;

         StEEmcTower &tower = mEEmcA2EMkr->hittower( index, 0 );
         Float_t energy = tower.energy();

         mEnergy->Fill( energy );
         mEnergyPerTrack->Fill( energy/num );

         cout << "EEMC tower: " << index << ' ' << num << ' ' << energy << endl;
      };
   };

   return kStOK;
};

ClassImp(StEEmcFgtLHTrackQa);

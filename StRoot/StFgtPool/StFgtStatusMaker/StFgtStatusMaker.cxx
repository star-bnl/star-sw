/***************************************************************************
 *
 * $Id: StFgtStatusMaker.cxx,v 1.3 2012/01/31 16:49:14 wwitzke Exp $
 * Author: C. K. Riley, Nov 2011 & S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtStatusMaker.cxx,v $
 * Revision 1.3  2012/01/31 16:49:14  wwitzke
 * Changed for cosmic test stand.
 *
 * Revision 1.2  2012/01/31 09:15:34  sgliske
 * includes updated since status and Ped makers moved to Pool
 *
 * Revision 1.1  2012/01/31 08:59:43  sgliske
 * moved StFgtStatus maker to StFgtPool
 *
 * Revision 1.8  2012/01/28 13:06:31  sgliske
 * fixed some indexing issues in StFgtStatusMaker and StFgtPedStatQA
 *
 * Revision 1.7  2012/01/26 13:13:12  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.6  2012/01/18 18:53:01  sgliske
 * minor bug fix
 *
 * Revision 1.5  2012/01/18 18:07:35  sgliske
 * directly use elec coord domian--no geoIds
 *
 * Revision 1.4  2012/01/17 22:24:46  sgliske
 * removed hack for DB and
 *  fixed bug in writing status to txt file
 *
 * Revision 1.3  2012/01/17 20:13:12  sgliske
 * Completely new system, based on pedestal and RMS values.
 *
 * Revision 1.2  2011/12/07 17:17:54  ckriley
 * minor update
 *
 * Revision 1.1  2011/11/25 20:22:37  ckriley
 * creation of statusmaker
 *
 *
 *
 **************************************************************************/

#include <string>
#include "StFgtStatusMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
#include "StRoot/StFgtPool/StFgtPedMaker/StFgtPedMaker.h"

// constructor
StFgtStatusMaker::StFgtStatusMaker( const Char_t* name, const Char_t* pedMakerName ) :
   StMaker( name ), mPedMkrName( pedMakerName ), mPedMkr(0), mTimeBin(2),
   mMinPed( 100 ), mMaxPed( 1200 ), mMinRMS( 10 ), mMaxRMS( 80 ), mMinFrac( 0.6 ), mMaxFrac( 0.95 ), mMaxDead( 64 ), mHasFinished( 0 ) {

   mStatus = new status_t [ kFgtNumElecIds ];
   mApvData.resize( kFgtNumRdos * kFgtNumArms * kFgtApvsPerArm );
};

// deconstructor
StFgtStatusMaker::~StFgtStatusMaker(){
   if( mStatus )
      delete[] mStatus;
};

// initialize
Int_t StFgtStatusMaker::Init(){
   Int_t ierr = kStOk;

   // make sure the file can be opened
   if( !mFilename.empty() ){
      std::ofstream fout( mFilename.data() );
      if( !fout ){
         LOG_FATAL << "Error opening file '" << mFilename << "'" << endm;
         ierr = kStFatal;
      };
   };

   // make sure there's a pedmaker
   mPedMkr = static_cast< StFgtPedMaker* >( GetMaker( mPedMkrName.data() ) );
   if( !mPedMkr ){
      LOG_FATAL << "Cannot find StFgtPedMaker" << endm; 
      ierr = kStFatal;
   };

   // make sure it will compute the peds for this timebine
   if( !ierr ){
      if( !mPedMkr->mTimeBinMask & (1<<mTimeBin) ){
         LOG_WARN << "StFgtPedMaker is not set to compute the time bin needed for StFgtStatusMaker" << endm;
         mPedMkr->mTimeBinMask |= (1<<mTimeBin);
         LOG_WARN << "StFgtPedMaker::mTimeBinMask set to 0x" << std::hex << mPedMkr->mTimeBinMask << std::dec << endm;
      };
   };

   // default to invalid geoId/elecId
   UChar_t defaultValue = (1<<INVALID_ID);   // currently 0x30
   for( UChar_t *p = mStatus; p != &mStatus[ kFgtNumElecIds ]; ++p )
      (*p) = defaultValue;

   mApvData.assign( kFgtNumRdos * kFgtNumArms * kFgtApvsPerArm, apvData_t() );

   return ierr;
};

// here is the real computation of the maker
Int_t StFgtStatusMaker::Finish(){
   Int_t ierr = kStOk;

   if( !mHasFinished ){
      mHasFinished = 1;

      if( !mPedMkr->mHasFinished )
         ierr = mPedMkr->Finish();

      StFgtPedMaker::pedDataVec_t& pedVec = mPedMkr->mDataVec;
      StFgtPedMaker::pedDataVec_t::iterator pedVecIter;
      StFgtDb *fgtTables = 0;
         
      if( !ierr ){
         cout << "StFgtStatusMaker::Finish()" << endl;

         if( !mPedMkr->mDbMkrName.empty() ){
            if( !mPedMkr->mFgtDbMkr ){
               LOG_FATAL << "Pointer to Fgt DB Maker is null" << endm;
               ierr = kStFatal;
            };
            if( !ierr ){
               fgtTables = mPedMkr->mFgtDbMkr->getDbTables();

               if( !fgtTables ){
                  LOG_FATAL << "Pointer to Fgt DB Tables is null" << endm;
                  ierr = kStFatal;
               };
            };
         };
      };

      if( !ierr ){
         Int_t pedIdx = 0;
         for( pedVecIter = pedVec.begin(); pedVecIter != pedVec.end(); ++pedVecIter, ++pedIdx ){
            if( pedVecIter->n ){
               Int_t timebin = pedIdx % kFgtNumTimeBins;

               if( timebin == mTimeBin ){
                  Int_t elecId = pedIdx / kFgtNumTimeBins;
                  UChar_t& status = mStatus[ elecId ];

                  // reset status
                  status = 0;

                  if( pedVecIter->ped < mMinPed || pedVecIter->ped > mMaxPed )
                     status |= (1<<PED_OUT_OF_RANGE);

                  if( pedVecIter->RMS < mMinRMS || pedVecIter->RMS > mMaxRMS )
                     status |= (1<<RMS_OUT_OF_RANGE);

                  if( pedVecIter->fracClose < mMinFrac || pedVecIter->fracClose > mMaxFrac )
                     status |= (1<<FRAC_OUT_OF_RANGE);

                  apvData_t &data = mApvData[ elecId / kFgtNumChannels ];
                  if( !status )
                     --data.numDead;
                  data.stripStatusVec.push_back( &mStatus[ elecId ] );
               };
            };
         };

         // now check for APVs with too many dead strips
         UChar_t deadStatus = (1<<APV_DEAD);
         apvDataVec_t::iterator apvDataIter;
         std::vector< status_t* >::iterator statusPtrIter;
         for( apvDataIter = mApvData.begin(); apvDataIter != mApvData.end(); ++apvDataIter )
            if( apvDataIter->numDead > mMaxDead )
               for( statusPtrIter = apvDataIter->stripStatusVec.begin(); statusPtrIter != apvDataIter->stripStatusVec.end(); ++statusPtrIter )
                  *(*statusPtrIter) |= deadStatus;
      };

      if( !ierr && !mFilename.empty() )
         ierr = saveToFile();
   };

   return ierr;
};

// functions that actually do the saving
Int_t StFgtStatusMaker::saveToFile(){
   Int_t ierr = kStOk;

   if( !mFilename.empty() ){
      std::ofstream fout( mFilename.data(), std::ios_base::out & std::ios_base::trunc );

      if( !fout ){
         LOG_ERROR << "Error opening file '" << mFilename << "'" << endm;
         //cerr << "Error opening file '"  << mFilename << "'" << endl;
         ierr = kStFatal;
      };

      Int_t elecId = 0;
      UChar_t badId = (1<<INVALID_ID);

      // do not output those for which no data was present
      for( UChar_t *statusPtr = mStatus; statusPtr != &mStatus[ kFgtNumElecIds ]; ++statusPtr, ++elecId )
         if( !( *statusPtr & badId ) )
            fout << std::dec << elecId << " 0x" << std::hex << (UShort_t)(*statusPtr) << endl;
   };

   return ierr;
};

ClassImp( StFgtStatusMaker );

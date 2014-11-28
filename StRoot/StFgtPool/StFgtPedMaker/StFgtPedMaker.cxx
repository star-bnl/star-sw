/*!
 * \class StFgtPedMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedMaker.cxx,v 1.2 2012/01/31 16:47:47 wwitzke Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: See header
 *
 ***************************************************************************
 *
 * $Log: StFgtPedMaker.cxx,v $
 * Revision 1.2  2012/01/31 16:47:47  wwitzke
 * Changed for cosmic test stand change.
 *
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.12  2012/01/30 10:42:22  sgliske
 * strip containers now contain adc values for
 * all time bins.  Also fixed bug where setType modified the timebin
 * rather than the type.
 *
 * Revision 1.11  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.10  2012/01/18 18:53:01  sgliske
 * minor bug fix
 *
 * Revision 1.9  2012/01/18 18:07:28  sgliske
 * directly use elec coord domian--no geoIds
 *
 * Revision 1.8  2012/01/17 21:56:26  sgliske
 * Short_t geoId -> Int_t geoId
 *
 * Revision 1.7  2012/01/17 20:08:20  sgliske
 * Many updates
 *
 * Revision 1.6  2011/11/01 18:54:55  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.5  2011/09/30 19:08:45  sgliske
 * general update
 *
 * Revision 1.4  2011/09/27 00:49:00  sgliske
 * cosmic QA update
 *
 * Revision 1.3  2011/09/26 16:55:52  sgliske
 * Continued work on cosmic QA plots
 *
 * Revision 1.2  2011/09/22 21:21:05  sgliske
 * first working version
 *
 * Revision 1.1  2011/09/22 14:10:13  sgliske
 * minor update
 *
 *
 **************************************************************************/

#include <string>
#include "StFgtPedMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtStrip.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtPool/StFgtCosmicTestStandGeom/StFgtCosmicTestStandGeom.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

// constructor
StFgtPedMaker::StFgtPedMaker( const Char_t* name ) : StMaker( name ), mHasFinished(0) {
   // set to all zeros
   mDataVec.resize( kFgtNumTimeBins * kFgtNumElecIds );
};

// initialize
Int_t StFgtPedMaker::Init(){
   Int_t ierr = kStOk;

   if( !mFilename.empty() ){
      // make sure the file can be opened
      std::ofstream fout( mFilename.data() );
      if( !fout ){
         LOG_ERROR << "Error opening file '" << mFilename << "'" << endl;
         ierr = kStFatal;
      };
   };

   // set db pointer, if needed
   if( !ierr && !mDbMkrName.empty() ){
      // get the maker pointer
      mFgtDbMkr = static_cast< StFgtDbMaker* >( GetMaker( mDbMkrName.data() ) );

       if( !ierr && !mFgtDbMkr ){
          LOG_FATAL << "Error finding FgtDbMkr" << endm;
          ierr = kStFatal;
       };

      if( !mFgtDbMkr->InheritsFrom("StFgtDbMaker") ){
         LOG_FATAL << "StFgtDbMkr does not inherit from StFgtDbMaker" << endm;
         LOG_FATAL << "Name is '" << mFgtDbMkr->GetName() << "', class is '" << mFgtDbMkr->ClassName() << endm;
         ierr = kStFatal;
      };
   };

   return ierr;
};

// make, i.e. compute sums.
// actual pedistals are computed in ::Finish()
Int_t StFgtPedMaker::Make(){
   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   if( !ierr ){
      for( UInt_t discIdx=0; discIdx<fgtCollectionPtr->getNumDiscs(); ++discIdx ){
         StFgtStripCollection *stripCollectionPtr = fgtCollectionPtr->getStripCollection( discIdx );
         if( stripCollectionPtr ){
            StSPtrVecFgtStrip& stripVec = stripCollectionPtr->getStripVec();
            StSPtrVecFgtStripIterator stripIter;

            for( stripIter = stripVec.begin(); stripIter != stripVec.end(); ++stripIter ){
               for( Int_t timeBin = 0; timeBin < kFgtNumTimeBins; ++timeBin ){
                  Bool_t pass = (( 1<<timeBin & mTimeBinMask ) && timeBin > 0 && timeBin < kFgtNumTimeBins);

                  if( pass ){
                     Int_t rdo, arm, apv, channel;
                     (*stripIter)->getElecCoords( rdo, arm, apv, channel );
                     Int_t elecId = StFgtGeom::getElectIdFromElecCoord( rdo, arm, apv, channel );
                     Short_t adc = (*stripIter)->getAdc( timeBin );

                     if( adc ){
                        Int_t code = kFgtNumTimeBins * elecId + timeBin;

                        pedData_t &data = mDataVec[ code ];
                        ++data.n;
                        data.sum += adc;
                        data.sumsq += adc*adc;
                     };
                  };
               };
            };
         };
      };
   };

   return ierr;
};

// save as needed
Int_t StFgtPedMaker::Finish(){
   Int_t ierr = kStOk;

   if( !mHasFinished ){
      mHasFinished = 1;
      cout << "StFgtPedMaker::Finish()" << endl;

      // first thing is to finalize the sum;
      pedDataVec_t::iterator dataVecIter;
      for( dataVecIter = mDataVec.begin(); dataVecIter != mDataVec.end(); ++dataVecIter ){
         //Int_t timebin = dataVecIter->first % kFgtNumTimeBins;
         Int_t n = dataVecIter->n;

         if( n ){
            // take average
            dataVecIter->sum /= n;

            // compute st. dev.
            dataVecIter->sumsq /= n;
            dataVecIter->sumsq -= dataVecIter->sum * dataVecIter->sum;
            dataVecIter->sumsq = sqrt( dataVecIter->sumsq );

            // Note: now sumsq is st. dev. and sum is average

            // check for nan
            if( dataVecIter->ped != dataVecIter->ped ){
               dataVecIter->ped = 0;
               dataVecIter->RMS = 10000;
            } else if ( dataVecIter->RMS != dataVecIter->RMS ){
               dataVecIter->RMS = 10000;
            };
         };
      };

      ierr = saveToFile();
   };

   return ierr;
};

// functions that actually do the saving
Int_t StFgtPedMaker::saveToFile(){
   Int_t ierr = kStOk;

   if( !mFilename.empty() ){
      std::ofstream fout( mFilename.data(), std::ios_base::out & std::ios_base::trunc );

      StFgtDb *fgtTables = 0;
      if( !mDbMkrName.empty() ){
         if( !mFgtDbMkr ){
            LOG_FATAL << "Pointer to Fgt DB Maker is null" << endm;
            ierr = kStFatal;
         };
         if( !ierr ){
            fgtTables = mFgtDbMkr->getDbTables();

            if( !fgtTables ){
               LOG_FATAL << "Pointer to Fgt DB Tables is null" << endm;
               ierr = kStFatal;
            };
         };
      };

      if( !fout ){
         LOG_ERROR << "Error opening file '" << mFilename << "'" << endm;
         //cerr << "Error opening file '"  << mFilename << "'" << endl;
         ierr = kStFatal;
      };

      pedDataVec_t::iterator dataVecIter;
      Int_t idx = 0;
      for( dataVecIter = mDataVec.begin(); dataVecIter != mDataVec.end() && !ierr; ++dataVecIter, ++idx ){
         Int_t timebin = idx % kFgtNumTimeBins;
         Int_t elecId = idx / kFgtNumTimeBins;

         if( dataVecIter->n && ( dataVecIter->ped || dataVecIter->RMS ) )
            fout << elecId << ' ' << timebin << ' ' << dataVecIter->ped << ' ' << dataVecIter->RMS << endl;
      };
   };

   return ierr;
};

ClassImp( StFgtPedMaker );

/*!
 * \class StFgtPedReader 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedReader.cxx,v 1.1 2012/01/31 08:52:51 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtPedReader.cxx,v $
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.4  2012/01/27 13:24:50  sgliske
 * updated to correspond with new PedMakers
 * Now keyed by elecId
 *
 * Revision 1.3  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.2  2011/09/24 02:14:09  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.1  2011/09/22 21:21:08  sgliske
 * first working version
 *
 *
 **************************************************************************/

#include <string>
#include <iostream>
#include <fstream>
#include <Rtypes.h>
#include "StFgtPedReader.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

// constructor
inline StFgtPedReader::StFgtPedReader( const Char_t* filename ) :
   mTimeBinMask( 0x10 ), mFilename( filename ) {

   // set to all zeros
   mPedVec.resize( kFgtNumTimeBins * kFgtNumElecIds );
};

// initialize
Int_t StFgtPedReader::Init(){
   Int_t ierr = 0;

   std::ifstream fin( mFilename.data() );

   if( !fin ){
      std::cerr << "Error opening file '" << mFilename << "'" << std::endl;
      ierr = 1;
   };

   if( !ierr ){
      Int_t timebin, geoId;
      Float_t ped, stdev;

      while( !fin.eof() && !ierr ){
         fin >> geoId >> timebin >> ped >> stdev;

         if( 1<<timebin & mTimeBinMask )
            mPedVec[ key_t( geoId, timebin ) ] = ped_t( ped, stdev );
      };
   };

   return ierr;
};

// accessor: input is geoId and timebin, output is ped and
// st. dev. (err)
void StFgtPedReader::getPed( Int_t geoId, Int_t timebin, Float_t& ped, Float_t& err ) const {
   const ped_t& pedpair = mPedVec[ key_t( geoId, timebin ) ];
   ped = pedpair.ped;
   err = pedpair.err;
};

ClassImp( StFgtPedReader );

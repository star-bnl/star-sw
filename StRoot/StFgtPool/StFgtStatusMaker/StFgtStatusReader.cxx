/***************************************************************************
 *
 * $Id: StFgtStatusReader.cxx,v 1.1 2012/01/31 08:59:43 sgliske Exp $
 * Author: C. K. Riley, Nov 2011
 *
 ***************************************************************************
 *
 * Description: see header.
 *
 ***************************************************************************
 *
 * $Log: StFgtStatusReader.cxx,v $
 * Revision 1.1  2012/01/31 08:59:43  sgliske
 * moved StFgtStatus maker to StFgtPool
 *
 * Revision 1.3  2012/01/27 13:38:08  sgliske
 * updated to correspond with new StatusMakers,
 * Now keyed by elecId
 *
 * Revision 1.2  2011/12/01 00:41:34  ckriley
 * make compatible with db stuff
 *
 * Revision 1.1  2011/11/25 20:22:37  ckriley
 * creation of statusmaker
 *
 *
 *
 **************************************************************************/

#include <string>
#include <iostream>
#include <fstream>
#include <Rtypes.h>
#include "StFgtStatusReader.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

// constructor
inline StFgtStatusReader::StFgtStatusReader( const Char_t* filename ) :
   mFilename( filename ) {

   // set to all 0xFF -- i.e. fail
   mStatusVec.resize( kFgtNumTimeBins * kFgtNumElecIds );
};

// initialize
Int_t StFgtStatusReader::Init(){
   Int_t ierr = 0;
   mStatusVec.assign( kFgtNumTimeBins * kFgtNumElecIds, 0xFF );

   std::ifstream fin( mFilename.data() );

   if( !fin ){
      std::cerr << "Error opening file '" << mFilename << "'" << std::endl;
      ierr = 1;
   };

   if( !ierr ){
      Int_t elecId, status;

      while( !fin.eof() && !ierr ){
         fin >> elecId >> status;
         mStatusVec[ elecId ] = status;
      };
   };

   return ierr;
};

ClassImp( StFgtStatusReader );

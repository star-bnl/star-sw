/*!
 * \class StFgtPedReader 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedReader.h,v 1.1 2012/01/31 08:52:51 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Reads the pedistals from file, and provides querying
 * of pedistals in a similar manner as the DB.  Note: this is not a Maker.
 *
 ***************************************************************************
 *
 * $Log: StFgtPedReader.h,v $
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
 * Revision 1.1  2011/09/22 21:21:10  sgliske
 * first working version
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_PED_READER_
#define _ST_FGT_PED_READER_

#include <string>
#include <vector>
#include <Rtypes.h>

#include "StRoot/StFgtUtil/StFgtConsts.h"

class StFgtPedReader {
 public:
   // constructors
   StFgtPedReader( const Char_t* filename = "" );

   // default OK
   // StFgtPedReader(const StFgtPedReader&);

   // equals operator -- default OK
   // StFgtPedReader& operator=(const StFgtPedReader&);

   // deconstructor
   virtual ~StFgtPedReader();

   // initialize
   Int_t Init();

   // accessor: input is elecId and timebin, output is ped and
   // st. dev. (err).
   void getPed( Int_t elecId, Int_t timebin, Float_t& ped, Float_t& err ) const;

   // setting time bin mask will reduce the number of peds read from
   // the file, and increase the access time for each individual
   // pedistal.
   void setTimeBinMask( Short_t mask = 0xFF );

 protected:
   // mask for which time bins to read
   Short_t mTimeBinMask;

   // input file
   std::string mFilename;

   // key structure
   struct key_t {
      Int_t code;

      key_t( Int_t elecId = 0, Int_t timeBin = 0);
      //Bool_t operator<( const key_t& rhs ) const;
      operator int() const;
   };

   // ped structure
   struct ped_t {
      Float_t ped;
      Float_t err;

      ped_t( Float_t p=0, Float_t err=1e10 );
      operator float() const;
      Float_t getPed();
      Float_t getErr();
   };

   typedef std::vector< ped_t > PedVec_t;
   PedVec_t mPedVec;

 private:   
   ClassDef(StFgtPedReader,1);

}; 

// inline functions

// deconstructor
inline StFgtPedReader::~StFgtPedReader(){ /* */ };

// modifier
inline void StFgtPedReader::setTimeBinMask( Short_t mask ){ mTimeBinMask = mask; };

// key_t functions
inline StFgtPedReader::key_t::key_t( Int_t elecId, Int_t timeBin ){
   code = elecId*kFgtNumTimeBins + timeBin;
};

inline StFgtPedReader::key_t::operator int() const {
   return code;
};

// ped_t functions
inline StFgtPedReader::ped_t::ped_t( Float_t p, Float_t e ) : ped(p), err(e) { /* */ };


#endif

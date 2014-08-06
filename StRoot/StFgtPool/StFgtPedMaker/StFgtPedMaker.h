/*!
 * \class StFgtPedMaker 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedMaker.h,v 1.2 2014/08/06 11:43:11 jeromel Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Computes the pedistals, with the option of writing
 * them to disc or (eventually) to the DB.
 *
 ***************************************************************************
 *
 * $Log: StFgtPedMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:11  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2012/01/31 08:52:51  sgliske
 * StFgtPedMaker moved to StFgtPool
 *
 * Revision 1.6  2012/01/26 13:13:11  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.5  2012/01/25 11:25:02  sgliske
 * Added GetCVS tag
 *
 * Revision 1.4  2012/01/17 20:08:20  sgliske
 * Many updates
 *
 * Revision 1.3  2011/11/01 18:54:55  sgliske
 * Updated to correspond with StEvent containers, take 2.
 *
 * Revision 1.2  2011/09/22 21:21:06  sgliske
 * first working version
 *
 * Revision 1.1  2011/09/22 14:10:13  sgliske
 * minor update
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_PED_MAKER_
#define _ST_FGT_PED_MAKER_

#include <string>
#include "StMaker.h"
class StFgtDbMaker;

class StFgtPedMaker : public StMaker {
 public:
   // constructors
   StFgtPedMaker( const Char_t* name = "FgtPedMaker" );

   // default OK
   // StFgtPedMaker(const StFgtPedMaker&);

   // equals operator -- default OK
   // StFgtPedMaker& operator=(const StFgtPedMaker&);

   // deconstructor
   virtual ~StFgtPedMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // modifiers
   void setToSaveToFile( const Char_t* filename );  // filename == "" forces not to save to file
   void setTimeBinMask( Short_t mask = 0xFF );
   void setFgtDbMkrName( std::string& name );
   void setIsCosmic();

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   // mask for which time bins to save
   Short_t mTimeBinMask;

   // to hold sums
   struct pedData_t {
      Int_t n;
      union {    // i.e. two names for the same piece of memory
         Float_t sum;
         Float_t ped;
      };
      union {
         Float_t sumsq;
         Float_t RMS;
      };
      Float_t fracClose;

      pedData_t( Int_t nIn=0, Float_t s=0, Float_t ssq=0, Float_t f=0 ) : n(nIn), sum(s), sumsq(ssq), fracClose(f) { /* */ };
   };
   typedef std::vector< pedData_t > pedDataVec_t;

   pedDataVec_t mDataVec;

   // for saving to file
   std::string mFilename;

   // functions that actually do the saving
   Int_t saveToFile();

   // and for accessing the DB
   // if name is "", then use naive cosmic setup
   std::string  mDbMkrName;
   StFgtDbMaker *mFgtDbMkr;

   // has finished
   Bool_t mHasFinished;

 private: 
   friend class StFgtStatusMaker;
   friend class StFgtPedStatQA;
  
   ClassDef(StFgtPedMaker,1);

}; 

// inline functions

// deconstructor
inline StFgtPedMaker::~StFgtPedMaker(){ /* */ };

// modifiers
inline void StFgtPedMaker::setToSaveToFile( const Char_t* filename ){ mFilename = filename; };
inline void StFgtPedMaker::setTimeBinMask( Short_t mask ){ mTimeBinMask = mask; };
inline void StFgtPedMaker::setFgtDbMkrName( std::string& name ){ mDbMkrName = name; };
inline void StFgtPedMaker::setIsCosmic(){ mDbMkrName = ""; };
inline const char *StFgtPedMaker::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFgtPedMaker.h,v 1.2 2014/08/06 11:43:11 jeromel Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
}

#endif

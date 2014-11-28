/***************************************************************************
 *
 * $Id: StFgtStatusMaker.h,v 1.2 2014/08/06 11:43:13 jeromel Exp $
 * Author: C. K. Riley, Nov 2011 & S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: computes the strip status.
 *
 ***************************************************************************
 *
 * $Log: StFgtStatusMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:13  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2012/01/31 08:59:43  sgliske
 * moved StFgtStatus maker to StFgtPool
 *
 * Revision 1.6  2012/01/28 11:34:34  sgliske
 * changed the name of the not connected bit to masked out
 *
 * Revision 1.5  2012/01/26 13:13:12  sgliske
 * Updated to use StFgtConsts, which
 * replaces StFgtEnums and StFgtGeomDefs
 *
 * Revision 1.4  2012/01/25 11:25:10  sgliske
 * Added GetCVS tag
 *
 * Revision 1.3  2012/01/18 18:07:35  sgliske
 * directly use elec coord domian--no geoIds
 *
 * Revision 1.2  2012/01/17 20:13:12  sgliske
 * Completely new system, based on pedestal and RMS values.
 *
 * Revision 1.1  2011/11/25 20:22:37  ckriley
 * creation of statusmaker
 *
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_STATUS_MAKER_
#define _ST_FGT_STATUS_MAKER_

#include <string>
#include "StMaker.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"
class StFgtPedMaker;

class StFgtStatusMaker : public StMaker {
 public:
   // enum of status bits
   enum { PED_OUT_OF_RANGE = 0, RMS_OUT_OF_RANGE = 1, FRAC_OUT_OF_RANGE = 2, APV_DEAD = 4, 
          MASKED_OUT = 5, INVALID_ID = 6 };

   // constructors
   StFgtStatusMaker( const Char_t* name = "FgtStatusMaker", const Char_t* pedMakerName = "FgtPedMaker" );

   // default OK
   // StFgtStatusMaker(const StFgtStatusMaker&);

   // equals operator -- default OK
   // StFgtStatusMaker& operator=(const StFgtStatusMaker&);

   // deconstructor
   virtual ~StFgtStatusMaker();

   Int_t Init();
   Int_t Finish();

   // Int_t Make();  No make--just uses output from StFgtPedMaker


   // modifiers
   void setToSaveToFile( const Char_t* filename );  // filename == "" forces not to save to file
   void setTimeBin( Short_t timeBin = 2 );          // only uses the pedestals from one time bin
   void setPedRange( Float_t min, Float_t max );
   void setRmsRange( Float_t min, Float_t max );
   void setFracRange( Float_t min, Float_t max );
   void setMaxDeadPerApv( Float_t max );

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   // for the ped maker
   std::string mPedMkrName;
   StFgtPedMaker *mPedMkr;

   // which time bins to use
   Short_t mTimeBin;

   // for saving to file
   std::string mFilename;

   // parameters
   Float_t mMinPed, mMaxPed, mMinRMS, mMaxRMS, mMinFrac, mMaxFrac;
   Int_t mMaxDead;

   // function that actually does the saving
   Int_t saveToFile();

   // internal data
   typedef UChar_t status_t;
   struct apvData_t {
      Int_t numDead;
      std::vector< status_t* > stripStatusVec;

      apvData_t() : numDead( kFgtNumChannels ) { /* */ };   // Default to 128, i.e. all dead
   };
   typedef std::vector< apvData_t > apvDataVec_t;

   status_t* mStatus;

   // to count number of dead strips per APV
   apvDataVec_t mApvData;

   // if has finished
   Bool_t mHasFinished;

 private:   
   friend class StFgtPedStatQA;

   ClassDef(StFgtStatusMaker,1);
}; 

// inline functions

// modifiers
inline void StFgtStatusMaker::setToSaveToFile( const Char_t* filename ){ mFilename = filename; };
inline void StFgtStatusMaker::setTimeBin( Short_t bin ){ mTimeBin = bin; };

inline void StFgtStatusMaker::setPedRange( Float_t min, Float_t max ){  mMinPed = min; mMaxPed = max; };
inline void StFgtStatusMaker::setRmsRange( Float_t min, Float_t max ){  mMinRMS = min; mMaxRMS = max; };
inline void StFgtStatusMaker::setFracRange( Float_t min, Float_t max ){ mMinFrac = min; mMaxFrac = max; };
inline void StFgtStatusMaker::setMaxDeadPerApv( Float_t max ){ mMaxDead = max; };
inline const char *StFgtStatusMaker::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StFgtStatusMaker.h,v 1.2 2014/08/06 11:43:13 jeromel Exp $ built " __DATE__ " " __TIME__ ;
   return cvs;
}

#endif

/***************************************************************************
 *
 * $Id: StFgtDaq2RootMaker.h,v 1.2 2012/03/05 20:35:46 sgliske Exp $
 * Author: S. Gliske, Jan 2011
 *
 ***************************************************************************
 *
 * Description: Converts a sfs or daq file to a root file.  All data
 * is in a single branch, which is an array of adc values.  The order
 * of the adcs are: disc=quad, apv, channel, timebin, i.e. the first 7
 * events are for quad 0, apv 0, channel 0, then next seven for quad 0,
 * apv 0, channel 1, etc.
 *
 * Designed only for cosmic data!
 *
 ***************************************************************************
 *
 * $Log: StFgtDaq2RootMaker.h,v $
 * Revision 1.2  2012/03/05 20:35:46  sgliske
 * update to export DAQ data as well
 *
 * Revision 1.1  2012/01/28 09:29:26  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_DAQ_2_ROOT_MAKER_
#define _ST_FGT_DAQ_2_ROOT_MAKER_

#include "StMaker.h"
class TFile;
class TTree;

class StFgtDaq2RootMaker : public StMaker {
 public:
   // constructors
   StFgtDaq2RootMaker( const Char_t* name = "fgtDaq2Root",
                       const Char_t* outputfile = "testout.txt" );
   virtual ~StFgtDaq2RootMaker();

   // default OK
   // StFgtDaq2RootMaker(const StFgtDaq2RootMaker&);
   // StFgtDaq2RootMaker& operator=(const StFgtDaq2RootMaker&);

   void Clear(const Option_t* opts = "");
   Int_t Init();
   Int_t Make();
   Int_t Finish();

   enum { kNumTimeBins = 7, kNumChans = 128, kNumApv = 10, kNumQuad = 3, kNumData = kNumTimeBins*kNumChans*kNumApv*kNumQuad };

   struct chan_t {
      Int_t tb[kNumTimeBins];
   };

   struct apv_t {
      chan_t chan[kNumChans];
   };

   struct quad_t {
      apv_t apv[kNumApv];
   };

   struct cosmicFgt_t {
      quad_t quad[kNumQuad];
   };

 protected:
   // for output
   std::string mFileName;
   TFile *mTFile;
   TTree *mTTree;

   cosmicFgt_t mData;

 private:   
   ClassDef(StFgtDaq2RootMaker,1);
}; 

#endif

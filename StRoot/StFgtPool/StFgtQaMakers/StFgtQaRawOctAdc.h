/***************************************************************************
 *
 * $Id: StFgtQaRawOctAdc.h,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Plot ADC vs channel for a given per octant.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaRawOctAdc.h,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2012/01/24 11:55:49  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_RAQ_OCT_ADC_H_
#define _ST_FGT_QA_RAQ_OCT_ADC_H_

#include <string>
#include <sstream>

#include "StMaker.h"
class StFgtCollection;
class TH2F;

class StFgtQaRawOctAdc : public StMaker {
 public:
   // constructors
   StFgtQaRawOctAdc( const Char_t* name = "fgtQaRawOctAdc", Int_t rdo = 1, Int_t arm = 0, Int_t apvStart = 0,
                     Int_t timeBin = 2 );

   // default OK
   // StFgtQaRawOctAdc(const StFgtQaRawOctAdc&);

   // equals operator -- default OK
   // StFgtQaRawOctAdc& operator=(const StFgtQaRawOctAdc&);

   // deconstructor
   virtual ~StFgtQaRawOctAdc();

   Int_t Init();
   Int_t Make();

   // accessor
   TH2F* getHist();

 protected:
   // the histogram
   TH2F *mHist;

   // For selecting the octant
   Int_t mRdo, mArm, mApvStart, mTimeBin;

 private:   
   ClassDef(StFgtQaRawOctAdc,1);

}; 

// inline functions

// modifiers
inline TH2F* StFgtQaRawOctAdc::getHist(){ return mHist; };

#endif

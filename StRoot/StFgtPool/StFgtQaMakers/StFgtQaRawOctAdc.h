/***************************************************************************
 *
 * $Id: StFgtQaRawOctAdc.h,v 1.3 2012/03/05 20:35:15 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Plot ADC vs channel for a given per octant.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaRawOctAdc.h,v $
 * Revision 1.3  2012/03/05 20:35:15  sgliske
 * update--still not really working
 *
 * Revision 1.2  2012/01/31 12:53:28  sgliske
 * updates
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2012/01/24 11:55:49  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_RAW_OCT_ADC_H_
#define _ST_FGT_QA_RAW_OCT_ADC_H_

#include <string>
#include <sstream>

#include "StMaker.h"
class StFgtCollection;
class TH2F;

class StFgtQaRawOctAdc : public StMaker {
 public:
   // constructors
   StFgtQaRawOctAdc( const Char_t* name = "fgtQaRawOctAdc", Int_t timeBin = 2 );

   // default OK
   // StFgtQaRawOctAdc(const StFgtQaRawOctAdc&);

   // equals operator -- default OK
   // StFgtQaRawOctAdc& operator=(const StFgtQaRawOctAdc&);

   // deconstructor
   virtual ~StFgtQaRawOctAdc();

   Int_t Init();
   Int_t Make();

   // accessor
   std::vector< TH2F* > getHistVec();
   TH2F* getHist( Int_t idx );

   // modifier
   void setTimeBin( Int_t tb );

 protected:
   // typedef
   typedef std::vector< TH2F* > HistVec_t;

   // the histogram
   HistVec_t mHistVec;

   // For selecting the timebin
   Int_t mTimeBin, mAdcBins;
   Float_t mAdcMin, mAdcMax;

 private:   
   ClassDef(StFgtQaRawOctAdc,1);

}; 

// inline functions

inline std::vector< TH2F* > StFgtQaRawOctAdc::getHistVec(){ return mHistVec; };
inline TH2F* StFgtQaRawOctAdc::getHist( Int_t idx ){ return mHistVec.at(idx); };
inline void StFgtQaRawOctAdc::setTimeBin( Int_t tb ){ mTimeBin = tb; };

#endif

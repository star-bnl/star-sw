/***************************************************************************
 *
 * $Id: StFgtQaHighStrips.h,v 1.2 2012/01/31 12:53:28 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Plots number a histogram for each octant of
 * distribution of the number of strips per event above thresholds.
 * It assumes that StFgtA2CMaker has already removed all strips not
 * above threshold, so it just counts the remaining strips.
 *
 ***************************************************************************
 *
 * $Log: StFgtQaHighStrips.h,v $
 * Revision 1.2  2012/01/31 12:53:28  sgliske
 * updates
 *
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.1  2012/01/30 17:18:10  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_QA_HIGH_STRIPS_H_
#define _ST_FGT_QA_HIGH_STRIPS_H_

#include "StMaker.h"
class StFgtCollection;
class TH2F;
class TH1F;
//class StFgtDbMaker;

class StFgtQaHighStrips : public StMaker {
 public:
   // constructors
   StFgtQaHighStrips( const Char_t* name = "fgtQaHighStrips" );

   // default OK
   // StFgtQaHighStrips(const StFgtQaHighStrips&);

   // equals operator -- default OK
   // StFgtQaHighStrips& operator=(const StFgtQaHighStrips&);

   // deconstructor
   virtual ~StFgtQaHighStrips();

   Int_t Init();
   Int_t Make();

   // accessor
   TH2F* getHist2D();
   std::vector< TH1F* > getHistVec();

 protected:
   // typedef
   typedef std::vector< TH1F* > HistVec_t;

   // the histograms
   TH2F *mHist2D;
   HistVec_t mHistVec;

/*    // for the DB */
/*    std::string  mDbMkrName; */
/*    StFgtDbMaker *mFgtDbMkr; */

   // ranges
   Int_t mMaxNum;

 private:   
   ClassDef(StFgtQaHighStrips,1);

}; 

// inline functions

// modifiers
inline TH2F* StFgtQaHighStrips::getHist2D(){ return mHist2D; };
inline std::vector< TH1F* > StFgtQaHighStrips::getHistVec(){ return mHistVec; };

#endif

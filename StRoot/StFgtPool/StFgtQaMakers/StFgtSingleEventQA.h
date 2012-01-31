/***************************************************************************
 *
 * $Id: StFgtSingleEventQA.h,v 1.1 2012/01/31 09:26:17 sgliske Exp $
 * Author: S. Gliske, Jan 2012
 *
 ***************************************************************************
 *
 * Description: Make plots of single events.  
 *
 ***************************************************************************
 *
 * $Log: StFgtSingleEventQA.h,v $
 * Revision 1.1  2012/01/31 09:26:17  sgliske
 * StFgtQaMakers moved to StFgtPool
 *
 * Revision 1.2  2012/01/24 05:45:31  sgliske
 * debugged--mostly :)
 *
 * Revision 1.1  2012/01/24 03:32:17  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_SINGLE_EVENT_QA_H_
#define _ST_FGT_SINGLE_EVENT_QA_H_

#include <string>
#include <sstream>

#include "StMaker.h"
class StFgtCollection;
class TH2F;

class StFgtSingleEventQA : public StMaker {
 public:
   // constructors
   StFgtSingleEventQA( const Char_t* name = "FgtSingleEvent" );

   // default OK
   // StFgtSingleEventQA(const StFgtSingleEventQA&);

   // equals operator -- default OK
   // StFgtSingleEventQA& operator=(const StFgtSingleEventQA&);

   // deconstructor
   virtual ~StFgtSingleEventQA();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   // modifiers
   void setFilename( const Char_t* filename );

 protected:
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;

   // for saving to file
   std::string mFilename;
   TFile *mTFile;

   // to make the histograms
   void makeHists( Int_t ev, Int_t tb, Int_t disc, Int_t quad, Bool_t isShort, TH2F* &hP, TH2F* &hR );

   // to get track of the event
   Int_t mEventNum;

 private:   
   ClassDef(StFgtSingleEventQA,1);

}; 

// inline functions

// modifiers
inline void StFgtSingleEventQA::setFilename( const Char_t* filename ){ mFilename = filename; };

#endif

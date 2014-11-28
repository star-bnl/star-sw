/*!
 * \class StFgtJanGainMaker
 * \author Jan Balewski , February 2012
 *
 ***************************************************************************
 *
 * Description: detects & counts bad APVs in FGT events
 *
 ***************************************************************************/

#ifndef StFgtJanGainMaker_HH
#define StFgtJanGainMaker_HH

#include <string>
#include <TF1.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TTree.h>
#include <TCanvas.h>
#include "StMaker.h"

class StFgtDbMaker;
class StFgtDb;
class StFgtCollection;

//========================================================

class StFgtJanGainMaker : public StMaker {
 public:
   // constructors
  StFgtJanGainMaker( const Char_t* dbMkrName = "" ,const Char_t* name = "FgtJanGain"  );


   // deconstructor
  virtual ~StFgtJanGainMaker(){};

   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void setHList(TObjArray * x){HList=x;}

   
 protected:
   // since this isn't saved anywhere else
   static const Int_t mMaxNumTimeBins;
    void initHistos();

 private: 
   // friend class StFgtStatusMaker; 

   std::string mDbMkrName;
   StFgtDbMaker* mFgtDbMkr;;
   StFgtCollection* mFgtCollectionPtr;
   
   Int_t iEvt; 	

   // histograms
   TObjArray *HList;
   enum {mxHA=32}; TH1 * hA[mxHA];
 
   ClassDef(StFgtJanGainMaker,1);

}; 


#endif
/**************************************************************************
 *
 * $Log: StFgtJanGainMaker.h,v $
 * Revision 1.1  2012/02/07 08:25:29  balewski
 * *** empty log message ***
 *
 * Revision 1.1  2012/02/04 22:03:41  balewski
 * start
 *
 *
 **************************************************************************/

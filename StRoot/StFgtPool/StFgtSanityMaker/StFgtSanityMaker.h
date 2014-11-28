/*!
 * \class StFgtSanityMaker
 * \author Jan Balewski , February 2012
 *
 ***************************************************************************
 *
 * Description: detects & counts bad APVs in FGT events
 *
 ***************************************************************************/

#ifndef StFgtSanityMaker_HH
#define StFgtSanityMaker_HH

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

class StFgtSanityMaker : public StMaker {
 public:
   // constructors
  StFgtSanityMaker( const Char_t* name = "FgtSanity" , const Char_t* dbMkrName = ""  );


   // deconstructor
  virtual ~StFgtSanityMaker(){};

   Int_t Init();
   Int_t Make();
   Int_t Finish();


#if 0   
   TString fname;
   Int_t fitThresh;
   Int_t plotThresh;
   Bool_t fixTau;
   Int_t Ntimebin;
#endif  
   
 protected:
   // since this isn't saved anywhere else
   static const Int_t mMaxNumTimeBins;


 private: 
   // friend class StFgtStatusMaker; 

   std::string mDbMkrName;
   StFgtDbMaker* mFgtDbMkr;;
   StFgtCollection* mFgtCollectionPtr;
   
   Int_t iEvt; 	
   TH1F *hh;

   ClassDef(StFgtSanityMaker,1);

}; 


#endif
/**************************************************************************
 *
 * $Log: StFgtSanityMaker.h,v $
 * Revision 1.1  2012/02/04 22:03:41  balewski
 * start
 *
 *
 **************************************************************************/

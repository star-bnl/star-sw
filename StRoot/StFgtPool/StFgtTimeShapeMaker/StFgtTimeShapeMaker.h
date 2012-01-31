/*!
 * \class StFgtTimeShapeMaker 
 * \author Len K. Eun, Jan 2012
 */

/***************************************************************************
 *
 * Author: Len K. Eun, Jan 2012
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_TIMESHAPE_MAKER_
#define _ST_FGT_TIMESHAPE_MAKER_

#include <string>
#include <TF1.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TTree.h>
#include <TCanvas.h>
#include "StMaker.h"

class StFgtDbMaker;

class StFgtTimeShapeMaker : public StMaker {
 public:
   // constructors
   StFgtTimeShapeMaker( const Char_t* name = "FgtTimeShapeMaker" );

   // default OK
   // StFgtTimeShapeMaker(const StFgtTimeShapeMaker&);

   // equals operator -- default OK
   // StFgtTimeShapeMaker& operator=(const StFgtTimeShapeMaker&);

   // deconstructor
   virtual ~StFgtTimeShapeMaker();

   Int_t Init();
   Int_t Make();
   Int_t Finish();

   Int_t InitTree();
   void FitFunc();
   void InitFX();
   void InitFX(Float_t tau);

   TString fname;
   Int_t fitThresh;
   Int_t plotThresh;
   Bool_t fixTau;
   Int_t Ntimebin;
   
 protected:
   // since this isn't saved anywhere else
   static const Int_t mMaxNumTimeBins;
   struct MyFunc;

 private: 
   friend class StFgtStatusMaker; 

   TH1F* hh;
   
   TFile* fFgt;
   TTree* tFgt;
  	
   Int_t iEvt;
   Int_t rdo;
   Int_t arm;
   Int_t apv;
   Int_t chn;
   Int_t adc[7];
   Int_t ped;
   Int_t adcmax;
   Int_t mmax;
   Int_t mmin;
   Float_t chi2;
   Float_t fmax;
   Float_t tau;
   Float_t t0;
   Float_t offset;
   Int_t errCode;

   TF1* FX;
   TF1* fs;

   TH1F* hGood[120];
   TH1F* hBad[120];
   TF1* fGood[120];
   TF1* fBad[120];
   Int_t igoodCnt;
   Int_t ibadCnt;

   ClassDef(StFgtTimeShapeMaker,1);

}; 


#endif

#ifndef STAR_StEmcPedestalMaker
#define STAR_StEmcPedestalMaker

#include <TString.h>
#include <TF1.h>
#include <TH2.h>
#include <TH1.h>

#include "StEmcCalibMaker.h"
#include "StEmcCalibrationMaker.h"

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000
#define MAXTOWERCHANNEL 18000

class StEmcPedestalMaker : public StEmcCalibMaker {
protected: 		
    TH1F*                       mPedestal;
    TH1F*                       mRms;
    TH1F*                       mChi;
    TH1F*                       mStatus;
    
    Int_t                       mNPedEvents;
    Float_t                     mPedInterval;
    Int_t                       mLastPedTime;
    Int_t                       mLastPedDate;
    Bool_t                      mStarted;
    TString                     mSavePath;
    TString                     mTablesPath;
    Bool_t			mSaveTables;
                            
public:
     
                                StEmcPedestalMaker(const Char_t *name="EmcPedestal");
   virtual                      ~StEmcPedestalMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              Clear(Option_t *option=""); 

              Float_t           getPedestal(Int_t id) const {return mPedestal->GetBinContent(id);}
              Float_t           getRms(Int_t id) const  {return mRms->GetBinContent(id);}
              Float_t           getChi(Int_t id) const {return mChi->GetBinContent(id);}
              Float_t           getStatus(Int_t id) const {return mStatus->GetBinContent(id);}

              Int_t             getNPedEvents() const {return mNPedEvents;}
              void              setNPedEvents(Int_t a) {mNPedEvents = a;}

              Float_t           getPedInterval() const {return mPedInterval;}
              void              setPedInterval(Float_t time) {mPedInterval = time;}

	      const Char_t     *getSavePath() const {return mSavePath;}
	      void              setSavePath(const Char_t *path) {mSavePath = path ? path : "";}
	    
	      const Char_t     *getTablesPath() const {return mTablesPath;}
	      void              setTablesPath(const Char_t *path) {mTablesPath = path ? path : "";}
	    
	      Bool_t 		getSaveTables() const {return mSaveTables;}
	      void 		setSaveTables(Bool_t save) {mSaveTables = save;}
	    
              void              calcPedestals();
              void              saveToDb(const Char_t *timestamp, const Char_t *tableFilename = 0) const;
              void              saveToDb(Int_t date, Int_t time) const;
	      void              savePedestals(Int_t date, Int_t time, Bool_t DB = false) const;
	      void              loadPedestals(const Char_t *filename); 
                            
   ClassDef(StEmcPedestalMaker, 2)  
};

#endif

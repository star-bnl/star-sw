#ifndef STAR_StEmcPedestalMaker
#define STAR_StEmcPedestalMaker
#include "StMaker.h"
#include "TH2.h"
#include "TH1.h"
#include "StEmcCalibrationMaker.h"
#include "TString.h"
#include "TF1.h"
#include "StEmcCalibMaker.h"

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000
#define MAXTOWERCHANNEL 18000

class StEmcPedestalMaker : public StEmcCalibMaker 
{
  protected: 		
    TH1F*                       mPedestal;
    TH1F*                       mRms;
    TH1F*                       mChi;
    TH1F*                       mStatus;
    
    int                         mNPedEvents;
    float                       mPedInterval;
		int                         mLastPedTime;
		int                         mLastPedDate;
		bool                        mStarted;
                            
  public:
     
                                StEmcPedestalMaker(const char *name="EmcPedestal");
   virtual                      ~StEmcPedestalMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              Clear(Option_t *option=""); 
	    
              void              calcPedestals();
              void              saveToDb(char*); 
              void              saveToDb(int,int);
							void              savePedestals(int,int,bool = false); 
              void              setNPedEvents(int a) { mNPedEvents = a;}
              void              setPedInterval(float time) { mPedInterval = time;}
              float             getPedestal(int id) {return mPedestal->GetBinContent(id);}
              float             getRms(int id) {return mRms->GetBinContent(id);}
              float             getChi(int id) {return mChi->GetBinContent(id);}
              float             getStatus(int id) {return mStatus->GetBinContent(id);}
              int               getNPedEvents() { return mNPedEvents;}
                            
   ClassDef(StEmcPedestalMaker, 1)  
};

#endif

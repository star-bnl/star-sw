#ifndef STAR_StEmcEqualMaker
#define STAR_StEmcEqualMaker
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

class StEmcEqualMaker : public StEmcCalibMaker 
{
  protected: 		
      TH1F                      *mA;
      TH1F                      *mB;
      TH1F                      *mStatus;
      TH1F                      *mRefSlopes;
      TH1F                      *mRefAmps;
      TH1F                      *mSlopes;
      TH1F                      *mAmps;
      TH1F                      *mADistr;
      TH1F                      *mBDistr;
      TH2F                      *mSlopesTheta;
				                                            
  public:
     
                                StEmcEqualMaker(const char *name="EmcEqual");
   virtual                      ~StEmcEqualMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              Clear(Option_t *option=""); 
	 
	            void              equalize(int=4,int = 2,bool = false); 
	            void              equalizeRelative(int,int,int=4,bool=false); 
	            void              equalizeToFunction(int,TF1*); 
						  
							void              calcSlopes();
							
							void              saveEqual(int=0,int=0);
							void              loadEqual(char*);
							
							TH1F*             getA() { return mA;}
							TH1F*             getB() { return mB;}
							TH1F*             getStatus() { return mStatus;}
							TH1F*             getRefSlopes() { return mRefSlopes;}
							TH1F*             getRefAmps() { return mRefAmps;}
							TH1F*             getSlopes() { return mSlopes;}
							TH1F*             getAmps() { return mAmps;}
 							TH2F*             getSlopesTheta() { return mSlopesTheta;}
							
							TH1F*             getEtaBinSpec(int,int,TH2F* SPEC=0);
			        TH1F*             rebin(int,const char *name = "tmp",TH2F* SPEC = NULL);
							
							void              drawEtaBin(int, int,TH2F* SPEC=0);
                           
   ClassDef(StEmcEqualMaker, 1)  
};

#endif

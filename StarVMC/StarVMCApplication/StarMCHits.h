#ifndef StarMCHits_h
#define StarMCHits_h
#include "StarVMCDetector.h"
#include "St_g2t_Chair.h"
#include "StarHitVector.h"
#include "StarVMCDetectorSet.h"
class StarMCHits : public TDataSet {
 public:
  virtual ~StarMCHits() {}
  static StarMCHits       *instance(const Char_t *name="StarMCHits",		      		       
		    	            const Char_t *title="") {
    if (fgInstance) return fgInstance;
    return new StarMCHits(name, title); 
  }
  virtual Int_t     	   Init();					      		       
  virtual void      	   Clear(const Option_t* opt = "");
  virtual void      	   Step();					      		       
  virtual void      	   SetHitHolder(TDataSet  *m) {fHitHolder = m;}	      	       
  virtual void             SetDebug(Int_t m=0);
  virtual TDataSet  	  *GetHitHolder() {return fHitHolder;}                  				       
  virtual StarVMCDetector *GetCurrentDetector() {return fCurrentDetector;}
  virtual void      	   FillG2Table();							       	    
  virtual void             FinishEvent();
  virtual Int_t            Debug()        { return fDebug;}
 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  static StarMCHits *fgInstance;
  GHit_t             fHit;
  TDataSet          *fHitHolder;
  StarVMCDetector   *fCurrentDetector;
  Int_t              fDebug;
  UInt_t             fSeed;
  Int_t              fEventNumber;
  ClassDef(StarMCHits,1)
};
#endif

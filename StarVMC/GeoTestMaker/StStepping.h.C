#ifndef StarMCHits_h
#define StarMCHits_h
#include "StarVMCDetector.h"
#include "St_g2t_Chair.h"
#include "StarHitVector.h"

class StarMCHits : public TDataSet {
 public:
  virtual ~StarMCHits() {SafeDelete(fDetList); SafeDelete(fVolUserInfo);}
  static StarMCHits       *instance(const Char_t *name="",		      		       
		    	            const Char_t *title="") {
    if (fgInstance) return fgInstance;
    return new StarMCHits(name, title); 
  }
  virtual StarMCHits  	  *Instance() const {return fgInstance;}   		       
  virtual Int_t     	   Init();					      		       
  virtual void      	   Clear(const Option_t* opt = "");
  virtual void      	   Step();					      		       
#if 0
  virtual Float_t   	   GetHitK(Int_t k);				      		       
#endif
  virtual void      	   SetHitHolder(TDataSet  *m) {fHitHolder = m;}	      	       
  virtual void             SetDebug(Int_t m=0);
  virtual TDataSet  	  *GetDetectors() {return fDetectors;}		      	       
  virtual TObjArray 	  *GetVolUserInfo() {return fVolUserInfo;}	      		       
  virtual TDataSet  	  *GetHitHolder() {return fHitHolder;}                  				       
  virtual StarVMCDetector *GetCurrentDetector() {return fCurrentDetector;}
  virtual void      	   FillG2Table();							       	    
  virtual void             FinishEvent();
  virtual Int_t            Debug()        { return fDebug;}
  static  void             MakeDetectorDescriptors();
  static  const Char_t    *MakeDetectorDescriptor(const Char_t *det);
 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  static StarMCHits *fgInstance;
  GHit_t             fHit;
  TDataSet          *fDetectors;
  THashList         *fDetList;
  TObjArray         *fVolUserInfo;
  TDataSet          *fHitHolder;
  StarVMCDetector   *fCurrentDetector;
  Int_t              fDebug;
  UInt_t             fSeed;
  Int_t              fEventNumber;
  ClassDef(StarMCHits,1)
};
#endif

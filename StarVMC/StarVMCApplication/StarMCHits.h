#ifndef StarMCHits_h
#define StarMCHits_h
#include "St_g2t_Chair.h"
#include "StHitDescriptor.h"
#include "StarHitVector.h"
class TGeant3;


class StarMCHits : public TDataSet {
 public:
  virtual ~StarMCHits() {SafeDelete(fDetList); SafeDelete(fVolUserInfo);}
  static StarMCHits       *instance(const Char_t *name="",		      		       
		    	            const Char_t *title="") {
    if (fgInstance) return fgInstance;
    return new StarMCHits(name, title); 
  }
  virtual TDataSet  	  *Instance() const {return (TDataSet *) fgInstance;}   		       
  virtual Int_t     	   Init();					      		       
  virtual void      	   Clear(const Option_t* opt = "");
  virtual void      	   Step();					      		       
#if 0
  virtual Float_t   	   GetHitK(Int_t k);				      		       
#endif
  virtual void      	   SetHitHolder(TDataSet  *m) {fHitHolder = m;}	      	       
  virtual TDataSet  	  *GetDetectors() {return fDetectors;}		      	       
  virtual TObjArray 	  *GetVolUserInfo() {return fVolUserInfo;}	      		       
  virtual TDataSet  	  *GetHitHolder() {return fHitHolder;}                  				       
  virtual StHitDescriptor *GetCurrentDetector() {return fCurrentDetector;}
  virtual void      	   FillG2Table();							       	    
  virtual void             FinishEvent();
  static  TTable    	  *NewTable(const Char_t *classname, const Char_t *name="", Int_t nrows=100); 
  static  St_g2t_Chair    *NewChair(const Char_t *type, const Char_t *name="");                       
 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  static StarMCHits *fgInstance;
  static TGeant3    *fgGeant3;
  GHit_t             fHit;
  TDataSet          *fDetectors;
  TDataSet          *fGeoData;
  THashList         *fDetList;
  TObjArray         *fVolUserInfo;
  TDataSet          *fHitHolder;
  StHitDescriptor   *fCurrentDetector;
  ClassDef(StarMCHits,1)
};
#endif

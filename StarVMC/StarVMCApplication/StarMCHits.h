// $Id: StarMCHits.h,v 1.6 2007/01/05 21:37:42 potekhin Exp $
// $Log: StarMCHits.h,v $
// Revision 1.6  2007/01/05 21:37:42  potekhin
// Add CVS tags
//
#ifndef StarMCHits_h
#define StarMCHits_h
#include "St_g2t_Chair.h"
#include "StHitDescriptor.h"
#include "StarHitVector.h"
#include "StarMCHitCollection.h"


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
  virtual void      	   StepLegacy();
#if 0
  virtual Float_t   	   GetHitK(Int_t k);				      		       
#endif
  virtual void      	   SetHitHolder(TDataSet  *m) {fHitHolder = m;}	      	       

  virtual void             SetDebug(Int_t m=0)        {fDebug = m;}
  virtual void             SetLegacy(Int_t m=0)       {fLegacy = m;}

  virtual TDataSet  	  *GetDetectors()   {return fDetectors;}		      	       
  virtual TObjArray 	  *GetVolUserInfo() {return fVolUserInfo;}	      		       
  virtual TDataSet  	  *GetHitHolder()   {return fHitHolder;}                  				       

  virtual StHitDescriptor *GetCurrentDetector() {return fCurrentDetector;}

  virtual void      	   FillG2Table();							       	    
  virtual void             FinishEvent();
  static  TTable    	  *NewTable(const Char_t *classname, const Char_t *name="", Int_t nrows=100); 
  static  St_g2t_Chair    *NewChair(const Char_t *type, const Char_t *name="");                       
  virtual Int_t            Debug()        { return fDebug;}

  virtual Int_t            Legacy()       { return fLegacy;}

  virtual StarMCHitCollection *HitCollection(void) { return fHitCollection;}

 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  StarMCHitCollection *fHitCollection;
  static StarMCHits   *fgInstance;
  GHit_t               fHit;
  TDataSet            *fDetectors;
  TDataSet            *fGeoData;
  THashList           *fDetList;
  TObjArray           *fVolUserInfo;
  TDataSet            *fHitHolder;
  StHitDescriptor     *fCurrentDetector;
  Int_t                fDebug;
  Int_t                fLegacy;
  UInt_t               fSeed;
  Int_t                fEventNumber;
  ClassDef(StarMCHits,1)
};
#endif

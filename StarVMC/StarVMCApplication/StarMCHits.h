#ifndef StarMCHits_h
#define StarMCHits_h
#include "StarVMCDetector.h"
#include "St_g2t_Chair.h"
#include "StarHitVector.h"
#include "StarVMCDetectorSet.h"
/* commons/agcrdig.inc
      Integer          IWA,   JS,JD,JX,JXD,JDS,JDU
      COMMON /AGCRDIG/ IWA(2),JS,JD,JX,JXD,JDS,JDU
      Integer          Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia
      Character*4                                         cs,cd
      COMMON /AGCHITV/ Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia,cs,cd
*/
typedef struct {
  Int_t iwa[2], js,jd,jx,jxd,jds,jdu;
} Agcrdig_t;
typedef struct {
  Int_t Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia;
  Char_t  cs[4],cd[4];
} Agchitv_t;
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
  virtual Agchitv_t*  Agchitv()  const {return fAgchitv;}
 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  static StarMCHits *fgInstance;
  GHit_t             fHit;
  TDataSet          *fHitHolder;
  StarVMCDetector   *fCurrentDetector;
  Int_t              fDebug;
  UInt_t             fSeed;
  Int_t              fEventNumber;
  Agchitv_t *fAgchitv;          //! AGCHITV common structure
  ClassDef(StarMCHits,1)
};
#endif

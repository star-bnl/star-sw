#ifndef StarMCHits_h
#define StarMCHits_h
#include "StarVMCDetector.h"
#include "St_g2t_Chair.h"
#include "StarHitVector.h"
#include "StarVMCDetectorSet.h"
//----------AGCHDIG atlsim/agchit.inc
// *  AGCHITL and AGCHITV are communication between AgFHIT0/1/AggetDIG,AgFPATH.
//       Integer          IWA,   JS,JD,JX,JDX,JXD,JDS,JDU
//       COMMON /AGCHDIG/ IWA(2),JS,JD,JX,JDX,JXD,JDS,JDU
//       Integer          Iprin,Iv,Ia,Nv,Mb,Nc1,Nc2,NumMX,Lp
//       Parameter        (Lp=15)
//       Real             Org,Fct
//       Character*4      cs,cd
//       COMMON /AGCHITV/ Iprin,Iv,Ia,Nv,Mb,Nc1,Nc2,cs,cd,
//      >                 Org(Lp),Fct(Lp),NumMX(Lp)

typedef struct {
  Int_t   Iprin,Iv,Ia,Nv,Mb,Nc1,Nc2;
  Char_t  cs[4],cd[4];
  Float_t Org[15],Fct[15];
  Int_t   NumMX[15];
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

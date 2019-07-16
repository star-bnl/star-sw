#ifndef StarMCHits_h
#define StarMCHits_h
#include "StarVMCDetector.h"
#include "St_g2t_Chair.h"
#include "StarHitVector.h"
#include "StarVMCDetectorSet.h"
class g2t_event_st;
class g2t_vertex_st;
class g2t_track_st;
class St_g2t_event;
class St_g2t_vertex;
class St_g2t_track;
/* commons/agcrdig.inc
      Integer          IWA,   JS,JD,JX,JXD,JDS,JDU
      COMMON /AGCRDIG/ IWA(2),JS,JD,JX,JXD,JDS,JDU
      Integer          Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia
      Character*4                                         cs,cd
      COMMON /AGCHITV/ Iprin,Nvb,Nw,Last,Mb,Nc1,Nc2,Iv,Ia,cs,cd
      integer          idigi
      common /AgCdigi/ idigi(15)
*/
typedef struct {
  Int_t idigi[15];
} Agcdigi_t;
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
  virtual void             BeginEvent();
  virtual void             FinishEvent();
  virtual void             BeginPrimary() {}
  virtual void             PreTrack();
  virtual void             PostTrack() {}
  virtual void             FinishPrimary() {}
  virtual Int_t            Debug()        { return fDebug;}
  virtual Agcdigi_t*       Agcdigi()  const {return fAgcdigi;}
  virtual Agchitv_t*       Agchitv()  const {return fAgchitv;}
  virtual void             SetCurrent_g2t_track (g2t_track_st*  trk) {ftrackCurrent  = trk;}
  virtual void             SetCurrent_g2t_vertex(g2t_vertex_st* vtx) {fvertexCurrent = vtx;}
  virtual g2t_track_st*    Current_g2t_track () {return ftrackCurrent;}
  virtual g2t_vertex_st*   Current_g2t_vertex() {return fvertexCurrent;}
  
 private:
  StarMCHits(const Char_t *name="StarMCHits",const Char_t *title="");
  static StarMCHits *fgInstance;
  GHit_t             fHit;
  TDataSet          *fHitHolder;
  StarVMCDetector   *fCurrentDetector;
  Int_t              fDebug;
  UInt_t             fSeed;
  Int_t              fEventNumber;
  Agcdigi_t         *fAgcdigi;          //! AGCDIGI common structure
  Agchitv_t         *fAgchitv;          //! AGCHITV common structure
  g2t_event_st      *feventCurrent;
  g2t_vertex_st     *fvertexCurrent;
  g2t_track_st      *ftrackCurrent;
  St_g2t_event      *fg2t_event;
  St_g2t_vertex     *fg2t_vertex;
  St_g2t_track      *fg2t_track;
  ClassDef(StarMCHits,1)
};
#endif

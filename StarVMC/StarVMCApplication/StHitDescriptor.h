#ifndef StHitDescriptor_h
#define StHitDescriptor_h
#include "TTable.h"
#include "TDataSet.h"
#include "St_g2t_Chair.h"
#include "tables/St_det_user_Table.h"
#include "tables/St_det_path_Table.h"
#include "tables/St_det_hit_Table.h"
class St_g2t_Chair;
class StHitDescriptor : public TNamed {
 public:
  StHitDescriptor(TDataSet *Det = 0, St_det_user* UserDesc=0, St_det_path* PathDesc=0, 
		St_det_hit *HitDesc = 0) :
  TNamed("",""), fVersion(1), fDet(Det), fUserDesc(UserDesc), fPathDesc(PathDesc), fHitDesc(HitDesc), 
    fChair(0), fNVL(0) {if (fDet) {SetName(fDet->GetName()), SetTitle(fDet->GetTitle());}}
  virtual ~StHitDescriptor() {}
  void           SetChair(St_g2t_Chair *Chair=0) {fChair = Chair;}
  void           SetNVL(Int_t n) {fNVL = n;}
  void           SetVersion(Int_t n) {fVersion = n;}
  TDataSet      *GetDetector() {return fDet;}
  St_det_path   *GetPathDesc() {return fPathDesc;}
  St_det_user   *GetUserDesc() {return fUserDesc;}
  St_det_hit    *GetHitDesc()  {return fHitDesc;}
  St_g2t_Chair  *GetChair()    {return fChair;}
  Int_t          GetVersion()  {return fVersion;}
  void           Clear(const Option_t* opt = "") {}
  Int_t          GetNVL() {return fNVL;}
 private:
  Int_t         fVersion;
  TDataSet     *fDet;
  St_det_user  *fUserDesc;
  St_det_path  *fPathDesc;
  St_det_hit   *fHitDesc;
  St_g2t_Chair *fChair;
  Int_t         fNVL;
};
#endif

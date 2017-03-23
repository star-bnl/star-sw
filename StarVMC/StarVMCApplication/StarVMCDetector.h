#ifndef StarVMCDetector_h
#define StarVMCDetector_h
#include "TDataSet.h"
#include "TString.h"
#include "TArrayI.h"
#include "StEnumerations.h"
#include "St_g2t_Chair.h"
class StarVMCDetector : public TDataSet {
 public:
   //----- detector set flags in TGeoVolume
  enum EDetSetBits {
    kActive       = BIT(1),  // if TGeoVolume object is active
    kChecked      = BIT(2)   //  -"-                 is checked
  };
  StarVMCDetector(const Char_t *name);
  ~StarVMCDetector();
  
  void SetChair(St_g2t_Chair *Chair=0) {fChair = Chair;}
  void SetFMT(const Char_t *fmt);
  void SetNVmax(Int_t N, Int_t *array);
  void SetNVp10(Int_t N, Int_t *array) {fN10.Set(N, array);}
  void SetIds(Int_t N=0, Int_t *array=0);
  const TString &GetFMT()   const {return *&fFMT;}
  const TArrayI &GetNVmax() const {return *&fNVmax;}
  const TArrayI &GetIds()         {return *&fIds;}
  St_g2t_Chair  *GetChair();
  Int_t          GetNVL()   const {return fNVmax.GetSize();}
  Int_t          GetVolumeIdFromNubv(Int_t *numbv);
  void           GetNumbv(const Char_t *path, Int_t *numbvR);
  static const Char_t *FormPath(const Char_t *FMT, Int_t N, Int_t *numbv);
  Int_t          GetVolumeId(const Char_t *path);
  Int_t          GetVolumeId(const Char_t *path, Int_t *numbv);
  Int_t          GetVolumeId(Int_t *numbv);
  Int_t          GetElement(Int_t volumeId);
  const StDetectorId Id() {return fId;}
  void           SetVolIdOffset(Int_t k) {fVolIdoffset = k;}
  const Char_t  *FormPath(const Char_t *FMT, Int_t *numbvR);
  void           Clear(const Option_t* opt="");
  static  TTable    	  *NewTable(const Char_t *classname, const Char_t *name="", Int_t nrows=100); 
  static  St_g2t_Chair    *NewChair(const Char_t *type, const Char_t *name="");                       
  static Int_t g2t_volume_id(const Char_t *detN, Int_t *numbv);
 private:
  
  StDetectorId fId;
  Int_t   fK;      // position of the detector in StarDetectorMap array
  TString fFMT;
  TArrayI fNVmax;
  TArrayI fN10;
  Int_t   fVolIdoffset; 
  TArrayI fIds;
  TArrayI fSortedId;
  Int_t **fSId;
  St_g2t_Chair *fChair;
  ClassDef(StarVMCDetector,1) // Map between g2t_volume_id and VMS path
};
#endif

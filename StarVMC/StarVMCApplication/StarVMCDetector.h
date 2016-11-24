#ifndef StarVMCDetector_h
#define StarVMCDetector_h
#include "TDataSet.h"
#include "TString.h"
#include "TArrayI.h"
#include "StEnumerations.h"
#include "St_g2t_Chair.h"
#if 0
#include "Math/ParamFunctor.h"
#endif
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
  void           GetNumbvFromVolueId(Int_t VolumeID, Int_t *Numbv);
  void           GetNumbv(const Char_t *path, Int_t *numbv);
  Int_t          GetVolumeId(const Char_t *path);
  Int_t          GetVolumeId(const Char_t *path, Int_t *numbv);
  Int_t          GetVolumeId(Int_t *numbv);
  Int_t          GetElement(Int_t volumeId);
  void           SetVolIdOffset(Int_t k) {fVolIdoffset = k;}
  const Char_t  *GetPath(Int_t volumeId);
  static const Char_t *FormPath(const Char_t *FMT, Int_t N, Int_t *numbv);
  void           Clear(const Option_t* opt="");
  static  TTable    	  *NewTable(const Char_t *classname, const Char_t *name="", Int_t nrows=100); 
  static  St_g2t_Chair    *NewChair(const Char_t *type, const Char_t *name="");                       
#if 0
   template <class PtrObj, typename MemFn> 
   void SetFunction( PtrObj& p, MemFn memFn ){ 
     // set from a pointer to a member function
     fFunctor = ROOT::Math::ParamFunctor(p,memFn); 
   } 
   template <typename Func> 
   void SetFunction( Func f ){
     // set function from a generic C++ callable object 
     fFunctor = ROOT::Math::ParamFunctor(f); 
   } 
   static Int_t g2t_volume_id(Int_t *numbv) {(Int_t) fFunctor((Double_t*) numbv, 0);}
#endif
   static Int_t g2t_volume_id(const Char_t *detN, Int_t *numbv);
#if 0
   Int_t TpcVolumeId(Int_t N, Int_t *Nubv);
   Int_t SvtVolumeId(Int_t N, Int_t *Numbv);
   Int_t SsdVolumeId(Int_t N, Int_t *Numbv);
   Int_t TofVolumeId(Int_t N, Int_t *Numbv);
   Int_t CtbVolumeId(Int_t N, Int_t *Numbv);
   Int_t EmcVolumeId(Int_t N, Int_t *Numbv);
   Int_t SmdVolumeId(Int_t N, Int_t *Numbv);
   Int_t EemVolumeId(Int_t N, Int_t *Numbv);
   Int_t EsmVolumeId(Int_t N, Int_t *Numbv);
   Int_t FtpVolumeId(Int_t N, Int_t *Numbv);
   Int_t VpdVolumeId(Int_t N, Int_t *Numbv);
   Int_t RchVolumeId(Int_t N, Int_t *Numbv);
   Int_t ZdcVolumeId(Int_t N, Int_t *Numbv);
   Int_t PmdVolumeId(Int_t N, Int_t *Numbv);
   Int_t BbcVolumeId(Int_t N, Int_t *Numbv);
   Int_t PixVolumeId(Int_t N, Int_t *Numbv);
   Int_t IstVolumeId(Int_t N, Int_t *Numbv);
   Int_t FstVolumeId(Int_t N, Int_t *Numbv);
   Int_t FgtVolumeId(Int_t N, Int_t *Numbv);
   Int_t IgtVolumeId(Int_t N, Int_t *Numbv);
   Int_t FpdVolumeId(Int_t N, Int_t *Numbv);
   Int_t FscVolumeId(Int_t N, Int_t *Numbv);
   Int_t MtdVolumeId(Int_t N, Int_t *Numbv);
   Int_t EtrVolumeId(Int_t N, Int_t *Numbv);
#endif
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
#if 0
  ROOT::Math::ParamFunctor fFunctor;   //! Functor object to wrap any C++ callable object
#endif
  ClassDef(StarVMCDetector,1) // Map between g2t_volume_id and VMS path
};
#endif

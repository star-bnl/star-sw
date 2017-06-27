#ifndef StarVMCDetectorSet_h
#define StarVMCDetectorSet_h
#include "TDataSet.h"
#include "THashList.h"
#include "TGeoNode.h"
#include "TString.h"
#include "StarVMCDetector.h"
class StarVMCDetectorSet : public TDataSet {
 public:
  virtual ~StarVMCDetectorSet() {SafeDelete(fDetHash); SafeDelete(fDetectorDescriptors);}
  static   StarVMCDetectorSet *instance();  		       
  virtual  Int_t     	       Init();					      		       
  virtual  void                SetDebug(Int_t m=0) {fDebug = m;}
  virtual  THashList          *GetDetectorHash() {return fDetHash;}
  virtual  Int_t               Debug()        { return fDebug;}
  virtual  
  void                         MakeDetectorDescriptors();
  Int_t                        LoopOverTgeo(TGeoNode *nodeT = 0, TString pathT = "");
  const StarVMCDetector*       GetVMCDetector(StDetectorId Id);
 private:
  StarVMCDetectorSet(const Char_t *name="StarVMCDetectorSet",const Char_t *title="");
  static StarVMCDetectorSet *fgInstance;
  static TDataSet           *fDetectorDescriptors;
  THashList         *fDetHash;
  Int_t              fDebug;
  ClassDef(StarVMCDetectorSet,1)
};
#endif

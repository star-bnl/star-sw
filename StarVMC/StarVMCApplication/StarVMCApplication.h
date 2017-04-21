// $Id: StarVMCApplication.h,v 1.3 2009/08/10 19:06:59 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
//

#ifndef Star_MC_APPLICATION_H
#define Star_MC_APPLICATION_H
#include "TDataSet.h"
#include "TGeoNode.h"
#include "TVirtualMCApplication.h"
#include "StarMCPrimaryGenerator.h"
#include "StarStack.h"
#include "StarMagField.h"
#include "StarMCHits.h"
#include "TTable.h"
#include "Ttypes.h"
class TGeoPhysicalNode;
class TGeoHMatrix;
class TGeoRotation;
struct VMCPath2Detector_st  {   /* path to the detector element  */
  Char_t     VName[32];         /* Volume of branching           */
  Int_t      Ncopy;             /* number of branchings          */
  Int_t      Nb;                /* number of bit needed          */     
};
class St_VMCPath2Detector : public TTable {
 public:
  ClassDefTable(St_VMCPath2Detector,VMCPath2Detector_st)
  ClassDef(St_VMCPath2Detector,1) //C++ container for VMC detector path
};

class StarVMCApplication : public TVirtualMCApplication {
 public:
  StarVMCApplication(const char* name=0,  const char *title=0);
  virtual ~StarVMCApplication();
  
  // static access method
  static StarVMCApplication* Instance() { return (StarVMCApplication*)(TVirtualMCApplication::Instance()); } 

  // methods
  void InitMC(const char *setup=0);
  Bool_t RunMC(Int_t nofEvents); // kFALSE if run has been stopped
  void FinishRun();
  
  virtual void ConstructGeometry();
  virtual void InitGeometry();
  virtual void GeneratePrimaries();
  virtual void BeginEvent();
  virtual void BeginPrimary();
  virtual void PreTrack();
  virtual void Stepping();
  virtual void PostTrack();
  virtual void FinishPrimary();
  virtual void FinishEvent();
  virtual void Field(const Double_t* x, Double_t* b) const;
  virtual void AddParticles() {}
  virtual void SetPrimaryGenerator(StarMCPrimaryGenerator *m = 0) {
    fPrimaryGenerator = m; 
    if (fPrimaryGenerator) fPrimaryGenerator->SetStack(fStarStack);
  }
  virtual void SetMagField(StarMagField *m = 0) {fMagField = m;}
  virtual void SetStepping(StarMCHits *m = 0) {fMcHits = m;}
  virtual StarMCPrimaryGenerator *GetPrimaryGenerator() const {return fPrimaryGenerator;}
  virtual StarMagField           *GetMagField() const {return fMagField;}
  virtual StarStack            *GetStack() const {return fStarStack;}
    
  virtual Double_t                 TrackingRmax() const { return 1.e4; }		  
  virtual Double_t 		   TrackingZmax() const { return 1.e5; } 		  
  virtual void     		   SetDebug(Int_t m);
  virtual Int_t    		   Debug() {return fDebug;}				  
  virtual void     		   DoMisAlignment(Bool_t m = kFALSE) {fAlignment = m;}	  
  static  Int_t     		   LoopOverTgeo(TGeoNode *nodeT = 0, TString pathT = "");
  static  void     		   GeometryDb(TDataSet *Detectors=0);			  
  virtual Bool_t   		   MisalignGeometry();					  
  static  TDataSet 		  *DetectroDescriptors() {return fgDetSets;}             
  void  ForceDecay(const Char_t *nameP, 
		   const Char_t *mode1A = 0, const Char_t *mode1B = 0, const Char_t *mode1C = 0, Float_t branch1 = 0,
		   const Char_t *mode2A = 0, const Char_t *mode2B = 0, const Char_t *mode2C = 0, Float_t branch2 = 0,
		   const Char_t *mode3A = 0, const Char_t *mode3B = 0, const Char_t *mode3C = 0, Float_t branch3 = 0);
 private:
  // methods
  
  // data members
  StarStack*             fStarStack;
  StarMCPrimaryGenerator*  fPrimaryGenerator;
  StarMagField*            fMagField;
  StarMCHits*              fMcHits;
  Double_t*                fFieldB;
  Int_t                    fDebug;
  Bool_t                   fAlignment;
  Bool_t                   fAlignmentDone;
  static  TDataSet        *fgDetSets;
  ClassDef(StarVMCApplication,1)  //Interface to MonteCarlo application
};

// $Log: StarVMCApplication.h,v $
// Revision 1.3  2009/08/10 19:06:59  fisyak
// Add different fitted parameters types (Sti,SD,SC,Dca)
//
// Revision 1.2  2009/02/25 20:22:26  fisyak
// Add the first version for misalignment
//
#endif //Star_MC_APPLICATION_H

// $Id: StarVMCApplication.h,v 1.1 2005/04/25 20:44:28 fisyak Exp $
// Class StarVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
//

#ifndef Star_MC_APPLICATION_H
#define Star_MC_APPLICATION_H

#include "TVirtualMCApplication.h"
#include "StarMCPrimaryGenerator.h"
#include "StarMCStack.h"
class StarMagField;
class StarMCHits;
class StarVMCApplication : public TVirtualMCApplication {
 public:
  StarVMCApplication(const char* name=0,  const char *title=0);
  virtual ~StarVMCApplication();
  
  // static access method
  static StarVMCApplication* Instance() { return (StarVMCApplication*)(TVirtualMCApplication::Instance()); } 

  // methods
  void InitMC(const char *setup=0);
  void RunMC(Int_t nofEvents);
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
    if (fPrimaryGenerator) fPrimaryGenerator->SetStack(fStack);
  }
  virtual void SetMagField(StarMagField *m = 0) {fMagField = m;}
  virtual void SetStepping(StarMCHits *m = 0) {fMcHits = m;}
  virtual StarMCPrimaryGenerator *GetPrimaryGenerator() const {return fPrimaryGenerator;}
  virtual StarMagField           *GetMagField() const {return fMagField;}
  virtual StarMCStack            *GetStack() const {return fStack;}
  private:
  // methods
  
  // data members
  StarMCStack*             fStack;
  StarMCPrimaryGenerator*  fPrimaryGenerator;
  StarMagField*            fMagField;
  StarMCHits*              fMcHits;
  Double_t*                fFieldB;
  ClassDef(StarVMCApplication,1)  //Interface to MonteCarlo application
};

// inline functions
#endif //Star_MC_APPLICATION_H


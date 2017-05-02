// $Id: StVMCApplication.h,v 1.4 2017/05/02 19:34:04 perev Exp $
// Class StVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
//
#include "TVirtualMCApplication.h"
#ifndef StMC_APPLICATION_H
#define StMC_APPLICATION_H

class GCall;

class StVMCApplication : public TVirtualMCApplication 
{
 public:
  StVMCApplication(const char* name=0,  const char *title=0);
  virtual ~StVMCApplication();
  int Init();
  int Trig(int nTrig=1);  

  virtual void GeneratePrimaries();
  virtual void Stepping();
  virtual void ConstructGeometry();
  virtual void InitGeometry();
  virtual void BeginEvent();
  virtual void BeginPrimary();
  virtual void PreTrack();
  virtual void PostTrack();
  virtual void FinishPrimary();
  virtual void FinishEvent();
  virtual void Field(const double* x, double* b) const;
  virtual void AddParticles() {}
//functions setters
 void SetInit             (GCall* gc) { mInit             =gc;}
 void SetStepping         (GCall *gc) { mStepping         =gc;}
 void SetPrimaryGenerator (GCall* gc) { mPrimaryGenerator =gc;}
 void SetConstructGeometry(GCall* gc) { mConstructGeometry=gc;}
 void SetInitGeometry     (GCall* gc) { mInitGeometry	  =gc;}
 void SetBeginEvent  	  (GCall* gc) { mBeginEvent	  =gc;}
 void SetBeginPrimary	  (GCall* gc) { mBeginPrimary	  =gc;}
 void SetPreTrack	  (GCall* gc) { mPreTrack	  =gc;}
 void SetPostTrack	  (GCall* gc) { mPostTrack	  =gc;}
 void SetFinishPrimary 	  (GCall* gc) { mFinishPrimary	  =gc;}
 void SetFinishEvent      (GCall* gc) { mFinishEvent	  =gc;}
 void SetField            (GCall* gc) { mField		  =gc;}
 void SetDebug            (int db=1);

//functions getters
 GCall *GetInit             () { return mInit             ;}
 GCall *GetStepping         () { return mStepping         ;}
 GCall *GetPrimaryGenerator () { return mPrimaryGenerator ;}
 GCall *GetConstructGeometry() { return mConstructGeometry;}
 GCall *GetInitGeometry     () { return mInitGeometry	  ;}
 GCall *GetBeginEvent  	    () { return mBeginEvent	  ;}
 GCall *GetBeginPrimary	    () { return mBeginPrimary	  ;}
 GCall *GetPreTrack	    () { return mPreTrack	  ;}
 GCall *GetPostTrack	    () { return mPostTrack	  ;}
 GCall *GetFinishPrimary    () { return mFinishPrimary	  ;}
 GCall *GetFinishEvent      () { return mFinishEvent	  ;}
 GCall *GetField            () { return mField		  ;}
  void  SetRZmax(double rMax,double zMax) { mRmax=rMax;mZmax=zMax; }
    
  virtual double TrackingRmax() const { return mRmax; }
  virtual double TrackingZmax() const { return mZmax; } 
  virtual int    Debug() 	const { return mDebug;}
  
 private:
// 		methods
// 		data members
 
 char   mBeg[1];
 int    mDebug;
 int    mNStepping;
 double mRmax;
 double mZmax;
 GCall* mInit;  
 GCall* mStepping;  
 GCall* mPrimaryGenerator;  
 GCall* mConstructGeometry;
 GCall* mInitGeometry;
 GCall* mBeginEvent;
 GCall* mBeginPrimary;
 GCall* mPreTrack;
 GCall* mPostTrack;
 GCall* mFinishPrimary;
 GCall* mFinishEvent;
 GCall* mField;
 char   mEnd[1];

ClassDef(StVMCApplication,0)  //Interface to MonteCarlo application
};

#endif //StMC_APPLICATION_H

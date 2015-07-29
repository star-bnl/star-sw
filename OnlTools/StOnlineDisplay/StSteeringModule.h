// $Id: StSteeringModule.h,v 1.9 2015/07/29 16:53:19 smirnovd Exp $

#ifndef STAR_StSteeringModule
#define STAR_StSteeringModule

/*!
 *                                                                     
 * \class  StSteeringModule
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StSteeringModule virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StSteeringModule
 *
 *
 */                                                                      

#ifndef TModule_H
#include "TModule.h"
#endif

#include "StDetectorGeometryInterface.h"
#ifdef __CINT__
  class QObject;
#else
#  include "qobject.h"
#endif


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).

class  StDataReadModule;
class  TCanvas;
class  TQtRootViewer3D;
class  StuDraw3DEvent;
   
class StSteeringModule : public QObject, public TModule, StDetectorGeometryInterface{
#ifndef __CINT__
   Q_OBJECT
#endif
 private:
  // Private method declaration if any
  Int_t  fAnimate;
  Bool_t fAnimating;
 protected:
  // Protected method if any
   StDataReadModule          *fDataReadModule;
 public: 
  StSteeringModule(const char *name="Steering");
  virtual       ~StSteeringModule();
  virtual Int_t  Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty TModule::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty TModule::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSteeringModule.h,v 1.9 2015/07/29 16:53:19 smirnovd Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
          
  }
  public: 
   // StDetectorGeometryInterface methods
            
    virtual void   AddVolume(const char *name);
    virtual Int_t  BuildGeometry();
    virtual void   Modified();
    virtual Int_t  NextEvent();
    virtual Int_t  NextFile();        
    virtual void   PrintVolumes();
    virtual void   RemoveVolume(const char *name);
    virtual void   SetFileName(const char* fileName);
    virtual void   SetDrawOption(Option_t *option);
    
    // St Data interfcae
      
    virtual void SetEventNumber(int eventNumber);
    virtual void SetRunNumber(int runNumber);
    virtual void SetMagneticField(int field=50);
    virtual void SetRecording(bool on=true);
    virtual void SetDemo(Bool_t on=kTRUE);
    virtual Bool_t Demo() const;
    virtual bool Recording()  const;
    virtual void SetDaqFileName(const char *fileName);
    virtual Int_t RemakeEvent();
    const TString &MountPoint() const;
    virtual void ResetConnection();
    
    // StDisplayInterface methods
    void  Animate(Int_t on=340);
    virtual Int_t DisplayEvent(Bool_t refresh=kTRUE);
    virtual Int_t DisplayGeometry(Bool_t refresh=kTRUE, Bool_t ifModified=kTRUE);
    virtual  void Refresh();
    virtual  void SetCanvas(TCanvas*);        
    virtual  void SetL3TracksOn(Int_t on = 1);
    virtual  void SetL3HitsOn  (Int_t on = 1);
    virtual  void SetEmcHitsOn (Int_t on = 1);
    void Set3DViewer( TQtRootViewer3D *viewer);
    virtual void  SetCoin3DReady(Bool_t ready=true);

    virtual void   SetGuiObject(QObject *gui);
    virtual void   NextEventsSlot(int interval);
    virtual void   StopEvents();
    virtual StuDraw3DEvent *Display();
    // Thread synch:
    Bool_t IsDisplayNext() const ;
#ifndef __CINT__
   public slots:
   void  Animating();
#endif
#ifndef Q_MOC_RUN
    ClassDef(StSteeringModule,0)   //StSteeringModule for online display          
#endif
};

#endif

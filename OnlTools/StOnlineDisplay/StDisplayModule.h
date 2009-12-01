// $Id: StDisplayModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $

#ifndef STAR_StDisplayModule
#define STAR_StDisplayModule

/*!
 *                                                                     
 * \class  StDisplayModule
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StDisplayModule virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StDisplayModule
 *
 *
 */                                                                      

#ifndef TModule_H
#include "TModule.h"
#endif

class TCanvas;
class TVirtualPad;
class TQtRootViewer3D;
class TDataProvider;
class TCoinEmcTowers;

// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).

class StDisplayModule : public TModule {
 private:
  // Private method declaration if any
 
 protected:
  // Protected data-members
   TCanvas      *fPadBrowserCanvas; 	//!
   Int_t         fL3TracksOn; //! should we draw tpc tracks
   Int_t         fEmcHitsOn;  //! should we draw Emc towers
   Int_t         fL3HitsOn;   //! should we draw tpc hits
   TQtRootViewer3D *f3DViewer; //! external 3D viewer;
   Bool_t        fNeedsClear3DView;//! flag to clear the current view
   TCoinEmcTowers *fEmcTowers;
   Bool_t        fCoin3DReady;
   TVirtualPad  *CreateCanvas();
   TDataProvider *fSizeProvider;
   TDataProvider *fColorProvider;
   bool           fRecording;


 protected:
  // Protected methods 
   TVirtualPad *GetEventPad();

 public: 
  StDisplayModule(const char *name="DisplayModule");
  virtual       ~StDisplayModule();
  virtual void   Clear(Option_t *option="");
  virtual void DrawObject(TObject *,Option_t *option="",Bool_t first=kFALSE);
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void  SetCoin3DReady(Bool_t ready=true) { fCoin3DReady = ready;   }
  void SetColorProvider( TDataProvider *provider) { fColorProvider=provider;} 
  void SetSizeProvider( TDataProvider *provider)  { fSizeProvider=provider; }  
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty TModule::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty TModule::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StDisplayModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  Int_t DisplayGeometry(Bool_t refresh=kTRUE,Bool_t isModified=kTRUE);
  Int_t DisplayEvent(Bool_t refresh=kTRUE);
  virtual void SetRecording(bool on=true);
  virtual bool Recording()  const;
  void  Refresh();
  void SetL3TracksOn(Int_t on = 1) { fL3TracksOn = on; }
  void SetL3HitsOn  (Int_t on = 1) { fL3HitsOn = on; }
  void SetEmcHitsOn (Int_t on = 1) { fEmcHitsOn = on; }
  void Set3DViewer( TQtRootViewer3D *viewer) {  f3DViewer = viewer;} 
  TQtRootViewer3D  *Get3DViewer() const { return f3DViewer; }
  
  public:     // StDisplayInterface methods
        
    virtual void  SetCanvas(TCanvas*);
        
       
         // ClassDef(StDisplayModule,0)   //StAF chain virtual base class for Makers
};

#endif

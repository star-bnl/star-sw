// @(#)root/base:$Name:  $:$Id: TQtRootViewer3D.cxx
// Author: Valeri Fine 05/05/2005

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtRootViewer3D                                                      //
//                                                                      //
// Generic 3D shapes viewer.                                            //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtRootViewer3D.h"
#include "TObject3DView.h"
#include "TDataSetIter.h"
#include "TQtGLViewerImp.h"
#include "TVirtualPad.h"
#include "TGQt.h"

#include "TList.h"
#include "TError.h"

#include  "TQtGLIncludes.h"

#include "TObjectOpenGLViewFactory.h"
#if QT_VERSION >= 0x40000
//Added by qt3to4:
#include <QPixmap>
#endif /* QT_VERSION */



ClassImp(TQtRootViewer3D)

//______________________________________________________________________________
void SlotDisconnect::CleanPrimitives()
{
   if (fMaster) fMaster->ClearPrimitives();
}

//______________________________________________________________________________
void SlotDisconnect::Disconnect()
{
   if (fMaster){  
      fMaster->MakeViewerNil();
      /*fMaster->fViewer = 0; printf("SlotDisconnect::Disconnect()\n"); */
      fMaster->Disconnect(); 
   } 
}
//______________________________________________________________________________
void SlotDisconnect::DestroyMaster()
{ 
   Disconnect();
   TQtRootViewer3D *master = fMaster; 
   fMaster = 0; 
   emit Destroyed(master);
   delete master;
}
//______________________________________________________________________________
void  SlotDisconnect::UpdateView(QPixmap *)
{
  if ( fMaster && (fMaster->fPad == (TVirtualPad*)gPad) ) fMaster->UpdateView();
}

//______________________________________________________________________________
TQtRootViewer3D::TQtRootViewer3D(TVirtualPad*pad)
: TVirtualViewer3D(),fView3DFactory(0), fPad(pad), fListOfPrimitives(0), fViewer(0), fDepth(3), fBuildingScena(kFALSE)
{ 
   // fprintf(stderr,"TQtRootViewer3D::TQtRootViewer3D this=%p\n", this); 
}

//______________________________________________________________________________
void   TQtRootViewer3D::Viewer()
{
   if (!fViewer){
      fView3DFactory = TObject3DViewFactoryABC::View3DFactory("ogl");
      if (!fView3DFactory) {
         TObject3DViewFactoryABC::Registr(new TObjectOpenGLViewFactory(), "ogl");
         fView3DFactory = TObject3DViewFactoryABC::View3DFactory("ogl");
      }
      fListOfPrimitives.SetViewFactory(fView3DFactory);
      fViewer = fPad ? 
            new TQtGLViewerImp(fPad,fPad->GetName(),fPad->UtoPixel(1.),fPad->VtoPixel(0.))
           :
            new TQtGLViewerImp(fPad,"OpenGL Viewer");
      // fprintf(stderr,"QtRootViewer3D %p for TPad %p : %s \n", this, fPad, fPad->GetName());
      if (fPad) {
         fDisconnectSlot = new SlotDisconnect(this);
         fPad->Connect("Closed()","TQtRootViewer3D", this, "DisconnectPad()");
         QObject::connect(gQt->Emitter(),SIGNAL(padPainted(QPixmap*)),fDisconnectSlot, SLOT(UpdateView(QPixmap*)));
      }
   }
}
//______________________________________________________________________________
TQtRootViewer3D::~TQtRootViewer3D()
{
    TGLViewerImp *sav = fViewer;
    if (sav && fDisconnectSlot) {
       QObject::disconnect(&(sav->Signals()),SIGNAL(destroyed()), fDisconnectSlot, SLOT(DestroyMaster()));
//       QObject::disconnect(sav,SIGNAL(viewerAbout2Close()), fDisconnectSlot, SLOT(CleanPrimitives()));
       ClearPrimitives();
    }
    if (fDisconnectSlot) {  delete fDisconnectSlot; fDisconnectSlot = 0; }
    Disconnect();
    if (sav) delete sav;
}
//______________________________________________________________________________
Bool_t  TQtRootViewer3D::CanLoopOnPrimitives() const { 
    return kTRUE; 
}

//______________________________________________________________________________
Int_t  TQtRootViewer3D::AddObject(TObject *obj, Option_t *drawOption, Bool_t *addChildren )
{
  // Add one TObject to the viewer at the end of the list
  Int_t result = 0;
  if (addChildren) {}
  if (obj && obj->InheritsFrom("TAtt3D") ) {
    Option_t *opt = drawOption;
    if (!opt || !opt[0]) opt = obj->GetDrawOption();
    TObject3DView *view = new TObject3DView(obj,opt, fView3DFactory);
    view->SetName("TopView");
    fListOfPrimitives.TDataSet::AddLast(view);
    result = 1;
  }
  return result;
}

//______________________________________________________________________________
Int_t  TQtRootViewer3D::AddObjectFirst(TObject *obj, Option_t *drawOption, Bool_t *addChildren )
{
  // Add one front TObject to the viewer
  Int_t result = 0;
  if (addChildren) {}
  if (obj && obj->InheritsFrom("TAtt3D") ) {
    Option_t *opt = drawOption;
    if (!opt || !opt[0]) opt = obj->GetDrawOption();
    TObject3DView *view = new TObject3DView(obj,opt, fView3DFactory);
    view->SetName("TopView");
    fListOfPrimitives.TDataSet::AddFirst(view);
    result = 1;
  }
  return result;
}
//______________________________________________________________________________
Int_t  TQtRootViewer3D::AddRawObject(ULong_t placedID, UInt_t optMask)
{
   // Add the raw GL list directly to the scene (
   if (fViewer) {
      fViewer->MakeCurrent();
      fViewer->AddGLList(placedID, TGLViewerImp::EObject3DType(optMask));
   } 
   return 0;
}
// When they can, TPad::Paint() and TPad::PaintModified() simply
// call the following function:
//______________________________________________________________________________
void   TQtRootViewer3D::PadPaint(TVirtualPad *pad)
{
   // Entry point for updating scene contents via VirtualViewer3D
   // interface.
   // For now this is handled by TGLViewer as it remains
   // the 'Viewer3D' of given pad.
   if (!fPad) fPad = pad;
   if (pad != fPad)
   {
      Error("TGLScenePad::PadPaint", "Mismatch between pad argument and data-member!");
      return;
   }
   UpdateView();
}

//______________________________________________________________________________
void   TQtRootViewer3D::ObjectPaint(TObject* obj, Option_t* opt)
{
   // Override of virtual TVirtualViewer3D::ObjectPaint().
   // to adopt the exist AddObject interface
    AddObject(obj,opt);
}
//______________________________________________________________________________
void   TQtRootViewer3D::Print(const Option_t *fileName) const
{
   // The only interface we can use here is TObject::Print() :(
   TQtRootViewer3D *that = ((TQtRootViewer3D *)this);
   that->Viewer() ;
   const char *fn =  fileName && fileName[0] ? fileName : "RoottViewer3D.wrl";
   if (fViewer ) {
      that->fViewer->Print(fn,"wrl");
   }
}

//______________________________________________________________________________
void   TQtRootViewer3D::PrintObjects()
{
   this->Print("wrl");
}

//______________________________________________________________________________
void   TQtRootViewer3D::BeginScene() 
{
   BeginScene(fPad);
}
//______________________________________________________________________________
void  TQtRootViewer3D::BeginScene(TVirtualPad *pad)
{
   // called by TPad::Paint | PaintModified
   // Create the 3D view of the object from the list of the primitives
   fBuildingScena = kTRUE;
   Viewer(); 
   // Bool_t began3DScene = kFALSE;
   if (pad) {
      ClearPrimitives(); 
      TObjOptLink *lnk = (TObjOptLink*)pad->GetListOfPrimitives()->FirstLink();
      TObject *obj = 0;
      while (lnk) {
         obj = lnk->GetObject();

        // Create a 3D view for the encountered a 3D shape
        Int_t objCount = 0;
        if (obj->InheritsFrom(TCollection::Class())){
           TIter next((TCollection*)obj);
           TObject *addMe  = 0;
           while ( (addMe = next())  ) objCount += AddObject(addMe,lnk->GetOption());
        } else {
            objCount += AddObject(obj,lnk->GetOption());
        }
        // printf(" TQtRootViewer3D::BeginScene obj %s 3d=%d\n", obj->GetName(), obj->InheritsFrom("TAtt3D"));
        //Int_t depth = fDepth;
        // if (obj->InheritsFrom("TGeoVolume")) depth *= 2;
        // obj->Paint(lnk->GetOption());
        lnk = (TObjOptLink*)lnk->Next();
      }
   }
}
//______________________________________________________________________________
Bool_t TQtRootViewer3D::BuildingScene() const{
   // called by TPad::Paint | PaintModified
   return  fBuildingScena;
}
//______________________________________________________________________________
void TQtRootViewer3D::ClearPrimitives()
{
   if (fViewer)  {
      fViewer->MakeCurrent();
      fViewer->SetUpdatesEnabled(FALSE);
      fViewer->Clear();
   }
   fListOfPrimitives.Delete();
   fListOfPrimitives.Clear();
}
 //______________________________________________________________________________
void   TQtRootViewer3D::CloseScene() 
{
    // called by EndScene
   if (fViewer) {
      if (gPad) 
         fViewer->SetBackgroundColor(gPad->GetFillColor());
#if 0     
       fListOfPrimitives.CompileViewLevel();
#endif     
       TDataSetIter nextList(&fListOfPrimitives);
       TObject3DView *glo = 0;
       while( (glo = (TObject3DView *)nextList()  )) {
          fViewer->AddGLList(glo->GetViewId(), glo->IsSolid() ? TGLViewerImp::kSolid : TGLViewerImp::kWired );
#ifdef EXTRASELECTION
          if ( glo->GetViewId(TObject3DViewFactoryABC::kSelectable) ) 
             fViewer->AddGLList(glo->GetViewId(TObject3DViewFactoryABC::kSelectable), TGLViewerImp::kSelecting );
#endif
      }
   }
}
//______________________________________________________________________________
void  TQtRootViewer3D::EndScene(){
   // called by TPad::Paint | PaintModified
   // This is a signal the scene has been closed and we should refresh the display
   if (fViewer) {
     fViewer->MakeCurrent();
     
     fViewer->SetUpdatesEnabled(FALSE);
     fViewer->Clear();
     
         CloseScene();
         
     fViewer->SetUpdatesEnabled(TRUE);
     fViewer->Update();
   }
   fBuildingScena = kFALSE;
}

//______________________________________________________________________________
void  TQtRootViewer3D::SetDrawOption(Option_t *option)
{
   if (fViewer)  fViewer->SetDrawOption(option);
}
//______________________________________________________________________________
Option_t   *TQtRootViewer3D::GetDrawOption() const
{
   return fViewer? fViewer->GetDrawOption(): 0;
}
//______________________________________________________________________________
void   TQtRootViewer3D::DisconnectPad()
{
   if (fPad) {
      // fprintf(stderr," TQtRootViewer3D::DisconnectPad() fPad= %p %s\n", fPad, fPad->GetName());
      QObject::disconnect(gQt->Emitter(),SIGNAL(padPainted(QPixmap*)),fDisconnectSlot, SLOT(UpdateView(QPixmap*)));
      if (fViewer) {
          fViewer->DisconnectPad();
      }
      fPad->TQObject::Disconnect("Closed()",this, 0);
      fPad->TQObject::Disconnect("Modified()",this, 0);
      fPad = 0;
   }
}
//______________________________________________________________________________
void   TQtRootViewer3D::Disconnect() 
{
   if (fPad)  {
      // fprintf(stderr," TQtRootViewer3D::Disconnect() fViewer=%p\n", fViewer);
      if (fViewer) {
         QObject::disconnect(&(fViewer->Signals()),SIGNAL(destroyed()), fDisconnectSlot, SLOT(DestroyMaster()));
         // QObject::disconnect(fViewer,SIGNAL(viewerAbout2Close()),fDisconnectSlot, SLOT(CleanPrimitives()));
         fViewer->DisconnectPad();
      }
      fPad->TQObject::Disconnect("Closed()",this, 0);
      fPad->TQObject::Disconnect("Modified()",this, 0);
#if ROOT_VERSION_CODE < ROOT_VERSION(5,07,00)
      TVirtualViewer3D *currentPadViewer = fPad->GetViewer3D(0);
#else
      TVirtualViewer3D *currentPadViewer = fPad->GetViewer3D("");
#endif
      if (currentPadViewer == (TVirtualViewer3D *)this ) 
         fPad->ReleaseViewer3D();
   }
   fViewer = 0;
}

//______________________________________________________________________________
const TGLViewerImp *TQtRootViewer3D::GetViewerImp() const
{ 
   // return the QGLWidget implementation
   return fViewer;                      
}
//______________________________________________________________________________
TGLViewerImp *TQtRootViewer3D::GetViewerImp()
{ 
   // return the QGLWidget implementation
   return fViewer;                      
}

//   Dummy methdos  --

//______________________________________________________________________________
// Simple object addition - buffer represents a unique single positioned object
Int_t  TQtRootViewer3D::AddObject(const TBuffer3D &, Bool_t *) 
{ return 0;}

//______________________________________________________________________________
   // Complex object addition - for adding placed objects which have common template 
   // shapes. In this case buffer describes template shape (aside from kCore). 
Int_t  TQtRootViewer3D::AddObject(UInt_t , const TBuffer3D &, Bool_t *) 
{return 0;}

//______________________________________________________________________________
void TQtRootViewer3D::Clear(Option_t *)
{
    ClearPrimitives();
}

//______________________________________________________________________________
#if  ROOT_VERSION_CODE <  ROOT_VERSION(5,00,00) 
void  TQtRootViewer3D::OpenComposite(const TBuffer3D &, Bool_t *)
{}
#else
Bool_t TQtRootViewer3D::OpenComposite(const TBuffer3D &, Bool_t *)
{ return kFALSE; }
#endif
//______________________________________________________________________________
void   TQtRootViewer3D::SetUpdatesEnabled(bool on)
{
   // Proxy to enable  / disable  the view widget updates
  if (fViewer) fViewer->SetUpdatesEnabled(on);
}
//______________________________________________________________________________
void  TQtRootViewer3D::UpdateView()
{
   BeginScene();
   EndScene();
}

//______________________________________________________________________________
void  TQtRootViewer3D::MakeViewerNil()
{
   fViewer = 0;
}
//______________________________________________________________________________
void  TQtRootViewer3D::SetFooter(const char *title)
{
   if (fViewer) fViewer->SetFooter(title);  
}

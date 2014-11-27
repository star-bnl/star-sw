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

#include <cassert>
//#include "TQtRootViewer3D.h"
#include "TQtRootCoinViewer3D.h"
#include "TQtCoinViewerImp.h"
#include "TObjectCoinViewFactory.h"

#include "TDataSetIter.h"

#include "TGQt.h"
#include "TEnv.h"
#include <qstringlist.h>
#include <Inventor/SoDB.h> 
#include <Inventor/SoInput.h> 

/*
#include "TObject3DView.h"

#include "TQtGLViewerImp.h"

#include "TList.h"
#include "TError.h"

#include  "TQtGLIncludes.h"

#include "TObjectOpenGLViewFactory.h"
#if QT_VERSION >= 0x40000
//Added by qt3to4:
#include <QPixmap>
#endif 
*/
/* QT_VERSION */

#include "TVirtualPad.h"

ClassImp(TQtRootCoinViewer3D)
//______________________________________________________________________________
TQtRootCoinViewer3D::TQtRootCoinViewer3D(TVirtualPad * pad)
	: TQtRootViewer3D(pad),fViewAll(kTRUE)
{ }

//______________________________________________________________________________
void   TQtRootCoinViewer3D::Viewer()
{
   if (!fViewer) {
   fView3DFactory = TObject3DViewFactoryABC::View3DFactory("oiv");
   if (!fView3DFactory) {
      SoDB::init();
      TObject3DViewFactoryABC::Registr(new TObjectCoinViewFactory(), "oiv");
      fView3DFactory = TObject3DViewFactoryABC::View3DFactory("oiv");
      // prepare the list of the search directories for "iv" files
      // that matches the list of the ROOO macros directories
      const char *macroPath = gEnv->GetValue("Root.MacroPath","");
      if (macroPath && macroPath[0]) {
          SoInput::removeDirectory(".");
#ifdef R__WIN32
         const char sep = ';';
#else
         const char sep = ':';
#endif
         TString path(macroPath); path.ReplaceAll("\\","/");
         QStringList macroPathList =  QStringList::split(QChar(sep),QString((const char*)path));
         for ( QStringList::Iterator it = macroPathList.begin(); it != macroPathList.end(); ++it ) 
         {
             SoInput::addDirectoryLast((const char*)(*it));
         }
      }
   }
   assert(fView3DFactory);
   fListOfPrimitives.SetViewFactory(fView3DFactory);
   
   fViewer = fPad ?
         new TQtCoinViewerImp(fPad,fPad->GetName(),fPad->UtoPixel(1.),fPad->VtoPixel(0.) )
         :
         new TQtCoinViewerImp(fPad,"Coint3DViewer");
         
#ifdef STAR_ONLINE_MONITOR      
      ((TQtCoinViewerImp *)fViewer)->move(0,0);
      ((TQtCoinViewerImp *)fViewer)->showMaximized();
#endif
      if (fViewer && !fDrawOption.IsNull()) fViewer->SetDrawOption(fDrawOption.Data());
      fDisconnectSlot = new SlotDisconnect(this);
      QObject::connect(&(fViewer->Signals()),SIGNAL(destroyed()), fDisconnectSlot, SLOT(DestroyMaster()));
      if (fPad) {
         fPad->Connect("Closed()","TQtRootCoinViewer3D", this, "DisconnectPad()");
         QObject::connect(gQt->Emitter(),SIGNAL(padPainted(QPixmap*)),fDisconnectSlot, SLOT(UpdateView(QPixmap*)));
      }
   }
   // printf("TQtRootCoinViewer3D::Viewer end\n");
}
//______________________________________________________________________________
TQtRootCoinViewer3D::~TQtRootCoinViewer3D()
{  }


//______________________________________________________________________________
void TQtRootCoinViewer3D::ClearPrimitives()
{
   if (fViewer)  {
      //fViewer->MakeCurrent();
      fViewer->SetUpdatesEnabled(FALSE);
      fViewer->Clear();
   }
   fListOfPrimitives.Delete();
   fListOfPrimitives.Clear();
}
//______________________________________________________________________________
void  TQtRootCoinViewer3D::CloseScene() 
{
   // called by EndScene
    TDataSetIter nextList(&fListOfPrimitives);
    TObject3DView *glo = 0;
    while( (glo = (TObject3DView *)nextList()  )) {
       // glo->ls(0); 
       fViewer->AddRootChild(glo->GetViewId()
                          , glo->IsSolid() ? TGLViewerImp::kSolid : TGLViewerImp::kWired);
    }
    if (fViewAll) { fViewer->ViewAll(); fViewAll = kFALSE; }
}
//______________________________________________________________________________
void  TQtRootCoinViewer3D::EndScene()
{
   // printf("TQtRootCoinViewer3D::EndScene\n");

   fViewer->SetUpdatesEnabled(FALSE);
   fViewer->Clear();

   CloseScene();

   fViewer->Update();
   fViewer->SetUpdatesEnabled(TRUE);

}
//______________________________________________________________________________
Int_t  TQtRootCoinViewer3D::AddRawObject(ULong_t placedID, UInt_t optMask)
{
   if (fViewer) {
      fViewer->AddRootChild(placedID,TGLViewerImp::EObject3DType(optMask));
   }
   return 0;
}

//______________________________________________________________________________
void   TQtRootCoinViewer3D::DisconnectPad()
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
void   TQtRootCoinViewer3D::Disconnect() 
{
   if (fPad)  {
      // fprintf(stderr," TQtRootViewer3D::Disconnect() fViewer=%p\n", fViewer);
      if (fViewer) {
         QObject::disconnect(&(fViewer->Signals()),SIGNAL(destroyed()), fDisconnectSlot, SLOT(DestroyMaster()));
          //QObject::disconnect(fViewer,SIGNAL(viewerAbout2Close()),fDisconnectSlot, SLOT(CleanPrimitives()));
         fViewer->DisconnectPad();
      }
      fPad->TQObject::Disconnect("Closed()",this, 0);
      fPad->TQObject::Disconnect("Modified()",this, 0);
#if ROOT_VERSION_CODE < ROOT_VERSION(5,07,00)
      TVirtualViewer3D *currentPadViewer = fPad->GetViewer3D(0);
#else
      TVirtualViewer3D *currentPadViewer = fPad->GetViewer3D((const char*)0);
#endif
      if (currentPadViewer == (TVirtualViewer3D *)this ) 
         fPad->ReleaseViewer3D();
   }
   fViewer = 0;
}


//______________________________________________________________________________
void  TQtRootCoinViewer3D::MakeViewerNil()
{
   fViewer = 0;
}

//______________________________________________________________________________
void TQtRootCoinViewer3D::Clear(Option_t *)
{
    ClearPrimitives();
}
//______________________________________________________________________________
void TQtRootCoinViewer3D::SetDrawOption(Option_t *option)
{ 
     fDrawOption = option;
     if (fViewer) fViewer->SetDrawOption(option);
}
//______________________________________________________________________________
Option_t   *TQtRootCoinViewer3D::GetDrawOption() const
{
   return fViewer ? fViewer->GetDrawOption()
                  :
                   (fDrawOption.IsNull() ? 0 : fDrawOption.Data());
}

/*
//______________________________________________________________________________
Int_t  TQtRootCoinViewer3D::AddObject(TObject *obj, Option_t* drawOption , Bool_t * addChildren )
{
    //return 0;
    return  TQtRootViewer3D::AddObject(obj,drawOption,addChildren );
}
//______________________________________________________________________________
 Int_t  TQtRootCoinViewer3D::AddObjectFirst(TObject *obj, Option_t* drawOption , Bool_t * addChildren )
{
    //return 0;
    return  TQtRootViewer3D::AddObjectFirst(obj,drawOption,addChildren );
}
*/

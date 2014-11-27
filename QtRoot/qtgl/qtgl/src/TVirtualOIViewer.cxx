//*-- Author :    Valery Fine(fine@bnl.gov)   29/12/96
 
 
//------------------------------------------------------------------------
// Copyright(c) 2001, V.Fine (STAR collaboration, BNL)
//
// Permission to use, copy, modify and distribute this software and its
// documentation for non-commercial purposes is hereby granted without fee,
// provided that the above copyright notice appears in all copies and
// that both the copyright notice and this permission notice appear in
// the supporting documentation. The authors make no claims about the
// suitability of this software for any purpose.
// It is provided "as is" without express or implied warranty.
//------------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TVirtualOIViewer                                                     //
//                                                                      //
// This base platform independ class to creates 3D view via             //
//  Open Inventor (tm) package                                          //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// #include <iostream.h>
#include "TVirtualOIViewer.h"
 
 
#ifndef R__QT
#include <Inventor/Xt/SoXt.h>
#include <Inventor/Xt/SoXtRenderArea.h>
#include <Inventor/Xt/viewers/SoXtExaminerViewer.h>
#include <Inventor/Xt/viewers/SoXtFlyViewer.h>
#include <Inventor/Xt/viewers/SoXtPlaneViewer.h>
#include <Inventor/Xt/viewers/SoXtWalkViewer.h>
#else
#include <Inventor/Qt/SoQt.h>
#include <Inventor/Qt/SoQtRenderArea.h>
#define SoXtRenderArea SoQtRenderArea

#include <Inventor/Qt/viewers/SoQtExaminerViewer.h>
#define SoXtExaminerViewer SoQtExaminerViewer

#include <Inventor/Qt/viewers/SoQtFlyViewer.h>
#define SoXtFlyViewer SoQtFlyViewer

#include <Inventor/Qt/viewers/SoQtPlaneViewer.h>
#define SoXtPlaneViewer SoQtPlaneViewer

// No WalkViewer for Coint3D
// #include <Inventor/Qt/viewers/SoQtWalkViewer.h>
// #define SoXtWalkViewer SoQtWalkViewer

#endif
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoSelection.h>
#include <Inventor/nodes/SoCallback.h>
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoTranslation.h>
#include <Inventor/nodes/SoCamera.h>

#include <Inventor/elements/SoCacheElement.h>

#include <Inventor/actions/SoSearchAction.h> 
#include <Inventor/actions/SoToVRML2Action.h>
#include <Inventor/actions/SoGetBoundingBoxAction.h>
#include <Inventor/actions/SoWriteAction.h>

#include <Inventor/VRMLnodes/SoVRMLGroup.h>
#include <Inventor/manips/SoTransformBoxManip.h>
#include <Inventor/manips/SoTabBoxManip.h>
#include <Inventor/annex/HardCopy/SoHardCopy.h>
#include <Inventor/annex/HardCopy/SoVectorizePSAction.h>
#include <Inventor/annex/HardCopy/SoVectorOutput.h>
 
#include "TSystem.h"
#include "TROOT.h"
#include "TApplication.h"
#include "TSeqCollection.h"
#include "TMap.h"
#include "TObjString.h"
#include "TError.h"
 
#include "Buttons.h"
#include "TQPadOpenGLView.h"
#include "TVirtualPad.h"
#include "TView.h"
#include "TQVirtualGL.h"
#include "TColor.h"

#include "TQtRootAction.h"

#ifdef R__QT
#include <qwidget.h>
#include <qmainwindow.h>
#include <qframe.h>
#include <qmenubar.h>
#include <qfiledialog.h>

#define Widget QWidget*
#endif

#define BORDER 10.0f
enum EVirtualOIViewerCommands {
    kFileGLOpen,
    kFileOpen,
    kFileAdd,
    kFileCloseOIViewer,
    kFileSave,
    kFileSaveAs,

    kViewerExamine,
    kViewerPlane,
    kMixGLView
};

static TQtBrowserMenuItem_t gMenu_Data[] = {  
//{ filename,      tooltip,            staydown,  id,              button}
/* File Menu */  
    { "&Open Inventor file",kFileOpen,		 Qt::CTRL+Qt::Key_O, "Open a new iv file",				        "" },
    { "&Add",		      	 kFileAdd,		 Qt::ALT +Qt::Key_A, "Add a new iv file to existing drawings","" },
    { "&Save",			       kFileSave,		 Qt::CTRL+Qt::Key_S, "Save current drawing to iv file ",		  "" },
    { "&Save As ...",		 kFileSaveAs,	 0, 		            "Save current drawing as ...  ",			  "" },
    { "&Close",             kFileCloseOIViewer,Qt::ALT +Qt::Key_F4,"Close the Open Inventor widget",  "" },
/* Viewer Menu */ 
    { "&Examine Viewer",	 kViewerExamine, Qt::ALT +Qt::Key_E, "Switch to examine viewer",				  "" },
    { "&Plane Viewer",		 kViewerPlane,	 Qt::CTRL+Qt::Key_P, "Switch to plane viewer",				     "" },
/* Option Menu */ 
    { "&Mix with OpenGL",	 kMixGLView,     Qt::ALT +Qt::Key_M, "Combine the Inventor view (from file) with the OpenGL view (memory resident",				  "" },
/* End of menu items */
    {0,0,0,"",""} 

}; 


//______________________________________________________________________________
// Inventor call back function
static void InventorCallback(void *d, SoAction *action)
{
   if (!d) return;
   TVirtualOIViewer *currentViewer = (TVirtualOIViewer *)d;
   if ( currentViewer ) {
      if (action->isOfType(SoGLRenderAction::getClassTypeId()) )
      {
         SoCacheElement::invalidate(action->getState());
         //glEnable(GL_COLOR_MATERIAL);
         //GLfloat ambientColor[] = {0.2, 0.2, 0.2, 1.0};
         //glMaterialfv(GL_FRONT,GL_AMBIENT,ambientColor);
         //GLfloat shininess[] = {0.5};
         //glMaterialfv(GL_FRONT,GL_SHININESS,shininess);
         currentViewer->Paint("");

         //glDisable(GL_COLOR_MATERIAL);

      }
      else if (action->isOfType(SoGetBoundingBoxAction::getClassTypeId()) )
      {

         Float_t minBound[3]={-1000,-1000,-1000};
         Float_t maxBound[3]={ 1000, 1000, 1000};

         currentViewer->GetGLView()->GetPad()->GetView()->GetRange(minBound,maxBound);
         if (minBound[0] == maxBound[0]) {
            SoCacheElement::invalidate(action->getState());
         }
         SoGetBoundingBoxAction *boundAction =  (SoGetBoundingBoxAction *)action;
         Float_t min = TMath::Min(TMath::Min(minBound[0],minBound[1]),minBound[2]);
         Float_t max = TMath::Max(TMath::Max(maxBound[0],maxBound[1]),maxBound[2]);
         boundAction->
            extendBy(SbBox3f(min,min,min,max,max,max));

         //boundAction->
         //   extendBy(SbBox3f(minBound[0],minBound[1],minBound[2]
         //   ,maxBound[0],maxBound[1],maxBound[2])
         //      );
             // I had to add the line below into 
             //  SoNode::GLRenderS:780 to supress the annoying warning
             // This is very dirty but I have to do that (Valeri Fine 24.05.2003 fine@bnl.gov)
             //    err =  GL_NO_ERROR;

         const SbVec3f &centerPoint =  boundAction->getBoundingBox().getCenter();
         boundAction->setCenter(centerPoint, false);
         float x,y,z;
         centerPoint.getValue(x,y,z);
         // printf(" new center = %f %f %f \n", x,y,z);
      }
   }
}

//______________________________________________________________________________
static void SelectCB(void * /*closure*/ , SoPath * p)
{
   SoPath * path = new SoPath(*p);
   path->ref();
   path->truncate(path->getLength()-1);
   SoSeparator * group = (SoSeparator *) path->getTail();
   if ( !group->getChild(0)->isOfType(SoTransform::getClassTypeId())) {
      group->insertChild(new SoTransform, 0);
   }	
   SoTransform * transform = (SoTransform *) group->getChild(0);
   path->append(transform);
   static SoTabBoxManip * manip = 0;
   // if (!manip) {
      manip = new SoTabBoxManip;
   //   manip->ref();
   // }
   manip->replaceNode(path);
   path->unref();
}

//______________________________________________________________________________
static void DeselectCB(void * /*closure*/, SoPath * p) 
{
   SoPath * path = new SoPath(*p);
   path->ref();
   path->truncate(path->getLength()-1);
   SoSeparator * group = (SoSeparator *) path->getTail();
   SoTabBoxManip * manip = (SoTabBoxManip *) group->getChild(0);
   if ( manip->isOfType(SoTabBoxManip::getClassTypeId()) ) {
      path->append(manip);
      manip->replaceManip(path, new SoTransform);
   }
   path->unref();
}

 
// ClassImp(TVirtualOIViewer)
 
 
//______________________________________________________________________________
TVirtualOIViewer::TVirtualOIViewer()  : fInventorViewer(0),fRootNode(0),fGLNode(0)
                                       ,fMaterial(0), fRootCallback(0)
                                       ,fWidth(0),fHeight(0),fTitle("TVirtualOIViewer")
                                       ,fMainWidget(0), fMenuBar(0)
{ }
//______________________________________________________________________________
TVirtualOIViewer::TVirtualOIViewer(TPadOpenGLView *padview,const char * /*title*/, UInt_t width, UInt_t height)
    : fInventorViewer(0),fRootNode(0),fGLNode(0), fMaterial(0), fRootCallback(0)
    , fWidth(width),fHeight(height),fTitle("title")
    , fMainWidget(0), fMenuBar(0)
{
   // Create browser with a specified width and height.
   fGLView  = padview;
   gVirtualGL->SetTrueColorMode();
   SetDrawList(0);
}
//______________________________________________________________________________
TVirtualOIViewer::TVirtualOIViewer(TPadOpenGLView *padview,const char * /*title*/, Int_t /*x*/, Int_t /*y*/,
                             UInt_t width, UInt_t height)  : fInventorViewer(0)
                            ,fRootNode(0),fGLNode(0), fMaterial(0), fRootCallback(0)
                            ,fWidth(width),fHeight(height),fTitle("title")
                            ,fMainWidget(0), fMenuBar(0)
{
   // Create browser with a specified width and height and at position x, y.
 
    fGLView  = padview;
    gVirtualGL->SetTrueColorMode();
    SetDrawList(0);
} 
//______________________________________________________________________________
TVirtualOIViewer::~TVirtualOIViewer()
{   // Coin3D GLViewer destructor.
    fRootNode->unref();
    QWidget *toKill = fMainWidget;
    fMainWidget = 0;
    if (toKill ) {
       delete toKill;
       DeleteContext();
    }
}
//______________________________________________________________________________
void  TVirtualOIViewer::CreateContext()
{
   CreateViewer();
}

//______________________________________________________________________________
SoNode *TVirtualOIViewer::CreateSceneNode(const char* fileDecor, bool withGL)
{
   //
   // Create 2 Open Iventor nodes:
   // ROOT G3D OpenGL objects
   // and "decoration" from "root.iv"
   //
   if (!fRootNode) {
     fRootNode = new SoSeparator();
     fRootNode->ref();
 #if 0
     if(withGL) {

        fGLNode = new SoSeparator();
        fRootNode->addChild(fGLNode);

        fMaterial = new SoMaterial();
        fGLNode->addChild(fMaterial);

        fRootCallback = new SoCallback;
        fGLNode->addChild(fRootCallback);

        fRootCallback->setCallback(InventorCallback,this);
     }
#endif     
     SoInput viewDecor;
     if (fileDecor && fileDecor[0] && !gSystem->AccessPathName(fileDecor) && viewDecor.openFile(fileDecor)) {
        SoSeparator *extraObjects = SoDB::readAll(&viewDecor);
        if (extraObjects) {
           SoSelection *selection = new SoSelection;
           selection->addSelectionCallback(SelectCB, NULL); 
           selection->addDeselectionCallback(DeselectCB, NULL); 
           selection->addChild(extraObjects);
           fRootNode->addChild(selection);
        }
     }
     if(withGL) {

        fGLNode = new SoSeparator();
        fRootNode->addChild(fGLNode);

        fMaterial = new SoMaterial();
        fGLNode->addChild(fMaterial);

        fRootCallback = new SoCallback;
        fGLNode->addChild(fRootCallback);

        fRootCallback->setCallback(InventorCallback,this);
     }

   }  
   return fRootNode;
}

//______________________________________________________________________________
void TVirtualOIViewer::CreateViewer() {
   if (!fInventorViewer) 
      CreateViewer((const char *)fTitle,fWidth,fHeight);
}

//______________________________________________________________________________
void TVirtualOIViewer::CreateViewer(const char *title,UInt_t width, UInt_t height)
{
   static void *topWindow = 0;
   if (!fGLView) return;
   // assert(fInventorViewer);
   if (!topWindow) topWindow = SoQt::init(gApplication->Argv(0));
   assert(!fInventorViewer);
   TVirtualPad *thisPad = fGLView->GetPad();
   if (thisPad) {
      fMainWidget = new QMainWindow(0,"Coin3dViewer",Qt::WDestructiveClose|Qt::WType_TopLevel);
      connect(fMainWidget,SIGNAL(destroyed()),this,SLOT(Disconnect()));
      fMainWidget->setCaption(title);
      QFrame *glFrame = new QFrame(fMainWidget);
      fMainWidget->setCentralWidget (glFrame);

      MakeActions();
      MakeMenu();

      fInventorViewer = ViewerFactory(glFrame);

      QString caption = thisPad->GetTitle();
      //fSaveFile = thisPad->GetName();
      //fSaveFile += ".";
      //fSaveFile += "jpg";
      caption += ": Coin3D viewer";
      fMainWidget->setCaption(caption);

      //  Pick the background color
      Color_t color = thisPad->GetFillColor();
      TColor *background = gROOT->GetColor(color);
      if (background) {
         float rgb[3];
         background->GetRGB(rgb[0],rgb[1],rgb[2]);
         fInventorViewer->setBackgroundColor(SbColor(rgb));
      }

      // set Invetor widget
      // check for the special file from the GeomViewer this is a tempo solution
      const char *specilaFileName = "GeomBrowser_tmp.iv";
      if (gSystem->AccessPathName(specilaFileName)) 
         fInventorViewer->setSceneGraph(CreateSceneNode());
      else
         fInventorViewer->setSceneGraph(CreateSceneNode(specilaFileName,false));

      // fInventorViewer->setTitle(title);
      // fInventorViewer->setSize(SbVec2s((short)width,(short)height));
      fMainWidget->resize(width,height);
      fMainWidget->show();
      InitGLWindow();
   }
}

//______________________________________________________________________________
void TVirtualOIViewer::InitGLWindow()
{
   // X11 specific code to initialize GL window.
   if (fInventorViewer) {
     gVirtualGL->SetTrueColorMode();
   }
}
//______________________________________________________________________________
void TVirtualOIViewer::MakeActions() 
{
   int i=0;
   while (gMenu_Data[i].fMenuText!=NULL) {
      // skip the separators 
      TQtRootAction *action = new TQtRootAction(fMainWidget, gMenu_Data[i]);
      fActions.insert(action->Id(),action);
      connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
      i++;
   }
}
//______________________________________________________________________________
void TVirtualOIViewer::MakeMenu()
{
   if (fMainWidget) {
      if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
      QMenuBar   *mainMenu = fMainWidget->menuBar();
      fMenuBar = mainMenu;

      // Int_t iMainMenuStart = i;
      QPopupMenu *fileMenu      = new QPopupMenu();
      mainMenu->insertItem("&File",fileMenu);
      QPopupMenu *viewerMenu      = new QPopupMenu();
      mainMenu->insertItem("&Viewer",viewerMenu);

      fActions[kFileOpen]		->addTo(fileMenu);  
      fActions[kFileAdd]		->addTo(fileMenu);      
      fActions[kFileCloseOIViewer]		->addTo(fileMenu);      
                                        fileMenu->insertSeparator();
      //   fActions[kFileSave]   	->addTo(fileMenu);
      fActions[kFileSaveAs]   	->addTo(fileMenu);
      fActions[kFileSaveAs]->setEnabled(false);
      QPopupMenu *optionMenu     = new QPopupMenu();
      mainMenu->insertItem("&Option",optionMenu);
      fActions[kMixGLView]	->addTo(optionMenu);  
          fActions[kMixGLView]->setToggleAction(true);
          fActions[kMixGLView]->setOn(true);


      //   fActions[kViewerExamine]     ->addTo(viewerMenu);      
      //   fActions[kViewerPlane]   	->addTo(viewerMenu);
   }
}

//______________________________________________________________________________
void TVirtualOIViewer::Paint(Option_t *)
{
   CreateViewer();
  // Paint G3D objects  
   Int_t myGLIst = GetGLView()->GetGLList()-1;
   assert(myGLIst );
   if (myGLIst) {

     // Pick the current TPad rotation
     // vf disable the rotation to match the file gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kView);

     // Draw G3D objects
     gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kModel);
   }
}

//______________________________________________________________________________
void  TVirtualOIViewer::ProcessMessage()
{
   TQtRootAction *actionSender = (TQtRootAction *)sender ();
   switch (actionSender->Id()) {
   case kFileOpen:         FileOpenCB();   	 break;
   case kFileAdd:          FileAddCB();   	 break;
   case kFileCloseOIViewer: FileCloseCB();    break;
   case kFileSave:         FileSaveCB();   	 break;
   case kFileSaveAs:       FileSaveAsCB();  	 break;

   case kViewerExamine:    ViewerExamineCB(); break;
   case kViewerPlane:      ViewerPlaneCB();   break;
 
   default:    break;

   };
}

//______________________________________________________________________________
void TVirtualOIViewer::Update()
{  
 //  SoXtExaminerViewer *exViewer = dynamic_cast<SoXtExaminerViewer *>(fInventorViewer);
   CreateViewer();
   // vf if (!fInventorViewer) CreateViewer();
   SoXtExaminerViewer *exViewer = (SoXtExaminerViewer *)fInventorViewer;
   if (exViewer) {
     if (exViewer->isAnimating()) exViewer->stopAnimating();
     // -- do not reset to the home position 
     //  vf 20122004 exViewer->resetToHomePosition();
   }
   gVirtualGL->SetRootLight(kFALSE);
   // The Coin SoGLRenderAction class has been patched to suspend the annoying message
   // vf patch to remove the annoying message
   //
   //SoGLRenderAction::beginTraversal(SoNode * node)
   //{
   //  if (THIS->isrendering) {
   //    inherited::beginTraversal(node);
   //    return;
   //  }
   //
   // // vf 23.05.2003  int err_before_init = glGetError();
   //    err_before_init = GL_NO_ERROR; // vf 23.05.2003

   fInventorViewer->render();
}
 //______________________________________________________________________________
 ULong_t TVirtualOIViewer::GetViewerID() const
 {
  return ULong_t((QMainWindow *)fMainWidget);
 }
 
//______________________________________________________________________________
SoXtRenderArea *TVirtualOIViewer::ViewerFactory(void *widget, Int_t viewerType)
{
  // Select Open Inventor viewers by type index 
  SoXtRenderArea *viewer  = 0;
  switch (viewerType) {
    case 1:
      viewer = new SoXtExaminerViewer((Widget) widget);
      break;
    case 2:
      viewer = new SoXtFlyViewer((Widget) widget);
      break;
    case 3:
      viewer = new SoXtPlaneViewer((Widget) widget);
      break;
    case 4:
#ifndef R__QT
       // Coin3D: TMP DISABLED: walkviewer not properly implemented yet. 20020624 mortene.
      viewer = new SoXtWalkViewer((Widget) widget);
      break;
#endif      
    default:
      viewer = new SoXtExaminerViewer((Widget) widget);
      break;
  };
  return viewer;
}

//______________________________________________________________________________
void TVirtualOIViewer::Disconnect()
{
   // The user has destroyed the widget and it killed itself   
   fMainWidget = 0;
   DeleteContext();
   delete this;
}

//______________________________________________________________________________
void TVirtualOIViewer::InsertData(const char* filename, bool withGL) 
{
   if(fRootNode) { 
      fRootNode->unref();
      fRootNode = 0;
   }
   fInventorViewer->setSceneGraph(CreateSceneNode(filename, withGL));
}

//______________________________________________________________________________
void TVirtualOIViewer::FileOpenCB() 
{
   QString fn = QFileDialog::getOpenFileName( QString::null, "Inventor (*.iv);;WRML (*.wrl)", 0);
   if(!fn.isEmpty())
      InsertData(fn.ascii(),fActions[kMixGLView]->isOn());
}
//______________________________________________________________________________
void TVirtualOIViewer::FileAddCB() 
{
   QString fn = QFileDialog::getOpenFileName( QString::null, "Inventor (*.iv);;WRML (*.wrl)", 0);
   SoInput input;
   if(fn && input.openFile(fn.ascii()) ) {
      fRootNode->addChild(SoDB::readAll(&input));		    
   } else {
      printf("Error Open File\n");
   }
}
//______________________________________________________________________________
void TVirtualOIViewer::FileCloseCB() 
{
   if (fMainWidget) {
      fMainWidget->setUpdatesEnabled (false);
      fMainWidget->close();
   } else {
      delete this;
   }
}
//______________________________________________________________________________
void TVirtualOIViewer::SavePostScript(const char *filename) 
{

   SoVectorizePSAction * va = new SoVectorizePSAction;    
   SoVectorOutput * out = va->getOutput();

   if (out->openFile(filename)) 
   {
      SbVec2s vpsize = fInventorViewer->getViewportRegion().getViewportSizePixels();
      float vpratio = ((float)vpsize[0]) / ((float)vpsize[1]); 

      // 
      if (vpratio > 1.0f) {
         va->setOrientation(SoVectorizeAction::LANDSCAPE);
         vpratio = 1.0f / vpratio;
      }
      else {
         va->setOrientation(SoVectorizeAction::PORTRAIT);
      }

      va->beginStandardPage(SoVectorizeAction::A4, BORDER);

      // try to fill as much "paper" as possible

      // FIXME: consider making getPageSize() public 
      //SbVec2f size = va->getPageSize();
      SbVec2f size = SbVec2f(210.0f - BORDER*2.0f, 
         297.0f - BORDER*2.0f);

      float pageratio = size[0] / size[1];
      float xsize, ysize;

      if (pageratio < vpratio) {
         xsize = size[0];
         ysize = xsize / vpratio;
      }
      else {
         ysize = size[1];
         xsize = ysize * vpratio;
      }

      float offx = BORDER + (size[0]-xsize) * 0.5f;
      float offy = BORDER + (size[1]-ysize) * 0.5f;

      va->beginViewport(SbVec2f(offx, offy), SbVec2f(xsize, ysize));
      va->calibrate(fInventorViewer->getViewportRegion());

      fprintf(stdout,"Vectorizing...");
      fflush(stdout);

      va->apply(fInventorViewer->getSceneManager()->getSceneGraph());
      fprintf(stdout,"done\n");
      fflush(stdout);

      fprintf(stdout,"Creating postscript file (%s)...", filename);
      fflush(stdout);
      va->endViewport();    
      va->endPage();
      out->closeFile();

      fprintf(stdout,"done\n");
      fflush(stdout);
      delete va;
   } else {
      fprintf(stderr,"Unable to open %s for writing\n", filename);
   }
}

//______________________________________________________________________________
void TVirtualOIViewer::SaveFile(const char* filename) 
{
   if(filename) fFileName = filename;
   if(fFileName) 
   {
      if(fFileName.right(3) == ".ps") {
         SavePostScript(fFileName.ascii());
      } else { 
         SoOutput output; SoNode *r = 0;
         if ( output.openFile(fFileName.ascii()) && (r = fInventorViewer->getSceneGraph()) )
         {
            SoWriteAction wa(&output);
            wa.apply(r);
}  }  }  }

//______________________________________________________________________________
void TVirtualOIViewer::FileSaveCB() 
{
    SaveFile();           	
}

//______________________________________________________________________________
void TVirtualOIViewer::FileSaveAsCB() 
{
   fFileName = QFileDialog::getSaveFileName( QString::null, "Inventor (*.iv);;Post Script (*.ps)", 0);
   SaveFile(fFileName.ascii());
}
//______________________________________________________________________________
void TVirtualOIViewer::ViewerPlaneCB() {
/*
    delete fInventorViewer;
    fInventorViewer = ViewerFactory(GLFr, 3);
    fInventorViewer->setSceneGraph(CreateSceneNode());
    Update();
   fMainWidget->show();
   InitGLWindow();
*/
}

//______________________________________________________________________________
void TVirtualOIViewer::ViewerExamineCB() {
/*
printf("%p\n", GLFr);
    delete fInventorViewer;
    fInventorViewer = ViewerFactory(GLFr);
    fInventorViewer->setSceneGraph(CreateSceneNode());
   Update();
fMainWidget->show();
InitGLWindow();
*/
}

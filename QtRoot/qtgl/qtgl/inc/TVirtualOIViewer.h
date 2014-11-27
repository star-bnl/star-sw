// @(#)root/gl:$Name:  $:$Id: TVirtualOIViewer.h,v 1.5 2013/08/30 16:00:17 perev Exp $
// Author: Valery Fine  5/19/2001

#ifndef ROOT_TVirtualOIViewer
#define ROOT_TVirtualOIViewer

//------------------------------------------------------------------------------
// Copyright(c) 2001, V.Fine (STAR collaboration, BNL)
//
// Permission to use, copy, modify and distribute this software and its
// documentation for non-commercial purposes is hereby granted without fee,
// provided that the above copyright notice appears in all copies and
// that both the copyright notice and this permission notice appear in
// the supporting documentation. The authors make no claims about the
// suitability of this software for any purpose.
// It is provided "as is" without express or implied warranty.
//------------------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TVirtualOIViewer                                                     //
//                                                                      //
// This class is based platformindepened class to create                //
// Open Inventor viewer for g3d class objects                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef ROOT_TGLViewerImp
#include "TGLViewerImp.h"
#endif

#ifndef GL_RootGLU
# include "TRootGLU.h"
#endif

#include "TString.h"

#include <qobject.h>
class SoSeparator;
class SoNode;
class SoMaterial;
class SoCallback;
#ifndef R__QT
 class SoXtRenderArea;
#else
 class QMainWindow;
 class QMenuBar;
 class SoQtRenderArea;
#endif

#include <qintdict.h>
class TQtRootAction;

class TVirtualOIViewer : public QObject, public TGLViewerImp {
Q_OBJECT


private:
   QIntDict<TQtRootAction> fActions; 

protected:

#ifndef R__QT
   SoXtRenderArea         *fInventorViewer;
#else
   SoQtRenderArea         *fInventorViewer;
#endif
   SoSeparator            *fRootNode;
   SoSeparator            *fGLNode;
   SoMaterial             *fMaterial;
   SoCallback             *fRootCallback;
   UInt_t                  fWidth;  // to postpont the widget construction
   UInt_t                  fHeight; // to postpont the widget construction
   QString                 fTitle;  // to postpont the widget construction
   QString		            fFileName; // keep the file name

   // main window should be done aside 
   QMainWindow       *fMainWidget;
   QMenuBar          *fMenuBar;


   SoNode  *CreateSceneNode(const char* fileDecor="root.iv", bool withGL = true);
   void     InitGLWindow();
   void     DeleteGLWindow();
#ifndef R__QT
   static   SoXtRenderArea *ViewerFactory(void *widget=0, Int_t viewerType=1);
#else
   static   SoQtRenderArea *ViewerFactory(void *widget=0, Int_t viewerType=1);
#endif
                TVirtualOIViewer();
           void CreateViewer();
   virtual void MakeActions(); 
   virtual void MakeMenu(); 
           void SavePostScript(const char *filename);
  
public:
   TVirtualOIViewer(TPadOpenGLView *pad, const char *title="OpenInventor Viewer", UInt_t width = 600, UInt_t height = 600);
   TVirtualOIViewer(TPadOpenGLView *pad, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height);
   virtual ~TVirtualOIViewer();

   virtual void  CreateContext();
   virtual void  DeleteContext(){;}

   virtual void  MakeCurrent(){;}
   virtual void  SwapBuffers(){;}

   virtual void  Iconify() { }
   virtual void  Show() { }
   virtual void  Paint(Option_t * = "");
   virtual void  Update();
   
   virtual ULong_t GetViewerID() const;

   // overridden from TGMainFrame
   virtual void  CloseWindow(){;}
   virtual void  CreateViewer(const char *title,UInt_t width, UInt_t height);

// Own methods
    virtual void     SetFilename(const char* fn) { fFileName = fn; }
    virtual QString &getFilename() { return fFileName; }
    virtual void  InsertData(const char* filename, bool withGL = true);
    virtual void  SaveFile(const char *filename = 0);

public slots:
  virtual void Disconnect();

  
  void ProcessMessage();

  void FileOpenCB();
  void FileAddCB();
  void FileSaveCB();
  void FileSaveAsCB();
  void FileCloseCB();

  void ViewerExamineCB(); 
  void ViewerPlaneCB();

   //ClassDef(TVirtualOIViewer,0)  // ROOT OpenInventor viewer
};
#endif

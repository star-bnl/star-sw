// @(#)root/x3d:$Name:  $:$Id: TQtViewerX3D.cxx,v 1.3 2013/08/30 16:00:29 perev Exp $
// Author: Valeri Fine   30/10/02
/****************************************************************************
 ** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
 **                                    All rights reserved.
 ** This file may be distributed under the terms of the Q Public License
 ** as defined by Trolltech AS of Norway and appearing in the file
 ** LICENSE.QPL included in the packaging of this file.
 **
 *****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TViewerX3D                                                           //
//                                                                      //
// C++ interface to the X3D viewer                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "TQtViewerX3D.h"
#include "TQtX3DWidget.h"
#include "TGQt.h"
#include "TSystem.h"
#include "TAtt3D.h"
#include "X3DBuffer.h"
#include "TVirtualPad.h"
#include "TView.h"
#include "TMath.h"
#include "TROOT.h"
#include "TClass.h"

#include "TRootHelpDialog.h"
#include "qaction.h"
#include "qapplication.h"
#include "qclipboard.h"
#include "qimage.h"
#include "qmessagebox.h"
#include "qpopmenu.h"
#include "qfiledialog.h"
#include "qmenubar.h"
#include "qevent.h"
#include "qpainter.h"
#include "qprinter.h"
#include "qpixmap.h"

#include <X11/Xlib.h>

#include "HelpText.h"

const char gHelpX3DViewer[] = "\
     PRESS \n\
     \tw\t--- wireframe mode\n\
     \te\t--- hidden line mode\n\
     \tr\t--- hidden surface mode\n\
     \tu\t--- move object down\n\
     \ti\t--- move object up\n\
     \to\t--- toggle controls style\n\
     \ts\t--- toggle stereo display\n\
     \td\t--- toggle blue stereo view\n\
     \tf\t--- toggle double buffer\n\
     \th\t--- move object right\n\
     \tj\t--- move object forward\n\
     \tk\t--- move object backward\n\
     \tl\t--- move object left\n\
     \tx a\t--- rotate about x\n\
     \ty b\t--- rotate about y\n\
     \tz c\t--- rotate about z\n\
     \t1 2 3\t--- autorotate about x\n\
     \t4 5 6\t--- autorotate about y\n\
     \t7 8 9\t--- autorotate about z\n\
     \t[ ] { }\t--- adjust focus\n\n\
     HOLD the left mouse button and MOVE mouse to ROTATE object\n\n\
";

extern "C" {
  long     x3d_main(Float_t *longitude, Float_t *latitude, Float_t *psi,
                    Option_t *option, Window_t parent);
  void     x3d_set_display(Display_t display);
  int      x3d_dispatch_event(Handle_t event);
  void     x3d_update();
  void     x3d_get_position(Float_t *longitude, Float_t *latitude, Float_t *psi);
  int      x3d_exec_command(int px, int py, char command);
  void     x3d_terminate();
}

Bool_t TQtViewerX3D::fgActive = kFALSE;

//______________________________________________________________________________
TQtX3DWidget::TQtX3DWidget(Float_t *longitude, Float_t *latitude, Float_t *psi,QWidget *parent,Option_t *option)
   : QWidget(parent,"x3dWidget")
{
   // Create a canvas container.
   // Call 'x3d' package
   setWFlags(getWFlags () | Qt::WRepaintNoErase | Qt:: WResizeNoErase );
   setBackgroundMode(Qt::NoBackground);
   x3d_set_display(Long_t(QPaintDevice::x11AppDisplay ()));
   fX3DWin = (Long_t)x3d_main(longitude, latitude, psi,option, winId());
}

//______________________________________________________________________________
void TQtX3DWidget::mouseReleaseEvent(QMouseEvent *ev){
  if (ev->button() == Qt::LeftButton) {
     Float_t longitude_rad, latitude_rad, psi_rad;
     x3d_get_position(&longitude_rad, &latitude_rad, &psi_rad);
     ev->accept () ;
     emit x3dPosition(longitude_rad, latitude_rad, psi_rad);
  }
} 
//______________________________________________________________________________
void TQtX3DWidget::paintEvent ( QPaintEvent *ev ) { 
    fprintf(stderr
           ," QtX3DWidget::paintEvent ( QPaintEvent *ev ) erased = %d\n"
           ,ev->erased());
  // vf  if (fX3DWin) x3d_update(); 
}
//______________________________________________________________________________
void TQtX3DWidget::resizeEvent ( QResizeEvent *ev )
{ QWidget::resizeEvent(ev); }
//______________________________________________________________________________
bool TQtX3DWidget::x11Event(XEvent *ev) {
  // Dispatch X11 event for the child x3d window
   if (!fX3DWin) return FALSE;
   bool res = TRUE;
   switch (ev->type ) {
      case  Expose:
         (*ev).xexpose.window = fX3DWin;
      case  KeyPress:
      case  ColormapNotify:
         x3d_dispatch_event(Long_t(ev));        
         break;
      case  MotionNotify:
         if ( (*ev).xmotion.state & Button1Mask ) { 
             x3d_dispatch_event(Long_t(ev));       
         } 
         break;
      case  ConfigureNotify:
         (*ev).xconfigure.window = fX3DWin;
         x3d_dispatch_event(Long_t(ev));        
         break;
      default:
         res = FALSE;
         break;
   };
   return res;
}
//______________________________________________________________________________
TQtX3DWidget::~TQtX3DWidget(){ fX3DWin = 0; x3d_terminate(); }   
//______________________________________________________________________________
int TQtX3DWidget::execCommand(int px, int py, char command) 
{
   return x3d_exec_command(px,py,command);
}


ClassImp(TQtViewerX3D)
//______________________________________________________________________________
TQtViewerX3D::TQtViewerX3D():QMainWindow(){}
//______________________________________________________________________________
TQtViewerX3D::TQtViewerX3D(TVirtualPad *pad, Option_t *option, const char *title,
                       UInt_t width, UInt_t height)
   : QMainWindow(0,"x3dviewer", Qt::WDestructiveClose)
     ,fPad(0),fOption(option), fSaveType("JPEG")
{
   // Create ROOT X3D viewer.

   if (fgActive) {
      QMessageBox::warning(0, "X3D Viewer Warning"
                            , "Can have only one X3D viewer active");
      return;
   }
   if (pad) {
      fPad    = pad;
      fSaveFile = fPad->GetName();
      fSaveFile += ".";
      fSaveFile += "jpg";
      QString caption = fPad->GetTitle();
      caption += ": X3D viewer";
      setCaption(caption);
      resize(width, height);
      CreateViewer(title);
      show();

      fgActive = kTRUE;
   }
}
//______________________________________________________________________________
TQtViewerX3D::TQtViewerX3D(TVirtualPad *pad, Option_t *option, const char *title,
                       Int_t x, Int_t y, UInt_t width, UInt_t height)
   : QMainWindow(0,"x3dviewer",Qt::WDestructiveClose | Qt::WRepaintNoErase | Qt:: WResizeNoErase )
     ,fPad(0),fOption(option), fSaveType("JPEG")
{
   // Create ROOT X3D viewer.

   if (fgActive) {
      QMessageBox::warning(0, "X3D Viewer Warning"
                            , "Can have only one X3D viewer active");
      return;
   }
   if (pad) {
      fPad    = pad;
      fSaveFile = fPad->GetName();
      fSaveFile += ".";
      fSaveFile += "jpg";
      QString caption = fPad->GetTitle();
      caption += ": X3D viewer";
      setCaption(caption);

      setGeometry(x, y, width, height);
      CreateViewer(title);
      show();

      fgActive = kTRUE;
   }
}
//______________________________________________________________________________
TQtViewerX3D::~TQtViewerX3D()
{
   // Delete ROOT X3D viewer.

   if (!fPad) return;
   fgActive = kFALSE;
}
//______________________________________________________________________________
//
// Slots
//______________________________________________________________________________
void TQtViewerX3D::Disconnect()
{
   // Disconnect the source TPad from this widget
   fPad = 0;
}

//______________________________________________________________________________
void TQtViewerX3D::NewViewer(){
#if  ROOT_VERSION_CODE >= ROOT_VERSION(4,03,3)   
      fprintf(stderr,"  There is 3D viewer implementation with the Qt layer for ROOT 4.03 yet !!!\n");
#else   
   if (fPad) fPad->x3d();   
#endif  
}
//______________________________________________________________________________
void TQtViewerX3D::PrintCB(){
   QPrinter p;
   QWidget *c = centralWidget();
   if (c && p.setup()) {
      QPainter pnt(&p);
      pnt.drawPixmap(0,0,QPixmap::grabWindow( c->winId () ));
      pnt.end();
   }
}
//______________________________________________________________________________
void TQtViewerX3D::CopyCB()
{
 //  Copy the current x3d view to the system clipboard 
   QWidget *c = centralWidget();
   if (c) { 
      QClipboard *cb = QApplication::clipboard();
      cb->setPixmap(QPixmap::grabWindow(c->winId () )) ;
   }
}
//______________________________________________________________________________
void TQtViewerX3D::CopyFrameCB()
{
   // Copy the entire window including the menu and the status bar
   QClipboard *cb = QApplication::clipboard();
   cb->setPixmap(QPixmap::grabWidget(topLevelWidget()));
}
//______________________________________________________________________________
void TQtViewerX3D::SaveCB()
{ 
   QWidget *c = centralWidget();
   if (!c) return;
   if (fSaveFile.IsNull()) 
      SaveAsCB();
   else 
      QPixmap::grabWindow( c->winId () ).save(fSaveFile.Data(),fSaveType.Data());
}
//______________________________________________________________________________
void TQtViewerX3D::SaveAsCB()
{ 
   QWidget *c = centralWidget();
   if (!c) return;
   QString filter = 
     "Image (";

   QString defExtension[] = {"bmp","C"};

   UInt_t i;
   for (i = 0; i < QImageIO::outputFormats().count(); i++ ) 
   {
      if (i) filter +=',';
      filter += "*.";
      QString str = QString( QImageIO::outputFormats().at( i ) );
      filter += str.lower();
   }
   filter +=");;";

   QString selectedFilter;

   QString thatFile = QFileDialog::getSaveFileName(gSystem->WorkingDirectory()
    , filter, centralWidget(), "SaveAs"
    , "Save the current x3d view as"
    , &selectedFilter);

   if (thatFile.isEmpty()) return;
 
   UInt_t nExt = sizeof(defExtension)/sizeof(const char *);
   QString e;
   for (i = 0; i < nExt-1; i++) {
     e = '.'; e += defExtension[i];
     if (selectedFilter.contains(e)) break;
   }

   if (! thatFile.contains('.'))  thatFile += '.';
   if (thatFile.at(thatFile.length()-1) == '.')  thatFile += defExtension[i];

   fSaveFile = (const char *)thatFile;
   QPixmap::grabWindow( c->winId () ).save(fSaveFile.Data(),fSaveType.Data());
}
//______________________________________________________________________________
void TQtViewerX3D::AboutCB()
{ 
   QMessageBox::aboutQt(0);
   QString rootVersion = "ROOT ";
   rootVersion += gROOT->GetVersion();
   rootVersion += "with Qt interface";
   QMessageBox::about(0,rootVersion
     ,"ROOT Qt interface Copyright (C) 2001-2003, Valeri Fine. Brookhaven National Laboratory. All right reserved.");
}
//______________________________________________________________________________
void TQtViewerX3D::HelpCB()
{
   QMessageBox::information(0,"x3d key commands",gHelpX3DViewer);
}
//______________________________________________________________________________
void TQtViewerX3D::CreateViewer(const char * /*name*/)
{
   MakeMenu();
   setWFlags(getWFlags () | Qt::WRepaintNoErase | Qt:: WResizeNoErase );
   setBackgroundMode(Qt::NoBackground);
   InitX3DWindow();
}
//______________________________________________________________________________
void TQtViewerX3D::MakeMenu()
{
   // Create a "save" action
   QAction *saveAction =  new QAction("Save", "&Save", CTRL+Key_S, this, "save" );
   connect ( saveAction, SIGNAL( activated() ) , this, SLOT( SaveCB() ) );

   const char * saveText = "<p><img source=\"save\"> "
                "Click this button to save a <em>3D image</em>to the current image file. <br>"
                "You can also select the <b>Save</b> command "
                "from the <b>Fine</b> menu.</p>";
   saveAction->setWhatsThis( saveText );

   // Create a "save as" action
   QAction *saveAsAction =  new QAction("SaveAs", "Save As", CTRL+Key_A, this, "saveas" );
   connect ( saveAsAction, SIGNAL( activated() ) , this, SLOT( SaveAsCB() ) );

   const char * saveAsText = "<p><img source=\"save\"> "
                "Click this button toselect file and save a <em>3D image</em>there. <br>"
                "You can also select the <b>Save As</b> command "
                "from the <b>Fine</b> menu.</p>";
   saveAsAction->setWhatsThis( saveAsText );
 
   // Create a "print" action
   QAction *printAction =  new QAction("Print", "&Print", CTRL+Key_P, this, "print" );
   connect ( printAction, SIGNAL( activated() ) , this, SLOT( PrintCB() ) );

   const char * printText = "<p><img source=\"print\"> "
                "Click this button to print a <em>3D image</em>. <br>"
                "You can also select the <b>Print</b> command "
                "from the <b>File</b> menu.</p>";
   printAction->setWhatsThis( printText );

   // Create a "copy" action
   QAction *copyAction =  new QAction("Copy", "&Copy", CTRL+Key_C, this, "copy" );
   connect ( copyAction, SIGNAL( activated() ) , this, SLOT( CopyCB() ) );

   const char * copyText = "<p><img source=\"copy\"> "
                "Click this button to copy a <em>3D image</em>to the system clipborad. <br>"
                "You can also select the <b>Copy</b> command "
                "from the <b>Edit</b> menu.</p>";
   copyAction->setWhatsThis( copyText );
 
    // Create a "copy frame" action
   QAction *copyFrameAction =  new QAction("Frame", "Copy &Frame", CTRL+Key_F, this, "frame" );
   connect ( copyFrameAction, SIGNAL( activated() ) , this, SLOT( CopyFrameCB() ) );
   const char * copyFrameText = "<p><img source=\"frame\"> "
                "Click this button to copy a <em>tge frame of the 3D image</em>to the system clipborad. <br>"
                "You can also select the <b>Copy Frame</b> command "
                "from the <b>Edit</b> menu.</p>";
   copyFrameAction->setWhatsThis( copyFrameText );

   // Create "close" action
   QAction *fileCloseAction = new QAction( "Close", "&Close", ALT+Key_X, this,
      "close" );
   connect ( fileCloseAction, SIGNAL( activated() ) , this,  SLOT( close() ) );

   
   QMenuBar   *mainMenu = menuBar();

   // -- populate the menu bar
      QPopupMenu *fileMenu      = new QPopupMenu();
      mainMenu->insertItem("&File",fileMenu);
 
      QPopupMenu *editMenu      = new QPopupMenu();
      mainMenu->insertItem("&Edit",editMenu);

      QPopupMenu *helpMenu   = new QPopupMenu();
      mainMenu->insertItem("&Help",helpMenu);
  // -- The menu bar has been completed

 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  fileMenu
   
   saveAction     ->addTo(fileMenu);
   saveAsAction   ->addTo(fileMenu);
                    fileMenu->insertSeparator();
   printAction    ->addTo(fileMenu);
                    fileMenu->insertSeparator();
   fileCloseAction->addTo(fileMenu);


 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  editMenu
    copyAction     ->addTo(editMenu);
    copyFrameAction->addTo(editMenu);
   
 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  helpMenu
    helpMenu->insertItem("&Help",this,SLOT(HelpCB()));
    helpMenu->insertSeparator();
    helpMenu->insertItem("&About",this,SLOT(AboutCB()));
}
//______________________________________________________________________________
Int_t TQtViewerX3D::ExecCommand(int px, int py, char command)
{
// This function may be called from a script to animate an X3D picture
// px, py  mouse position
//command = 0       --- move to px,py
//        = w       --- wireframe mode   
//        = e       --- hidden line mode   
//        = r       --- hidden surface mode   
//        = u       --- move object down   
//        = i       --- move object up   
//        = o       --- toggle controls style   
//        = s       --- toggle stereo display   
//        = d       --- toggle blue stereo view   
//        = f       --- toggle double buffer   
//        = h       --- move object right   
//        = j       --- move object forward   
//        = k       --- move object backward   
//        = l       --- move object left   
//        = x a     --- rotate about x   
//        = y b     --- rotate about y   
//        = z c     --- rotate about z   
//        = 1 2 3   --- autorotate about x   
//        = 4 5 6   --- autorotate about y   
//        = 7 8 9   --- autorotate about z   
//        = [ ] { } --- adjust focus 
// Example:
/*
{
   gSystem->Load("libX3d");
   TCanvas *c1 = new TCanvas("c1");
   TFile *f = new TFile("hsimple.root");
   TTree *ntuple = (TTree*)f->Get("ntuple");
   ntuple->SetMarkerColor(kYellow);
   ntuple->Draw("px:py:pz");
   TQtViewerX3D *x3d = new TQtViewerX3D(c1,"");
   for (Int_t i=0;i<500;i++) {
      Int_t px = i%500;
      Int_t py = (2*i)%200;
      x3d->ExecCommand(px,py,0);  //rotate
      if (i%20 >10) x3d->ExecCommand(px,py,'j'); //zoom
      if (i%20 <10) x3d->ExecCommand(px,py,'k'); //unzoom
   }
}
*/
   emit executeCommand(px, py, command);
   return 0;
}
//______________________________________________________________________________
void TQtViewerX3D::InitX3DWindow()
{
   // Setup geometry and initialize X3D.

   if (!fPad)   return;
   TObject *obj;
   char x3dopt[32];

   TView *view = fPad->GetView();
   if (!view) {
       fprintf(stderr,"Error:  InitX3DWindow: view is not set");
      return;
   }

   gSize3D.numPoints = 0;
   gSize3D.numSegs   = 0;
   gSize3D.numPolys  = 0;

   TObjLink *lnk = fPad->GetListOfPrimitives()->FirstLink();
   while (lnk) {
      obj = lnk->GetObject();
      TAtt3D *att;
#ifdef R__RTTI
      if ((att = dynamic_cast<TAtt3D*>(obj)))
#else
      if ((att = (TAtt3D*)obj->IsA()->DynamicCast(TAtt3D::Class(), obj)))
#endif
         att->Sizeof3D();
      lnk = lnk->Next();
   }

   printf("Total size of x3d primitives:\n");
   printf("     gSize3D.numPoints= %d\n",gSize3D.numPoints);
   printf("     gSize3D.numSegs  = %d\n",gSize3D.numSegs);
   printf("     gSize3D.numPolys = %d\n",gSize3D.numPolys);

   if (!AllocateX3DBuffer()) {
      fprintf(stderr,"InitX3DWindow\nx3d buffer allocation failure");
      return;
   }

   lnk = fPad->GetListOfPrimitives()->FirstLink();
   while (lnk) {
      obj = lnk->GetObject();
      if (obj->InheritsFrom(TAtt3D::Class())) {
         strcpy(x3dopt,"x3d");
         strcat(x3dopt,fOption.Data());
         obj->Paint(x3dopt);
      }
      lnk = lnk->Next();
   }

   const Float_t kPI = Float_t (TMath::Pi());

   Float_t longitude_rad = ( 90 + view->GetLongitude()) * kPI/180.0;
   Float_t  latitude_rad = (-90 + view->GetLatitude() ) * kPI/180.0;
   Float_t       psi_rad = (      view->GetPsi()      ) * kPI/180.0;

   TQtX3DWidget *x3dWidget = new TQtX3DWidget(&longitude_rad, &latitude_rad, &psi_rad,this,fOption.Data());
   setCentralWidget (x3dWidget);
   connect (x3dWidget, SIGNAL( x3dPosition(float, float, float) ) , this, SLOT( SetPadView(float, float, float) ) );
   connect (this, SIGNAL( executeCommand(int, int, char)),x3dWidget,SLOT(execCommand(int, int, char)));
  //  QWidget *srcWidget = fPad->GetCanvas()
  //  connect ((QPixmap *)(TQt::iwid(fPad->GetPixmapID())), SIGNAL( destroyed() ) , this, SLOT( Disconnect() ) );
}
//______________________________________________________________________________
void TQtViewerX3D::Update()
{  update();  }
//______________________________________________________________________________
void TQtViewerX3D::SetPadView(float longitude_rad, float latitude_rad, float psi_rad)
{
   // Qt slot:
   // Adjust TPad view according the last X3D view position
   if (fPad) {
      const Float_t kPI = Float_t (TMath::Pi());

      Int_t irep;

      Float_t longitude_deg = longitude_rad * 180.0/kPI - 90;
      Float_t  latitude_deg = latitude_rad  * 180.0/kPI + 90;
      Float_t       psi_deg = psi_rad       * 180.0/kPI;

      fPad->GetView()->SetView(longitude_deg, latitude_deg, psi_deg, irep);

      fPad->SetPhi(-90 - longitude_deg);
      fPad->SetTheta(90 - latitude_deg);

      fPad->Modified(kTRUE);
      fPad->Update();
   }
}


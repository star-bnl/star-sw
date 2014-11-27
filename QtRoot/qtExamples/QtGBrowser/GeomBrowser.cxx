// Author: Valeri Fine   2/02/2009
// ****************************************************************************
// ** $Id: GeomBrowser.cxx,v 1.1 2013/08/30 16:00:05 perev Exp $
#include "GeomBrowser.h"
#include "StarGeomTreeWidget.h"
#ifdef STAR
#  include "StChain.h"
#  include "St_geant_Maker/St_geant_Maker.h"
#endif

#include "TROOT.h"
#include "TDataSetIter.h"
#include "TSystem.h"
#include "TFile.h"
#include "TKey.h"
#include "TGeoManager.h"
#include "TPad.h"
#include "TVirtualViewer3D.h"
#include "TQtGLViewerImp.h"
#include "TQtRootViewer3D.h"
#include "TGQt.h"
#include "TQtWidget.h"
#include "TQtRootCommandCombo.h"
#include "TQtRangeControl.h"
#ifndef  NO_GEANT_MAKER
#  include "TextEdit.h"
#endif
#include <QApplication>
#include <QComboBox>
#include <QFileInfo>
#include <QMessageBox>
#include <QSplitter>
#include <QToolBar>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QDir>
#include <QAction>
#include <QMenuBar>
#include <QMenu>
#include <QFileDialog>
#include <QList>
#include <QByteArray>
#include <QImageWriter>
#include <QPrinter>
#include <QPrintDialog>
#include <QDebug>
#include <QStatusBar>

int GeomBrowser::Geant3Init = 0;
//_____________________________________________________________________________
static void RefreshCanvas(TQtWidget *w) 
{
   // Select the proper gPad to force 3D refersh in synch
   if (w) {
      TVirtualPad *savePad = (gPad == w->GetCanvas() ) ? 0 : gPad; 
      if (savePad) w->cd(); 
      w->Refresh(); 
      if (savePad) savePad->cd(); 
   }
}

//_______________________________________________________________________
 GeomBrowser::GeomBrowser(QWidget *parent): QMainWindow (parent) 
 , fTreeWidget(0),fFile(0),fChain(0),fGeant(0),fGeometrySelector(0)
 , fCurrentViewer(0),fGeometryLoaded(false)
 , fSingleVolumeCanvas(0), fComplexVolumeCanvas(0)
 , fRootCommand(0), fCurrentWidget(0)
 , fFile_New(0),  fFile_Open(0),  fFile_Reload(0), fFile_Save(0), fFile_SaveAs(0)
 , fFile_Print(0), fFile_Exit(0)  
 , fView_Coin3DAction(0), fView_GLAction(0)
 , fEditGeoSrc(0), fStatusBar(0),fDepthControl(0)
 { 
   this->setWindowTitle(tr("STAR Geometry Browser"));
   CreateActions();
	CreateMenu();
	CreateToolBar();
	CreateGuiLayout();
   CreateStatusBar();
   Init(); 
   Connect();
 }
//_____________________________________________________________________________
GeomBrowser::~GeomBrowser()
{
    delete fFile; fFile = 0;
}

//_____________________________________________________________________________
void GeomBrowser::Connect()
{
   // Connect signals and slots;
   fFile_New-> setEnabled(false);
   connect(fFile_Open,(SIGNAL(triggered())),   this,SLOT(fileOpenSlot())  );
   connect(fFile_Reload,(SIGNAL(triggered())), this,SLOT(fileReloadSlot()));
   
   connect(fFile_Save,SIGNAL(triggered()),     this,SLOT(fileSaveSlot())  );
   connect(fFile_SaveAs,SIGNAL(triggered()),   this,SLOT(fileSaveAsSlot()));
   connect(fFile_Print,SIGNAL(triggered()),    this,SLOT(filePrintSlot()) );
   connect(fFile_Exit,SIGNAL(triggered()),     this,SLOT(fileExitSlot())  );
   
   connect(fView_GLAction,    SIGNAL(triggered()),this,SLOT(viewGLSlot()) );
   connect(fView_Coin3DAction,SIGNAL(triggered()),this,SLOT(viewCoin3DSlot()));
   
   if (fGeometrySelector) 
      connect(fGeometrySelector,SIGNAL(currentIndexChanged(const QString &))
                       ,this,SLOT(STAR_geometry_activated( const QString &)));
                       
   connect(fTreeWidget,SIGNAL(DrawObject(TObject *,bool))
                       ,this,SLOT(DrawObject(TObject *, bool)));
                       
   connect(fTreeWidget, SIGNAL(ObjectInfo(QString ))
         , fStatusBar,  SLOT( showMessage(const QString & )));
   
   connect(fDepthControl, SIGNAL(ValueChanged(int))
         , this,  SLOT( DrawObject())); 
}
/// Create QAction for the menu and tool bars
//_____________________________________________________________________________
void GeomBrowser::CreateActions()
{
   /// Create QAction for the menu and tool bars
   fFile_New    = new QAction( QIcon(":/FileNew.xpm"),"&New",    this);
   fFile_Open   = new QAction( QIcon(":/fileopen.xpm"),"&Open",  this);
   fFile_Open   ->setToolTip(tr("Open an existing geometry file"));

   fFile_Reload = new QAction( QIcon(":/reload.xpm")  ,"Reload", this);
   fFile_Save   = new QAction( QIcon(":/FileSave.xpm"),"&Save",  this);
   fFile_SaveAs = new QAction(                       "Save As",  this);
   fFile_Print  = new QAction( QIcon(":/printer.xpm"),"&Print",  this);

   fFile_Exit   = new QAction( QIcon(":/quit.xpm")    ,"E&xit",  this);
   fFile_Exit   ->setToolTip(tr("Close all windows and terminate the applciation"));

   fView_Coin3DAction  = new QAction( QIcon(":/snapshot.xpm"),"Open&Inventor", this);
   fView_Coin3DAction  ->setToolTip(tr("Open the Coin3D-based 3D geometry viewer"));
   fView_GLAction      = new QAction( QIcon(":/qglviewer.icon.xpm") , "Open&GL"      , this);
   fView_GLAction      ->setToolTip(tr("Open the QGLViewer-based 3D geometry viewer"));

#ifndef  NO_GEANT_MAKER
   fEditGeoSrc         = new QAction(  "Edit Geant Geometry", this);
   fEditGeoSrc         ->setToolTip(tr("Find the MORTRAN file defining the picked volume to edit"));
   fEditGeoSrc->setCheckable(true);
#endif
}

/// Create menu
//_____________________________________________________________________________
void GeomBrowser::CreateMenu()
{
   /// Create menu
   QMenu *fileMenu = menuBar()->addMenu(tr("&File"));
   fileMenu->addAction(fFile_New);
   fileMenu->addAction(fFile_Open);
   fileMenu->addAction(fFile_Reload);
          fileMenu->addSeparator();
   fileMenu->addAction(fFile_Save);
   fileMenu->addAction(fFile_SaveAs);
          fileMenu->addSeparator();
   fileMenu->addAction(fFile_Print);
          fileMenu->addSeparator();
   fileMenu->addAction(fFile_Exit);
   
   QMenu *viewMenu = menuBar()->addMenu(tr("&View"));
   viewMenu->addAction(fView_Coin3DAction);
   viewMenu->addAction(fView_GLAction);
}

/// Create tool bars 
//_____________________________________________________________________________
void GeomBrowser::CreateToolBar()
{
   /// Create tool bars 
   QToolBar *fileMenu = new QToolBar(this);
   fileMenu->addAction(fFile_Open);
   fileMenu->addAction(fFile_Exit);
   fileMenu->addAction(fFile_New);
   fileMenu->addAction(fFile_Reload);
          fileMenu->addSeparator();
   fileMenu->addAction(fFile_Save);
   fileMenu->addAction(fFile_SaveAs);
          fileMenu->addSeparator();
   fileMenu->addAction(fFile_Print);
          fileMenu->addSeparator();
   addToolBar(fileMenu);
   
   QToolBar *viewMenu = new QToolBar(this);
   viewMenu->addAction(fView_Coin3DAction);
   viewMenu->addAction(fView_GLAction);
   addToolBar(viewMenu);
   
   QToolBar *depthTool = new QToolBar(this);
   fDepthControl = new TQtRangeControl(depthTool);
   depthTool->addWidget(fDepthControl);
   addToolBar(depthTool);

#ifndef  NO_GEANT_MAKER   
   QToolBar *editTool = new QToolBar(this);
   editTool->addAction(fEditGeoSrc);
   addToolBar(editTool);
   
   QToolBar *selectTool = new QToolBar(this);
   fGeometrySelector = new QComboBox(selectTool);
   selectTool->addWidget (fGeometrySelector);   
   addToolBar(selectTool);
#endif
}

//_____________________________________________________________________________
void GeomBrowser::CreateGuiLayout()
{
   QWidget *thisCentralWidget = new QWidget(this);
   setCentralWidget(thisCentralWidget);
   fSingleVolumeCanvas       = new TQtWidget(this);
   fComplexVolumeCanvas      = new TQtWidget(this);
   QSplitter *hsplitter = new QSplitter(Qt::Vertical,this);
   hsplitter->setOpaqueResize(false);
   hsplitter->addWidget(fSingleVolumeCanvas);
   hsplitter->addWidget(fComplexVolumeCanvas);

   QSplitter *vsplitter = new QSplitter(Qt::Horizontal,this);
   vsplitter->setOpaqueResize(false);
   fTreeWidget  = new StarGeomTreeWidget(this);
   vsplitter->addWidget(fTreeWidget);
   vsplitter->addWidget(hsplitter);   
   
   QVBoxLayout *vbox = new QVBoxLayout(thisCentralWidget);
   fRootCommand      = new TQtRootCommandCombo(this);
   vbox->addWidget(vsplitter);
   vbox->addWidget(fRootCommand);
}

//_____________________________________________________________________________
void GeomBrowser::CreateStatusBar()
{
  fStatusBar = statusBar();
}


//_____________________________________________________________________________
void GeomBrowser::Init()
{
#ifndef WIN32
   if (gSystem->Load("libTable") <0) 
   {
      QString errorMsg = "Can not load the ROOT shared library: ";
      errorMsg += "libTable.so";
      QMessageBox::critical(this, "ROOT shared library load error", errorMsg
         , QMessageBox::Ok,QMessageBox::NoButton );

   }
   if (gSystem->Load("libGeom")<0) 
   {
      QString errorMsg = "Can not load the ROOT shared library: ";
      errorMsg += "libGeom.so";
      QMessageBox::critical(this, "ROOT shared library load error", errorMsg
         , QMessageBox::Ok,QMessageBox::NoButton );

   }
#endif
//   QWhatsThis::whatsThisButton(geometry);
#ifdef  NO_GEANT_MAKER
   if (fGeometrySelector) {
      fGeometrySelector->setEnabled(FALSE);
      fGeometrySelector->hide();
   }
#else
   // Create a text editor
   fTextEdit = new TextEdit(this);
   fTextEdit->setCaption( "GEANT3 geometry editor" );
   fTextEdit->resize( 640, 800 );
   // Set the selection mode
  // vf   fTreeWidget->setSelectionMode(QListView::Extended);
   
   // populate the standard geometry type   
   // create the STAR search path
   QString file("pams/geometry/geometry/geometry.g");
   QString path(".:$STAR");
   connect(fTextEdit,SIGNAL(textSaved(const QString &)),this,SLOT(RemakeGeomSlot(const QString &)));
   const char *found = gSystem->Which((const char*)path, (const char*)file);
   if (found) {
     if (found[0]) {
       QFile geomFile(found);
       if (geomFile.open(IO_ReadOnly)) {
       QString line;
       QRegExp exp("^\\s+\\bCase\\b.+");
	   char bufLine[1024];
       while (geomFile.readLine(bufLine, sizeof(bufLine)) >=0 ) 
       {
	      line = bufLine;
          if (line.contains( exp ) ) {
             line = line.simplifyWhiteSpace ();
             QString tag = line.section(' ',1,1);
             // printf(" new tag found --<%s>-- \n --<%s>--\n",(const char*) line, (const char*)tag);
             // remove "on"
             tag.remove("on");
             tag.stripWhiteSpace();
             // exceptions:
             //    " HELP NO_BREM LOW_EM TPC_ ONLY"
             if (!(tag.isEmpty()
                 || tag.contains("COMPLETE") 
                 || tag.contains("HELP") 
                 || tag.contains("_ON") 
                 || tag.contains("_OFF") 
                 || tag.contains("NO_BREM") 
                 || tag.contains("LOW") 
                 || tag.contains("TPC_")
                 || tag.contains("DUMM")
                 ||  tag.contains("ONLY") ) ) {
              // add to the combo box
                  fGeometrySelector->insertItem(tag);
                // printf(" new tag found --<%s>-- current = %d\n", (const char*)tag, fGeometrySelector->currentItem());
             } 
          }         
       }
     } else {
        assert(0);
     } 
   }
 }
 delete [] found;
#endif
   fGeometryLoaded = false;
   fChain          = 0;
   fGeant          = 0;
//   fGeoManager2Delete = 0;
   fCurrentViewer     = 0;
   fFile            = 0;
//   fIconSet.setPixmap(QPixmap::fromMimeSource("wirebox.xpm"),QIconSet::Automatic);

   // Attach the validator to facilitate the ROOT <tab> completion
   // fTabCompValidator = new TQtTabValidator(comboBox1);
   // comboBox1->setValidator(fTabCompValidator);

   // Predefine some ROOT commands
   fRootCommand->addItem("gGeometry->SetBomb(1.7);");
   fRootCommand->addItem("gPad->SetFillColor(kBlack);");
   fRootCommand->addItem("gPad->SetFillColor(kWhite);");
   fRootCommand->addItem(".qqqqqq");
   fRootCommand->addItem(".q");
//   spinBox1->setValue(3);
   fComplexVolumeCanvas->GetCanvas()->SetFillColor(kBlack); 
   // do we have the QGLViewer?
   char *libRQTGL = 0;
   libRQTGL = gSystem->DynamicPathName("libRQTGL",kTRUE);
      fView_GLAction->setEnabled(libRQTGL);
    //  strange why it crashes like the heel 
    //  if (libRQTGL) delete [] libRQTGL;
      libRQTGL = 0;

   // do we have the Coin3D viewer?
   libRQTGL = gSystem->DynamicPathName("libRQIVTGL",kTRUE);
      fView_Coin3DAction->setEnabled(libRQTGL);
      // if (libRQTGL) delete [] libRQTGL;

   fCurrentWidget = fSingleVolumeCanvas; fCurrentWidget->cd();

   // Open pre-defined ROOT file
   TKey *key = 0;
   char **argv = qApp->argv();
   if (qApp->argc() > 1 && QFileInfo(argv[1]).isReadable() ) {

      fSaveFileName = argv[1];
      qDebug() << " file " << fSaveFileName;
      fFile=new TFile(fSaveFileName.toStdString().c_str());
      fSaveFileName = "";
      
      // TUpdateList listLock(fTreeWidget);

      TIter next(fFile->GetListOfKeys());
      // Fill the "tree view" with the object info from the file
      int countObject = 0;
      while((key = (TKey*) next())) {
         if (fTreeWidget->AddModel2ListView(key->ReadObj(),QString())) countObject++;
      }
      if (!countObject)   
           QMessageBox::warning(this,"Open ROOT geometry file","No 3D object was found here");
 
      // Create a separate Wwigdet with ROOT Object browser, Just in case
      //  new TBrowser();
   } else {
      //   fileOpen();
     fSaveFileName = "$STAR/StarDb/VmcGeometry/";
     if (gGeoManager) {
        fTreeWidget->AddModel2ListView(gGeoManager->GetTopVolume(),QString());
      }
   }
}

//_____________________________________________________________________________
void GeomBrowser::fileOpenMacro( const QString &fileName )
{
  // Read ROOT macro with the ROOT TGeo geometry model
    QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
    if (!gROOT->LoadMacro(fileName.toStdString().c_str()))  {
      TDataSet *topSet = (TDataSet *)gROOT->ProcessLineFast("CreateTable()");
      if (topSet) {
         // Look for the "Geometry" set
		   QString name = QFileInfo(fileName).completeBaseName();
         fTreeWidget->AddModel2ListView(gGeoManager,name);
      }
      fOpenFileName = fileName;
    } else {
       QMessageBox::warning(this,"Open ROOT macro geometry file","Can not open the macro file");
    }
    CleanGeoManager();
    QApplication::restoreOverrideCursor();
}

//_____________________________________________________________________________
void GeomBrowser::fileOpenRoot( const QString &fileName )
{
  QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
  if (fFile) delete fFile;
     QString base = QFileInfo(fileName).completeBaseName(); 
     fFile = TFile::Open(fileName.toStdString().c_str());
     if (!fFile->IsZombie()) {
        TKey *key = 0;
        TIter next(fFile->GetListOfKeys());
        // Fill the "tree view" with the object info from the file
 
        TQtLockUpdateWidget listLock(fTreeWidget);

        int countObject = 0;
        while((key = (TKey*) next())) {
           TObject *o = key->ReadObj();
           QString name = QString("%1.%2").arg(base).arg(o->GetName());
           if (fTreeWidget->AddModel2ListView(o,name)) countObject++;              
        }
        if (!countObject)   
           QMessageBox::warning(this,"Open ROOT geometry file"
                                    ,"No 3D object was found here");
        fOpenFileName = fileName;
     } else {
        delete fFile; fFile=0;
        QString errorMsg = "Zombie ROOT file: ";
        errorMsg += fileName;
        QMessageBox::critical(this, "ROOT file access error", errorMsg
            , QMessageBox::Ok,QMessageBox::NoButton );
     }
     CleanGeoManager();
     QApplication::restoreOverrideCursor();
}

//_____________________________________________________________________________
St_geant_Maker & GeomBrowser::Geant() 
{
    // create St_geant_MAker if needed
#ifndef NO_GEANT_MAKER
    if (!fGeant) {
       gSystem->Load("St_base");
       gSystem->Load("StChain");
       gSystem->Load("St_Tables");
       gSystem->Load("St_g2t.so");
       gSystem->Load("StarMagField");
       gSystem->Load("St_geant_Maker");  
       gSystem->Load("StUtilities");
#ifdef STAR
       fChain = new StChain(); 
       fGeant = new St_geant_Maker();
#endif
    }
#endif 
    return *fGeant;
}

//_____________________________________________________________________________
void GeomBrowser::fileOpenZebra( const QString &fileName )
{
   // fprintf(stderr,"fileOpenZebra <%s>\n", (const char*)kuipCmd );
#ifndef NO_GEANT_MAKER
   QString kuipCmd  = "gfile p  ";
   kuipCmd         +=  fileName;
   QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
   Geant().SetInputFile(fileName);
   if (Geant3Init) {
       Geant().Do("gdrop all"); // To allow calling Init more then one time;
       Geant().Do((const char *)kuipCmd); 
       Geant().Do("gclose all");
   } else {
      fChain->Init(); Geant3Init = 1;
   }
   // comboBox2->setEnabled(FALSE); // we can communicate GEANT one time ony :(
   QApplication::restoreOverrideCursor();
#else
   if (fileName.isEmpty()) {}
   if (fGeometrySelector) fGeometrySelector->setEnabled(FALSE); // we can communicate GEANT one time only :(
#endif
}
//_____________________________________________________________________________
void GeomBrowser::fileOpenInventor( const QString &fileName )
{
   // Open the new Coin widget and feed the IV file there
   if (fView_Coin3DAction->isEnabled() ) {
      TVirtualViewer3D *viewer = viewCoin3DSlot(); 
      if (viewer) {
         TQtRootViewer3D *v  = (TQtRootViewer3D*)(viewer);
         if (v) {
            TGLViewerImp *viewerImp = v->GetViewerImp();
            if (viewerImp) viewerImp->ReadInputFile(fileName.toStdString().c_str());
         }
      }
   }
}
//_____________________________________________________________________________
TVirtualViewer3D *GeomBrowser::viewCoin3DSlot()
{
// 
   if (fCurrentViewer) 
      ((TQtRootViewer3D*)fCurrentViewer)->DisconnectPad();
      
   TVirtualPad *pad3d = fComplexVolumeCanvas->GetCanvas() ? fComplexVolumeCanvas->GetCanvas() : gPad;      
   fCurrentViewer = TVirtualViewer3D::Viewer3D(pad3d,"oiv");
   if (fCurrentViewer) {
       // Create Open GL viewer
       TGQt::SetCoinFlag(1);
       fCurrentViewer->BeginScene();
       fCurrentViewer->EndScene();
       TQtRootViewer3D *v  = (TQtRootViewer3D*)(fCurrentViewer);
       if (v) {
           TGLViewerImp *viewerImp = v->GetViewerImp();
           if (viewerImp) 
           {
               viewerImp->MakeCurrent();
               connect(&viewerImp->Signals(),SIGNAL( ObjectSelected(TObject *, const QPoint&))
                 , this, SLOT(ObjectSelected(TObject *, const QPoint &))); 
               connect(&viewerImp->Signals(),SIGNAL( ObjectSelected(TObject *, const QPoint&))
                 , fTreeWidget, SLOT(SelectByTObject(TObject *, const QPoint &))); 
               connect(&viewerImp->Signals(),SIGNAL( destroyed())
                 , this, SLOT(ViewerDestroyed()));
           }
       }
    } else {
         fView_Coin3DAction->setEnabled(false);
    }
    return fCurrentViewer;
}
//_____________________________________________________________________________
void GeomBrowser::SelectGeometry( const QString &geomTag )
{
   // Select the standard geometry from the combobox if any
   // Find the geometry
   // printf(" GeomBrowser::SelectGeometry <%s> staring from %d\n", (const char *)geomTag,fGeometrySelector->currentItem());
   int indx = fGeometrySelector->findText(geomTag,Qt::MatchFixedString | Qt::MatchWrap);
   if (indx > 0) {
       fGeometrySelector->setCurrentIndex(indx);
       // selection should end up with "STAR_geometry_activated" slot after all
   } else  {
       QMessageBox::warning (fGeometrySelector,"STAR Geometry Tag",QString("STAR GEANT Geometry Tag <b>\"%1\"</b> was not found").arg(geomTag));
   }
}

//_____________________________________________________________________________
void GeomBrowser::STAR_geometry_activated( const QString &geoVersion )
{
#ifndef NO_GEANT_MAKER
   QString kuipCmd  = "detp geometry ";
   kuipCmd         +=  geoVersion;
   QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
   // fprintf(stderr,"STAR_geometry_activated <%s>\n", (const char*)kuipCmd );
   if (!fGeometryLoaded)  fGeometryLoaded = !gSystem->Load("geometry");
   if (fGeometryLoaded) {
      // pre-define the kuipCommand to used within St_geant::Geometry method
      Geant().LoadGeometry((const char*)kuipCmd );
      if (Geant3Init) {
          Geant().Do("gdrop all"); // To allow calling Init more then one time;
          Geant().Do((const char *)kuipCmd); 
          if (false) 
             Geant().Do("make geometry"); 
          else 
             Geant().Geometry();
         // Geant().Do("gclose all");
      } else {          
         fChain->Init(); Geant3Init = 1;     
      }
      // fGeometrySelector->setEnabled(FALSE); // we can communicate GEANT one time ony :(
      TVolume *v = dynamic_cast<TVolume *>(Geant().GetDataSet("HALL"));
      if (v) {
         // Make CAVE invisible
         // v->SetName((const char*)geoVersion);
         TVolume *cave = (TVolume *)v->FindByName("CAVE");
         if (cave) {
            cave->SetVisibility(TVolume::ENodeSEEN(2));
         }
         TVolume *hall = (TVolume *)v->FindByName("HALL");
         if (hall) {
              hall->SetVisibility(TVolume::ENodeSEEN(2));
              v = hall;
         }
         gPad->SetFillColor(kBlack);
         fTreeWidget->AddModel2ListView(v,geoVersion);
      }
   }
   QApplication::restoreOverrideCursor();
#else
  if   (geoVersion.isEmpty() ){}    
#endif
}

/// [SLOTS]:
/// DrawObject draw the obj onto TCanvas
///
/// Only TVolume obejct is drawn. All other types are ignored
///  obj != 0: The pointer to TVolume obj to be draw
///      == 0 The pointer to obkect is defined by StarGeomTreeWidget::CurrentObject()
///
///  bool expanded == true; choose fComplexVolumeCanvas TCanbas as the desination TCanvas
///                         and use  fDepthControl->Value() to define the number of the layers to be drawn down
///       expanded == false; choose  fSingleVolumeCanvas adn draw the first layer if visible                
//_____________________________________________________________________________
void GeomBrowser::DrawObject(TObject *obj,bool expanded)
{
   if (!obj && fTreeWidget) obj = fTreeWidget->CurrentObject();
   if (obj) {
      TVolume *volume = dynamic_cast<TVolume *>(obj);
      TQtWidget *view = expanded ? fComplexVolumeCanvas:fSingleVolumeCanvas;
      if(volume && view) {
         QCursor currentCursor = cursor();
         int depth = expanded ? fDepthControl->Value() : 1;
          // Check the item deep visibility
         TVolume::ENodeSEEN s = volume->GetVisibility();
         TDataSet  *set = volume;
         if ( s & TVolume::kThisUnvisible ) {
            TDataSetIter nextVolume(volume, depth);
            set = 0;
            while ( (set = nextVolume())  ) {
               s = ((TVolume *)set)->GetVisibility();
               if ( !(s & (TVolume::kThisUnvisible) )) break;
            }
         }
          // setCursor(Qt::WaitCursor);
         setEnabled(false);
         view->cd();
         if (set) {
            volume->Draw(QString().setNum(depth).toStdString().c_str());
         } else
            view->GetCanvas()->Clear();
         RefreshCanvas(view);
         setEnabled(true);
      }
   }
}

//_____________________________________________________________________________
void GeomBrowser::ObjectSelected( TObject *obj, const QPoint &)
{
   // [slot] to accept the selected object to expand the ListTreeView
   
   if (obj->InheritsFrom(TDataSet::Class()) ) {
      // suspend the list view update
#ifndef NO_GEANT_MAKER
#if 1    
     QString srcFile=(const char*)Geant().GetVolumeSrcFile(obj->GetName());
     if (!srcFile.isEmpty() && fEditGeoSrc->isChecked() ) {
        fTextEdit->show();
        fTextEdit->load(srcFile);
        fTextEdit->findBlock(QString(obj->GetName()));
//        QRect itemRec = fTreeWidget->itemRect(fTreeWidget->selectedItem());
//        QPoint pos(itemRec.x(),itemRec.y());
//        pos = fTreeWidget->mapToGlobal(pos);
//        QWhatsThis::display(srcFile,pos);
      }
#endif      
#endif     
   }
}


//_____________________________________________________________________________
void GeomBrowser::fileOpenSlot()
{
   static QString thisCintCommand;
   static QString filetypes = "STAR Geometry macro (*.C);"
                              ";ROOT files (*.root);"
#ifndef  NO_GEANT_MAKER
                              ";GEANT3 Zebra file (*.fz)"
#endif
                              ";";
   QString selectedFilter;
   QString dir = fSaveFileName;
   if (dir.isEmpty()) dir = gSystem->WorkingDirectory();
   else   {
      TString exp(fSaveFileName.toStdString().c_str());
      gSystem->ExpandPathName(exp);
      dir = exp.Data();
      dir = QFileInfo(dir).path();
   }

   QString fileName = QFileDialog::getOpenFileName (this
      , "Open ROOT file "
      ,  dir
      , filetypes
      , &selectedFilter);
   if (!fileName.isEmpty()){
      QFileInfo openFile(fileName);
      if (openFile.suffix().contains("C")) {
         fileOpenMacro(fileName);
      } else if (openFile.suffix().contains("fz")) {
         fileOpenZebra(fileName);
      } else {
         fileOpenRoot(fileName);
      }
   }
}

//_____________________________________________________________________________
void GeomBrowser::fileSaveSlot()
{
    // Save the  current TCanvas is defined otherwise the SingleVolumeCanvas;
    if (!(fSaveFileName.isEmpty()) )  {
	   TQtWidget *sav = fCurrentWidget ? fCurrentWidget : fSingleVolumeCanvas;
       sav->Save(fSaveFileName);
    } else {
      fileSaveAsSlot(); 
    }
}
//_____________________________________________________________________________
void GeomBrowser::fileSaveAsSlot()
{
   QString filter = 
      "C++ macro (*.cpp,*.cxx,*.C);"
      ";Postscript (*.ps);"
      ";Encapsulated Postscript (*.eps);"
      ";Scalable Vector Graphics (*.svg);"
      ";ROOT file (*.root);"
      ";Image (";

	  
   QList<QByteArray> formats =  QImageWriter::supportedImageFormats();
   QList<QByteArray>::const_iterator j;
   int i = 0;
   for (j = formats.constBegin(); j != formats.constEnd(); ++j)
   {
      if (i) filter +=',';
      filter += "*.";	  
      QString str =  *j; i++;      
	   filter += str.toLower();
   }
   filter +=");";
   filter +=";all files (*.*);;";

   QString selectedFilter;
   QString dir = fSaveFileName;
   if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
   else               dir = QFileInfo(dir).path();
   
   QString thatFile = QFileDialog::getSaveFileName(this, tr("Save File"),
                            dir,filter);

   if (thatFile.isEmpty()) return;
   fSaveFileName = thatFile;

   QString fileNameExtension = QFileInfo(thatFile).suffix();
   QString  saveType = fileNameExtension.toUpper();

   fSingleVolumeCanvas->Save(fSaveFileName,saveType.toStdString().c_str(),-1);
}
//_____________________________________________________________________________
void GeomBrowser::filePrintSlot()
{
   // Print the  current TCanvas is defined otherwise the SingleVolumeCanvas;
   QPrinter printer;
   QPrintDialog printDialog(&printer, this);   
   if (printDialog.exec() == QDialog::Accepted) {
      QPainter pnt(&printer);
      TQtWidget *sav = fCurrentWidget ? fCurrentWidget : fSingleVolumeCanvas;
      pnt.drawPixmap(0,0,*sav->GetOffScreenBuffer());
   }
} 
//_____________________________________________________________________________
void GeomBrowser::fileExitSlot()
{
  gROOT->ProcessLine(".q");
}

//_____________________________________________________________________________
void GeomBrowser::RemakeGeomSlot( const QString &)
{
#ifndef NO_GEANT_MAKER
   Geant().SetRemake(kTRUE);
#endif
}

//_____________________________________________________________________________
void GeomBrowser::viewGLSlot()
{
   // Draw the 3d image from the second (top) pad only 
   TVirtualPad *pad3d = fSingleVolumeCanvas->GetCanvas() ? fSingleVolumeCanvas->GetCanvas() : gPad;
//   TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(gPad,"ogl");
   TVirtualViewer3D *viewer = TVirtualViewer3D::Viewer3D(pad3d,"ogl");
   if (viewer) {
      // Create Open GL viewer
      TGQt::SetCoinFlag(0);
      viewer->BeginScene();
      viewer->EndScene();
      TQtRootViewer3D *v  = (TQtRootViewer3D*)(viewer);
      if (v) {
         TGLViewerImp *viewerImp = v->GetViewerImp();
         if (viewerImp) 
         {
            connect(&viewerImp->Signals(),SIGNAL( ObjectSelected(TObject *, const QPoint&))
                 , this, SLOT(ObjectSelected(TObject *, const QPoint &)));
         }
      }
   }  else {
      fView_GLAction->setEnabled(FALSE);
   }
}

//_____________________________________________________________________________
void GeomBrowser::fileReloadSlot()
{
  // Block the widgets update 
   QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );
   
   fSingleVolumeCanvas->setUpdatesEnabled( FALSE );
   fComplexVolumeCanvas->setUpdatesEnabled( FALSE );
   
   fSingleVolumeCanvas->GetCanvas()->Clear();
   fComplexVolumeCanvas->GetCanvas()->Clear();
   fSingleVolumeCanvas->GetCanvas()->Update();
   fComplexVolumeCanvas->GetCanvas()->Update();
   // clean 
   fTreeWidget->ClearCB();   
   if (!fOpenFileName.isEmpty()){
      QFileInfo openFile(fOpenFileName);
      if (openFile.suffix().contains("C"))
         fileOpenMacro(fOpenFileName);   
      else {
         fileOpenRoot(fOpenFileName);
      }
   } else {
     fileOpenSlot();
   }
   fSingleVolumeCanvas->setUpdatesEnabled( TRUE );
   fComplexVolumeCanvas->setUpdatesEnabled( TRUE );
   CleanGeoManager();
   QApplication::restoreOverrideCursor();
}

//_____________________________________________________________________________
void GeomBrowser::CleanGeoManager()
{
#if 0
    if (fGeoManager2Delete) {
       delete fGeoManager2Delete;fGeoManager2Delete = 0;
       gGeoManager = 0;
    }
#endif
}

//_____________________________________________________________________________
void GeomBrowser::ViewerDestroyed()
{
   // disconnect the current viewer 
   fCurrentViewer = 0;
}

 #if 0

#include "TVolumePosition.h" 


//________________________________________________________________
static pair<QString,QString> MakeVolumeDsc(const QString &s) 
{
  // First 4  symbols are the string key
  
  QString key = s.left(4);
  pair<QString, QString> volumeDiscriptor(key,s);
  return volumeDiscriptor;
}
//________________________________________________________________
static map<QString,QString> MakeVolumeMap(const QString &fileName)
{
    QFile file( fileName );
    map<QString,QString> thisMap;
    if ( file.open( IO_ReadOnly ) ) {
        QTextStream stream( &file );
        QString line;
        while ( !stream.atEnd() ) {
            line = stream.readLine(); // line of text excluding '\n'
            thisMap.insert(MakeVolumeDsc(line));
        }
        file.close();
    }
    return thisMap;
}

//________________________________________________________________
static const QString  &GetVolumeDescriptor(const QString &volumeName,bool richText=false)
{
   static bool first = true;
   static map<QString,QString> volumeMap;
   static QString dsc;
   static bool errorMessage = false;// show it at once;
   if (first) {
      first = false;
      TString helpFile = "volumes.txt";
      const char *fullPath = gSystem->Which("./:./StDb/geometry:$STAR/StDb/geometry",helpFile);
      if (fullPath) {
        volumeMap = MakeVolumeMap(fullPath);
      } else if (!errorMessage) {
        errorMessage = true; // show it at once;
        QMessageBox::critical(0
              ,"STAR Geometry narrative description"
              ,QString("No file <%1> file under %2 was found")
                       .arg(helpFile.Data())
                       .arg("./:./StDb/geometry:$STAR/StDb/geometry"));
      }
      delete [] fullPath;  fullPath=0;
   }
   map<QString,QString>::iterator it;
   it = volumeMap.find(volumeName);
   if (it!=volumeMap.end())  {
      dsc = richText ? "<p><b>" : "";
      dsc += it->second;
   } else dsc = "";
   dsc.replace(" comment =",richText ? ":</b> ": ": ");
   return dsc;
}


//_____________________________________________________________________________
void GeomBrowser::listView1_selectionChanged()
{
   QListViewItem *i =fTreeWidget->currentItem();
   if (i) {
      if ( i->isSelected() ) listView1_selectionChanged(i);
   }
}

//_____________________________________________________________________________
void GeomBrowser::listView1_selectionChanged( QListViewItem *item )
{
    if (item) {
       // fprintf(stderr,"listView1_selectionChanged %s\n", (const char *) item->text(0));
       drawItem(item, 1, fComplexVolumeCanvas);
#if o      
      TQtObjectListItem* itemRoot =(TQtObjectListItem* )item;
      TObject *obj = itemRoot->Object();
      TObject *objInfo = obj;
      TVolume *volume = dynamic_cast<TVolume *>(obj);
      objInfo = volume->GetShape();
      if (fInspector) delete fInspector;
      fInspector  = (TQtInspectImp *)(gGuiFactory->CreateInspectorImp(objInfo,frame6->width(),frame6->height()));
      // fInspector->Hide();
      fInspector->move(frame6->pos());
      //  fInspector->reparent(frame6,0,QPoint(0,0)); 
      //fInspector->Show();
#endif      
   }
}


//_____________________________________________________________________________
void GeomBrowser::listView1_contextMenuRequested( QListViewItem *item, const QPoint &pos, int col )
{
   if (item && col >= 0) {
     // Count the number of the selected items
      QPtrList<QListViewItem> lst;
      int nSelected = 0;
      QListViewItemIterator it(fTreeWidget, QListViewItemIterator::Selected );
      while ( it.current() ) {
          lst.append( it.current() );
          ++it;++nSelected;
      }
      if (nSelected == 1) {
         if (!fContextMenu) fContextMenu = new TContextMenu("BrowserContextMenu");
         TQtObjectListItem* that =(TQtObjectListItem* )item;
         if (that->Object())
            fContextMenu->Popup(pos.x(),pos.y(), that->Object(),(TBrowser *)0);
      } else {
         int response = -1; //QMessageBox::question(fTreeWidget,"Change the volume visibility","Visible","Both","Child","none");
         static QPopupMenu *contextMenu = 0;
         static int menus[5];
         if (!contextMenu) {
            contextMenu = new QPopupMenu(fTreeWidget);
            int itemPosition = -1;
            // menu title :
            contextMenu->insertItem("Visibility:");
            contextMenu->insertSeparator(); 

            itemPosition = contextMenu->insertItem("&Both");
            int j =0;
            menus[j++] = itemPosition;
            contextMenu->setWhatsThis(itemPosition,"Make the selected volumes and its children visible");
            
            contextMenu->setWhatsThis(itemPosition=contextMenu->insertItem("&Children")
               ,"Make the selected the children of the selected volumes visible but the volume itself none");
            menus[j++] = itemPosition;
            
            contextMenu->setWhatsThis(itemPosition=contextMenu->insertItem("&None")
               ,"Make the selected the volumes invisible");
            menus[j++] = itemPosition;
            contextMenu->insertSeparator();
             contextMenu->setWhatsThis(itemPosition=contextMenu->insertItem("&Save")
               ,"Save the selected object into ROOT file");
            menus[j++] = itemPosition;
           
             contextMenu->setWhatsThis(itemPosition=contextMenu->insertItem("&Color")
               ,"Change the color of the selected object");
            menus[j++] = itemPosition;
        }
         response = contextMenu->exec(QCursor::pos());
         if (response != -1 ) {
            TUpdateList listLock(fTreeWidget);
            QListViewItem  *i;
            bool saved = false;
            Color_t rootColor = -1;
            Style_t rootStyle = -1;
            TVolume *topVolumeToSave = 0;
            TFile  *file2Save = 0;
            for ( i = lst.first(); i; i = lst.next() ) {
               TQtObjectListItem* itemRoot =(TQtObjectListItem* )i;
               TObject *obj = itemRoot->Object();
               TVolume *volume = dynamic_cast<TVolume *>(obj);
               // check visibility
               if (volume) {
                  TVolume::ENodeSEEN s = volume->GetVisibility();
                  if  ( response == menus[0] ) {
                     itemRoot->setState(QCheckListItem::On);       s = TVolume::kBothVisible; 
                  } else if (response == menus[1]) { 
                     itemRoot->setState(QCheckListItem::NoChange); s = TVolume::kThisUnvisible;
                  } else if (response == menus[2]) { 
                     itemRoot->setState(QCheckListItem::Off)     ; s = TVolume::kNoneVisible;
                  } else if (response == menus[3]) { 
                      if (topVolumeToSave) {
                         topVolumeToSave->Add(volume);
                      } else if (!saved) {
                         saved  = true;
                        // Save the object
                         QString filter = "ROOT file (*.root);";
                         QString selectedFilter;
                         QString dir = fSaveFileName;
                         if (dir.isEmpty()) dir = gSystem->WorkingDirectory(); 
                         else               dir = QFileInfo(dir).dirPath();

                         QString thatFile = QFileDialog::getSaveFileName(dir
                            , filter, this, "SaveAs"
                            , "Save the volulme  as"
                            , &selectedFilter);
  
                         if (thatFile.isEmpty()) {
                            response = -1;
                         } else {
                            TDirectory *save = gDirectory;
                            file2Save = TFile::Open((const char *)thatFile,"RECREATE");
                            topVolumeToSave = new TVolume("GeomBrowse","saved",(TShape *)0);
                            save->cd();
                        }
                     }
                  } else if (response == menus[4]) { 
                     // Change the object color
                     Color_t vc = volume->GetLineColor();
                     Style_t vs = volume->GetFillStyle();
                     if (rootColor == -1)  {
                        float r,g,b;
                        int a = 0;
                        gROOT->GetColor(vc)->GetRGB(r,g,b);
                        if (4000 >= vs && vs < 5000) a = vs-4000;
                        QRgb initial = qRgba(int(r*255),int(g*255),int(b*255),a);
                        bool ok;
                        QRgb color = QColorDialog::getRgba(initial, &ok, this,"Change the Volume Color" );
                        if (ok) {
                           int red   = qRed(color);
                           int green = qGreen(color);
                           int blue  = qBlue(color);
                           int alpha = qAlpha(color);
                           rootColor = TColor::GetColor(red, green, blue);
                           if ( (alpha > 0) && (alpha != vs-4000) ) rootStyle = 4000+alpha;
                        } else {
                           response = -1;
                        }
                     }
                     if (rootColor != -1 && vc != rootColor) volume->SetLineColor(rootColor);
                     if (rootStyle != -1 && vs != rootStyle) volume->SetFillStyle(rootStyle);
                  } else { response = -1; }
                  // set visibility
                  if (volume->GetVisibility() != s) volume->SetVisibility(s);
               }
            }
            if (file2Save) {
               TDirectory *save = gDirectory;
               file2Save->cd();
               topVolumeToSave->Write();
               file2Save->Close();
               delete file2Save; file2Save=0;
               delete topVolumeToSave; topVolumeToSave = 0;
               save->cd();
            }
         }
         if (response != -1) RefreshCanvas(fSingleVolumeCanvas);
      }
   }
}

//_____________________________________________________________________________
void GeomBrowser::slider1_valueChanged( int val )
{
   drawItem(fCurrentDrawn, val, fSingleVolumeCanvas ); 
}


//_____________________________________________________________________________
inline static Int_t CountInstances(TVolume *parent,  TVolume *child) 
{
   // Find the number of the positions of the child volume 
   Int_t counter = 0;
   TList *positions = parent->GetListOfPositions();
   TIter next(positions);
   while (TVolumePosition *pos = (TVolumePosition *)next() )
      if (pos->GetNode() == child ) counter++;
   return counter;   
}

//_____________________________________________________________________________
void GeomBrowser::listView1_expanded(QListViewItem *item)
{
   if (item) {
      // fprintf(stderr,"listView1_expanded %s\n", (const char *) item->text(0));
      TQtObjectListItem* itemRoot =(TQtObjectListItem* )item;
      TObject *obj = itemRoot->Object();
      TVolume *volume = dynamic_cast<TVolume *>(obj);
      
      TUpdateList listLock(item->listView());

      TDataSetIter next(volume);
      TVolume *child = 0;
      while ( (child = (TVolume *)next()) ) {
         TQtObjectListItem* itemChild = new TQtObjectListItem(child,itemRoot,child->GetName(),QCheckListItem::CheckBox);     
         itemChild->setText(1,child->GetTitle());
         itemChild->setText(3,child->ClassName());
         Int_t nVolume = CountInstances(volume,child);
         if (nVolume >1) 
             itemChild->setText(2,QString("> #%1").arg(nVolume));
         itemChild->setExpandable(child->GetListSize());
         SetVisibility(itemChild, child->GetVisibility());
      }
   }
}

//_____________________________________________________________________________
void GeomBrowser::listView1_onItem( QListViewItem *item )
{
   if (item) {  
      TQtObjectListItem* itemRoot =(TQtObjectListItem* )item;
      TObject *obj = itemRoot->Object();
      if (obj && gPad) {
         QString m = "The ";
         TVolume::ENodeSEEN s = ((TVolume *)obj)->GetVisibility();
         if ( s & TVolume::kThisUnvisible) m += "in";
         m += "visible volume: ";
         QString dsc= GetVolumeDescriptor(obj->GetName());
         if (dsc.isEmpty() ) {
            const char *info = obj->GetObjectInfo(gPad->XtoPixel(0),gPad->YtoPixel(0));
            if (info) m += info;
         } else {
           m += dsc;
         }
         statusBar()->message(m);            
      }
#if 0      
      TVolume *volume = dynamic_cast<TVolume *>(obj);
      if (fInspector) delete fInspector;
      fInspector  = dynamic_cast<TQtInspectImp *>(gGuiFactory->CreateInspectorImp(obj));
      fInspector->reparent(this);
      fInspector->Show();
#endif      
#ifndef WIN32         
      gSystem->ProcessEvents();
#endif         
   }
}


//_____________________________________________________________________________
void GeomBrowser::dtor()
{
    delete fFile; fFile = 0;
    delete fContextMenu; fContextMenu = 0;
}
//_____________________________________________________________________________
void GeomBrowser::rootCommandExecute()
{
   // TQtTabValidator::Clear();
   gROOT->ProcessLine(fRootCommand->lineEdit()->text());
   RefreshCanvas(fSingleVolumeCanvas);
   //gROOT->ProcessLine("gPad->Update();");
}
//_____________________________________________________________________________
void GeomBrowser::listView1_clicked( QListViewItem *item )
{
   if (item) {
      TQtObjectListItem* itemRoot =(TQtObjectListItem* )item;
      TObject *obj = itemRoot->Object();
      TVolume *volume = dynamic_cast<TVolume *>(obj);

      // check visibility
      if (volume) {
         TVolume::ENodeSEEN s = volume->GetVisibility();
#if (QT_VERSION > 0x030100)
         switch ( itemRoot->state() ){
            case QCheckListItem::On      : s = TVolume::kBothVisible;   break;
            case QCheckListItem::NoChange: s = TVolume::kThisUnvisible; break;
            case QCheckListItem::Off     : s = TVolume::kNoneVisible;   break;
            default:                       s = TVolume::kBothVisible;   break;
               //      case TVolume::kSonUnvisible: s = QCheckListItem::Off; break;
         };
#else
         s = ( itemRoot->isOn() ) ? TVolume::kBothVisible : TVolume::kThisUnvisible ;
#endif         
         // set visibility
         if (volume->GetVisibility() != s) {
            volume->SetVisibility(s);

            QString m = "The ";
            if ( s & TVolume::kThisUnvisible) m += "in";
            m += "visible volume: ";
            const char *info = obj->GetObjectInfo(gPad->XtoPixel(0),gPad->YtoPixel(0));
            if (info) m += info;
            statusBar()->message(m);            

            // adjust 1 level view
            if (item == item->listView()->selectedItem() ) 
               drawItem(item, 1, fComplexVolumeCanvas);

            // adjust multi-level  view
            if (fCurrentDrawn) RefreshCanvas(fSingleVolumeCanvas);
         }
         QString dsc = GetVolumeDescriptor(volume->GetName(),true);
         if (dsc.isEmpty()) {
            dsc = QString("<p>No desciption was found for <b>%1</b><br>Edit the <code>volumes.txt</code> file")
                          .arg(volume->GetName());
         }
         // ???QWhatsThis::display(dsc, QCursor::pos()+QPoint(100,0)); 

      }
   }
}
//_____________________________________________________________________________
void GeomBrowser::listView1_collapsed( QListViewItem *item )
{
   if (item) {
      // check fCurrentDrawn
      TUpdateList listLock(item->listView());

      QListViewItemIterator it(item);
      while ( it.current() ) {
         if ( (it.current() != item) && (it.current() == fCurrentDrawn)  ) 
         {   fCurrentDrawn =0;  break;   }
         ++it;
      }
      // clean the collapsed branch
      QListViewItem* child = 0;;
      while ( (child  =  item->firstChild()) ){
            delete child;
      }
      if (!item->depth()) item->setOpen(true);
   }
}

//_____________________________________________________________________________
void GeomBrowser::listView1_doubleClicked( QListViewItem *item )
{
   if (item && fCurrentDrawn != item) {
      // remove the old mark
      { 
        TUpdateList listLock(item->listView());
        
        TObject  *obj = 0;
        const QIconSet  *set = 0;
        if ( fCurrentDrawn ) {
           obj = ((TQtObjectListItem *)fCurrentDrawn)->Object();
           if (obj) {
              if (obj->InheritsFrom(TVolume::Class()) )
                 set =   TQtIconBrowserImp::Shape2GeoShapeIcon(((TVolume *)obj)->GetShape()->ClassName());
              else if (obj->InheritsFrom(TVolumeView::Class()) )
                 set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TVolumeView *)obj)->GetShape()->ClassName());
              else if (obj->InheritsFrom(TShape::Class()) ) 
                set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TShape *)obj)->ClassName());
              else if (obj->InheritsFrom(TGeoVolume::Class()) ) 
                 set = TQtIconBrowserImp::IconList()->GetIcon(((TGeoVolume *)(obj))->GetShape()->GetName());
           }
           fCurrentDrawn->setPixmap(0,set ? set->pixmap(QIconSet::Small,true,QIconSet::On) : QPixmap());
           fCurrentDrawn->setPixmap(1,QPixmap());
        }

         fCurrentDrawn = item;
         obj = ((TQtObjectListItem *)fCurrentDrawn)->Object();
         if (obj) {
            if (obj->InheritsFrom(TVolume::Class()) )
              set =   TQtIconBrowserImp::Shape2GeoShapeIcon(((TVolume *)obj)->GetShape()->ClassName());
            else if (obj->InheritsFrom(TVolumeView::Class()) )
               set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TVolumeView *)obj)->GetShape()->ClassName());
            else if (obj->InheritsFrom(TShape::Class()) )
               set =  TQtIconBrowserImp::Shape2GeoShapeIcon(((TShape *)obj)->ClassName());
            else if (obj->InheritsFrom(TGeoVolume::Class()) )
               set = TQtIconBrowserImp::IconList()->GetIcon(((TGeoVolume *)(obj))->GetShape()->GetName());
         }
        // highlight the new item
         fCurrentDrawn->setPixmap(0,fIconSet.pixmap(QIconSet::Small,true,QIconSet::On)); 
//       fCurrentDrawn->setPixmap(0,QPixmap::fromMimeSource("arrow_right.xpm")); 
         fCurrentDrawn->setPixmap(1,set ? set->pixmap(QIconSet::Small,true,QIconSet::On) 
                                        : QPixmap::fromMimeSource("arrow_left.xpm")); 
      }
      drawItem(fCurrentDrawn, spinBox1->value(), fSingleVolumeCanvas ); 
   }
}



//_____________________________________________________________________________
void GeomBrowser::SetVisibility( TQtObjectListItem * item, TVolume::ENodeSEEN vis )
{
   // Convert the the visbility status to the checkmark  
   if (item) {
#if (QT_VERSION > 0x030100)
      item->setTristate(item->isExpandable());
      QCheckListItem::ToggleState s = QCheckListItem::Off;
      switch (vis) {
            case TVolume::kBothVisible:   s = QCheckListItem::On;       break;
            case TVolume::kSonUnvisible:  s = QCheckListItem::On;       break;
            case TVolume::kThisUnvisible: s = item->isExpandable() ? 
                                             QCheckListItem::NoChange : QCheckListItem::Off;
                                                                        break;
            case TVolume::kNoneVisible:   s = QCheckListItem::Off;      break;
      };
      item->setState(s); 
#else
      bool s = true;
      switch (vis) {
            case TVolume::kBothVisible:   s = true; break;
            case TVolume::kSonUnvisible:  s = true; break;
            case TVolume::kThisUnvisible: s = false; break;
            case TVolume::kNoneVisible:   s = false; break;
      };
      item->setOn(s); 
#endif   
   }
}



//_____________________________________________________________________________
void GeomBrowser::TurnGeomSrcEditor(bool on)
{
   // Turn on/off the source file seacth and look up
   if (on)
      QWhatsThis::display("Select the 3D image of the detector volume to popu the text editor");
}


#endif

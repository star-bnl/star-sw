#include "TVirtualPad.h"
#include "TContextMenu.h"

#include "TQVirtualGL.h"
#include "TQPadOpenGLView.h"
#include "TQtCoinWidget.h"

#define QGLVIEWER

#include "TQtCoinViewerImp.h"
//#include "TQtGLViewerWidget.h"
#include "TObject3DView.h"

#include "TSystem.h"
#include "TROOT.h"
#include "TEnv.h"
#include "TColor.h"

#include <qprinter.h>
#include <qpixmap.h>
#include <qapplication.h>
#include <qclipboard.h>
#include <qregexp.h> 
#include <qimage.h>

#if QT_VERSION < 0x40000
#  include <qfiledialog.h>
#  include <qpopupmenu.h>
#  include <qwhatsthis.h> 
#  include <qaction.h>
#  include <qpngio.h> 
#else 
#  include <QImageWriter>
#  include <QFileDialog>
#  include <QMenu>
#  include <QWhatsThis> 
//Added by qt3to4:
#  include <QLabel>
#  include <QAction>
#  include <QActionGroup>
#  include <QDebug>
#endif 

#include <qfile.h>
#include <qfileinfo.h>
#include <qmessagebox.h>
#include <qmenubar.h>
#include <qimage.h>
#include <qgl.h> 

#include <qpainter.h>
#include <qtextstream.h>
#include <qstatusbar.h>
#include <qsplitter.h>

#include <qevent.h>

#ifdef WIN32
#  ifdef CopyFile
#     undef CopyFile
#  endif
#endif


#include "TObject3DView.h"
#include "TView.h"

#include <qlayout.h> 
#include <qframe.h> 

#include <Inventor/SoOffscreenRenderer.h>

#include <vector>
#include  "TQtGLIncludes.h"

// grid icons
#include "cA00.xpm.h"
#include "cA01.xpm.h"
#include "cA10.xpm.h"
#include "cA11.xpm.h"
#include "cAcc.xpm.h"
#include "cAGrid.xpm.h"

/*
//______________________________________________________________________________
TQtCoinViewerImp::TQtCoinViewerImp(TPadOpenGLView *pad, const char *title,
                       UInt_t width, UInt_t height)
	: TGLViewerImp(pad,title,width,height)
{	
if QT_VERSION < 0x40000
   : QMainWindow(0,"glviewer", Qt::WDestructiveClose)
#else 
   : Q3MainWindow(0,"glviewer", Qt::WDestructiveClose)
#endif 
   , TGLViewerImp(pad,title,width,height)
   , fSaveType("JPEG"),fMaxSnapFileCounter(2),fGLWidget(0),fPad(0),fContextMenu(0),fSelectedView(0),fSelectedViewActive(kFALSE)
   , fSelectionViewer(kFALSE),fSelectionHighlight(kFALSE),fShowSelectionGlobal(kFALSE),fWantRootContextMenu(kFALSE)
   , fSnapShotAction(0)
 {
   if (pad) {
      TVirtualPad *thisPad = pad->GetPad();
      if (thisPad) {

         // Create the default SnapShot file name and type if any
         const char *fileDir = gSystem->Getenv("SnapShotDirectory");
         if (!(fileDir  && fileDir[0]) && ( gEnv ) ) {
             fileDir  = gEnv->GetValue("Gui.SnapShotDirectory",(const char *)0);
         }
         if (fileDir  && fileDir[0]) {  fSaveFile = fileDir; fSaveFile += "/"; }

         fSaveFile += thisPad->GetName();
         fSaveFile += ".";
         fSaveFile += "jpg";
         QString caption = thisPad->GetTitle();
         caption += ": OpenGL viewer";
         setCaption(caption);
         resize(width, height);
         fGLView = pad;
         CreateViewer(title);
         MakeMenu();
         int parts[] = {43,7,10,39};
         CreateStatusBar(parts,4);
         SetDrawList(0);
         ShowStatusBar();
         show();
      }
   }
   fMaxSnapFileCounter = CreateSnapShotCounter();

}
*/
/*
//______________________________________________________________________________
TQtGLViewerImp::TQtGLViewerImp(TPadOpenGLView *pad, const char *title,
                       Int_t x, Int_t y, UInt_t width, UInt_t height)

}
*/
//______________________________________________________________________________
TQtCoinViewerImp::TQtCoinViewerImp(TVirtualPad *pad, const char *title,
                       UInt_t width, UInt_t height)
 : QMainWindow(
#if QT_VERSION < 0x40000
     0,"coinviewer", Qt::WDestructiveClose
#endif 
   )
   , TGLViewerImp(0,title,width,height),fCoinWidget(0)
   , fSnapShotAction(0), fShowFrameAxisAction(0)
   //,fGLWidget(0),fSelectedView(0),fSelectedViewActive(kFALSE)
   //, fSelectionViewer(kFALSE),fSelectionHighlight(kFALSE),fShowSelectionGlobal(kFALSE)
{
#if QT_VERSION >= 0x40000
   setAttribute(Qt::WA_DeleteOnClose);
#endif   
   printf("TQtCoinViewerImp::TQtCoinViewerImp begin Pad=%p\n", pad);
       //Create the default SnapShot file name and type if any
      
   QString caption = " Coin : ";
   caption += pad ? pad->GetTitle() : "No TPad";
   caption += ": viewer";
   setCaption(caption);
   resize(width, height);
   fGLView = 0;
   CreateViewer(title);      
   fCoinWidget ->SetPad(pad);
   int parts[] = {43,7,10,39};
   CreateStatusBar(parts,4);
   MakeMenu();
   SetDrawList(0);
   show();
   // fMaxSnapFileCounter = 2;//CreateSnapShotCounter();
   printf("TQtCoinViewerImp::TQtCoinViewerImp end\n");
}
/*
//______________________________________________________________________________
TQtCoinViewerImp::TQtCoinViewerImp(TVirtualPad *pad, const char *title,
                       Int_t x, Int_t y, UInt_t width, UInt_t height)
	:TGLViewerImp(0,title,x,y,width,height)
	{
*/
/*
#if QT_VERSION < 0x40000
   : QMainWindow(0,"glviewer",Qt::WDestructiveClose | Qt::WRepaintNoErase | Qt:: WResizeNoErase )
#else 
   : Q3MainWindow(0,"glviewer",Qt::WDestructiveClose | Qt::WNoAutoErase | Qt:: WResizeNoErase )
#endif 
   , TGLViewerImp(0,title,x,y,width,height)
   , fSaveType("JPEG"),fMaxSnapFileCounter(2),fGLWidget(0),fPad(pad),fContextMenu(0),fSelectedView(0),fSelectedViewActive(kFALSE)
   ,fSelectionViewer(kFALSE),fSelectionHighlight(kFALSE),fShowSelectionGlobal(kFALSE),fWantRootContextMenu(kFALSE)
   , fSnapShotAction(0)
{
   if (fPad) {
      // Create the default SnapShot file name and type if any
      const char *fileDir = gSystem->Getenv("SnapShotDirectory");
      if (!(fileDir  && fileDir[0]) && ( gEnv ) ) {
         fileDir  = gEnv->GetValue("Gui.SnapShotDirectory",(const char *)0);
      }
      if (fileDir  && fileDir[0]) {  fSaveFile = fileDir; fSaveFile += "/"; }
      fSaveFile += fPad->GetName();
      fSaveFile += ".";
      fSaveFile += "jpg";
      QString caption = fPad->GetTitle();
      caption += ": OpenGL viewer";
      setCaption(caption);

      setGeometry(x, y, width, height);
      fGLView = 0;
      CreateViewer(title);
      MakeMenu();
      int parts[] = {43,7,10,39};
      CreateStatusBar(parts,4);
      SetDrawList(0);
      show();
   }
   fMaxSnapFileCounter = CreateSnapShotCounter();
   
}  

//______________________________________________________________________________
TQtGLViewerImp::TQtGLViewerImp(TQtGLViewerImp &parent) :
#if QT_VERSION < 0x40000
     QMainWindow(&parent,"glviewerutil", Qt::WDestructiveClose | Qt::WType_TopLevel)
#else 
     Q3MainWindow(&parent,"glviewerutil",Qt::WDestructiveClose | Qt::WType_TopLevel)
#endif 
   , TGLViewerImp(0,"selection",parent.width(),parent.height())
   , fSaveType(parent.fSaveType),fMaxSnapFileCounter(2),fGLWidget(0),fPad(parent.fPad),fContextMenu(0),fSelectedView(0),fSelectedViewActive(kFALSE)
   , fSelectionViewer(kTRUE),fSelectionHighlight(kFALSE),fShowSelectionGlobal(kFALSE),fWantRootContextMenu(kFALSE)
   , fSnapShotAction(0)
{
   // create a satelite widget
   connect(this,SIGNAL(destroyed() ),&parent, SLOT(DisconnectSelectorWidgetCB()));
   QString caption;
   if (fPad) {
      // Create the default SnapShot file name and type if any
      const char *fileDir = gSystem->Getenv("SnapShotDirectory");
      if (!(fileDir  && fileDir[0]) && ( gEnv ) ) {
         fileDir  = gEnv->GetValue("Gui.SnapShotDirectory",(const char *)0);
      }
      if (fileDir  && fileDir[0]) {  fSaveFile = fileDir; fSaveFile += "/"; }
      fSaveFile += fPad->GetName();
      fSaveFile += ".";
      fSaveFile += "jpg";
      caption = fPad->GetTitle();
   }
   fGLView = 0;
   caption += ": selection viewer";
   setCaption(caption);
   resize(2*parent.width()/3,2*parent.height()/3);
   CreateViewer(parent.GLWidget(),"selection");
   MakeMenu();
   int parts[] = {43,7,10,39};
   CreateStatusBar(parts,4);
   SetDrawList(0);
   show();
   fMaxSnapFileCounter = CreateSnapShotCounter();
}
*/
//______________________________________________________________________________
TQtCoinViewerImp::~TQtCoinViewerImp()
{ }

//______________________________________________________________________________
void TQtCoinViewerImp::AddRootChild(ULong_t id, EObject3DType type)
{ 
   if (fCoinWidget) fCoinWidget->AddRootChild(id,type);
}

//______________________________________________________________________________
void TQtCoinViewerImp::ClearCB()
{
   if (fCoinWidget) fCoinWidget->ClearCB();
}

//______________________________________________________________________________
void TQtCoinViewerImp::Clear(const char *opt)
{
   if (fCoinWidget) fCoinWidget->Clear(opt);
}
/*
//______________________________________________________________________________
void TQtGLViewerImp::CreateStatusBar(Int_t nparts)
{
 //  Create the status bar with "nparts" separate portions
  QStatusBar *thisStatusBar = statusBar();
  Int_t i=0;
  fStatusBar.resize(nparts);
  for (i=0;i<nparts;i++) {
    QLabel *l = new QLabel(thisStatusBar);
    thisStatusBar->addWidget(l,1,TRUE);
    // remember to delete later
    fStatusBar.insert(i,l);  
  }
}
*/

//______________________________________________________________________________
void TQtCoinViewerImp::CreateStatusBar(Int_t * /*parts*/, Int_t /*nparts*/ )
{
  //  Create the status bar with "nparts" separate portions
  // The initial relative size of each part is defined by "parts" array
/*
  QStatusBar *thisStatusBar = statusBar();
#ifdef WIN32
  thisStatusBar->setSizeGripEnabled(FALSE);
#endif
  // Any number of widgets may be controlled by just
  // one splitter
  QSplitter *split = new QSplitter(thisStatusBar);
  thisStatusBar->addWidget(split,1,FALSE);

  fStatusBar.resize(nparts);
  Int_t iField=0;
  for (iField=0; iField<nparts; iField++) {
    QLabel *infoBox = new QLabel(split);
    infoBox->setIndent(3);
    QSize s = infoBox->size();
    s.setWidth(parts[iField]);
    infoBox->resize(s);

    // remember to delete later
    fStatusBar.insert(iField,infoBox);
  }
  */
}
/*
//______________________________________________________________________________
int TQtCoinViewerImp::CreateSnapShotCounter()
{
   // Define the number of the snapshot frames based on either
   // "SnapShotFileCounter" environment variable
   //    use: 
   //          setenv SnapShotFileCounter 100
   // to produce 100  different frames in raw
   // or the ROOT parameters (via ".rootrc" file) 
   //
   //   Gui.SnapShotFileCounter 100
   //
   const char *dcounter = gSystem->Getenv("SnapShotFileCounter");
   if (!(dcounter && dcounter[0]) && ( gEnv ) ) {
        dcounter  = gEnv->GetValue("Gui.SnapShotFileCounter","2");
   }
   if (dcounter && dcounter[0]) {
      int count = QString(dcounter).toInt();
      if (count > 2 )  gfDefaultMaxSnapFileCounter = count;
   }
   return gfDefaultMaxSnapFileCounter;
}
*/
//______________________________________________________________________________
TContextMenu &TQtCoinViewerImp::ContextMenu() 
{
   // Create the TContextMenu if needed and return it
   return  fCoinWidget ? fCoinWidget->ContextMenu() : *(TContextMenu *)0;
}

/*
//______________________________________________________________________________
void TQtGLViewerImp::DisconnectSelectorWidgetCB()
{
   // [slot] to disconnect the destroyed  "selection" widget
  fSelectedView = 0; 
}

//______________________________________________________________________________
void TQtGLViewerImp::SetStatusText(const char *text, Int_t partidx, Int_t stype)
{  
  // Set Text into the 'npart'-th part of the status bar
  if (Int_t(fStatusBar.size()) > partidx) {
    if (stype >=  0) {
       TColor *rootColor = gROOT->GetColor(stype);
       float rgb[3];
       rootColor->GetRGB(rgb[0],rgb[1],rgb[2]);
       fStatusBar[partidx]->setPaletteForegroundColor(QColor(int(255*rgb[0]),int(255*rgb[1]),int(255*rgb[2])));
    }
    fStatusBar[partidx]->setText(text);
  }
}
//______________________________________________________________________________
void TQtGLViewerImp::ShowStatusBar(Bool_t show)
{
   // Show / Hide the status bar
   
  if (show) statusBar()->show();
  else      statusBar()->hide();
}
*/
//______________________________________________________________________________
 void  TQtCoinViewerImp::DisconnectPad()
 {
   QObject::disconnect(&Signals(),SIGNAL( ObjectSelected(TObject *, const QPoint&)));
   QObject::disconnect(&Signals(),SIGNAL( HandleSelected(ULong_t, const QPoint&)));
   QObject::disconnect(&Signals(),SIGNAL(ImageSaved(QString ,QString , int)));
   if (fCoinWidget) fCoinWidget->DisconnectPad();
 }
//______________________________________________________________________________
void TQtCoinViewerImp::SetUpdatesEnabled(bool enable)
{  
    setUpdatesEnabled(enable);                            
    if (fCoinWidget) fCoinWidget->SetUpdatesEnabled(enable);
}
//______________________________________________________________________________
bool TQtCoinViewerImp::GetUpdatesEnabled() const
{  
   return fCoinWidget ?
             fCoinWidget->GetUpdatesEnabled()
           : isUpdatesEnabled();
}
//______________________________________________________________________________
Option_t   *TQtCoinViewerImp::GetDrawOption() const
{
   return fCoinWidget ? fCoinWidget->GetDrawOption(): 0;
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetDrawOption(Option_t *option)
{
   if (fCoinWidget) fCoinWidget->SetDrawOption(option);
}
//______________________________________________________________________________
TVirtualPad *TQtCoinViewerImp::GetPad() 
{
   if (GetGLView()) return GetGLView()->GetPad();
   return  fCoinWidget ? fCoinWidget->GetPad() : 0;
}
/*
//______________________________________________________________________________
//
// Slots
//______________________________________________________________________________
//______________________________________________________________________________
void TQtGLViewerImp::ActivateSelectorWidgetCB(bool on)
{
     // Active the separate window to show the selected objects
    fSelectedViewActive =  on;
    CreateSelectionViewer();
}
//______________________________________________________________________________
void TQtGLViewerImp::ActivateSelectionHighlighCB(bool on)
{
     // Active the separate window to show the selected objects
    fSelectionHighlight  =  on;
}
//______________________________________________________________________________
void TQtGLViewerImp::ActivateSelectionGlobalCB(bool on)
{
     // Show the selected object on the global coordinate system
    fShowSelectionGlobal =  on;
}
*/
//______________________________________________________________________________
void TQtCoinViewerImp::AddGLList(unsigned int list, EObject3DType type)
{
   printf("TQtCoinViewerImp::AddGLList\n");
   if (fCoinWidget) fCoinWidget->AddGLList(list, type);
}

//______________________________________________________________________________
void TQtCoinViewerImp::RemoveGLList(unsigned int list)
{
	printf("TQtCoinViewerImp::RemoveGLList\n");
   if (fCoinWidget) fCoinWidget->RemoveGLList(list);
}
/*
//______________________________________________________________________________
void TQtGLViewerImp::NewViewer(){
  new TQtGLViewerImp(GetGLView());

*/
//______________________________________________________________________________
void TQtCoinViewerImp::PrintCB()
{
   if (fCoinWidget) fCoinWidget->PrintCB();
}

//______________________________________________________________________________
void TQtCoinViewerImp::CopyCB()
{
   //  Copy the current 3d view to the system clipboard 
   if (fCoinWidget)  fCoinWidget->CopyCB();
}
//______________________________________________________________________________
void TQtCoinViewerImp::CopyFrameCB()
{
   // Copy the entire window including the menu and the status bar
   QClipboard *cb = QApplication::clipboard();
   cb->setPixmap(QPixmap::grabWidget(topLevelWidget()));
}

//______________________________________________________________________________
void TQtCoinViewerImp::OpenCB()
{ 
   if (fCoinWidget) {
      QString filter = "OpenInventor IV files (*.iv);;WRL File (*.wrl)";
      QString selectedFilter;
      QString thatFile = QFileDialog::getOpenFileName(
#if QT_VERSION < 0x40000
          gSystem->WorkingDirectory()
        , filter
        , centralWidget()
        , "Open"
        , "Add graph to coin scene");
#else 
          centralWidget()
        , tr("Add graph to coin scene")
        , gSystem->WorkingDirectory()
        , filter
        );
#endif 
      
      fCoinWidget->ReadInputFile(thatFile);
   }
}

//______________________________________________________________________________
static QString ListOfFilters() 
{
   QString filter = "";
#if QT_VERSION < 0x40000
   //  Add the Qt image formats
   unsigned int i=0;
   filter =  "Image (";
   for (i = 0; i < QImageIO::outputFormats().count(); i++ ) 
   {
      if (i) filter +=',';
      filter += "*.";
      QString str = QString( QImageIO::outputFormats().at( i ) );
      filter += str.lower();
   }
   filter +=");";
#endif

   SoOffscreenRenderer r(*(new SbViewportRegion));
   int num = r.getNumWriteFiletypes();

   if (num == 0) {
     (void)fprintf(stdout,
                   "No image formats supported by the "
                  "SoOffscreenRenderer except SGI RGB and Postscript.\n");
   } else {
      for (int i=0; i < num; i++) {
         SbPList extlist;
         SbString fullname, description;
         r.getWriteFiletypeInfo(i, extlist, fullname, description);
         if (i > 0) filter+= ";";
         filter += fullname.getString(); 
         // (void)fprintf(stdout, "%s: %s (extension%s: ",
         //             fullname.getString(), description.getString(),
         //             extlist.getLength() > 1 ? "s" : "");
         filter+= " ( " ;
         for (int j=0; j < extlist.getLength(); j++) {
            if (j>0) filter+= ", "; filter+= "*."; filter+=(const char*) extlist[j];
            // (void)fprintf(stdout, "%s%s", j>0 ? ", " : "", (const char*) extlist[j]);
         }
         filter += " );";
         // (void)fprintf(stdout, ")\n");
      }
   }
   return filter;
}

//______________________________________________________________________________
static QStringList ExtensionList(const QString &filter)
{
   // Return the list of the extsntion from the file dialog filter
   QRegExp rx("(\\*\\.\\w+\\b)");
   QStringList extension;
   int pos = 0;
   while ( pos >= 0 ) {
      pos = rx.search(filter,pos);
      if ( pos > -1 ) {
         extension += rx.cap(1).replace("*.","");
         pos  += rx.matchedLength();
      }
   }   
   return extension ;
}
//______________________________________________________________________________
void TQtCoinViewerImp::ReadInputFile(const char *fileName)
{
   // render the extrenal Open Inventor file
    if (fCoinWidget) fCoinWidget->ReadInputFile(fileName);
}
//______________________________________________________________________________
void TQtCoinViewerImp::SaveCB()
{ 
   if (fCoinWidget) {
      printf("TQtCoinViewerImp::SaveCB\n");
      //if (!c) return;
      QString filter = ListOfFilters();
      filter +=";Graphics Interchange Format (*.gif);";
      filter +=";Transparent Img File (*.rgbt);;WRL File (*.wrl);;IV files (*.iv);";
      filter +=";Moving Picture Experts Group (*.mpg);";
      
      QString selectedFilter;
      QString thatFile = QFileDialog::getSaveFileName(
#if QT_VERSION < 0x40000
         gSystem->WorkingDirectory()
       , filter
       , centralWidget()
       , "SaveAs"
       , "Save the current view as"
       , &selectedFilter);
#else 
         centralWidget()
       , tr("Save the current view as")
       , gSystem->WorkingDirectory()
       , filter
       , &selectedFilter);
#endif 
   
      QStringList selectedExtensions = ExtensionList(selectedFilter);
   
      if (thatFile.isEmpty()) return;
      QString e;
      QFileInfo outFile( thatFile );
      e = outFile.extension(FALSE).lower();
      int extIndex = selectedExtensions.findIndex(e);
     
      if (extIndex == -1) {
         // use the first extension
         e = selectedExtensions[0];
         // add this extenstion tho the existent file
         thatFile += ".";  thatFile += e;
      }
//      printf("selectedFilter = %s\n", selectedFilter.data());//      printf("thatFile = %s\n", thatFile.data())


//      printf("e = <%s>\n", e.data());
      
      // EXECUTE THE ACTION
      fCoinWidget->Save(thatFile,e);
   }
}
//______________________________________________________________________________
void TQtCoinViewerImp::SaveAsCB()
{ 
  if (fCoinWidget) fCoinWidget->SaveAsCB();
}

//______________________________________________________________________________
void TQtCoinViewerImp::SaveSnapShot(bool yes)
{
  if (fCoinWidget) fCoinWidget->SaveSnapShot(yes);
}
//______________________________________________________________________________
void TQtCoinViewerImp::Save(const QString &filename,const QString  &type)
{
   if (fCoinWidget) fCoinWidget->Save(filename,type);
}
//______________________________________________________________________________
void TQtCoinViewerImp::Save(const char *filename, const char *type)
{
   Save (QString(filename),QString(type));
}
//______________________________________________________________________________
void TQtCoinViewerImp::Print(const char *filename, const char *type)
{
   Save(filename,type);
}
//______________________________________________________________________________
void TQtCoinViewerImp::Print(const QString &filename,const QString  &type)
{
   Save(filename,type);
}

/*
//______________________________________________________________________________
void TQtGLViewerImp::CopyFile(const QString &fileName2Copy,Int_t counter)
{
  QFileInfo infoOldFile(fileName2Copy);
  QString newName = infoOldFile.dirPath() + "/" + infoOldFile.baseName(TRUE) +
  "_S."+infoOldFile.extension(FALSE); 
  QFileInfo infoNewFile(newName);

QString shellCommand;
#ifndef WIN32  
   shellCommand = "cd " + infoOldFile.dirPath(true)  + " && " +
                  "cp " + infoOldFile.baseName(TRUE) + "-000" + QString::number(counter) + "." + infoOldFile.extension(FALSE)
                       + "   " + infoNewFile.baseName(TRUE) + "-000" + QString::number(counter) + "."
                         + infoNewFile.extension(FALSE);
#else
   shellCommand = 
                 "copy " + infoOldFile.baseName(TRUE) + "-000" + QString::number(counter) + "." + infoOldFile.extension(FALSE)
                         + "   " + infoNewFile.baseName(TRUE) + "-000" + QString::number(counter) + "." + infoNewFile.extension(FALSE);
#endif
   gSystem->Exec((const char*) shellCommand);
//   fprintf(stderr," ==Copy==  %s \n", (const char*) shellCommand);

   SaveHtml(newName,counter);
}
//______________________________________________________________________________
void TQtGLViewerImp::SaveHtml(Int_t counter)
{
   if (fSaveFile.IsNull())  SaveAsCB();
   if (!fSaveFile.IsNull()) {
     QString f(fSaveFile.Data());
     SaveHtml(f,counter);
   }
}
//______________________________________________________________________________
void TQtGLViewerImp::SaveHtml(QString &saveFile, Int_t counter)
{
#if 0
   if (fSaveFile.IsNull()) {
       SaveAsCB();
   }
   if (fSaveFile.IsNull()) return;
   
   QFileInfo info(fSaveFile.Data());
#else
   if (saveFile.isEmpty()) return;
   
   QFileInfo info(saveFile);
#endif   
   QString htmlFile(  info.dirPath(true) + "/" + info.baseName(TRUE) + "-000" +
                      QString::number(counter) +".html");
#ifdef GENERATE_HTML
   TVirtualPad *thisPad= GetPad();
   QString caption = thisPad->GetTitle();
   FILE *f = fopen((const char*)htmlFile, "w+");
   if (f) {
   QTextOStream(f) << "<html>"   << endl; 
   QTextOStream(f) << "<title>"  << endl;
   QTextOStream(f) <<  caption << endl;
//   QTextOStream(f) <<  caption() << endl;
   QTextOStream(f) << "</title>" << endl;
   QTextOStream(f) << "<head>"   << endl;
   QTextOStream(f) << "<meta http-equiv=\"REFRESH\" content=\"15\">" << endl;
   QTextOStream(f) << "</head>"  << endl;
   QTextOStream(f) << "<body>"   << endl;
   QTextOStream(f) << "<center>" << endl;
   QTextOStream(f) << "<H2> <a href=\"http://www.star.bnl.gov\">STAR@rhic</a></H2><br>" << endl;
//   QTextOStream(f) << "<H3>" << caption() << "</H3><br><hr><br>"   << endl;
   QTextOStream(f) << "<H3>" << caption << "</H3><br><hr><br>"   << endl;
   QTextOStream(f) << "<img src=\"" << info.baseName(TRUE) << "-000" + 
     QString::number(counter) + "." <<info.extension(FALSE)  << "\">" << endl;
//     QString::number(counter) + "." <<info.extension(FALSE)  << "\" width=100%>" << endl;
   QTextOStream(f) << "</center>"<< endl;
   QTextOStream(f) << "</body>"  << endl;
   QTextOStream(f) << "</html>"  << endl;
   fflush(f);
   fclose(f);
#ifndef WIN32  
   QString shellCommand =  "cd " + info.dirPath(true)  + " && " 
                         + "mv   " + info.baseName(TRUE) + "-000" + QString::number(counter) +".html "
                         + info.baseName(TRUE) + ".html";
#else
   QString shellCommand =  "cd  " + info.dirPath(true)  + " && " 
                         + "ren " + info.baseName(TRUE) + "-000" + QString::number(counter) +".html "
                                  + info.baseName(TRUE) + ".html";
#endif                         
#else
   QString shellCommand;
   QFileInfo html(htmlFile);
   if (html.exists() && html.isReadable() ) {
#ifndef WIN32
           shellCommand =  "cd " + info.dirPath(true)  + " && " 
                         + "cp " + htmlFile   + " "
                                 + "." + info.baseName(TRUE) + ".html && "
                         + "mv " + "." + info.baseName(TRUE) + ".html " + info.baseName(TRUE) + ".html";
#else
          shellCommand =   "cd  " + info.dirPath(true)  + " && "
                         + "copy "+ htmlFile            + "   "
                                  + "." + info.baseName(TRUE) + ".html && "
                         + "ren  "+ "." + info.baseName(TRUE) + ".html " + info.baseName(TRUE) + ".html";
#endif
   }
#endif
    if (!shellCommand.isEmpty())  gSystem->Exec((const char*) shellCommand);
    // fprintf(stderr," ***  %s \n", (const char*) shellCommand);
#ifdef GENERATE_HTML
   } else {
      char buffer[100];
      sprintf(buffer,"TQtGLViewerImp::SaveHtml file %s", (const char*)htmlFile);
      perror(buffer);
      assert(0);
   }
#endif
}
*/

//______________________________________________________________________________
void TQtCoinViewerImp::SnapShotSaveCB(bool on)
{  
	 if (fCoinWidget) fCoinWidget->SnapShotSaveCB(on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::SynchTPadCB(bool on)
{  
   if (fCoinWidget) fCoinWidget->SynchTPadCB (on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::ShowFrameAxisCB(bool on)
{  
   if (fCoinWidget) fCoinWidget->ShowFrameAxisCB(on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::ShowLightsCB(bool on)
{  
  if (fCoinWidget) fCoinWidget->ShowLightsCB(on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::WantRootContextMenuCB(bool on)
{
  // Create "ROOT Context menu" otherwise use the QGLViewer "right mouse click"
  if (fCoinWidget) fCoinWidget->WantRootContextMenuCB(on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::WantClipFileNodeMenuCB(bool on)
{
  // Allow clip the "file node" with the geometry decoration
  if (fCoinWidget) fCoinWidget->SetClipMask(on);
}

//______________________________________________________________________________
void TQtCoinViewerImp::AboutCB()
{ 
   QMessageBox::aboutQt(0);
   QString rootVersion = "ROOT ";
   rootVersion += gROOT->GetVersion();
   rootVersion += "with Qt interface";
   QMessageBox::about(0,rootVersion,"ROOT Qt interface Coin Viewer 2006 Zubanov Alexei AZubanov@gmail.com");
   if (fCoinWidget) fCoinWidget->AboutCB();
}

//______________________________________________________________________________
void TQtCoinViewerImp::HelpCB()
{
  if (fCoinWidget) fCoinWidget->HelpCB();
}
//______________________________________________________________________________
void TQtCoinViewerImp::CreateViewer(const char *name)
{
    if (!fCoinWidget) {
      fCoinWidget = new TQtCoinWidget(this);
      fCoinWidget->setName(name);
      setCentralWidget (fCoinWidget);
      connect(fCoinWidget,SIGNAL(ObjectSelected(TObject*,const QPoint &))
             , &this->Signals(), SIGNAL(ObjectSelected(TObject *, const QPoint&)));
      connect(fCoinWidget,SIGNAL(NodeSelected(ULong_t,const QPoint &))
             , &this->Signals(), SIGNAL(HandleSelected(ULong_t, const QPoint&)));
      connect(fCoinWidget,SIGNAL(ImageSaved(QString ,QString , int))
             , &this->Signals(), SIGNAL(ImageSaved(QString ,QString , int)));
   }
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetBoxSelection()
{
   if (fCoinWidget) fCoinWidget->SetBoxSelection();
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetLineSelection()
{
   if (fCoinWidget) fCoinWidget->SetLineSelection();
}

/*
//______________________________________________________________________________
void TQtGLViewerImp::CreateViewer(QGLWidget *share, const char *name)
{
   // Create satellite viewe to share the same GL lists
   if (!fGLWidget) {
      fGLWidget = new TQtGLViewerWidget( this,name,share );
      TQtGLViewerWidget *glw = (TQtGLViewerWidget *)fGLWidget;
#if 0      
      glw->setSceneCenter(((TQtGLViewerWidget *)share)->sceneCenter());
      glw->setSceneRadius(((TQtGLViewerWidget *)share)->sceneRadius());
      glw->setBackgroundColor(((TQtGLViewerWidget *)share)->backgroundColor());
#endif      
      glw->makeCurrent();
      glw->setSceneCenter(((TQtGLViewerWidget *)share)->sceneCenter());
      glw->setSceneRadius(((TQtGLViewerWidget *)share)->sceneRadius());
      glw->setBackgroundColor(((TQtGLViewerWidget *)share)->backgroundColor());

      setCentralWidget (fGLWidget);
      glw->setSnapshotFileName  (fSaveFile.Data());
      connect(fGLWidget,SIGNAL(objectSelected(TObject*,const QPoint &)),this,SLOT( ShowObjectInfo(TObject *, const QPoint&)));
   }
}
//______________________________________________________________________________
void TQtGLViewerImp::MakeCurrent() 
{
#ifdef QGLVIEWER
   fGLWidget->makeCurrent();
#endif
}
*/
//______________________________________________________________________________
void TQtCoinViewerImp::MakeMenu()
{
   // Create a "save" action
#if QT_VERSION < 0x40000
   QAction *saveAction =  new QAction("Save", "&Save", CTRL+Key_S, this, "save" );
   connect ( saveAction, SIGNAL( activated() ) , this, SLOT( SaveCB() ) );
#else 
   QAction *saveAction =  new QAction("&Save",  this);
   connect ( saveAction, SIGNAL( triggered() ) , this, SLOT( SaveCB() ) );
#endif 

   const char * saveText = "<p><img source=\"save\"> "
                "Click this button to save a <em>3D image</em>to the current image file. <br>"
                "You can also select the <b>Save</b> command "
                "from the <b>File</b> menu.</p>";
   saveAction->setWhatsThis( saveText );

   // Create a "save as" action
#if QT_VERSION < 0x40000
   QAction *saveAsAction =  new QAction("SaveAs", "Save As", CTRL+Key_A, this, "saveas" );
   connect ( saveAsAction, SIGNAL( activated() ) , this, SLOT( SaveAsCB() ) );
#else 
   QAction *saveAsAction =  new QAction("Save &As", this);
   connect ( saveAsAction, SIGNAL(triggered() ) , this, SLOT( SaveAsCB() ) );
#endif 

   const char * saveAsText = "<p><img source=\"save\"> "
                "Click this button to select file and save a <em>3D image</em>there. <br>"
                "You can also select the <b>Save As</b> command "
                "from the <b>File</b> menu.</p>";
   saveAsAction->setWhatsThis( saveAsText );
    
   // Create a "open" action
#if QT_VERSION < 0x40000
   QAction *openAction =  new QAction("Open", "Open", CTRL+Key_O, this, "open" );
   connect ( openAction, SIGNAL( activated() ) , this, SLOT( OpenCB() ) );
#else 
   QAction *openAction =  new QAction("&Open", this);
   connect ( openAction, SIGNAL( triggered() ) , this, SLOT( OpenCB() ) );
#endif 

   const char * openText = "<p><img source=\"save\"> "
                "Click this button to select file and open a <em>3D image</em>there. <br>"
                "You can also select the <b>Open</b> command "
                "from the <b>File</b> menu.</p>";
   openAction->setWhatsThis( openText );
 
   // Create a "save as" action
#if QT_VERSION < 0x40000
   fSnapShotAction  =  new QAction("snapShot", "Record", CTRL+Key_R, this, "record" );
   fSnapShotAction->setToggleAction(true);
#else 
   fSnapShotAction  =  new QAction("&Record", this);
   fSnapShotAction->setCheckable(true);
#endif 
   connect ( fSnapShotAction, SIGNAL( toggled(bool) ) , this, SLOT( SnapShotSaveCB(bool) ) );

   const char * snapShotText = "<p><img source=\"snapshot\"> "
                "Click this button to create the animation, i.e. to save the image each time the frame is updated";
   fSnapShotAction->setWhatsThis( snapShotText );

   // Create a "print" action
#if QT_VERSION < 0x40000
   QAction *printAction =  new QAction("Print", "&Print graph to stdout", CTRL+Key_P, this, "print" );
   connect ( printAction, SIGNAL( activated() ) , this, SLOT( PrintCB() ) );
#else 
   QAction *printAction =  new QAction("&Print", this);
   connect ( printAction, SIGNAL( triggered() ) , this, SLOT( PrintCB() ) );
#endif 

   const char * printText = "<p><img source=\"print\"> "
                "Click this button to print a <em>3D image</em>. <br>"
                "You can also select the <b>Print</b> command "
                "from the <b>File</b> menu.</p>";
   printAction->setWhatsThis( printText );

   // Create a "clear scene" action
#if QT_VERSION < 0x40000
   QAction *clearAction =  new QAction("Print", "&Clear Scene", CTRL+Key_C, this, "clear" );
   connect ( clearAction, SIGNAL( activated() ) , this, SLOT( ClearCB() ) );
#else 
   QAction *clearAction =  new QAction("&Clear", this);
   connect ( clearAction, SIGNAL( triggered() ) , this, SLOT( ClearCB() ) );
#endif 

   const char * clearText = "<p><img source=\"print\"> "
                "Click this button to clear a <em>3D image</em>. <br>"
                "You can also select the <b>Clear</b> command "
                "from the <b>File</b> menu.</p>";
   clearAction->setWhatsThis( clearText );

   // Create a "copy" action
#if QT_VERSION < 0x40000
   QAction *copyAction =  new QAction("Copy", "&Copy", CTRL+Key_C, this, "copy" );
   connect ( copyAction, SIGNAL( activated() ) , this, SLOT( CopyCB() ) );
#else 
   QAction *copyAction =  new QAction("&Copy", this);
   connect ( copyAction, SIGNAL( triggered() ) , this, SLOT( CopyCB() ) );
#endif 

   const char * copyText = "<p><img source=\"copy\"> "
                "Click this button to copy a <em>3D image</em>to the system clipborad. <br>"
                "You can also select the <b>Copy</b> command "
                "from the <b>Edit</b> menu.</p>";
   copyAction->setWhatsThis( copyText );
 
    // Create a "copy frame" action
#if QT_VERSION < 0x40000
   QAction *copyFrameAction =  new QAction("Frame", "Copy &Frame", CTRL+Key_F, this, "frame" );
   connect ( copyFrameAction, SIGNAL( activated() ) , this, SLOT( CopyFrameCB() ) );
#else 
   QAction *copyFrameAction =  new QAction("Copy &Frame", this);
   connect ( copyFrameAction, SIGNAL( triggered() ) , this, SLOT( CopyFrameCB() ) );
#endif 
   const char * copyFrameText = "<p><img source=\"frame\"> "
                "Click this button to copy <em>the frame of the 3D image</em>to the system clipborad. <br>"
                "You can also select the <b>Copy Frame</b> command "
                "from the <b>Edit</b> menu.</p>";
   copyFrameAction->setWhatsThis( copyFrameText );

   // Create "close" action
#if QT_VERSION < 0x40000
   QAction *fileCloseAction = new QAction( "Close", "&Close", Qt::ALT+Qt::Key_F4, this,
      "close" );
   connect ( fileCloseAction, SIGNAL( activated() ) , this,  SLOT( close() ) );
#else
   QAction *fileCloseAction = new QAction("Close", this);
   fileCloseAction->setShortcut(Qt::ALT+Qt::Key_F4);    
   connect ( fileCloseAction, SIGNAL( triggered() ) , this,  SLOT( close() ) );
#endif

   // Synchronize TPad rotation 
   
#if QT_VERSION < 0x40000
   QAction *synchAction =  new QAction("TPadSynch", "Synchronize with TPad", CTRL+Key_I, this, "synch" );
   synchAction->setToggleAction(true);
#else 
   QAction *synchAction =  new QAction("Synchron&ize with TPad", this);
   synchAction->setCheckable(true);
#endif 
   connect ( synchAction, SIGNAL( toggled(bool) ) , this, SLOT( SynchTPadCB(bool)  ) );
   const char * synchText = "<p><img source=\"frame\"> "
                "Select this option if your want the OpenGL view follows the <b>TPad</B> rotation";
   synchAction->setWhatsThis( synchText );

   // Show frame axis
   
#if QT_VERSION < 0x40000
   QAction *showFrameAxisAction =  new QAction("Frame3DAxis", "Show Frame axes", CTRL+Key_1, this, "frameaxis" );
   showFrameAxisAction->setToggleAction(true);
   connect ( showFrameAxisAction, SIGNAL( toggled(bool) ) , this, SLOT( ShowFrameAxisCB(bool)  ) );
   const char *showFrameAxisText = "Show the ROOT 3D object axes";
   showFrameAxisAction->setWhatsThis( showFrameAxisText);
#else 
   QAction *showFrameAxisAction =  fShowFrameAxisAction = new QAction("Show Frame axes", this);
   QMenu *showFrameAxisMenu = new QMenu("Set Axes &Origins", this);
   showFrameAxisAction->setCheckable(true);
   showFrameAxisAction->setShortcut(Qt::CTRL+Qt::Key_1);    
   connect ( showFrameAxisAction, SIGNAL( toggled(bool) ) , this, SLOT( ShowFrameAxisCB(bool)  ) );
   const char *xNames[] = {"X", "Y", "Z"};
   for (int i=0; i<3; ++i) { // 3 axes x.y.z
       QMenu *axMenu = showFrameAxisMenu->addMenu(QString("O")+xNames[i]); 
       QActionGroup *axGroup = new QActionGroup(this);
       axGroup->setExclusive(false);
       switch (i) {
          case 0:
             connect(axGroup,SIGNAL(triggered ( QAction *) ), this, SLOT(SetAxisPositionXCB(QAction *)));
             break;
          case 1:
             connect(axGroup,SIGNAL(triggered ( QAction *) ), this, SLOT(SetAxisPositionYCB(QAction *)));
             break;
          case 2:
             connect(axGroup,SIGNAL(triggered ( QAction *) ), this, SLOT(SetAxisPositionZCB(QAction *)));
             break;
       }
       static const char **axicons[] = { cA00, cA01, cA10, cA11, cAcc, cAGrid } ;
       static const char *locNames[] = { "-1/-1", "-1/+1", "+1/-1", "+1/+1", "0/0"} ;
       static const char *locTip[] = { "(min %1/min %2)", "(min %1 /max %2)", "(max %1/min %2)", "(max %1/max %2)", "(center %1/center %2)"} ;
       unsigned char initLocation = fCoinWidget ? fCoinWidget->GetLocation(i) : 0;
       for (int j=0;j<5; ++j) { // 5 possible locations for each axis
          QAction *a = new QAction(QIcon(QPixmap(axicons[j])),locNames[j],this);
          QString planeNames[2];
          int counter = 0;
          for (int kk =0; kk<3; kk++) {
             if (kk == i) continue;
             planeNames[counter++] = xNames[i];
          }
          QString tip = QString("The \"%1\" axis crosses the %2%3 plane at ").arg(xNames[i]).arg(planeNames[0]).arg(planeNames[1]) 
                + QString(locTip[j]).arg(planeNames[0]).arg(planeNames[1]);
          a->setToolTip(tip);
          a->setCheckable(true);
          if (initLocation & 0x1) a->setChecked(true);
          initLocation >>= 1;
          axGroup->addAction(a);
          axMenu->addAction(a);
       }
   }
#endif 

   // Show frame axis
   
#if QT_VERSION < 0x40000
   QAction *showSmallAxesAction =  new QAction("Small3DAxes", "Show \"small\" axes", CTRL+Key_2, this, "smallaxes" );
   showSmallAxesAction->setToggleAction(true);
#else 
   QAction *showSmallAxesAction =  new QAction("Show \"small\" axes", this);
   showSmallAxesAction->setCheckable(true);
   showSmallAxesAction->setShortcut(Qt::CTRL+Qt::Key_3);    
#endif 
   connect ( showSmallAxesAction, SIGNAL( toggled(bool) ) , this, SLOT( SmallAxesActionCB(bool)  ) );
   const char *showSmallAxesText = "Show the  small 3D axes at the bottom right corner of the screen. Can slow down the rendering !!!";
   showSmallAxesAction->setWhatsThis( showSmallAxesText);

#if QT_VERSION < 0x40000
   QAction *showLightsAction =  new QAction("GLLights", "Show &light", CTRL+Key_L, this, "gllight" );
   showLightsAction->setToggleAction(true);
#else 
   QAction *showLightsAction =  new QAction("Show &light",this);
   showLightsAction->setCheckable(true);
#endif 
   connect ( showLightsAction, SIGNAL( toggled(bool) ) , this, SLOT( ShowLightsCB(bool)  ) );
   const char * showLightsText = "<p><img source=\"frame\"> "
                "Show the light source to debug the code";
   showLightsAction->setWhatsThis( showLightsText );
   /*
   // Create a "Event selectable" action
#if QT_VERSION < 0x40000
   QAction *selectEventAction =  new QAction("Event", "Select Event", CTRL+Key_E, this, "event" );
#else 
   QAction *selectEventAction =  new QAction("Event", "Select Event", Qt::CTRL+Qt::Key_E, this, "event" );
#endif 
   connect ( selectEventAction, SIGNAL( toggled(bool) ) , this, SLOT( SelectEventCB(bool) ) );
   const char * selectEventText = "Turn this option on to be able to select the <b>event</b> (tracks and hits)  and similar \"wired\" object";
   selectEventAction->setToggleAction(true);
   selectEventAction->setWhatsThis( selectEventText);
   
   // Create a "Detector selectable" action
#if QT_VERSION < 0x40000
   QAction *selectDetectorAction =  new QAction("Detector", "Select Detector", CTRL+Key_D, this, "detector" );
#else 
   QAction *selectDetectorAction =  new QAction("Detector", "Select Detector", Qt::CTRL+Qt::Key_D, this, "detector" );
#endif 
   connect ( selectDetectorAction, SIGNAL( toggled(bool)  ) , this, SLOT( SelectDetectorCB(bool) ) );
   const char * selectDetectorText = "Turn this option on to be able to select the <b>detector</b> (solid) objects";
   selectDetectorAction->setWhatsThis(  selectDetectorText);
   selectDetectorAction->setToggleAction(true);

   // Create a "Detector selectable" action
#if QT_VERSION < 0x40000
   QAction *viewSelectionAction =  new QAction("Selection", "Show the selected object", CTRL+Key_T, this, "selection" );
#else 
   QAction *viewSelectionAction =  new QAction("Selection", "Show the selected object", Qt::CTRL+Qt::Key_T, this, "selection" );
#endif 
   connect ( viewSelectionAction, SIGNAL( toggled(bool)  ) , this, SLOT( ActivateSelectorWidgetCB(bool) ) );
   const char * viewSelectionActionText = "Turn this option on to see the selected object with the dedicted view";
   viewSelectionAction->setWhatsThis( viewSelectionActionText );
   viewSelectionAction->setToggleAction(true);

   // Show  the "selected object" with the separate widget using the global coordinate
#if QT_VERSION < 0x40000
   QAction *viewSelectionGlobalAction =  new QAction("SelectionGlobal", "Show the selected object in global system", CTRL+Key_I, this, "selectionglobal" );
#else 
   QAction *viewSelectionGlobalAction =  new QAction("SelectionGlobal", "Show the selected object in global system", Qt::CTRL+Qt::Key_I, this, "selectionglobal" );
#endif 
   connect ( viewSelectionGlobalAction, SIGNAL( toggled(bool)  ) , this, SLOT( ActivateSelectionGlobalCB(bool) ) );
   connect ( viewSelectionAction, SIGNAL( toggled(bool)  ), viewSelectionGlobalAction, SLOT( setEnabled(bool) ) );
   const char * viewSelectionGlobalActionText = "Turn this option on to show the selected object in the global coordinate system";
   viewSelectionGlobalAction->setWhatsThis( viewSelectionGlobalActionText );
   viewSelectionGlobalAction->setToggleAction(true);

   // Create a "Highlight the selected object" action
#if QT_VERSION < 0x40000
   QAction *viewSelectionHighlightAction =  new QAction("SelectionHighLight", "Highlight the selected object", CTRL+Key_I, this, "selectionhighlight" );
#else 
   QAction *viewSelectionHighlightAction =  new QAction("SelectionHighLight", "Highlight the selected object", Qt::CTRL+Qt::Key_I, this, "selectionhighlight" );
#endif 
   connect ( viewSelectionHighlightAction, SIGNAL( toggled(bool)  ) , this, SLOT( ActivateSelectionHighlighCB(bool) ) );
   const char * viewSelectionHighlightActionText = "Turn this option on to highlight the selected object";
   viewSelectionHighlightAction->setWhatsThis( viewSelectionHighlightActionText );
   viewSelectionHighlightAction->setToggleAction(true);
 */
   // Create a "Context Menu for  the selected object" action
#if QT_VERSION < 0x40000
   QAction *viewContextMenuAction =  new QAction("ContextMenu", "Context menu", CTRL+Key_I, this, "contextmenu" );
   viewContextMenuAction->setToggleAction(true);
#else 
   QAction *viewContextMenuAction =  new QAction("Context menu",this);
   viewContextMenuAction->setShortcut(Qt::CTRL+Qt::Key_I);    
   viewContextMenuAction->setCheckable(true);
#endif 
   connect ( viewContextMenuAction, SIGNAL( toggled(bool)  ) , this, SLOT( WantRootContextMenuCB(bool) ) );
   const char * viewContextMenuActionText  = "Show the ROOT context menu for the selected ROOT object";
   viewContextMenuAction->setWhatsThis( viewContextMenuActionText );

   // Create a "Context Menu for  the selected object" action
#if QT_VERSION < 0x40000
   QAction *clipDecorationMenuAction =  new QAction("ClipDecorationMenu", "Clip &decoration", CTRL+Key_D, this, "clipdecorationmenu" );
   clipDecorationMenuAction->setToggleAction(true);
#else 
   QAction *clipDecorationMenuAction =  new QAction("Clip &Decoration",this);
   clipDecorationMenuAction->setShortcut(Qt::CTRL+Qt::Key_D);    
   clipDecorationMenuAction->setCheckable(true);
#endif 
   connect ( clipDecorationMenuAction, SIGNAL( toggled(bool)  ) , this, SLOT( WantClipFileNodeMenuCB(bool) ) );
   const char * clipDecorationMenuActionText  = "Allow clipping the decoration geometry loaded from the extra IV file";
   viewContextMenuAction->setWhatsThis( clipDecorationMenuActionText );

   QMenuBar   *mainMenu = menuBar();

   // -- populate the menu bar
#if QT_VERSION < 0x40000
      QPopupMenu *fileMenu      = new QPopupMenu();
      mainMenu->insertItem("&File",fileMenu);
 /*
#if QT_VERSION < 0x40000
      QPopupMenu *editMenu      = new QPopupMenu();
#else 
      Q3PopupMenu *editMenu      = new Q3PopupMenu();
#endif 
      mainMenu->insertItem("&Edit",editMenu);
*/

      QPopupMenu *optionMenu    = new QPopupMenu();
      mainMenu->insertItem("&Options",optionMenu);

      QPopupMenu *helpMenu   = new QPopupMenu();
      mainMenu->insertItem("&Help",helpMenu);
  // -- The menu bar has been completed

 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  fileMenu
   
   saveAction     ->addTo(fileMenu);
   //saveAsAction   ->addTo(fileMenu);
   fSnapShotAction->addTo(fileMenu);
   openAction     ->addTo(fileMenu);
   clearAction     ->addTo(fileMenu);
                    fileMenu->insertSeparator();
   printAction    ->addTo(fileMenu);
                    fileMenu->insertSeparator();
   fileCloseAction->addTo(fileMenu);


 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  editMenu
    //copyAction     ->addTo(editMenu);
    //copyFrameAction->addTo(editMenu);
   
 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  optionMenu
    //synchAction->addTo(optionMenu);
    //synchAction->setOn(true);
    //synchAction->setEnabled (true);
#ifdef QGLVIEWER

    //QWidget *c = centralWidget();
    //TQtGLViewerWidget *glView = (TQtGLViewerWidget *)c;

    showFrameAxisAction->addTo(optionMenu);
    //showFrameAxisAction->setOn(glView? glView->FrameAxisScale()> 0 : false);
    showFrameAxisAction->setOn(false);
    showFrameAxisAction->setEnabled(true);
    
    showSmallAxesAction->addTo(optionMenu);
    showSmallAxesAction->setOn(false);
    showSmallAxesAction->setEnabled(true);
    showFrameAxisMenu->addTo(optionMenu);
    
/*
    showLightsAction->addTo(optionMenu);
    showLightsAction->setOn(false);
    showLightsAction->setEnabled (true);

    
    optionMenu->insertSeparator();

    selectEventAction->addTo(optionMenu);
    selectEventAction->setOn(glView ? glView->IsWiredSelectable() : false );
    selectEventAction->setEnabled (true);

    selectDetectorAction->addTo(optionMenu);
    selectDetectorAction->setOn( glView ? glView->IsSolidSelectable() : false );
    selectDetectorAction->setEnabled (true);
    
    optionMenu->insertSeparator();
    
    viewSelectionAction->addTo(optionMenu);
    viewSelectionAction->setOn( false );
    viewSelectionAction->setEnabled (true);
    
    viewSelectionGlobalAction->addTo(optionMenu);
    viewSelectionGlobalAction->setOn( false );
    viewSelectionGlobalAction->setEnabled (false);

    viewSelectionHighlightAction->addTo(optionMenu);
    viewSelectionHighlightAction->setOn( false );
    viewSelectionHighlightAction->setEnabled (true);
*/
    viewContextMenuAction->addTo(optionMenu);
    viewContextMenuAction->setOn( false );
    viewContextMenuAction->setEnabled (true);
    
    clipDecorationMenuAction->addTo(optionMenu);
    clipDecorationMenuAction->setOn( false );
    clipDecorationMenuAction->setEnabled (true);
    
#endif
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  helpMenu
    helpMenu->insertItem("&Help",this,SLOT(HelpCB()));
    helpMenu->insertSeparator();
    helpMenu->insertItem("&About",this,SLOT(AboutCB()));
#else
       // -- populate the menu bar
    QMenu *fileMenu      = mainMenu->addMenu("&File");
//    QMenu *editMenu      = mainMenu->addMenu("&Edit");
    QMenu *optionMenu    = mainMenu->addMenu("&Options");
    QMenu *helpMenu      = mainMenu->addMenu("&Help");      

  // -- The menu bar has been completed

 // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  fileMenu
   fileMenu->clear();
   
   fileMenu->addAction(saveAction);
   //saveAsAction   ->addTo(fileMenu);
   fileMenu->addAction(fSnapShotAction);
   fileMenu->addAction(openAction);
   fileMenu->addAction(clearAction);
                    fileMenu->addSeparator();
   fileMenu->addAction(printAction);
                    fileMenu->addSeparator();
   fileMenu->addAction(fileCloseAction);


#ifdef QGLVIEWER

    //QWidget *c = centralWidget();
    //TQtGLViewerWidget *glView = (TQtGLViewerWidget *)c;

    optionMenu->clear();
    optionMenu->addAction(showFrameAxisAction);
    optionMenu->addMenu(showFrameAxisMenu);
    //showFrameAxisAction->setOn(glView? glView->FrameAxisScale()> 0 : false);
    // showFrameAxisAction->setChecked(false);
    showFrameAxisAction->setEnabled(true);
    
    optionMenu->addAction(showSmallAxesAction);
    showSmallAxesAction->setChecked(false);
    showSmallAxesAction->setEnabled(true);
    
/*
    showLightsAction->addTo(optionMenu);
    showLightsAction->setOn(false);
    showLightsAction->setEnabled (true);

    
    optionMenu->insertSeparator();

    selectEventAction->addTo(optionMenu);
    selectEventAction->setOn(glView ? glView->IsWiredSelectable() : false );
    selectEventAction->setEnabled (true);

    selectDetectorAction->addTo(optionMenu);
    selectDetectorAction->setOn( glView ? glView->IsSolidSelectable() : false );
    selectDetectorAction->setEnabled (true);
    
    optionMenu->insertSeparator();
    
    viewSelectionAction->addTo(optionMenu);
    viewSelectionAction->setOn( false );
    viewSelectionAction->setEnabled (true);
    
    viewSelectionGlobalAction->addTo(optionMenu);
    viewSelectionGlobalAction->setOn( false );
    viewSelectionGlobalAction->setEnabled (false);

    viewSelectionHighlightAction->addTo(optionMenu);
    viewSelectionHighlightAction->setOn( false );
    viewSelectionHighlightAction->setEnabled (true);
*/
    viewContextMenuAction->addTo(optionMenu);
    viewContextMenuAction->setOn( false );
    viewContextMenuAction->setEnabled (true);
    
    clipDecorationMenuAction->addTo(optionMenu);
    clipDecorationMenuAction->setOn( false );
    clipDecorationMenuAction->setEnabled (true);

#endif
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 //  helpMenu
    helpMenu->clear();
    helpMenu->addAction("&Help",this,SLOT(HelpCB()));
    helpMenu->addSeparator();
    (helpMenu->addAction("&About",this,SLOT(AboutCB())))->setMenuRole(QAction::AboutRole);
#endif        
}
/*
//______________________________________________________________________________
void TQtGLViewerImp::Paint(Option_t *opt)
{
   CreateViewer();
#ifndef QGLVIEWER
   TGLViewerImp::Paint(opt);
#else
   if (opt);
   Update();
#endif

  //// Paint G3D objects  
  // Int_t myGLIst = GetGLView()->GetGLList()-1;
  // assert(myGLIst );
  // if (myGLIst) {

  //   // Pick the current TPad rotation
  //   gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kView);

  //   // Draw G3D objects
  //   gVirtualGL->RunGLList(myGLIst + TPadOpenGLView::kModel);
  // }
}
*/
//______________________________________________________________________________
void TQtCoinViewerImp::Update()
{  
   if (fCoinWidget) fCoinWidget->Update();
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetFullScreenView(bool on)
{
   // Shows the widget in full-screen mode.
   if (on) showFullScreen();
   else    showNormal();
}
//______________________________________________________________________________
bool  TQtCoinViewerImp::IsFullScreen( )       const
{   
   // Returns TRUE if the widget is full screen 
   return isFullScreen();
}

//______________________________________________________________________________
void TQtCoinViewerImp:: SetOffScreen(bool on)
{
   // Set the offscreen (batch) rendering mode
  if (fCoinWidget)fCoinWidget->SetOffScreen(on);
}
//______________________________________________________________________________
bool  TQtCoinViewerImp::IsOffScreen( )       const
{   
   // Returns TRUE if the image is to be rendered offscreen
   // (batch mode)
   return  fCoinWidget ? fCoinWidget->IsOffScreen(): kFALSE;
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetFooter(const char *text)
{
     // Set the footer text
   QString f(text);
   SetFooter(f);
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetFooter(QString &text)
{
   // Set the footer text
  if (fCoinWidget)  fCoinWidget->SetFooter(text);
}

/*
//______________________________________________________________________________
void TQtCoinViewerImp::SetPadSynchronize(Bool_t on)
{
  // Define whether the GL Widget view should follow TPad rotation
#ifndef QGLVIEWER
   if(on) {} // QtGLWidget provide no synch means
#else
   if (fGLWidget) ((TQtGLViewerWidget*)fGLWidget)->setPadSynchronize(on);
#endif
}
*/
//______________________________________________________________________________
void TQtCoinViewerImp::SetRotationAxisAngle(const float x, const float y, const float z, const float a)
{
   // Set the current rotation of the frame. 
   // Parameters are the rotation axis vector and its angle (in radians). 
   if (fCoinWidget) fCoinWidget->SetRotationAxisAngle(x,y,z,a);
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetBackgroundColor(Color_t color)
{
   if (fCoinWidget) fCoinWidget->SetBackgroundColor(color);
}

//______________________________________________________________________________
void TQtCoinViewerImp::ShowObjectInfo(TObject *obj, const QPoint &cursorPosition)
{
   if (obj && fCoinWidget) fCoinWidget->ShowObjectInfo(obj,cursorPosition);
}
/*
//______________________________________________________________________________
void TQtGLViewerImp::CreateSelectionViewer( ) 
{
   // Create another OpenGL viewer to show the selection 
   if (fSelectedViewActive && !fSelectedView)  {
      fSelectedView  = new TQtGLViewerImp(*this);
      fSelectedView->SelectEventCB(false);
      fSelectedView->SelectDetectorCB(false);
   }
}
*/
//______________________________________________________________________________
ULong_t TQtCoinViewerImp::GetViewerID() const
{
   return ULong_t((QMainWindow *) this);
}
//______________________________________________________________________________
void TQtCoinViewerImp::EnableObjectPick(bool enable)
{  
 //  [slot]  Proxy to enable the TObject picking
	if (fCoinWidget) fCoinWidget->EnableObjectPick(enable);
}
//______________________________________________________________________________
bool TQtCoinViewerImp::ObjectPickEnabled()  const
{
   // returns whether the Coin widget picks the TObject's
   // Need to protect the widget against of the images of the host TObject
   return fCoinWidget ? fCoinWidget->ObjectPickEnabled(): false; 
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetSnapFileCounter(int counter)
{ 
	if (fCoinWidget) fCoinWidget->SetSnapFileCounter(counter);
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetCliPlaneMan(bool on)
{
    if (fCoinWidget) fCoinWidget->SetClipPlaneMan(on);
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetAxisPositionCB(QAction *action, int axIndex)
{
    if (fCoinWidget && action) {
       // fCoinWidget->GetLocation(axIndex);
       QActionGroup *gr =  action->actionGroup();
       QList<QAction *>actions =  gr->actions ();
       unsigned char location = 0;
       for (int i = actions.size()-1; i>=0; --i) {
          location <<= 1;
          if ( actions.at(i)->isChecked () ) location |= 0x1;
       }
       // qDebug() <<" TQtCoinViewerImp::SetAxisPositionCB location: " << hex << location << " ax=" << axIndex;
       fCoinWidget->SetLocation(location,axIndex);
       if (fShowFrameAxisAction && fShowFrameAxisAction->isChecked()) fShowFrameAxisAction->trigger();
      //       ShowFrameAxisCB(true);
    }
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetAxisPositionXCB(QAction *action )
{   
    SetAxisPositionCB(action, 0);
}

//______________________________________________________________________________
void TQtCoinViewerImp::SetAxisPositionYCB(QAction *action)
{   
    SetAxisPositionCB(action, 1);
}
//______________________________________________________________________________
void TQtCoinViewerImp::SetAxisPositionZCB(QAction *action )
{   
    SetAxisPositionCB(action, 2);
}

//______________________________________________________________________________
void TQtCoinViewerImp::FrameAxisActionCB(bool on)
{
  SetCliPlaneMan(on); 
}
//______________________________________________________________________________
void TQtCoinViewerImp::SmallAxesActionCB(bool on)
{
     if (fCoinWidget) fCoinWidget->SmallAxesActionCB(on);
}
//______________________________________________________________________________
SoCamera *TQtCoinViewerImp::GetCamera() const 
{ 
   return fCoinWidget ? fCoinWidget->GetCamera(): 0;
}
//______________________________________________________________________________
SmAxisDisplayKit *TQtCoinViewerImp::GetAxis() const 
{ 
   return fCoinWidget ? fCoinWidget->GetAxis() : 0;
}
//______________________________________________________________________________
std::vector<int> TQtCoinViewerImp::GetMyGLList1() const
{ 
   static std::vector<int>  d;
   return fCoinWidget ? fCoinWidget->GetMyGLList1() : d; 
}
//______________________________________________________________________________
std::vector<int> TQtCoinViewerImp::GetMyGLList2() const
{ 
   static std::vector<int>  d;
   return fCoinWidget ? fCoinWidget->GetMyGLList2() : d; 
}
//______________________________________________________________________________
std::vector<int> TQtCoinViewerImp::GetMyGLList3() const
{
   static std::vector<int>  d;
   return fCoinWidget ? fCoinWidget->GetMyGLList3() : d; 
}
//______________________________________________________________________________
TObject  *TQtCoinViewerImp::GetSelected() const 
{ 
   return fCoinWidget ? fCoinWidget->GetSelected() : 0;
}
//______________________________________________________________________________
SoQtViewer  *TQtCoinViewerImp::GetCoinViewer()  const 
{
   return fCoinWidget ? fCoinWidget->GetCoinViewer():0; 
}
//______________________________________________________________________________
Bool_t  TQtCoinViewerImp::WantRootContextMenu() const 
{
   return fCoinWidget ? fCoinWidget->WantRootContextMenu():kFALSE;
}
//______________________________________________________________________________
void TQtCoinViewerImp::ViewAll()
{
   if (fCoinWidget)  fCoinWidget->ViewAll();
}

//______________________________________________________________________________
Bool_t   TQtCoinViewerImp::WasPicked(void *p) const { 
   return fCoinWidget ? fCoinWidget->WasPicked(p) : kFALSE;
}

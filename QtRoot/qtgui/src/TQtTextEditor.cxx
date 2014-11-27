// @(#)root/gui:$Id: TQtTextEditor.cxx,v 1.4 2013/08/30 16:00:25 perev Exp $
// Author: Bertrand Bellenot   20/06/06
// Author:Valeri Fine          20/05/10 (Qt-based implemenation)


//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtTextEditor                                                        //
//                                                                      //
//  A simple text editor that uses the QTextEdit widget.               //
//  It provides all functionalities of QTextEdit as copy, paste, cut,  //
//  search, go to a given line number. In addition, it provides the     //
//  possibilities for compiling, executing or interrupting a running    //
//  macro.                                                              //
//                                                                      //
//  This class can be used in following ways:                           //
//  - with file name as argument:                                       //
//    new TQtTextEditor("hsimple.C");                                    //
//  - with a TMacro* as argument:                                       //
//    TMacro *macro = new TMacro("hsimple.C");                          //
//    new TQtTextEditor(macro);                                          //
//                                                                      //
//  Basic Features:                                                     //
//                                                                      //
//  New Document                                                        //
//                                                                      //
//  To create a new blank document, select File menu / New, or click    //
//  the New toolbar button. It will create a new instance of            //
//  TQtTextEditor.                                                       //
//                                                                      //
//  Open/Save File                                                      //
//                                                                      //
//  To open a file, select File menu / Open or click on the Open        //
//  toolbar button. This will bring up the standard File Dialog for     //
//  opening files.                                                      //
//  If the current document has not been saved yet, you will be asked   //
//  either to save or abandon the changes.                              //
//  To save the file using the same name, select File menu / Save or    //
//  the toolbar Save button. To change the file name use File menu /    //
//  Save As... or corresponding SaveAs button on the toolbar.           //
//                                                                      //
//  Text Selection                                                      //
//                                                                      //
//  You can move the cursor by simply clicking on the desired location  //
//  with the left mouse button. To highlight some text, press the mouse //
//  and drag the mouse while holding the left button pressed.           //
//  To select a word, double-click on it;                               //
//  to select the text line - triple-click on it;                       //
//  to select all  do quadruple-click.                                  //
//                                                                      //
//  Cut, Copy, Paste                                                    //
//                                                                      //
//  After selecting some text, you can cut or copy it to the clipboard. //
//  A subsequent paste operation will insert the contents of the        //
//  clipboard at the current cursor location.                           //
//                                                                      //
//  Text Search                                                         //
//                                                                      //
//  The editor uses a standard Search dialog. You can specify a forward //
//  or backward search direction starting from the current cursor       //
//  location according to the selection made of a case sensitive mode   //
//  or not. The last search can be repeated by pressing F3.             //
//                                                                      //
//  Text Font                                                           //
//                                                                      //
//  You can change the text font by selecting Edit menu / Set Font.     //
//  The Font Dialog pops up and shows the Name, Style, and Size of any  //
//  available font. The selected font sample is shown in the preview    //
//  area.                                                               //
//                                                                      //
//  Executing Macros                                                    //
//                                                                      //
//  You can execute the currently loaded macro in the editor by         //
//  selecting Tools menu / Execute Macro; by clicking on the            //
//  corresponding toolbar button, or by using Ctrl+F5 accelerator keys. //
//  This is identical to the command ".x macro.C" in the root prompt    //
//  command line.                                                       //
//                                                                      //
//  Compiling Macros                                                    //
//                                                                      //
//  The currently loaded macro can be compiled with ACLiC if you select //
//  Tools menu / Compile Macro; by clicking on the corresponding        //
//  toolbar button, or by using Ctrl+F7 accelerator keys.               //
//  This is identical to the command ".L macro.C++" in the root prompt  //
//  command line.                                                       //
//                                                                      //
//  Interrupting a Running Macro                                        //
//                                                                      //
//  You can interrupt a running macro by selecting the Tools menu /     //
//  Interrupt; by clicking on the corresponding toolbar button, or by   //
//  using Shift+F5 accelerator keys.                                    //
//                                                                      //
//  Interface to CINT Interpreter                                       //
//                                                                      //
//  Any command entered in the Command combo box will be passed to      //
//  the CINT interpreter. This combo box will keep the commands history //
//  and will allow you to re-execute the same commands during an editor //
//  session.                                                            //
//                                                                      //
//  Keyboard Bindings                                                   //
//                                                                      //
//  The following table lists the keyboard shortcuts and accelerator    //
//  keys.                                                               //
//                                                                      //
//  Key:              Action:                                           //
//  ====              =======                                           //
//                                                                      //
//  Up                Move cursor up.                                   //
//  Shift+Up          Move cursor up and extend selection.              //
//  Down              Move cursor down.                                 //
//  Shift+Down        Move cursor down and extend selection.            //
//  Left              Move cursor left.                                 //
//  Shift+Left        Move cursor left and extend selection.            //
//  Right             Move cursor right.                                //
//  Shift+Right       Move cursor right and extend selection.           //
//  Home              Move cursor to begin of line.                     //
//  Shift+Home        Move cursor to begin of line and extend selection.//
//  Ctrl+Home         Move cursor to top of page.                       //
//  End               Move cursor to end of line.                       //
//  Shift+End         Move cursor to end of line and extend selection.  //
//  Ctrl+End          Move cursor to end of page.                       //
//  PgUp              Move cursor up one page.                          //
//  Shift+PgUp        Move cursor up one page and extend selection.     //
//  PgDn              Move cursor down one page.                        //
//  Shift+PgDn        Move cursor down one page and extend selection.   //
//  Delete            Delete character after cursor, or text selection. //
//  BackSpace         Delete character before cursor, or text selection.//
//  Ctrl+B            Move cursor left.                                 //
//  Ctrl+D            Delete character after cursor, or text selection. //
//  Ctrl+E            Move cursor to end of line.                       //
//  Ctrl+H            Delete character before cursor, or text selection.//
//  Ctrl+K            Delete characters from current position to the    //
//                    end of line.                                      //
//  Ctrl+U            Delete current line.                              //
//                                                                      //
//Begin_Html
/*
<img src="gif/TQtTextEditor.gif">
*/
//End_Html
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "TQtTextEditor.h"

#include "TROOT.h"
#include "TSystem.h"
#include "TMacro.h"
#include "TInterpreter.h"
#include <QTextStream>
#include <QFile>

#include <QFileInfo>
#include <QApplication>
#include <QMessageBox>
#include <QFileDialog>
#include <QFontDialog>
#include <QPrintDialog>
#include <QPrinter>
#include <QPainter>
#include <QTextEdit>
#include <QMenu>
#include <QMenuBar>
#include <QStatusBar>
#include <QToolBar>
#include <QLabel>
#include <QSplitter>
#include <QClipboard>
#include <QFontDialog>

#include "TQtRootAction.h"
#include "TObjString.h"
#include "TRootHelpDialog.h"
#include "HelpText.h"
#ifdef WIN32
#include "TWin32SplashThread.h"
#endif

namespace {

enum ETextEditorCommands {
   kM_FILE_NEW, kM_FILE_OPEN, kM_FILE_SAVE, kM_FILE_SAVEAS, kM_FILE_PRINT,
   kM_FILE_EXIT, kM_EDIT_CUT, kM_EDIT_COPY, kM_EDIT_PASTE, kM_EDIT_DELETE,
   kM_EDIT_SELECTALL, kM_SEARCH_FIND, kM_SEARCH_FINDNEXT, kM_SEARCH_GOTO,
   kM_TOOLS_COMPILE, kM_TOOLS_EXECUTE, kM_TOOLS_INTERRUPT, kM_HELP_CONTENTS,
   kM_HELP_ABOUT, kM_EDIT_SELFONT
};
   
static TQtBrowserMenuItem_t gMenu_Data[] = {
   { "&New",           kM_FILE_NEW ,       Qt::CTRL+Qt::Key_N,  "New File",       "ed_new.png"      }
  ,{ "&Open" ,         kM_FILE_OPEN,       Qt::CTRL+Qt::Key_O,  "Open File",      "ed_open.png"     }
  ,{ "&Save",          kM_FILE_SAVE,       Qt::CTRL+Qt::Key_S,  "Save File",      "ed_save.png"     }
  ,{ "Save &As",       kM_FILE_SAVEAS,           0,             "Save File As...","ed_saveas.png"   }
  ,{ "&Print",         kM_FILE_PRINT ,     Qt::CTRL+Qt::Key_P,  "Print",          "ed_print.png"    }
  ,{ "E&xit",          kM_FILE_EXIT  ,     Qt::CTRL+Qt::Key_Q,  "Exit" ,          "ed_quit.png"     }
  
  ,{ "Cu&t",           kM_EDIT_CUT,        Qt::CTRL+Qt::Key_X,  "Cut selection" , "ed_cut.png"      }
  ,{ "&Copy",          kM_EDIT_COPY  ,     Qt::CTRL+Qt::Key_C,  "Copy selection", "ed_copy.png"     }
  ,{ "&Paste",         kM_EDIT_PASTE ,     Qt::CTRL+Qt::Key_V,  "Paste selection","ed_paste.png"    }
  ,{ "De&lete",        kM_EDIT_DELETE,     Qt::CTRL+Qt::Key_Q,  "" ,              "ed_delete.png"   }
  
  ,{ "Select &All",    kM_EDIT_SELECTALL,  Qt::CTRL+Qt::Key_A,  "Select All" ,    ""                }
  ,{ "Set &Font",      kM_EDIT_SELFONT ,   0,                   "Set Font" ,      ""                }
  ,{ "&Compile Macro", kM_TOOLS_COMPILE ,  Qt::CTRL+Qt::Key_F7, "Compile Macro",  "ed_compile.png"  }
  ,{ "&Execute Macro", kM_TOOLS_EXECUTE,   Qt::CTRL+Qt::Key_F5, "Execute Macro",  "ed_execute.png"  }
  ,{ "&Interrupt",     kM_TOOLS_INTERRUPT, Qt::SHIFT+Qt::Key_F5,"Interrupt",      "ed_interrupt.png"}
  
  ,{ "&Find",          kM_SEARCH_FIND ,    Qt::CTRL+Qt::Key_F,   "" ,             "ed_find.png"     }
  ,{ "Find &Next",     kM_SEARCH_FINDNEXT, Qt::CTRL+Qt::Key_Q,   "" ,             "ed_findnext.png" }
  ,{ "&Goto Line",     kM_SEARCH_GOTO,     Qt::CTRL+Qt::Key_L,   "" ,             "ed_goto.png"     }
  ,{ "&Help Topics",   kM_HELP_CONTENTS ,  Qt::Key_F1,           "" ,             "ed_help.png"     }
  ,{ "&About",         kM_HELP_ABOUT,         0,                 "" ,             "ed_help.png"     }
  ,{ 0,0,0,0,0 }
};

QString filetypes = "ROOT Macros (*.C);"
                    ";Source files (*.cxx);"
                    ";Text files (*.txt);"
                    ";All files  (*);;";
}

// ClassImp(TQtTextEditor)
//______________________________________________________________________________
TQtTextEditor::TQtTextEditor( QWidget * parent, Qt::WindowFlags flags)
: QMainWindow(parent,flags)
  ,fStatusBar()          // for file name, line and col number
  ,fTextEdit()           // text edit widget
  ,fLabel()              // "command" label
  ,fMenuBar()            // editor's menu bar
  ,fMenuFile()           // "File" menu entry
  ,fMenuEdit()           // "Edit" menu entry
  ,fMenuSearch()         // "Search" menu entry
  ,fMenuTools()          // "Tools" menu entry
  ,fMenuHelp()           // "Help" menu entry
  ,fExiting(false)       // true if editor is closing
  ,fFilename("Untitled") // name of the opened file
  ,fMacro()              // pointer on the opened macro
  ,fActions()
{ 
  Build();
}

//______________________________________________________________________________
TQtTextEditor::TQtTextEditor(const QString &filename, QWidget *parent, unsigned int w,
                           unsigned int h) : QMainWindow(parent)
  ,fStatusBar()          // for file name, line and col number
  ,fTextEdit()           // text edit widget
  ,fLabel()              // "command" label
  ,fMenuBar()            // editor's menu bar
  ,fMenuFile()           // "File" menu entry
  ,fMenuEdit()           // "Edit" menu entry
  ,fMenuSearch()         // "Search" menu entry
  ,fMenuTools()          // "Tools" menu entry
  ,fMenuHelp()           // "Help" menu entry
  ,fExiting(false)       // true if editor is closing
  ,fFilename("Untitled") // name of the opened file
  ,fMacro()              // pointer on the opened macro
  ,fActions()
{
   // TQtTextEditor constructor with file name as first argument.

   Build();
#if 0
   if (p && p != gClient->GetDefaultRoot()) {
      // special case for TRootBrowser
      // remove the command line combo box and its associated label
      fComboCmd->UnmapWindow();
      fToolBar->RemoveFrame(fComboCmd);
      fLabel->UnmapWindow();
      fToolBar->RemoveFrame(fLabel);
      fToolBar->GetButton(kM_FILE_EXIT)->SetState(kButtonDisabled);
      fToolBar->Layout();
   }
#endif
   LoadFile(filename);
   resize(w,h);
}

//______________________________________________________________________________
TQtTextEditor::TQtTextEditor(TMacro *macro, QWidget *parent, unsigned int w, unsigned int h) 
: QMainWindow(parent)
  ,fStatusBar()          // for file name, line and col number
  ,fTextEdit()           // text edit widget
  ,fLabel()              // "command" label
  ,fMenuBar()            // editor's menu bar
  ,fMenuFile()           // "File" menu entry
  ,fMenuEdit()           // "Edit" menu entry
  ,fMenuSearch()         // "Search" menu entry
  ,fMenuTools()          // "Tools" menu entry
  ,fMenuHelp()           // "Help" menu entry
  ,fExiting(false)       // true if editor is closing
  ,fFilename("Untitled") // name of the opened file
  ,fMacro()              // pointer on the opened macro
  ,fActions()
{
   // TQtTextEditor constructor with pointer to a TMacro as first argument.

   Build();
#if 0   
   if (p && p != gClient->GetDefaultRoot()) {
      // special case for TRootBrowser
      // remove the command line combo box and its associated label
      fComboCmd->UnmapWindow();
      fLabel->UnmapWindow();
      fToolBar->GetButton(kM_FILE_EXIT)->SetState(kButtonDisabled);
      fToolBar->Layout();
   }
#endif
   if (macro) {
      fMacro = macro;
      TIter next(macro->GetListOfLines());
      TObjString *obj;
      while ((obj = (TObjString*) next())) {
         fTextEdit->append(obj->GetName());
      }
#if QT_VERSION >= 0x40500
      SetStatusText(QString("TMacro : %1: %2 lines read.")
                               .arg(macro->GetName())
                               .arg(fTextEdit->document()->lineCount()), 0);
#endif
      fFilename = macro->GetName();
      setWindowTitle(QString("TMacro : %1 [*] - TQtTextEditor").arg(fFilename));
      fFilename += ".C";
   }
   resize(w,h);
}

//______________________________________________________________________________
TQtTextEditor::~TQtTextEditor()
{
   // TQtTextEditor destructor.
}

//______________________________________________________________________________
void TQtTextEditor::DeleteWindow()
{
   // Delete TQtTextEditor Window.
  close();
}

//______________________________________________________________________________
void TQtTextEditor::MakeActions() {
   int i=0;
   while (gMenu_Data[i].fMenuText!=NULL) {
      // skip the separators 
      TQtRootAction *action = new TQtRootAction(this,gMenu_Data[i]);
      fActions.insert(action->Id(),action);
      connect( action, SIGNAL( activated() ) , this, SLOT(ProcessMessage()) );
      i++;
   }
}
//______________________________________________________________________________
void TQtTextEditor::Build()
{
   // Build TQtTextEditor widget.
   MakeActions() ;
   
   if (fMenuBar) { delete fMenuBar; fMenuBar = 0; }
   QMenuBar   *mainMenu = this->menuBar();
   fMenuBar = mainMenu;

   SetupEditor();

   fMenuFile   = mainMenu->addMenu("&File");  this->addToolBar(fToolFile   = new QToolBar(this));
   fMenuEdit   = mainMenu->addMenu("&Edit");  this->addToolBar(fToolEdit   = new QToolBar(this));
   fMenuSearch = mainMenu->addMenu("Search"); this->addToolBar(fToolSearch = new QToolBar(this));
   fMenuTools  = mainMenu->addMenu("Tools");  this->addToolBar(fToolTools  = new QToolBar(this));
                        mainMenu->addSeparator();
   fMenuHelp   = mainMenu->addMenu("&Help");  this->addToolBar(fToolHelp   = new QToolBar(this));
   
//  File menu:

   fMenuFile->clear();
   fMenuFile->addAction(fActions[kM_FILE_NEW]);         fToolFile->addAction(fActions[kM_FILE_NEW]);
                                       fMenuFile->insertSeparator();
   fMenuFile->addAction(fActions[ kM_FILE_OPEN]);       fToolFile->addAction(fActions[ kM_FILE_OPEN]);
   fMenuFile->addAction(fActions[ kM_FILE_SAVE]);       fToolFile->addAction(fActions[ kM_FILE_SAVE]);
   fMenuFile->addAction(fActions[ kM_FILE_SAVEAS]);     fToolFile->addAction(fActions[ kM_FILE_SAVEAS]);
                                       fMenuFile->insertSeparator();
   fMenuFile->addAction(fActions[ kM_FILE_PRINT]);      fToolFile->addAction(fActions[ kM_FILE_PRINT]);
                                       fMenuFile->insertSeparator();
   fMenuFile->addAction(fActions[ kM_FILE_EXIT]);    
   fActions[kM_FILE_EXIT]->setMenuRole(QAction::QuitRole);
//  Edit menu:

   fMenuEdit->clear(); 
   fMenuEdit->addAction(fActions[kM_EDIT_CUT]);         fToolEdit->addAction(fActions[kM_EDIT_CUT]);
   fMenuEdit->addAction(fActions[kM_EDIT_COPY]);        fToolEdit->addAction(fActions[kM_EDIT_COPY]);
   fMenuEdit->addAction(fActions[kM_EDIT_PASTE]);       fToolEdit->addAction(fActions[kM_EDIT_PASTE]);
   fMenuEdit->addAction(fActions[kM_EDIT_DELETE]);      fToolEdit->addAction(fActions[kM_EDIT_DELETE]);
                                       fMenuEdit->insertSeparator();
   fMenuEdit->addAction(fActions[kM_EDIT_SELECTALL]); //  fToolEdit->addAction(fActions[kM_EDIT_SELECTALL]);
                                       fMenuEdit->insertSeparator();
   fMenuEdit->addAction(fActions[kM_EDIT_SELFONT]);   //  fToolEdit->addAction(fActions[kM_EDIT_SELFONT]);
   
//  Tool menu:

   fMenuTools->clear();  
   fMenuTools->addAction(fActions[kM_TOOLS_COMPILE]);   fToolTools->addAction(fActions[kM_TOOLS_COMPILE]);
   fMenuTools->addAction(fActions[kM_TOOLS_EXECUTE]);   fToolTools->addAction(fActions[kM_TOOLS_EXECUTE]);
   fMenuTools->addAction(fActions[kM_TOOLS_INTERRUPT]); fToolTools->addAction(fActions[kM_TOOLS_INTERRUPT]);

   fActions[kM_EDIT_CUT]    ->setEnabled(FALSE);
   fActions[kM_EDIT_COPY]   ->setEnabled(FALSE);
//   fActions[kM_EDIT_DELETE] ->setEnabled(FALSE);
   fActions[kM_EDIT_PASTE]  ->setEnabled(FALSE);
   
//  Search menu:

   fMenuSearch->clear();
   fMenuSearch->addAction(fActions[kM_SEARCH_FIND]);    fToolSearch->addAction(fActions[kM_SEARCH_FIND]);
   fMenuSearch->addAction(fActions[kM_SEARCH_FINDNEXT]);fToolSearch->addAction(fActions[kM_SEARCH_FINDNEXT]);
   fMenuSearch->insertSeparator();
   fMenuSearch->addAction( fActions[kM_SEARCH_GOTO] );  fToolSearch->addAction( fActions[kM_SEARCH_GOTO] );
   
//  Help menu

   fMenuHelp->clear();
   fMenuHelp->addAction(fActions[ kM_HELP_CONTENTS]);    fToolHelp->addAction(fActions[kM_HELP_CONTENTS]);
   fMenuHelp->insertSeparator();
   fMenuHelp->addAction(fActions[  kM_HELP_ABOUT]);   // fTollHelp->addAction(fActions[kM_HELP_ABOUT]);
                                                         fToolHelp->addAction(fActions[ kM_FILE_EXIT]);
   //---- toolbar
#if 0
   fComboCmd   = new QComboBox(fToolBar, "");
   fCommand    = fComboCmd->GetTextEntry();
   fCommandBuf = fCommand->GetBuffer();

   fComboCmd->Resize(200, fCommand->GetDefaultHeight());
#endif
   
   setCentralWidget(fTextEdit = new QTextEdit() );
   
   int parts[] = { 75, 25 };
   CreateStatusBar(parts,sizeof(parts)/sizeof(int));

   setWindowTitle("Untitled[*] - TQtTextEditor");

   SetStatusText(fFilename, 0);
   ConnectSlots () ;
} 

//__________________________________________________________________________________
void  TQtTextEditor:: CanPaste()
{
   QClipboard *clipboard = QApplication::clipboard();
   QString originalText = clipboard->text();
   fActions[kM_EDIT_PASTE]->setEnabled(fTextEdit->canPaste() && !originalText.isEmpty() );
#if 0
   tmp.Form("Ln %ld, Ch %ld", pos.fY, pos.fX);
   SetStatusText(tmp.Data(), 1);
#endif
}

//__________________________________________________________________________________
void TQtTextEditor::ConnectSlots()
{
     connect(fTextEdit,SIGNAL(textChanged()), this, SLOT(DataChanged()));
     connect(fTextEdit, SIGNAL(copyAvailable(bool)),
             fActions[kM_EDIT_CUT], SLOT(setEnabled(bool)));
     connect(fTextEdit, SIGNAL(copyAvailable(bool)),
             fActions[kM_EDIT_COPY], SLOT(setEnabled(bool)));
     connect(fTextEdit,SIGNAL(cursorPositionChanged()),this,SLOT(CanPaste()));
}

//__________________________________________________________________________________
void TQtTextEditor::SetupEditor()
{
    QFont font;
    font.setFamily("Courier");
    font.setFixedPitch(true);
    font.setPointSize(12);

    fTextEdit = new QTextEdit;
    fTextEdit->setFont(font);
//    highlighter = new StGeomHighlighter(fTextEdit->document());
}

//______________________________________________________________________________
void TQtTextEditor::CreateStatusBar(int nparts)
{
  QStatusBar *statusBar = this->statusBar();
  int i=0;
  for (i=0;i<nparts;i++) {
    QLabel *l = new QLabel(statusBar);
    statusBar->addWidget(l,1,true);
    fStatusBar.insert(i,l);
  }
}
//______________________________________________________________________________
void TQtTextEditor::CreateStatusBar(int *parts, int nparts)
{
  QStatusBar *statusBar = this->statusBar();
#ifdef WIN32
  statusBar->setSizeGripEnabled(FALSE);
#endif
  // Any number of widgets may be controlled by just
  // one splitter
  QSplitter *split = new QSplitter(statusBar);
  statusBar->addWidget(split,1,FALSE);

  int iField=0;
  for (iField=0; iField<nparts; iField++) {
    QLabel *infoBox = new QLabel(split);
    infoBox->setIndent(3);
    QSize s = infoBox->size();
    s.setWidth(parts[iField]);
    infoBox->resize(s);
    fStatusBar.insert(iField,infoBox);
  }
}

//______________________________________________________________________________
void TQtTextEditor::SetStatusText(const QString &text, int partidx)
{
  // Set Text into the 'npart'-th part of the status bar
  if (int(fStatusBar.size()) > partidx) {
    fStatusBar[partidx]->setText(text);
  }
} 
//______________________________________________________________________________
void TQtTextEditor::LoadFile(char *fname)
{
   LoadFile(QString(fname));
}
//______________________________________________________________________________
void TQtTextEditor::LoadFile(const QString &fname)
{
   // Load a file into the editor. If fname is 0, a QFileDialog will popup.

//         tmp.Form("%s: %ld lines read.", fname, fTextEdit->ReturnLineCount());
    QString fileName = fname;

    if (fileName.isNull()) {
       // create the STAR search path
       QFileInfo file(fFilename);
       QString defaultDir =  file.exists() ?  file.absoluteFilePath () : "";
       fileName = QFileDialog::getOpenFileName(this,
            tr("Open Text File"), defaultDir ,filetypes);
    }

    if (!fileName.isEmpty()) {
        QFile file(fileName);
        if (file.open(QFile::ReadOnly | QFile::Text)) {
            fTextEdit->setPlainText(file.readAll());
         fFilename = fileName;
         setWindowTitle(QString("%1[*] - TQtTextEditor").arg(fFilename));
       } else  {
           QMessageBox::warning(this, tr("TQtTextEditor"),
                              tr("Cannot read file %1:\n%2.")
                              .arg(fileName)
                              .arg(file.errorString()));         
       }
    }
    DataChanged(false);
}
//______________________________________________________________________________
bool TQtTextEditor::SaveFile(const char *fname)
{
    return SaveFile(QString(fname));
}
//______________________________________________________________________________
bool TQtTextEditor::SaveFile(const QString &fname)
{
   // Save the edited text in the file "fname".
   bool Ok = false;
   QFile file(fname);
   if (file.open(QFile::WriteOnly | QFile::Truncate) ) {
     QTextStream out(&file);
     QApplication::setOverrideCursor(Qt::WaitCursor);
     out << fTextEdit->toPlainText();
     QApplication::restoreOverrideCursor();
     file.close();
#if QT_VERSION >= 0x40500
     SetStatusText( QString("%1: %2 lines written.")
                          .arg(fname)
                          .arg(fTextEdit->document()->lineCount())
                  , 0);
#endif
     setWindowTitle(QString("%1[*] - TQtTextEditor").arg(fname));
     DataChanged(false);
     Ok = true;
   } else {
      QMessageBox::warning (this, "Error saving file", QString("%1 %2").arg(fname).arg(file.errorString ()));
   }
   return Ok;
}

//______________________________________________________________________________
bool TQtTextEditor::SaveFileAs()
{
   // Save the edited text in a file selected with QFileDialog.
   // Shouldn't we create a backup file?

   QFileInfo file(fFilename);
   QString defaultDir =  file.exists() ?  file.absoluteFilePath () : "./";

   QString file2Save = QFileDialog::getSaveFileName (this, "Save File", defaultDir,filetypes);

   return (!file2Save.isEmpty() && SaveFile(file2Save));
}
//______________________________________________________________________________
bool   TQtTextEditor::DataChanged(bool yes)
{
  bool status = fTextEdit ? fTextEdit->document()->setModified(yes),yes : false;
  setWindowModified(TextChanged());
  return status;
}

//______________________________________________________________________________
int TQtTextEditor::IsSaved()
{
   // Check if file has to be saved in case of modifications.
   static const QString question="The text has been modified. Do you want to save the changes?";
   int ret = QMessageBox::Cancel;
   if (TextChanged()) {
      ret = QMessageBox::question (this,"ROOT TextEditor"
                                , question, QMessageBox::Yes, QMessageBox::No) ;
   }
   return ret;
}

//______________________________________________________________________________
void TQtTextEditor::PrintText()
{
   // Open the print dialog and send current buffer to printer.
   QPrinter printer;
   QPrintDialog printDialog(&printer, this);
   if (printDialog.exec() == QDialog::Accepted) {
     // print ...  
     fTextEdit->print(&printer);
     SetStatusText(QString("Printed: %1").arg(fFilename), 0);
   }
}

//______________________________________________________________________________
void TQtTextEditor::CloseWindow()
{
   // Close TQtTextEditor window.

   if (!fExiting) {
      fExiting = kTRUE;
      switch (IsSaved()) {
         case QMessageBox::Yes:
            if (fFilename == "Untitled")
               SaveFileAs();
            else
               SaveFile(fFilename);
            if ((TextChanged()) && (!parentWidget()))
               break;
         case QMessageBox::Cancel:default:
           if (! parentWidget () )
               break;
         case QMessageBox::No:
              close();
      }
      fExiting = kFALSE;
   }
   close();
}

//______________________________________________________________________________
void TQtTextEditor::ClearText()
{
   // Clear text edit widget.

   fTextEdit->clear();
   fMacro = 0;
   fFilename = "Untitled";
   setWindowTitle("Untitled[*] - TQtTextEditor");
   SetStatusText("New File", 0);
   DataChanged(false);
}

//______________________________________________________________________________
void TQtTextEditor::Search(bool again)
{
   // Invokes search dialog, or just search previous string if again is true.
   if (again) {
#if 0
      QTextCursor QTextDocument::find ( const QString & subString, const QTextCursor & cursor, FindFlags options = 0 ) const
   }
   else {
      QTextCursor QTextDocument::find ( const QString & subString, const QTextCursor & cursor, FindFlags options = 0 ) const
      fTextEdit->Search(kFALSE);
#endif   
   }
}

//______________________________________________________________________________
void TQtTextEditor::Goto()
{
   // Invokes goto dialog, and go to the specified line.
#if 0

   Long_t ret;

   new QGotoDialog(fClient->GetDefaultRoot(), this, 400, 150, &ret);

   if (ret >= 0)
      fTextEdit->Goto(ret-1);
#endif
}

//______________________________________________________________________________
void TQtTextEditor::CompileMacro()
{
   // Save the edited text in a temporary macro, then compile it.
#if 0
   if (fTextEdit->ReturnLineCount() < 3)
      return;
#endif
   if ((fMacro) || (fFilename !="Untitled")) {
      if (!SaveFileAs())
         return;
   }
   char *tmpfile = gSystem->ConcatFileName(gSystem->TempDirectory(),
                                gSystem->BaseName(fFilename.toLatin1().data()));
   SaveFile(tmpfile);
   gSystem->CompileMacro(tmpfile);
   gSystem->Unlink(tmpfile);
   delete tmpfile;
}

//______________________________________________________________________________
void TQtTextEditor::ExecuteMacro()
{
   // Save the edited text in a temporary macro, execute it, and then delete
   // the temporary file.
#if 0
   if (fTextEdit->ReturnLineCount() < 3)
      return;
#endif
   if (fMacro) {
      fMacro->Exec();
      return;
   }
#if 0
   if (TextChanged()) {
      int ret;
      new QMessageBox(fClient->GetRoot(), this, "TQtTextEditor",
            "The text has been modified. Do you want to save the changes?",
            kMBIconExclamation, kMBYes | kMBNo | kMBCancel, &ret);
      if (ret == kMBYes) {
         if (!fFilename.CompareTo("Untitled"))
            SaveFileAs();
         else
            SaveFile(fFilename.Data());
         DataChanged(false);
      }
      if (ret == kMBCancel)
         return;
   }
#endif
   if (fFilename != "Untitled") {
      //if (!SaveFileAs())
      //   return;
      fFilename += ".C";
   }
   gInterpreter->SaveContext();
   QString savdir = gSystem->WorkingDirectory();
   QString tmpfile = gSystem->BaseName(fFilename.toLatin1().data());
   tmpfile += "_exec";
   gSystem->ChangeDirectory(gSystem->DirName(fFilename.toLatin1().data()));
   SaveFile(tmpfile);
   gROOT->SetExecutingMacro(kTRUE);
   gROOT->Macro(tmpfile.toLatin1().data());
   gROOT->SetExecutingMacro(kFALSE);
   if (gInterpreter->IsLoaded(tmpfile.toLatin1().data()))
      gInterpreter->UnloadFile(tmpfile.toLatin1().data());
   gSystem->Unlink(tmpfile.toLatin1().data());
   gSystem->ChangeDirectory(savdir.toLatin1().data());
   gInterpreter->Reset();
}

//______________________________________________________________________________
void TQtTextEditor::InterruptMacro()
{
   // Interrupt execution of a macro.
   gROOT->SetInterrupt(kTRUE);
}

//______________________________________________________________________________
void TQtTextEditor::About()
{
   // Display ROOT splash screen.

#ifdef R__UNIX
   QString rootx;
# ifdef ROOTBINDIR
   rootx = ROOTBINDIR;
# else
   rootx = gSystem->Getenv("ROOTSYS");
   if (!rootx.isEmpty()) rootx += "/bin";
# endif
   rootx += "/root -a &";
   gSystem->Exec(rootx);
#else
#ifdef WIN32
   new TWin32SplashThread(kTRUE);
#else
   QString str(tr("About ROOT %1...")).arg(gROOT->GetVersion());
   TRootHelpDialog *hd = new TRootHelpDialog(this, str.toLatin1.data(), 600, 400);
   hd->SetText(gHelpAbout);
   hd->Popup();
#endif
#endif
}

//______________________________________________________________________________
void  TQtTextEditor::ProcessMessage()
{
   TQtRootAction *actionSender =  (TQtRootAction *)sender ();
   switch (actionSender->Id()) {

   case kM_FILE_NEW:        NewCB();         break;
   case kM_FILE_OPEN:       OpenCB();        break;
   case kM_FILE_SAVE:       SaveCB();        break;
   case kM_FILE_SAVEAS:     SaveAsCB();      break;
   case kM_FILE_PRINT:      PrintCB();       break;
   case kM_FILE_EXIT:       QuitCB();        break;
 
   case kM_EDIT_CUT:        CutCB();         break;
   case kM_EDIT_COPY:       CopyCB();        break;
   case kM_EDIT_PASTE:      PasteCB();       break;
   case kM_EDIT_DELETE:     DeleteCB();      break;
   case kM_EDIT_SELECTALL:  SelectAllCB();   break;
   case kM_SEARCH_FIND:     FindCB();        break;
   case kM_SEARCH_FINDNEXT: FindNextCB();    break;

   case kM_SEARCH_GOTO:     GotoCB();        break;
   case kM_TOOLS_COMPILE:   CompileCB();     break;
   case kM_TOOLS_EXECUTE :  ExecuteCB();     break;
   case kM_TOOLS_INTERRUPT: InterruptCB();   break;
   case kM_HELP_CONTENTS:   HelpContentsCB();break;
   case kM_HELP_ABOUT:      AboutCB();       break;
   case kM_EDIT_SELFONT:    SelectFontCB();  break;

   default:
      break;
   };
}

//______________________________________________________________________________
void   TQtTextEditor::NewCB() {
    // add the new tab
#if 0   
   IsSaved())
#endif   
   ClearText();
}

//______________________________________________________________________________
void   TQtTextEditor::OpenCB() {
#if 0  
  switch (IsSaved()) {
      case kMBCancel:
         break;
      case kMBYes:
        if (fFilename=="Untitled")
             SaveFileAs();
        else
            SaveFile(fFilename.Data());
        if (TextChanged())
             break;
      case kMBNo:
           break;
   }
#else
     LoadFile();
#endif
}
//______________________________________________________________________________
void   TQtTextEditor::SaveCB() {
  if (fFilename=="Untitled")
        SaveFileAs();
  else
        SaveFile(fFilename);
}

//______________________________________________________________________________
void   TQtTextEditor::SaveAsCB() {
   SaveFileAs();
}

//______________________________________________________________________________
void   TQtTextEditor::PrintCB() {
   PrintText();
}
//______________________________________________________________________________
void   TQtTextEditor::QuitCB() {
   CloseWindow();
}                  
//______________________________________________________________________________
void   TQtTextEditor::CutCB() {
  fTextEdit->cut();
}

//______________________________________________________________________________
void   TQtTextEditor::CopyCB() {
  fTextEdit->copy();
}

//______________________________________________________________________________
void   TQtTextEditor::PasteCB() {
  fTextEdit->paste();
}
//______________________________________________________________________________
void   TQtTextEditor::DeleteCB() { 
  fTextEdit->cut();
}

//______________________________________________________________________________
void   TQtTextEditor::SelectAllCB() {
   
     fTextEdit->selectAll();
#if 0
     if (fTextEdit->IsMarked()) {
         fMenuEdit->EnableEntry(kM_EDIT_CUT);
         fMenuEdit->EnableEntry(kM_EDIT_COPY);
          fMenuEdit->EnableEntry(kM_EDIT_DELETE);
      if (fToolBar->GetButton(kM_EDIT_CUT)->GetState() == kButtonDisabled) {
            fToolBar->GetButton(kM_EDIT_CUT)->SetState(kButtonUp);
            fToolBar->GetButton(kM_EDIT_COPY)->SetState(kButtonUp);
           fToolBar->GetButton(kM_EDIT_DELETE)->SetState(kButtonUp);
                        }
                     }
#endif
}

//______________________________________________________________________________
void   TQtTextEditor::SelectFontCB() {
   bool ok;
   QFont font = QFontDialog::getFont(
                 &ok, fTextEdit->font(), this);
   if (ok) fTextEdit->setFont(font);
} 

//______________________________________________________________________________
void   TQtTextEditor::CompileCB() {   CompileMacro();    }

//______________________________________________________________________________
void   TQtTextEditor::ExecuteCB() {    ExecuteMacro();   }

//______________________________________________________________________________
void   TQtTextEditor::InterruptCB() {  InterruptMacro(); }

//______________________________________________________________________________
void   TQtTextEditor::FindCB()     {   Search(kFALSE);   }

//______________________________________________________________________________
void   TQtTextEditor::FindNextCB() {   Search(kTRUE); }
 
//______________________________________________________________________________
void   TQtTextEditor::GotoCB() {
   Goto();
}
//______________________________________________________________________________
void   TQtTextEditor::HelpContentsCB() {
#if 0
    hd = new TRootHelpDialog(this, "Help on Editor...", 600, 400);
    hd->SetText(gHelpTextEditor);
    hd->Popup();
#endif
}

//______________________________________________________________________________
void   TQtTextEditor::AboutCB() {    About();   }

//______________________________________________________________________________
bool TQtTextEditor::TextChanged() const
{
  if (!fTextEdit) return false;
  return fTextEdit->document()->isModified();
}
//______________________________________________________________________________
void   TQtTextEditor::SetText(const char *text) {
   SetText(QString(text));
}
//______________________________________________________________________________
void   TQtTextEditor::SetText(const QString &text) {
    fTextEdit->setPlainText(text); 
}
//______________________________________________________________________________
void  TQtTextEditor::AddText(const char *text) { 
   AddText(QString(text));
}
//______________________________________________________________________________
void  TQtTextEditor::AddText(const QString &text) { 
    fTextEdit->append(text);
}

//______________________________________________________________________________
void   TQtTextEditor::AddLine(const char *string) { 
   AddLine(QString(string));
}
//______________________________________________________________________________
void   TQtTextEditor::AddLine(const QString &string) { 
    AddText(string);
}
//______________________________________________________________________________
void   TQtTextEditor::AddLineFast(const char *string) { 
    AddLineFast(QString(string)); 
}

//______________________________________________________________________________
void   TQtTextEditor::AddLineFast(const QString &string) { 
    AddText(string); 
}
//______________________________________________________________________________
//   QText         *GetText() const { return fTextEdit->GetText(); }

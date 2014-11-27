// @(#)root/gui:$Id: TQtTextEditor.h,v 1.3 2013/08/30 16:00:22 perev Exp $
// Author: Bertrand Bellenot   20/06/06

/*************************************************************************
 * Copyright (C) 1995-2000, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtTextEditor
#define ROOT_TQtTextEditor


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtTextEditor                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <QMainWindow>
#include <QString>
#include <QMap>

class QComboBox;
class QLabel;
class QLayoutHints;
class QMenuBar;
class QMenu;
class QStatusBar;
class QToolBar;
class TMacro;
class TString;
class QTimer;
class TQtRootAction;
class QTextEdit;
class QText;

class TQtTextEditor : public QMainWindow {
     Q_OBJECT
protected:                               // data-members:

   QVector<QLabel *> fStatusBar;         // for file name, line and col number
                                         // toolbar with common tool buttons
   QToolBar        *fToolFile;
   QToolBar        *fToolEdit;
   QToolBar        *fToolSearch;
   QToolBar        *fToolTools;
   QToolBar        *fToolHelp;
   
   QTextEdit       *fTextEdit;           // text edit widget
   QLabel          *fLabel;              // "command" label
   QMenuBar        *fMenuBar;            // editor's menu bar
   QMenu           *fMenuFile;           // "File" menu entry
   QMenu           *fMenuEdit;           // "Edit" menu entry
   QMenu           *fMenuSearch;         // "Search" menu entry
   QMenu           *fMenuTools;          // "Tools" menu entry
   QMenu           *fMenuHelp;           // "Help" menu entry
   bool             fExiting;            // true if editor is closing
   QString          fFilename;           // name of the opened file
   TMacro          *fMacro;              // pointer on the opened macro
   QMap<int, TQtRootAction*> fActions;

protected:                               // methods:

   virtual void              Build();
   virtual void        MakeActions();
   virtual void        SetupEditor();
           void  CreateStatusBar(int *parts, int nparts);
           void  CreateStatusBar(int nparts);
           void ConnectSlots();
public:

   TQtTextEditor( QWidget * parent = 0, Qt::WindowFlags flags = 0);

   TQtTextEditor(const QString &filename, QWidget * parent = 0,
                unsigned int w = 900, unsigned int h = 600);
   TQtTextEditor(TMacro *macro, QWidget * parent = 0, unsigned int w = 900,
                unsigned int h = 600);
   virtual ~TQtTextEditor();
   bool TextChanged() const;

public slots:
   void           ClearText();
   void           LoadFile(const QString &fname) ;
   void           LoadFile(char *fname = 0);
   bool           SaveFile(const QString &fname);
   bool           SaveFile(const char *fname);
   bool           SaveFileAs();
   void           PrintText();
   void           Search(bool ret);
   void           Goto();
   void           About();
   int            IsSaved();
   void           CompileMacro();
   void           ExecuteMacro();
   void           InterruptMacro();
   void           SetText(const QString &text);
   void           AddText(const QString &text);
   void           SetText(const char *text);
   void           AddText(const char *text);
   void           AddLine(const char *string);
   void           AddLineFast(const QString &string);
   void           AddLine(const QString &string);
   void           AddLineFast(const char *string);
   void           CanPaste();
//   QText         *GetText() const;

public slots:

   void   ProcessMessage();
   void   CloseWindow();
   void   DeleteWindow();

   void SetStatusText(const QString &text, int partidx);
   bool DataChanged(bool yes=true);
   
  // file menu 
   void NewCB();
   void OpenCB();
   void SaveCB();
   void SaveAsCB();
   void PrintCB();
   void QuitCB();

  // editor menu 
   void CopyCB();
   void CutCB();
   void PasteCB();
   void DeleteCB();
   void SelectAllCB();
   void FindCB();
   void FindNextCB();
   void GotoCB();
   void ExecuteCB();
   void CompileCB();
   void SelectFontCB();
   void HelpContentsCB();
   void AboutCB();
   
  // view  menu 
   void InterruptCB();

   // ClassDef(TQtTextEditor,0)  // Simple text editor using QTextEdit widget
};

#endif

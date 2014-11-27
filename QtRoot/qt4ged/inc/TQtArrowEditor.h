// Author: Eric Dumonteil   26/01/2006

/****************************************************************************
** $Id: TQtArrowEditor.h,v 1.3 2013/08/30 15:59:52 perev Exp $
**
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TArrowEditor, a ROOT GUI toolkit.             *
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  TQtArrowEditor                                                      //
//                                                                      //
//  Implements GUI for editing arrow attributes: shape, size, angle.    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TQtArrowEditor
#define ROOT_TQtArrowEditor

/* #ifndef ROOT_TQtGedAttFrame */
#include "TQtGedAttFrame.h"
/* #endif */
#include "TArrow.h"

class TQtArrowStyleSelect;
class QWidget;
class TQtFloatSpinBox;



class TQtArrowEditor : public TQtGedAttFrame<TArrow>{
#ifndef __CINT__
   Q_OBJECT
#endif
protected:

   TQtArrowStyleSelect  *fOptionCombo;      // arrow shapes combo box
   TQtFloatSpinBox      *fAngleEntry;       // opening angle entry
   TQtFloatSpinBox      *fSizeEntry;        // size entry

   virtual void BuildView(QWidget  *editorPanel);
   virtual void ConnectSignals2Slots();
   virtual void ChangeView();

public:
   TQtArrowEditor(QMainWindow *mainWidget, TCanvas *canvas, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, // kChildFrame,
		  Pixel_t back = 0) ;
   TQtArrowEditor(TCanvas *canvas, QWidget *parent=0, Int_t id=0,
               Int_t width = TQtGedAttInterfaceB::EditorDefaultWidth(), Int_t height = 30,
               UInt_t options = 0, // kChildFrame,
		  Pixel_t back = 0) ;
   virtual ~TQtArrowEditor() ;

public slots:
   virtual void   DoAngle(int );
   virtual void   DoOption(int );
   virtual void   DoSize(int );
   virtual void   DoAngle(double );
   virtual void   DoSize(double);

#ifndef Q_MOC_RUN
//MOC_SKIP_BEGIN
   ClassDef(TQtArrowEditor,0)  // axis editor
//MOC_SKIP_END
#endif
};

#endif

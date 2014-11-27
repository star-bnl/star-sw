// Author: Valeri Fine 22/03/2003
/*************************************************************************
 * Copyright (C) 2003 Valeri Fine
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

/*************************************************************************
 *   Modified by Valeri Fine to use Qt GUI labrary                       *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// GSelectTrack                                                          // 
//                                                                      //
// This File contains the declaration of the GTitleFrame-class for      //
// the RootShower application                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef GSHOWERPAD_H
#define GSHOWERPAD_H
#include <q3vbox.h>

class TQtWidget;
class TCanvas;

class GShowerPad : public Q3VBox {
   Q_OBJECT
public:
   GShowerPad(QWidget * parent = 0, const char * name = 0, Qt::WFlags f = 0 );
   TCanvas *GetCanvas();
public slots:
   void PadZoomForward();
   void PadZoomBackward();
   void Show3D();
protected:
       TQtWidget            *cA;
};
#endif

/* @(#)root/win32:$Name:  $:$Id: LinkDef.h,v 1.4 2013/08/30 16:00:19 perev Exp $ */

/****************************************************************************
** $Id: LinkDef.h,v 1.4 2013/08/30 16:00:19 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
 
// #pragma link C++ class TQt;
#pragma link C++ class TQtGUIFactory;
#pragma link C++ class TBrowserCustom;
#pragma link C++ class TQtColorSelect;
#pragma link C++ class TQtPatternSelect;
#pragma link C++ class TQtMarkerSelect;
#pragma link C++ class TEmbeddedPad;
#pragma link C++ class TQtTabValidator;
#pragma link C++ class TQtZoomPadWidget;
#pragma link C++ class TQRootCanvas;
#pragma link C++ class TQtPad2Html;
#pragma link C++ class TQtCanvas2Html;
#pragma link C++ class TQtPixmapBox;
#pragma link C++ class TEmitRootSignal;
#pragma link C++ class TColorEmit;
#endif

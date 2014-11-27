// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Definition of the "About" message box for the RootShower application //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOTSHOWERABOUT_H
#define ROOTSHOWERABOUT_H

#include <qmessagebox.h>

class RootShowerAbout : public QMessageBox {
protected:
    RootShowerAbout(QWidget *p, UInt_t w, UInt_t h,
	                UInt_t options = 0);
    virtual ~RootShowerAbout(){;}
public:
    static void About(QWidget *p, UInt_t w, UInt_t h);
};

#endif // ROOTSHOWERABOUT_H

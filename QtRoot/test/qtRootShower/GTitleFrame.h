// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

/*************************************************************************
 *   Modified by Valeri Fine to use Qt GUI labrary                       *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// GTitleFrame                                                          // 
//                                                                      //
// This File contains the declaration of the GTitleFrame-class for      //
// the RootShower application                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef GTITLEFRAME_H
#define GTITLEFRAME_H

#include "q3hbox.h"
//Added by qt3to4:
#include <QLabel>

class QLabel;

class GTitleFrame: public Q3HBox {
private:
    QLabel *fRightIconPicture;
public:

    // Constructor & destructor
    GTitleFrame(QWidget *p, const Text_t *mainText, const Text_t *subText,
		        UInt_t w, UInt_t h, UInt_t options=0);
    void ChangeRightLogo(Int_t frame);
    virtual ~GTitleFrame();
};


#endif // GTITLEFRAME_H

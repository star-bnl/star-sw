// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Definition of a dialog box used to access the main shower parameters //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include <q3tabdialog.h>

class Q3Table;
class Q3ListBox;
class Q3VButtonGroup;
class RootShower;

class SettingsDialog : public Q3TabDialog {
Q_OBJECT
private:

    Q3Table        *fDimensionTable;
    Q3Table        *fParticleTable;
    Q3ListBox      *fListBox;
    Q3VButtonGroup *fF1;

public:
    SettingsDialog(RootShower *p, unsigned int w, unsigned int h);                         
    virtual ~SettingsDialog();

public slots:
    virtual void ProcessMessage();
    virtual void HelpCB();
};

#endif

// @(#)root/gt:$Name:  $:$Id: TQtRootCommandCombo.cxx,v 1.4 2013/08/30 16:00:25 perev Exp $
// Author: Valeri Fine   11/01/2009

/****************************************************************************
** $Id: TQtRootCommandCombo.cxx,v 1.4 2013/08/30 16:00:25 perev Exp $
**
** Copyright (C) 2009 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
///////////////////////////////////////////////////////////////////////////
//
//  The TQtRootCommandCombo is a regular Qt QComboBox class that can send the text
//  entered by the user to the RootCint processor as soon as the user hits "return" key
//  by emitting the "CommandEntered(const QString &);" signal
//
//  The steering code can enable/disable the ROOT command execution via  
//  SetRootCommandExecute(bool on) method.
//
///////////////////////////////////////////////////////////////////////////

#include "TQtRootCommandCombo.h"
#include "TQtRootSlot.h"
#include <QLineEdit>
#include <QFile>
#include <QDir>
#include <QTextStream>
#include <QString>
#include "TSystem.h"

using namespace std;
//_____________________________________________________________________________
TQtRootCommandCombo::TQtRootCommandCombo(QWidget *parent) : QComboBox(parent)
, fRootCommandExecute(true)
{
   Init();
}

//_____________________________________________________________________________
void TQtRootCommandCombo::Init()
{
   setEditable (true);
   //InitFromHistory();
   setInsertPolicy(QComboBox::InsertAtTop);
   QSizePolicy comboBoxPolicy = sizePolicy();
   comboBoxPolicy.setHorizontalPolicy (QSizePolicy::Ignored);
   setSizePolicy(comboBoxPolicy);
   InsertFromHistory();
   setCurrentIndex(0);
   ConnectTreeSlots();
}
//_____________________________________________________________________________
void TQtRootCommandCombo::InsertFromHistory(int index) 
{
   QString defhist =  QDir::homePath () + "/.root_hist";
   QFile lunin(defhist);
   if (lunin.open(QIODevice::ReadOnly | QIODevice::Text)) {
     QTextStream in(&lunin);
     while (!in.atEnd()) {
         QString line = in.readLine();
         insertItem(index,line);
     }
   }
}

//_____________________________________________________________________________
void TQtRootCommandCombo::ConnectTreeSlots()
{
   // Connect the QLineEditor with the ROOT command interpreter
   connect(this->lineEdit(),SIGNAL(returnPressed ()), this, SLOT( rootCommandExecute()) );
   connect(this->lineEdit(),SIGNAL(returnPressed ()), this->lineEdit(), SLOT( clear()) );

   bool doRoot = fRootCommandExecute;
   fRootCommandExecute = !doRoot;  // to force the signal / slot connection
   SetRootCommandExecute(doRoot);
}

//_____________________________________________________________________________
TQtRootCommandCombo::~TQtRootCommandCombo() {}
//_____________________________________________________________________________
void TQtRootCommandCombo::rootCommandExecute() {    
   // Save and execute the last command if needed
   fLastComboLine = this->lineEdit()->text();
   if ( IsRootCommnadExecute() )  emit CommandEntered(fLastComboLine);
}

//_____________________________________________________________________________
void TQtRootCommandCombo::SetRootCommandExecute(bool on)
{
   // Define whether object should execute the text as the ROOT command
   // The default status is "execute"
   if (on != IsRootCommnadExecute() ) {
      fRootCommandExecute = on;
      if (fRootCommandExecute) {
            connect(this,SIGNAL(CommandEntered(const QString&))
               , TQtRootSlot::CintSlot(),SLOT(  ProcessLine(const QString &)) );
      } else {
           disconnect(this,SIGNAL(CommandEntered(const QString&))
                     , TQtRootSlot::CintSlot(),SLOT( ProcessLine(const QString &)) );
      }
   }
}


// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

#include <stdlib.h>
// #include <TRootHelpDialog.h>

#include "SettingsDlg.h"
#include "RootShower.h"
#include "constants.h"
#include "RSHelpText.h"

#include <q3hbox.h>
#include <q3buttongroup.h> 
#include <qradiobutton.h>
#include <qtooltip.h>
#include <q3vgroupbox.h> 
#include <q3listbox.h>
#include <q3table.h>
#include <qmessagebox.h>

// definition of structure used to retrieve
// PDG number and particle name into the list
// of available primaries

typedef struct {
    Int_t       pdg_code;
    const char *pdg_name;
} str_choice_def;

str_choice_def choice_def[] = {
      {    22,  "gamma"       },
      {    11,  "e-"          },
      {   -11,  "e+"          },
      {    13,  "mu-"         },
      {   -13,  "mu+"         },
      {    15,  "tau-"        },
      {   -15,  "tau+"        },
      {   111,  "pi0"         },
      {   211,  "pi+"         },
      {  -211,  "pi-"         },
      {   221,  "Eta"         },
      {   321,  "K+"          },
      {  -321,  "K-"          },
      {   130,  "K(L)0"       },
      {   310,  "K(S)0"       },
      {   113,  "rho(770)0"   },
      {   213,  "rho(770)+"   },
      {  -213,  "rho(770)-"   },
      {   223,  "omega(782)0" },
      {   333,  "phi(1020)0"  },
      {   443,  "J/psi(1S)0"  },
      {   511,  "B0"          },
      {   513,  "B*0"         },
      {   521,  "B+"          },
      {  -521,  "B-"          },
      {   523,  "B*+"         },
      {  -523,  "B*-"         },
      {   531,  "B(s)0"       },
      {   411,  "D+"          },
      {  -411,  "D-"          },
      {   421,  "D0"          },
      {   431,  "D(s)+"       },
      {  -431,  "D(s)-"       },
      {   433,  "D(s)*+"      },
      {  -433,  "D(s)*-"      },
      {    24,  "W+"          },
      {   -24,  "W-"          },
      {    23,  "Z0"          },
      {     0,    0           }
};

//_________________________________________________
// SettingsDialog
//
// SettingsDialog is a dialog accessing the RootShowerhower parameters.

//______________________________________________________________________________
SettingsDialog::SettingsDialog(RootShower *p,unsigned int /*w*/, unsigned int /*h*/ )
                          : Q3TabDialog(p,"SettingsDialog",true)
{
    // Create a dialog window. A dialog window pops up with respect to its
    // "main" window.
    Int_t i;
    Char_t tmp[20];

    setCaption("Shower Settings");
    //--------- create Tab widget and some composite frames for Tab testing
    Q3HBox *tf = new Q3HBox(this,"TargetPropertiesBox");
    addTab(tf,"Target properties");
 // First tab 
 // --  material 
    {  // we made the bracket just to highlight the first tab staff
       fF1 = new Q3VButtonGroup("Material",tf);
       fF1->setRadioButtonExclusive (true);
       {
          QRadioButton *radioButton =new QRadioButton ( "Polystyrene", fF1);
          QToolTip::add(radioButton,"Polystyrene");
          fF1->insert(radioButton,Polystyrene);

          radioButton =new QRadioButton ( "Bismuth germanate", fF1);
          QToolTip::add(radioButton,"Bismuth germanate (BGO)");
          fF1->insert(radioButton,BGO);

          radioButton =new QRadioButton ( "Cesium iodide", fF1);
          QToolTip::add(radioButton,"Cesium iodide (CsI)");
          fF1->insert(radioButton,CsI);

          radioButton =new QRadioButton ( "Sodium iodidee", fF1);
          QToolTip::add(radioButton,"Sodium iodide (NaI)");
          fF1->insert(radioButton,NaI);
          //  select one button
          fF1->setButton(p->fMaterial);
       }

       Q3VGroupBox  *dimensionBox = new Q3VGroupBox  ( "Dimensions", tf);
       dimensionBox->setAlignment(Qt::AlignRight);
       {
          // another matrix with text and buttons
          fDimensionTable = new Q3Table ( 3,1, dimensionBox);
          fDimensionTable->setRowLabels (QStringList::split(",","X [cm],Y [cm],Z [cm]"));
          fDimensionTable->setTopMargin( 0 );

          sprintf(tmp,"%1.3f",p->fDimX);
          fDimensionTable->setText(0,0,tmp);

          sprintf(tmp,"%1.3f",p->fDimY);
          fDimensionTable->setText(1,0,tmp);

          sprintf(tmp,"%1.3f",p->fDimZ);
          fDimensionTable->setText(2,0,tmp);
       }
    }
//---  second tab
    tf = new Q3HBox(this,"PhysicsSettingsx");
    addTab(tf,"Physics settings");
// Particle box
    {
       fListBox = new Q3ListBox(new Q3VGroupBox ( "Particle", tf),"Particle");
       for (i = 0; choice_def[i].pdg_name; i++) {
          fListBox->insertItem(choice_def[i].pdg_name);
       }

       Q3VGroupBox *groupBox  = new Q3VGroupBox  ( "E0 / B", tf);
       groupBox->setAlignment(Qt::AlignRight);
       {
          // another matrix with text and buttons
          fParticleTable = new Q3Table (2,1, groupBox);
          fParticleTable->setRowLabels (QStringList::split(",","E0 [GeV],B [kGauss]"));
          fParticleTable->setTopMargin( 0 );

          sprintf(tmp,"%1.4f",p->fE0);
          fParticleTable->setText(0,0,tmp);

          sprintf(tmp,"%1.4f",p->fB);
          fParticleTable->setText(1,0,tmp);

          for (i = 0; choice_def[i].pdg_name; i++) {
             if(p->fFirstParticle == choice_def[i].pdg_code) {
                fListBox->setSelected(i,true);
                fListBox->setCurrentItem ( i );
                fListBox->centerCurrentItem () ;
                break;
             }
          }
       }
    }
//  The common buttons
    setOkButton("&Ok");
    setCancelButton("&Cancel");
    setHelpButton ();
    connect(this, SIGNAL(applyButtonPressed ()),this,SLOT(ProcessMessage()));
    connect(this, SIGNAL(helpButtonPressed  ()),this,SLOT(HelpCB()));
}
//______________________________________________________________________________
SettingsDialog::~SettingsDialog()
{ }
//______________________________________________________________________________
//
//             SLOTS:
//______________________________________________________________________________
void SettingsDialog::ProcessMessage()
{
   // Process messages coming from widgets associated with the dialog.
   Int_t Selection;
   Q3ListBoxItem *selected = fListBox->selectedItem ();
   Selection = fListBox->index(selected);
   if(Selection > 37) {
      QMessageBox::critical (this, "Particle selection" 
                                 , "This particle is not implemented yet !"
                                 , QMessageBox::Ok,QMessageBox::NoButton);
      fListBox->setSelected(2,true);
      fListBox->setCurrentItem ( 2 );
      fListBox->centerCurrentItem () ;
   }
   // change the status of the parent RootShower widget
   RootShower *showerParent = (RootShower *)parentWidget();
   showerParent->fFirstParticle = choice_def[Selection].pdg_code;
   //  dimentsion
   showerParent->fDimX = fDimensionTable->text(0,0).toFloat();
   showerParent->fDimY = fDimensionTable->text(1,0).toFloat();
   showerParent->fDimZ = fDimensionTable->text(2,0).toFloat();

   showerParent->fE0 = fParticleTable->text(0,0).toFloat();
   showerParent->fB  = fParticleTable->text(1,0).toFloat();

   showerParent->fMaterial = fF1->id ( fF1->selected () );
}
//______________________________________________________________________________
void SettingsDialog::HelpCB()
{   QMessageBox::information(0,"Help on Settings Dialog",gSettingsHelp,QMessageBox::Ok); }

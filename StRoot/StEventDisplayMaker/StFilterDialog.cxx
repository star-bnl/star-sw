//*-- Author :    Valery Fine(fine@bnl.gov)   08/01/03  
// $Id: StFilterDialog.cxx,v 1.1 2003/01/17 01:36:16 fine Exp $

#include "StFilterDialog.h"
#ifdef R__QT
#include <qtable.h>
#include <qlayout.h>
#include <qhbox.h>
#include <qpushbutton.h>

//_______________________________________________________________________________________

static const char*  NamValQQ[]={
   "  RandomSelect ",
   "  RxyMin       ",
   "  RxyMax       ",
   "  ZMin         ",
   "  ZMax         ",
   "  PhiMin       ",
   "  PhiMax       ",
   "  PtMin        ",
   "  PtMax        ",
   "  QMin         ",
   "  QMax         ", 
   0};

static const float  DefsQQ[]={
   /*  RandomSelect=*/    1.00,
   /*  RxyMin      =*/    0.00,
   /*  RxyMax      =*/  200.00,
   /*  ZMin        =*/ -200.00,
   /*  ZMax        =*/ +200.00,
   /*  PhiMin      =*/ -180.00,
   /*  PhiMax      =*/ +180.00,
   /*  PtMin       =*/    0.00,
   /*  PtMax       =*/  999.00,
   /*  QMin        =*/   -1   ,
   /*  QMax        =*/   +1   ,
   0};

//_______________________________________________________________________________________
StFilterDialog::StFilterDialog(const char *wName,const char **NamVal,const float *defs, float *vals,int *flagg)
: QVBox()
{
  char cbuf[200];
  if (!wName) wName = "DefaultStFilterDialog";
  fNamVal = NamVal;
  if(!fNamVal) fNamVal = NamValQQ;
  fDefs = defs;
  if (!fDefs) fDefs = DefsQQ;
  fVals = vals;
  assert(fVals);
  
  int nCol=1,nRow;
  for (nRow=0;fNamVal[nRow];nRow++){};
  fNVals = nRow;
  fFlagg = flagg;

  setCaption(wName);

  QVBox *columns = this; // new QVBox(this);
  {
     // table widget
     fTable = new QTable(nRow,nCol,columns);
//     fTable->setColumnReadOnly(0,true);
     fTable->horizontalHeader ()->setLabel( 0, tr( "Cuts" ) );//  setText(iRow,0,fNamVal[iRow]);

     for (int iRow=0;iRow<nRow;iRow++) {
        fTable->verticalHeader ()->setLabel( iRow, tr( fNamVal[iRow] ) );//  setText(iRow,0,fNamVal[iRow]);
        sprintf(cbuf,"%+10g",fVals[iRow]);
        fTable->setText(iRow,0,cbuf);
     }
//     fTable->adjustColumn(0);

     // Ok / Reset buttons
     QWidget *buttons = new QWidget(columns);
     {
        QPushButton *ok         = new QPushButton("OK",buttons);
        connect(ok,SIGNAL(clicked()),this,SLOT(Update()));

        QPushButton *setDefault = new QPushButton("Reset",buttons);
        connect(setDefault,SIGNAL(clicked()),this,SLOT(Reset()));

        QBoxLayout * l = new QHBoxLayout( buttons );
        l->addItem(new QSpacerItem(10,1));
        l->addWidget( ok );
        l->addItem(new QSpacerItem(10,1));
        l->addWidget( setDefault );
        l->addItem(new QSpacerItem(10,1));
     }
  }
  Show();
}  
//_______________________________________________________________________________________
StFilterDialog::~StFilterDialog()   
{  }
//_______________________________________________________________________________________
void StFilterDialog::Reset()   
{
  char cbuf[100];  
  for (int irow=0; irow<fTable->numRows (); irow++) {
    sprintf(cbuf,"%+10g",fDefs[irow]);
    fTable->setText(irow,0,cbuf);
  }
  Show();
}
//_______________________________________________________________________________________
void StFilterDialog::Update()   
{
  const char *txt=0;

  { 
     // This a trick to make sure the last updated value has not been lost
     fTable->setCurrentCell(0,0);  fTable->setCurrentCell(1,0);
  }

  assert(fTable->numRows ()<=fNVals);
  for (int irow=0; irow<fNVals; irow++) {
    txt = fTable->text(irow,0);
    float f = strtod(txt,0);
//    printf("f = %f\n",f);
    if (fVals) fVals[irow]=f;
  }
  Show();
  assert(fFlagg);
  fFlagg[0]=0;
  // Close and delete the widget
  close (true );
}
//_______________________________________________________________________________________
void  StFilterDialog::Show()
{ 
   show();
   raise();
}
#endif

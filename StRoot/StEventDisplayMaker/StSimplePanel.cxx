/****************************************************************************
** Form implementation generated from reading ui file 'Panel.ui'
**
** Created: Wed Jan 8 14:32:54 2003
**      by: The User Interface Compiler ($Id: StSimplePanel.cxx,v 1.5 2009/02/03 23:07:48 fine Exp $)
**
** WARNING! All changes made in this file will be lost!
****************************************************************************/

#include "StSimplePanel.h"
#ifdef R__QT4
#include <qvariant.h>
#include <qbuttongroup.h>
#include <qvbuttongroup.h>
#include <qcheckbox.h>
#include <qgroupbox.h>
#include <qtoolbutton.h>
#include <qlayout.h>
#include <qtooltip.h>
#include <qwhatsthis.h>


//______________________________________________________________________________
/* 
 *  Constructs a StSimplePanel as a child of 'parent', with the 
 *  name 'name'
 */
StSimplePanel::StSimplePanel( QWidget* parent, const char* name, int nColumns )
    : QGroupBox( parent, name), fLayout(0)
{
    if ( !name ) setName( "StSimplePanel" );
    QVBoxLayout * l = new QVBoxLayout( this );
    l->addItem(new QSpacerItem(10,10));
    fLayout = new QGridLayout ( l, 1, nColumns,4,name);
    fLayout->setMargin ( 5 );

}
//______________________________________________________________________________
StSimplePanel::~StSimplePanel()
{
    // no need to delete child widgets, Qt does it all for us
}
//______________________________________________________________________________
void StSimplePanel::Add(QWidget *q, int pRow, int pCol, unsigned int aligment)
{
   // Qt::AlignAuto    - Aligns according to the language. Left for most, right for Arabic and Hebrew. 
   // Qt::AlignLeft    - Aligns with the left edge. 
   // Qt::AlignRight   - Aligns with the right edge.
   // Qt::AlignHCenter - Centers horizontally in the available space.
   // Qt::AlignJustify - Justifies the text in the available space

   if (pRow == -1) pRow = fLayout->numRows();
   fLayout->addWidget(q,pRow,pCol,aligment);
}
//______________________________________________________________________________
void StSimplePanel::AddMulti(QWidget *q,int fromCol, int toCol, int pRow
            , unsigned int aligment)
{
  //  Adds the widget w to the cell grid, spanning multiple rows/columns. 
  //  The cell will span from fromRow, fromCol to toRow, toCol.
  if (pRow == -1)  pRow = fLayout->numRows();
  if (toCol == -1) toCol = fLayout->numCols()-1;
  fLayout->addMultiCellWidget(q, pRow, pRow, fromCol,toCol,aligment);  
}

#endif

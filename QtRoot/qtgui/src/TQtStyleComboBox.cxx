// @(#)root/gui:$Name:  $:$Id: TQtStyleComboBox.cxx,v 1.7 2013/08/30 16:00:25 perev Exp $
// Author: Valeri Fine 07/07/2006
/****************************************************************************
**
** Copyright (C) 2006 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/
#include "TQtStyleComboBox.h"

#include "TEmbeddedPad.h"
#include "TQtLockWidget.h"
#include "TLine.h"
#include "TText.h"
#include "TQtPadFont.h"
#include "TQtPen.h"

#include <QPixmap>
#include <QSize>
#include <QResizeEvent>
#include <QItemDelegate>
#include <QPainter>
#include <QLineEdit>
#include <QDebug>


class TQtStyleComboDelegate : public QItemDelegate {
// This class to paint each combo box item with TPad pixmap
public:
   virtual ~TQtStyleComboDelegate() {}
   virtual void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex & index ) const ;

};
//______________________________________________________________________________
void TQtStyleComboDelegate::paint ( QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const 
{
if (option.state & QStyle::State_Selected) {
        painter->fillRect(option.rect, option.palette.highlight());
//        painter->setPen(QPen(option.palette.highlightedText(), 0));
     }

//     int size = qMin(option.rect.width(), option.rect.height());
//     QVariant model = index.model()->data(index, Qt::DisplayRole);
     QVariant model = index.model()->data(index, Qt::UserRole);
     painter->save();
     if (model.canConvert (QVariant::Pixmap)) {
        QFontMetrics metric = painter->fontMetrics ();
        QRect labelrect = metric.boundingRect ("99-");
        painter->drawPixmap(option.rect,model.value<QPixmap>());
//        painter->drawPixmap(labelrect.width(),option.rect.height(),model.value<QPixmap>());
     } else if (model.canConvert (QVariant::Pen)) {
        
         // Draw the text
         QString text = index.data(Qt::DisplayRole).toString();
         painter->drawText(option.rect, Qt::AlignVCenter, text); 

         // draw the lines
         QPen pen = model.value<QPen>();
         painter->setPen(pen);
         QFontMetrics fm(painter->font());
         int tsz =  fm.width(QString("99:_ "));
         painter->drawLine(tsz, option.rect.y() + option.rect.height()/2
                          ,option.rect.width(),         option.rect.y()+ option.rect.height()/2);
     } else if ( model.canConvert (QVariant::Font)) {
        // draw the font
        painter->setRenderHint(QPainter::Antialiasing, true);  

        QString text = index.data(Qt::DisplayRole).toString();
//        painter->setFont (option.font); 
        painter->setFont (model.value<QFont>()); 
//        painter->setPen(Qt::NoPen);
        if (option.state & QStyle::State_Selected)
           painter->setBrush(option.palette.highlightedText());
        painter->drawText(option.rect, Qt::AlignVCenter, text); 
     }
     painter->restore();
}

//______________________________________________________________________________
TQtStyleComboBox::TQtStyleComboBox( int listSize, QWidget *parent,const QString name)
: QComboBox(parent), fPad(0), fItemListSize (listSize)
{
    // The base class for all "style" selectors
   // Add the the prepared Qt item to the QComboBox
   setObjectName(name);
   setItemDelegate( new TQtStyleComboDelegate);
}
//______________________________________________________________________________
TQtStyleComboBox::~TQtStyleComboBox () 
{
   delete fPad; fPad = 0;
}
//______________________________________________________________________________
int TQtStyleComboBox::AddComboItem(QPixmap &pixmap, QString &seq)
{
   // Add the the prepared Qt item to the QComboBox
   QString indx = seq.rightJustify(2,' ',true);
   addItem(indx,QVariant(pixmap));
 //  setItemData(count(),pixmap,Qt::BackgroundRole);
   Pad().Clear();
   return count();
}
//______________________________________________________________________________
int TQtStyleComboBox::AddComboItem(QFont &font, QString &seq)
{
   // Add the the prepared Qt item to the QComboBox
   QString indx = seq; // seq.rightJustify(2,' ',true);
   addItem(indx,QVariant(font));
   return count();
}

//______________________________________________________________________________
int TQtStyleComboBox::AddComboItem(QPen &pen, QString &seq)
{
   // Add the the prepared Qt item to the QComboBox
   QString indx = seq.rightJustify(2,' ',true);
   addItem(indx,QVariant(pen));
   return count();
}
//______________________________________________________________________________
void TQtStyleComboBox::Build()
{ 
   // Rebuild the "pad" items to fit the new widget size
   TQtLockWidget(this);

   clear();
   TVirtualPad *padsav = gPad;
   for (Int_t i = 1; i <= fItemListSize; i++) AddItem(i,FALSE);
   if (padsav) padsav->cd();

   SetCurrentItem (1);  // to have first entry selected
}

//______________________________________________________________________________
void TQtStyleComboBox::SetCurrentItem(int style)
{
   // Select the item using ROOT style type
  setCurrentItem (style-1);
}

//______________________________________________________________________________
TEmbeddedPad  &TQtStyleComboBox::Pad() {
   if (!fPad) {
//      fPad = new  TEmbeddedPad("","",width()-58,height()-4,kWhite);
      fPad = new  TEmbeddedPad("","",width(),height(),kWhite);
#if ROOT_VERSION_CODE > ROOT_VERSION(5,20,0)
// Pending ROOT bug 38454 fix:  https://savannah.cern.ch/bugs/?func=detailitem&item_id=38454
     fPad->SetFillStyle(0);
#endif
      fPad->SetBorderSize(0);
   }
   return *fPad;
}
 //______________________________________________________________________________
void  TQtStyleComboBox::resizeEvent(QResizeEvent *e)
{
   QComboBox::resizeEvent(e);
   if (abs(e->oldSize().width() -  e->size().width()) > 1 ) {
     TVirtualPad *padsav = fPad;
     fPad = 0;
     Build();
     if (padsav) delete padsav;
   }
}

//______________________________________________________________________________
//
//   TQtLineStyleComboBox
//______________________________________________________________________________
TQtLineStyleComboBox::TQtLineStyleComboBox(QWidget *parent,QString name)
     : TQtStyleComboBox(10,parent,name)
{
   // Create the image of the line using the given styles and the offscreen TPad
}
//______________________________________________________________________________
void TQtLineStyleComboBox::AddItem(int lstyle, bool savepadflag)
{
   // Create the image of the line using the given styles and the offscreen TPad
   if (savepadflag){ }
   QString seq=QString("%1:").arg(QString::number(lstyle));
   TQtPen  pen;
   pen.SetLineStyle(lstyle);
   AddComboItem(pen,seq);
}
//______________________________________________________________________________
//
//   TQtLineWidthComboBox
//______________________________________________________________________________
//______________________________________________________________________________
TQtLineWidthComboBox::TQtLineWidthComboBox(QWidget *parent,QString name)
      : TQtStyleComboBox(16,parent,name)
{
   // Create the image of the line using the given styles and the offscreen TPad
}
//______________________________________________________________________________
void TQtLineWidthComboBox::AddItem(int lwidth, bool savepadflag)
{
   // Create the image of the line using the given styles and the offscreen TPad
   if (savepadflag){ }
   QString seq=QString("%1:").arg(QString::number(lwidth));
   QPen  pen;
   pen.setWidth(lwidth);
   AddComboItem(pen,seq);
}
//_____________________________________________________________________________
//
//  TQtFontTypeComboBox
//______________________________________________________________________________
 TQtFontComboBox::TQtFontComboBox(QWidget *parent,QString name)
      : TQtStyleComboBox(13,parent,name) 
{
   // Create the image of the fonts using the given font number and the offscreen TPad
   connect(this,SIGNAL(currentIndexChanged(int)),this,SLOT(SetFont(int)));
}
static const char *gFonts[] = {
      "times italic"
    , "times bold"
    , "times bold italic"
    , "helvetica"
    , "helvetica italic"
    , "helvetica bold"
    , "helvetica bold italic"
    , "courier"
    , "courier italic"
    , "courier bold"
    , "courier bold italic"
    , "symbol"
    , "times"
};
//______________________________________________________________________________
void TQtFontComboBox::AddItem(int lfont, bool savepadflag)
{
   // Create the image of the fonts using the given font number and the offscreen TPad
   if (savepadflag) {}
   QString seq=QString("%1: -%2-").arg(QString::number(lfont),QString(gFonts[lfont-1]));
   TQtPadFont font;
   font.SetTextFont(lfont*10);
   AddComboItem(font,seq);
}
//______________________________________________________________________________
QSize TQtFontComboBox::sizeHint() const
{
    QSize sz = QComboBox::sizeHint();
    QFontMetrics fm(font());
    sz.setWidth(fm.width(QLatin1Char('m'))*14);
    return sz;
}

//______________________________________________________________________________
void TQtFontComboBox::SetFont(int index) {
   // change the font for the LineEdit control
   QFont font = itemData(index).value<QFont>();
   QLineEdit *e = lineEdit ();
   if (e) e->setFont(font);
}

// Author: Valeri Fine   30/04/2003
/****************************************************************************
** $Id: TQtFloatSpinBox.cxx,v 1.6 2013/08/30 16:00:24 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TQtFloatSpinBox.h" 
#include <qlineedit.h>
#include <qvalidator.h>
#include "TMath.h"

// Other solution to consider yet:
// http://www.billbaxter.com/code/floatspin

#if QT_VERSION < 0x40000
//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( QWidget* parent, const char* name ):
QSpinBox(parent,name)
{  
   setValidator( new QDoubleValidator( MinValue(), MaxValue(), Digit(), this ) );
   SetDigit(1);
   editor()->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(int)), this, SLOT(EmitSignal(int)));
}

//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( int minValue, int maxValue, int step
                                  , QWidget *parent, const char *name):
QSpinBox (minValue,maxValue, step, parent, name), fDigit(0), fBase(1) 
{
   // special ctor for the integers. It should behave as the original QSpinBox 
   // SetDigit(0)
   QLineEdit *e=editor();
   if (e) e->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(int)), this, SLOT(EmitSignal(int)));
}

//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( float initvalue, float minimal, float maximal, int digit,
                         QWidget* parent, const char* name):
QSpinBox(parent,name), fDigit(digit), fBase(1) 
{
   setValidator( new QDoubleValidator( MinValue(), MaxValue(), Digit(), this ) );
   SetDigit(digit);
   SetValue   (initvalue);
   SetMinValue(minimal);
   SetMaxValue(maximal);
   QLineEdit *e=editor();
   if (e) e->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(int)), this, SLOT(EmitSignal(int)));
}
//______________________________________________________________________________
QString TQtFloatSpinBox::mapValueToText( int value )
{
   if (fBase > 1 ) {
   return QString( "%1" ) // 0.0 to 10.0
      .arg( float(value)/fBase,0, 'f', fDigit);
   } else {
      return  QSpinBox::mapValueToText(value);
   }
}
//______________________________________________________________________________
int TQtFloatSpinBox::mapTextToValue( bool *ok )
{
   if (fBase > 1 )
      return (int) ( fBase * text().toFloat() ); // 0 to 100
   else 
      return QSpinBox::mapTextToValue(ok);
}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetDigit(int dig) {
   // Reset the number of the displayed digit
   float oldMinValue = MinValue();
   float oldMaxValue = MaxValue();
   fDigit = dig; fBase = fDigit>0 ? int(TMath::Power(10,fDigit)) : 1 ;
         // reset the mina and max in respect of the new fBase and fDigit
   SetMinValue(oldMinValue);
   SetMaxValue(oldMaxValue);
   QDoubleValidator *valid = (QDoubleValidator *)validator();
   valid->setDecimals(fDigit);
};
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMinValue( float v) 
{ 
   QDoubleValidator *valid = (QDoubleValidator *)validator();
   valid->setBottom(v);
   setMinValue(int(v*fBase)); 
};
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMaxValue( float v) 
{ 
   QDoubleValidator *valid = (QDoubleValidator *)validator();
   valid->setTop(v);
   setMaxValue(int(v*fBase)); 
}
//______________________________________________________________________________
//
//  Property
//______________________________________________________________________________
   
//______________________________________________________________________________
float TQtFloatSpinBox::MinValue() const
{  return float(minValue())/fBase;    }

//______________________________________________________________________________
float	TQtFloatSpinBox::MaxValue() const
{  return float(maxValue())/fBase;    }

//______________________________________________________________________________
QString TQtFloatSpinBox::MinValueText() const
{ return ((TQtFloatSpinBox*)this)->mapValueToText(minValue());}

//______________________________________________________________________________
QString TQtFloatSpinBox::MaxValueText() const
{ return ((TQtFloatSpinBox*)this)->mapValueToText(maxValue());}

//______________________________________________________________________________
QString TQtFloatSpinBox::ValueText()    const
{ return ((TQtFloatSpinBox*)this)->mapValueToText(value());}

//______________________________________________________________________________
int   TQtFloatSpinBox::Digit()    const
{ return fDigit; }

//______________________________________________________________________________
float TQtFloatSpinBox::Value() const
{ return  float(value())/fBase; } 

//______________________________________________________________________________
void  TQtFloatSpinBox::SetValue   ( float v)
{ setValue   (int(v*fBase)); };
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMinValue( const QString &v)
{ SetMinValue(v.toFloat());}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMaxValue( const QString &v)
{ SetMaxValue(v.toFloat());}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetValue   ( const QString &v)
{ SetValue(v.toFloat());   }

#else
//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( QWidget* parent, const char* name ):
QDoubleSpinBox(parent)
{
   setName(name);
   SetDigit(1);
   lineEdit()->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(double)), this, SIGNAL(ValueChanged(double)));
}

//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( int minValue, int maxValue, int step
                                  , QWidget *parent, const char *name):
QDoubleSpinBox(parent)
{
   // special ctor for the integers. It should behave as the original QSpinBox 
   SetDigit(0);
   setRange(minValue, maxValue);
   setSingleStep(step);
   setName(name);
   lineEdit()->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(double)), this, SIGNAL(ValueChanged(double)));
}

//______________________________________________________________________________
TQtFloatSpinBox::TQtFloatSpinBox( float initvalue, float minimal, float maximal, int digit,
                         QWidget* parent, const char* name):
QDoubleSpinBox(parent) 
{
   SetDigit(digit);
   setName(name);
   SetValue   (initvalue);
   SetMinValue(minimal);
   SetMaxValue(maximal);
   lineEdit()->setAlignment(Qt::AlignRight);
   connect(this,SIGNAL(valueChanged(double)), this, SIGNAL(ValueChanged(double)));
}
//______________________________________________________________________________
QString TQtFloatSpinBox::mapValueToText( int )
{ 
   return "";// dummy to preserve the Qt3 interface
}
//______________________________________________________________________________
int TQtFloatSpinBox::mapTextToValue( bool *) {
   return  0; // dummy to preserve the Qt4 interface
}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetDigit(int dig) 
{   setDecimals(dig);           }
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMinValue( float v) 
{ 
   setMinimum(v);
}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMaxValue( float v) 
{ 
   setMaximum(v);
}
//______________________________________________________________________________
//
//  Property
//______________________________________________________________________________
   
//______________________________________________________________________________
float TQtFloatSpinBox::MinValue() const
{  return minimum();    }

//______________________________________________________________________________
float	TQtFloatSpinBox::MaxValue() const
{  return maximum();    }

//______________________________________________________________________________
QString TQtFloatSpinBox::MinValueText() const
{ return textFromValue(minimum());}

//______________________________________________________________________________
QString TQtFloatSpinBox::MaxValueText() const
{ return textFromValue(maximum()); }

//______________________________________________________________________________
QString TQtFloatSpinBox::ValueText()    const
{ return textFromValue(value());}

//______________________________________________________________________________
int   TQtFloatSpinBox::Digit()    const
{ return decimals(); }

//______________________________________________________________________________
float TQtFloatSpinBox::Value() const
{ return  value(); } 

//______________________________________________________________________________
void  TQtFloatSpinBox::SetValue   ( float v)
{ setValue (v); }
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMinValue( const QString &v)
{ setMinimum(v.toFloat());}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetMaxValue( const QString &v)
{ setMaximum(v.toFloat());}
//______________________________________________________________________________
void  TQtFloatSpinBox::SetValue   ( const QString &v)
{ setValue(v.toFloat());   }
#endif
//______________________________________________________________________________
//
//   Custom signals / slots 
//______________________________________________________________________________
void TQtFloatSpinBox::EmitSignal(int) {
   // emit float value changed signa;
   emit ValueChanged((double)Value());
}

#if 0
//______________________________________________________________________________
QSize TQtFloatSpinBox::sizeHint() const
{
    QSpinBox::sizeHint();
}


/*!
    \reimp
*/
//______________________________________________________________________________
QSize TQtFloatSpinBox::QSpinBox::minimumSizeHint() const
{
    QSpinBox::minimumSizeHint();
}
#endif

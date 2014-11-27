#ifndef ROOT_TQtFloatSpinBox
#define ROOT_TQtFloatSpinBox
// Author: Valeri Fine   30/04/2003
/****************************************************************************
** $Id: TQtFloatSpinBox.h,v 1.5 2013/08/30 16:00:21 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include <QtGlobal>

//MOC_SKIP_BEGIN
#include <QDoubleSpinBox>
//MOC_SKIP_END
// Other solution to consider yet:
// http://www.billbaxter.com/code/floatspin

//MOC_SKIP_BEGIN
class  /* Q_EXPORT */ TQtFloatSpinBox: public QDoubleSpinBox
//MOC_SKIP_END
{
    Q_OBJECT
    Q_PROPERTY( int digits READ Digit WRITE SetDigit )
    Q_PROPERTY( QString minValue READ MinValueText WRITE SetMinValue )
    Q_PROPERTY( QString maxValue READ MaxValueText WRITE SetMaxValue )
    Q_PROPERTY( QString Value    READ ValueText    WRITE SetValue )
protected:
   virtual QString mapValueToText( int value );
   virtual int		 mapTextToValue( bool* ok );

   int     fDigit; // number of decimal points to be represented
   int     fBase;  // fBase is 10 ** fDigit 
public:
   TQtFloatSpinBox ( QWidget* parent=0, const char* name=0 );
   TQtFloatSpinBox ( int minValue, int maxValue, int step = 1
      , QWidget *parent = 0, const char *name = 0);
   TQtFloatSpinBox ( float initvalue, float minimal, float maximal, int digit = 1
      , QWidget* parent=0, const char* name=0);
   ~TQtFloatSpinBox (){;}
   
//  Property
   
   float	MinValue() const;
   float	MaxValue() const;
   QString MinValueText() const;
   QString MaxValueText() const;
   QString ValueText()    const;
   int   Digit()    const;
   
   float Value() const;

public slots:
   virtual void  SetDigit(int dig);

   void  SetMinValue( float v);
   void  SetMaxValue( float v);
   void  SetValue   ( float v);
   
   void  SetMinValue( const QString &v);
   void  SetMaxValue( const QString &v);
   void  SetValue   ( const QString &v);

protected slots:
   void EmitSignal(int); 
signals:
   void ValueChanged(double);

};
#undef  ROOT_FLOATSPIN

#endif // ROOT_TQtFloatSpinBox

#ifndef ROOT_TQtFloatSlider
#define ROOT_TQtFloatSlider
// Author: Valeri Fine   3/02/2007
/****************************************************************************
** $Id: TQtFloatSlider.h,v 1.4 2013/08/30 16:00:21 perev Exp $
**
** Copyright (C) 2007 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include <QtGlobal>

//MOC_SKIP_BEGIN
#  include <QSlider>
//MOC_SKIP_END

//MOC_SKIP_BEGIN
class  /* Q_EXPORT */ TQtFloatSlider: public QSlider
//MOC_SKIP_END
{
    Q_OBJECT
    Q_PROPERTY( double minValue     READ MinValue     WRITE SetMinValue     )
    Q_PROPERTY( double maxValue     READ MaxValue     WRITE SetMinValue     )
    Q_PROPERTY( double value        READ Value        WRITE SetValue        )
    Q_PROPERTY( double lineStep     READ LineStep     WRITE SetLineStep     )
    Q_PROPERTY( double pageStep     READ PageStep     WRITE SetPageStep     )
    Q_PROPERTY( double tickInterval READ TickInterval WRITE SetTickInterval )
protected:
   double  fMinValue; // current min value
   double  fMaxValue; // current min value

   void ConnectSingals();
public:
   enum {kFloatSliderPrecision = 10000};
   TQtFloatSlider ( QWidget* parent=0, const char* name=0 );
   TQtFloatSlider ( Qt::Orientation orientation, QWidget * parent, const char * name = 0 );
   TQtFloatSlider ( int minValue, int maxValue, int pageStep, int value, Qt::Orientation orientation, QWidget * parent, const char * name = 0 );
   TQtFloatSlider ( double minValue, double maxValue, double pageStep, double value, Qt::Orientation orientation, QWidget * parent, const char * name = 0 );
   ~TQtFloatSlider (){;}


// Conversion between the float and base QSlider values   

   int     ToInt(double value) const;
   double  ToDouble(int value) const;
   double  Range()             const;

//  Property
   
   virtual void SetTickInterval(double);
   double TickInterval () const;
   double MinValue() const;
   double MaxValue() const;
   double LineStep() const;
   double PageStep() const;
   void   SetLineStep(double);
   void   SetPageStep(double);
   void   SetRange(double,double);
   
   double Value() const;

public slots:
   void  SetMinValue( double v);
   void  SetMaxValue( double v);
   void  SetValue   ( double v);   

protected slots:
   void EmitValueSignal(int); 
   void EmitMoveSignal(int); 
signals:
   void ValueChanged(double);
   void SliderMoved(double);

};
//____________________________________________________
inline  double  TQtFloatSlider::Range() const 
{ return fMaxValue-fMinValue; }
//____________________________________________________
inline  int TQtFloatSlider::ToInt(double value) const
{
   return (int)((value-fMinValue)/Range())*kFloatSliderPrecision;
}
//____________________________________________________
inline double  TQtFloatSlider::ToDouble(int value) const
{
   return value*Range()/kFloatSliderPrecision + fMinValue;
}

#undef  ROOT_FLOATSLIDER

#endif // ROOT_TQtFloatSpinBox

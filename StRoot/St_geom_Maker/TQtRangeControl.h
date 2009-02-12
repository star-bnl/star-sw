#ifndef STAR_TQtRangeControl_
#define STAR_TQtRangeControl_

#include <QWidget>
#include "ui_StGeomDepthControl.h"

class TQtRangeControl : public QWidget, private Ui::DepthControl
{
   Q_OBJECT
   protected:
         void Connect();
   public:
      TQtRangeControl(QWidget *parent=0);
      virtual ~TQtRangeControl();
      int Value() const;

   protected slots:
      void AdjustSlider(int);
      void AdjustSpinBox(int);

   public slots:
     void  SetMinValue( int v);
     void  SetMaxValue( int v);
     void  SetValue   ( int v);

  signals:
     void ValueChanged(int);
};

#endif

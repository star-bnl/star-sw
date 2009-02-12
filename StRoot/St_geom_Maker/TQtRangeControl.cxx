#include "TQtRangeControl.h"
#include "ui_StGeomDepthControl.h"
#include <QString>

using namespace Ui;

/// Construct the twin (slider / spin box ) depth control
//______________________________________________________________
TQtRangeControl::TQtRangeControl(QWidget *parent)
      : QWidget(parent)
{
   QHBoxLayout *l = new QHBoxLayout(this);
   setContentsMargins(0,0,0,0);
   setupUi(this);
   l->addWidget(widget); 

   SetMinValue(1);

   SetMaxValue(9);

   Connect();
   SetValue(3);
}
/// Connect signals and slots
//______________________________________________________________
void TQtRangeControl::Connect()
{
  connect(horizontalSlider,SIGNAL(valueChanged(int)),
        this,SLOT(AdjustSpinBox(int)));
  
  connect(doubleSpinBox,SIGNAL(valueChanged(int)),
        this,SLOT(AdjustSlider(int)));

  connect(horizontalSlider,SIGNAL(valueChanged(int)),
        this,SIGNAL(ValueChanged(int)));
  connect(doubleSpinBox,SIGNAL(valueChanged(int)),
        this,SIGNAL(ValueChanged(int)));  
}

//______________________________________________________________
TQtRangeControl::~TQtRangeControl(){}
/// Adjust the slider widget within emitting the signal
//______________________________________________________________
void TQtRangeControl::AdjustSlider(int value)
{
   horizontalSlider->blockSignals(true);
   horizontalSlider->setValue(value);
   horizontalSlider->blockSignals(false);
}
/// Adjust the spinbox widget within emitting the signal
//______________________________________________________________
void TQtRangeControl::AdjustSpinBox(int value)
{
   doubleSpinBox->blockSignals(true);
   doubleSpinBox->setValue(value);
   doubleSpinBox->blockSignals(false);
}

/// [SLOT] to set the minimum value
//______________________________________________________________
void  TQtRangeControl::SetMinValue( int v)
{
   horizontalSlider->setMinValue(v);
   doubleSpinBox->setMinValue(v);
}

/// [SLOT] to set the maximum value 
//______________________________________________________________
void  TQtRangeControl::SetMaxValue( int v)
{
   horizontalSlider->setMaxValue(v);
   doubleSpinBox->setMaxValue(v);
}

/// [SLOT] to set the current value
//______________________________________________________________
    void  TQtRangeControl::SetValue   (int v)
{
   horizontalSlider->setValue(v);
}
/// Value() returns the current value of the control
//______________________________________________________________
int TQtRangeControl::Value() const
{
   return horizontalSlider->value();
}


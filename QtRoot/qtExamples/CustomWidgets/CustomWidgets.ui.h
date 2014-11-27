/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/
void RootCustomWidget::init()
{
  tQtFloatSlider1->setOrientation(Qt::Horizontal);
//  tQtFloatSpinBox1->SetMinValue(-34.4);
//  tQtFloatSpinBox1->SetMinValue(+46.4);
  tQtFloatSlider1->SetRange( tQtFloatSpinBox1->MinValue()
                            ,tQtFloatSpinBox1->MaxValue());
  tQtFloatSlider1->SetPageStep(tQtFloatSlider1->Range()/10);
  tQtFloatSlider1->SetLineStep(tQtFloatSlider1->Range()/100);
  connect(tQtFloatSlider1,SIGNAL(ValueChanged(double)),this, SLOT(SliderValue(double)));
}

void RootCustomWidget::SliderValue(double v)
{
    //
   QRect slRect = tQtFloatSlider1->sliderRect ();
   tQtFloatSpinBox1->SetValue(v);
   //QWhatsThis::display(QString::number(v)
   // ,tQtFloatSlider1->mapToGlobal(QPoint(slRect.x(),slRect.y()+slRect.height())));
}


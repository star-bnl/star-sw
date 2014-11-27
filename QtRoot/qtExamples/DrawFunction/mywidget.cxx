//author Stefano Carrazza 12/03/09

#include <QtGui>
#include <QString>
#include <QAbstractButton>
#include "mywidget.h"
#include "ui_mywidget.h"
#include "TStyle.h"
#include "TF2.h"
#include "TQtFunViewer.h"
#include <time.h>

MyWidget::MyWidget(QWidget *parent) :
    QWidget(parent)
    , m_ui(new Ui::MyWidget)
    , fViewColorBottom (0)
    , fViewColorTop(0),fFun2(0)
{
  m_ui->setupUi(this);
// Add the default function
  m_ui->comboBox->addItem("0.4*x*sin(0.3*x)*y*sin(0.3*y)");
  connect( m_ui->comboBox->lineEdit(),SIGNAL(returnPressed())
        , this,SLOT(on_buttonBox_accepted()));
  Qt::WindowFlags flags;
  flags = Qt::Window | Qt::WindowMinimizeButtonHint;
  setWindowFlags( flags );
  setAttribute(Qt::WA_DeleteOnClose);

  //center mainwindow position on desktop
  QDesktopWidget *desktop = QApplication::desktop();

  int screenWidth, width;
  int screenHeight, height;
  int x, y;
  QSize windowSize;

  screenWidth = desktop->width(); // get width of screen
  screenHeight = desktop->height(); // get height of screen

  windowSize = size(); // size of our application window
  width = windowSize.width();
  height = windowSize.height();

  // little computations
  x = (screenWidth - width) / 2;
  y = (screenHeight - height) / 2;
  y -= 50;

  // move window to desired coordinates
  move ( x, y );
  srand(time(NULL));
}

MyWidget::~MyWidget()
{
    qApp->closeAllWindows();
    // delete fFun2;
    delete m_ui;
}

void MyWidget::changeEvent(QEvent *e)
{
    switch (e->type()) {
    case QEvent::LanguageChange:
        m_ui->retranslateUi(this);
        break;
    default:
        break;
    }
}
void MyWidget::on_buttonBox_clicked(QAbstractButton * button)
{
   if (button == m_ui->buttonBox->button(QDialogButtonBox::Reset)) {
     if (fViewColorTop)    fViewColorTop->Reset();
     if (fViewColorBottom) fViewColorBottom->Reset();
     fLastRange = QString();fLastFunction = QString();
   }
}
void MyWidget::on_buttonBox_rejected()
{
    qApp->closeAllWindows();
}

void MyWidget::on_buttonBox_accepted()
{
   QLineEdit *lineEdit =  m_ui->comboBox->lineEdit ();
   QString newFunction = lineEdit->text();
   QString txRange =  m_ui->doubleSpinBox->text() 
                     +m_ui->doubleSpinBox_6->text()
                     +m_ui->doubleSpinBox_2->text()
                     +m_ui->doubleSpinBox_5->text();
   if (newFunction.isEmpty() || ( (txRange == fLastRange) && (newFunction ==fLastFunction)) ) return;
   fLastRange = txRange;
   gStyle->SetPalette(1);
//   delete fFun2; fFun2 = 0;
//   TQtFunViewer owns fFun2
   double xmin = m_ui->doubleSpinBox->value();
   double xmax = m_ui->doubleSpinBox_6->value();
   double ymin = m_ui->doubleSpinBox_2->value();
   double ymax = m_ui->doubleSpinBox_5->value();
   TF2 *f =0;
   if ( fLastFunction != newFunction) {
       fLastFunction = newFunction;
       f = fFun2 = new TF2("f",newFunction.toStdString().c_str()
         ,xmin,xmax,ymin,ymax);
       f->SetNpx(60);
       f->SetNpy(60);
       f->SetFillColor(rand()%100);
   }
//   f->Draw("colz");
   if (!fViewColorBottom) fViewColorBottom = new TQtFunViewer(f);
   else                   fViewColorBottom->SetFun(f,xmin,xmax,ymin,ymax);
   QString tip = QString(tr("Colored bottom of the <br><center><b>%1</b> function")).arg(lineEdit->text());
   fViewColorBottom->setToolTip(tip);
   if (fViewColorBottom->isVisible ()) fViewColorBottom->update();
   else                                fViewColorBottom->show();

   if(!fViewColorTop){
      fViewColorTop  = new TQtFunViewer(f);
   } else {
      fViewColorTop->SetFun(f,xmin,xmax,ymin,ymax);
   }
   fViewColorTop->SetTop();
   fViewColorTop->SetColZ();

   tip = QString(tr("Multi Colored top of the <br><center><b>%1</b> function")).arg(lineEdit->text());
   fViewColorTop->setToolTip(tip);
   if (fViewColorTop->isVisible ())fViewColorTop->update();
   else                            fViewColorTop->show();
}

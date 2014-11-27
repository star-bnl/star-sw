#ifndef QTROOTEXAMPLES_HelloSignal
#define QTROOTEXAMPLES_HelloSignal

#include "TString.h"
#ifdef __CINT__
# define  slots
  class  QObject;
#else
# include <QObject>
#endif

class QWidget;
class QLineEdit;
class QLabel;
namespace Ui{
  class HelloSignal;
}

class HelloSignal : public QObject {
#ifndef __CINT__
   Q_OBJECT
#endif
  private:
      QWidget *fGuiWidget;
      QLineEdit  *fLineEdit;
      QLabel     *fLabel;
      TString     fLastString;
#ifndef __CINT__
      Ui::HelloSignal *fUi;
#endif
      HelloSignal(const HelloSignal&);
      void operator=(const HelloSignal&);
  public:
      HelloSignal(QWidget *parent=0);
      virtual  ~HelloSignal();
      const TString &Text() const;
public slots:     
      void Show();
      void Hide();
      void NewRootCommand();
      void SetPrompt(int indx);
};
extern  HelloSignal *gHelloSignal;

#endif

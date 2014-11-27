#ifndef HelloClick_H
#define HelloClick_H

#include  <qframe.h>

class TObject;
class TCanvas;
class TQtWidget;

class HelloClick : public QFrame
{
    Q_OBJECT
private:
   TQtWidget *fDynamicSliceWidget;
   TQtWidget *fDynamicExecWidget;

public:
    HelloClick(QWidget* parent = 0);
    virtual ~HelloClick(){;}

public slots:
    virtual void init();

protected:
     void DynamicSlice();
     void setupUi(QFrame *f);

public slots:
    void DynamicExec(TObject *select, unsigned int event, TCanvas *);
};

#endif // HelloClick_H

//author Stefano Carrazza 12/03/09

#ifndef MYWIDGET_H
#define MYWIDGET_H

#include <QtGui/QWidget>

class TQtFunViewer;
class TF2;
class QAbstractButton;

namespace Ui {
    class MyWidget;
}

class MyWidget : public QWidget {
    Q_OBJECT
    Q_DISABLE_COPY(MyWidget)
public:
    explicit MyWidget(QWidget *parent = 0);
    virtual ~MyWidget();

protected:
    virtual void changeEvent(QEvent *e);

private:
    Ui::MyWidget *m_ui;
    TQtFunViewer *fViewColorBottom;
    TQtFunViewer *fViewColorTop;
    TF2          *fFun2;
    QString       fLastFunction;
    QString       fLastRange;

private slots:
    void on_buttonBox_accepted();
    void on_buttonBox_rejected();
    void on_buttonBox_clicked(QAbstractButton * button);
};

#endif // MYWIDGET_H

#ifndef QTROOTEXAMPLE1_H
#define QTROOTEXAMPLE1_H

#include <ui_ex1.h>

class TObject;
class TFile;
class TCanvas;
class QListViewItem;
class QPoint;

namespace Ui {
    class qtrootexample1: public Ui_ex1 {};
} // namespace Ui

class qtrootexample1 : public QWidget, public Ui::qtrootexample1
{
    Q_OBJECT

public:
    qtrootexample1(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = 0);
    virtual ~qtrootexample1();

public slots:
    virtual void init();
    virtual void destroy();
    virtual void ListView_Clicked(QListWidgetItem * item);
    virtual void AddItemToListView( TObject * Key );
    virtual void execRoot();
    virtual void widget_destroyed( QObject * );
    virtual void CanvasEvent( TObject * obj, unsigned int event, TCanvas * canvas );
protected:
    TFile *fxDiskFile;
    TQtWidget *currentWidget;

protected slots:
    virtual void languageChange();

};

#endif // QTROOTEXAMPLE1_H

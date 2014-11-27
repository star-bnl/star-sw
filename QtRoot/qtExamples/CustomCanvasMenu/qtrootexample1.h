#ifndef STAR_QTROOTEXAMPLEMENU
#define STAR_QTROOTEXAMPLEMENU

#include "ui_qtrootexample1.h"
#include <QWidget>

class Q3ListViewItem;
class QPoint;
class TObject;
class TCanvas;
class QMenu;
class TContextMenu;
class TQtWidget;
class TFile;

class qtrootexample1 : public QWidget, public Ui_qtrootexample1
{
   Q_OBJECT
  private:
         TQtWidget *currentWidget;
         TFile     *fxDiskFile;
  protected:
       void init();
  public:
       qtrootexample1(QWidget *parent = 0);
  public slots:
      void ListView1_mouseButtonPressed( int, Q3ListViewItem *SelectedItem, const QPoint &, int );
      void AddItemToListView1(TObject *Key);
      void execRoot();
      void CanvasEvent(TObject *obj, unsigned int event, TCanvas *);
      void CustomizeIt (QMenu *contextMenu, TContextMenu *rootContextMenu) const;
};
#endif

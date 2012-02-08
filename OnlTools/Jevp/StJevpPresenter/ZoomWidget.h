#ifndef _ZOOM_WIDGET_H_
#define _ZOOM_WIDGET_H_

#include <qdialog.h>
#include <TQtWidget.h>
#include "JevpGui.h"
#include <qgroupbox.h>
#include <qabstractbutton.h>


class ZoomWidget;
class QLabel;
class Q3PopupMenu;

class ZPlotDisplay : public QFrame
{
  Q_OBJECT

public:

  int psizex;
  int psizey;

  JevpPlot *plot;
  QLabel *comment;
  int refid;   // 0 for curr, 1 for ref, 100+refid for others...
  ZoomWidget *zoomwidget;
  TQtWidget *myrootwidget;
  Q3PopupMenu *pop;

  virtual ~ZPlotDisplay();

  ZPlotDisplay(ZoomWidget *zoomWidget, JevpPlot *plot);
 
  void CrossOfDeath(TCanvas *canvas);
  void mousePressEvent(QMouseEvent *e);
  void dragEnterEvent(QDragEnterEvent *event);
  void dropEvent(QDropEvent *event);

  void replaceData(JevpPlot *plot);
  void invalidate();
  int menu_refid() { return (refid<100) ? refid + 100 : refid; };
  int true_refid() { return (refid<100) ? refid : refid - 100; };

  void zoomin();
  void zoomout();
  void setnewsize();
  void resetsize();

 signals:
  void selected(QWidget *widget);

 public slots:
  void gotclick();
};

class ZoomWidget : public QDialog
{
  Q_OBJECT

 public:
  ZoomWidget(JevpGui *logic, char *name);
  
  void dragEnterEvent(QDragEnterEvent *event);
  void mousePressEvent(QMouseEvent *e);

  JevpGui *logic;
  char *histoname;

  ZPlotDisplay *zplot;

  
 private:

  QHBoxLayout *allRefLayout;
  QGroupBox *allRefBox;
  
 public slots:
  void buttonPressed(QAbstractButton *button);
   // void accept() { exit(0); };
  
  
};

#endif

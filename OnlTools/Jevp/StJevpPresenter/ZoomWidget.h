#ifndef _ZOOM_WIDGET_H_
#define _ZOOM_WIDGET_H_

#include <qdialog.h>
#include "JevpGui.h"
#include <qgroupbox.h>
#include <qabstractbutton.h>
#include "RootWidget.h"

class ZoomWidget;
class QLabel;
class Q3PopupMenu;

// The root widget will be used to set the size
// it has a QVBoxLayout, so that the internal display widget can automatically resize
//
/* class RootWidget : public QWidget { */
/*     Q_OBJECT */

/*  private: */
/*     QWidget *displayWidget; */
/*     ULong_t wid; */
/*     TCanvas *canvas; */
/*     std::string *name; */
    
/*  public: */
/*     RootWidget(char*, QWidget *); */
/*     TCanvas *getCanvas(); */

/*     void resizeEvent(QResizeEvent *); */
/*     void paintEvent(QPaintEvent *); */
/* }; */


class ZPlotDisplay : public QFrame
{
  Q_OBJECT

public:

  int psizex;
  int psizey;

  JevpPlot *plot;
  QLabel *comment;
  ZoomWidget *parent;
  RootWidget *myrootwidget;
  Q3PopupMenu *pop;

  virtual ~ZPlotDisplay();

  ZPlotDisplay(ZoomWidget *parent, JevpPlot *plot);
 
  void CrossOfDeath(TCanvas *canvas);
  void mousePressEvent(QMouseEvent *e);
  void dragEnterEvent(QDragEnterEvent *event);
  void dropEvent(QDropEvent *event);

  void replaceData(JevpPlot *plot);
  void invalidate();

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

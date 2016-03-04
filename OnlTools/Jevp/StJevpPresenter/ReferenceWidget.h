#ifndef _REFERENCE_WIDGET_H_
#define _REFERENCE_WIDGET_H_

#include <qdialog.h>
#include <RootWidget.h>
#include "JevpGui.h"
#include <qgroupbox.h>


class ReferenceWidget;
class QLabel;
class Q3PopupMenu;

class PlotDisplay : public QFrame
{
  Q_OBJECT

public:
  PlotDisplay *next;
  PlotDisplay *prev;

  JevpPlot *plot;
  QLabel *comment;
  int refid;   // 0 for curr, 1 for ref, 100+refid for others...
  ReferenceWidget *refwidget;
  RootWidget *myrootwidget;
  Q3PopupMenu *pop;

  virtual ~PlotDisplay();

  PlotDisplay(ReferenceWidget *refWidget, JevpPlot *plot, int refid, int type);
 
  void CrossOfDeath(TCanvas *canvas);
  void mousePressEvent(QMouseEvent *e);
  void dragEnterEvent(QDragEnterEvent *event);
  void dropEvent(QDropEvent *event);

  void replaceData(JevpPlot *plot);
  void invalidate();
  int menu_refid() { return (refid<100) ? refid + 100 : refid; };
  int true_refid() { return (refid<100) ? refid : refid - 100; };

 signals:
  void selected(QWidget *widget);

 public slots:
  void gotclick();
};

class ReferenceWidget : public QDialog
{
  Q_OBJECT

 public:
  ReferenceWidget(JevpGui *logic, char *name);
  
  void dragEnterEvent(QDragEnterEvent *event);
  void mousePressEvent(QMouseEvent *e);
  void addPlotToMenu(PlotDisplay *display, JevpPlot *plot);
  
  void removePlot(PlotDisplay *pd);
  JevpGui *logic;
  char *histoname;

  PlotDisplay *firstPlot;
  PlotDisplay *lastPlot;  // the PlotDisplays for all refs...
  PlotDisplay *mainRefPlot;
  
 private:

  QHBoxLayout *allRefLayout;
  QGroupBox *allRefBox;
  
 public slots:
   // void accept() { exit(0); };
  
  
};

#endif

#ifndef PresenterConnect_h
#define PresenterConnect_h

#if QT_VERSION < 0x40000
#  include <qwidget.h>
#else /* QT4 */
#  include <QObject>
#endif /* QT4 */

class ServerStatus;
class PresenterGui;
class EvpPresenter;
class TCanvas;

class PresenterConnect : public QObject {

  Q_OBJECT

 private:
  PresenterGui* mGui;
  EvpPresenter* mPresenter;

  int mTab;
  int mSubTab;
  TCanvas* mCanvas;
 public:

  PresenterConnect(PresenterGui* gui, EvpPresenter* pre);
  

 public slots:
 void save();
 void saveAs();
 void live();
 void file();
 void update();
 void update(TCanvas* cc, int tab, int subTab);
 void update(TCanvas*, const char*);
 void print();
 void setTab(int);
 void setSubTab(int);
 void setCanvas(TCanvas*);
 void openReference();
 
 signals:
 void signalEventInfo(int, int, int, int, unsigned int, unsigned int, unsigned int, unsigned int);
 void signalServerInfo(ServerStatus*);
 void updateRequest();
};


#endif

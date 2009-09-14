#ifndef STAR_STQTDELAYREDRAWTIMER
#define STAR_STQTDELAYREDRAWTIMER

#include <QtCore/QTimer>
class TObject;

class StQtDelayRedrawTimer : public QTimer {
   private:
     TObject *fObj;
     bool   fExpanded;
   public:
      StQtDelayRedrawTimer(QObject *parent=0);
      virtual ~StQtDelayRedrawTimer() {}
   public slots:
      void DrawObject(int delay, TObject *obj,bool expanded);
      void DrawObject();
   signals:
      void DrawObjectSignal(TObject *obj,bool expanded);
};
#endif

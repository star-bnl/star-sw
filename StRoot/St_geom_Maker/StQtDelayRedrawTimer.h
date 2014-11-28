// Author: Valeri Fine   15/09/2009
// ****************************************************************************
// ** $Id: StQtDelayRedrawTimer.h,v 1.2 2009/09/17 17:40:49 fine Exp $
#ifndef STAR_STQTDELAYREDRAWTIMER
#define STAR_STQTDELAYREDRAWTIMER

#include <QtCore/QObject>
class TObject;
class QTimer;

class StQtDelayRedrawTimer : public QObject {
   Q_OBJECT
   private:
     QTimer  *fComplexVolumeTimer;
     QTimer  *fSingeVolumeTimer;
     TObject *fObj;
   public:
      StQtDelayRedrawTimer(QObject *parent=0);
      virtual ~StQtDelayRedrawTimer() {}
      virtual bool IsActive(bool expanded) const;
   public slots:
      void DrawObject(TObject *obj,bool expanded,int depth=3);
      void DrawObject();
   signals:
      void DrawObjectSignal(TObject *obj,bool expanded);
};
#endif

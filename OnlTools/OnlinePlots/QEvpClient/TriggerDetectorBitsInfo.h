#ifndef TriggerDetectorBitsInfo_h
#define TriggerDetectorBitsInfo_h

#if QT_VERSION < 0x40000
#  include "TriggerDetectorBitsInfoUi.h"
#else
#  include "ui_TriggerDetectorBitsInfoUi.h"
   typedef Ui_TriggerDetectorBitsInfoUi TriggerDetectorBitsInfoUi;
#endif

using namespace std;


class TriggerDetectorBitsInfo :
#if QT_VERSION > 0x40000
      public QWidget, 
#endif 
      public TriggerDetectorBitsInfoUi {
 public:
  TriggerDetectorBitsInfo( QWidget* parent = 0, const char* name = 0, Qt::WFlags fl = 0 ) :
#if QT_VERSION > 0x40000
       QWidget(parent, fl) {if (name) setName(name); setupUi(this); }
#else
       TriggerDetectorBitsInfoUi(parent, name, fl) { /* no-op */}
#endif
  public slots:

  virtual void setTriggerBitsRun(unsigned int v) { RunTrig->setText( utoa(v) ) ;}
  virtual void setTriggerBits(unsigned int v) { EventTrig->setText( utoa(v) ) ;}
  virtual void setDetectorBitsRun(unsigned int v) { RunDet->setText( utoa(v) );}
  virtual void setDetectorBits(unsigned int v) { EventDet->setText( utoa(v) );}

 private:
    QString utoa(unsigned int t) {
      QString  bitstring;
      int i = sizeof(t)-1;
      for (;i>=0;i--) {
         bitstring += QString::number((t&0xFF),2);
         bitstring +=  " ";
         t>>=8;
      }
      return bitstring;
    }
};


#endif

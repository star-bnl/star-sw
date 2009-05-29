#ifndef EventInfo_h
#define EventInfo_h

#include "qobject.h"
#if QT_VERSION < 0x40000
#  include "EventInfoUi.h"
#else
#  include "ui_EventInfoUi.h"
#endif

#include "qfont.h"
#include <bitset>
#include <sstream>
#if QT_VERSION >= 0x40000
#  include <QWidget>
#endif /* QT4 */

using namespace std;

class EventInfo :
#if QT_VERSION < 0x40000
      public EventInfoUi
#else
      public QWidget, public Ui_EventInfoUi
#endif
{
 public:
#if QT_VERSION < 0x40000
  EventInfo( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 ) : EventInfoUi(parent, name, fl) { /* no-op */}
#else /* QT4 */
  EventInfo( QWidget* parent = 0, const char* name = 0, Qt::WFlags fl = 0 ) 
       : QWidget(parent, fl){if (name) setName(name); setupUi(this); }
#endif /* QT4 */

  public slots:
    void setRunNumber(const char* t) { run->setText(t); }
#if QT_VERSION < 0x40000
    void setEventNumber(const char* t) { event->setText(t); }
#else /* QT4 */
    void setEventNumber(const char* t) { Ui_EventInfoUi::event->setText(t); }
#endif /* QT4 */
    void setEventCounter(const char* t) { count->setText(t); }
    void setTokenNumber(const char* t) { token->setText(t); }
    void setTriggerBits(const char* t) { triggerBits->setText(t); }
    void setDetectorBits(const char* t) { detectorBits->setText(t); }
    void setTriggerBitsRun(const char* t) { triggerBitsRun->setText(t); }
    void setDetectorBitsRun(const char* t) { detectorBitsRun->setText(t); }


    void setRunNumber(int t) { run->setText( itoa(t) ); }
#if QT_VERSION < 0x40000
    void setEventNumber(int t) { event->setText( itoa(t) ); }
#else /* QT4 */
    void setEventNumber(int t) { Ui_EventInfoUi::event->setText( itoa(t) ); }
#endif /* QT4 */
    void setEventCounter(int t) { count->setText( itoa(t) ); }
    void setTokenNumber(int t) { token->setText( itoa(t) ); }
    void setTriggerBits(unsigned int t) { triggerBits->setText( xtoa(t) ); }
    void setDetectorBits(unsigned int t) { detectorBits->setText( xtoa(t) ) ; }
    void setTriggerBitsRun(unsigned int t) { triggerBitsRun->setText( xtoa(t) ); }
    void setDetectorBitsRun(unsigned int t) { detectorBitsRun->setText( xtoa(t) ); }


 private:
    char txt[1024];
    char* itoa(int t) { sprintf(txt,"%d",t); return txt; }
    char* xtoa(unsigned int t) { sprintf(txt,"%x",t); return txt; }
    string utoa(unsigned int t) {
      ostringstream os;
      os << bitset<8*sizeof(t)>(t);
      return os.str();
    } 
};


#endif

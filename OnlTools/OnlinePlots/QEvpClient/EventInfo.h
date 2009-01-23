#ifndef EventInfo_h
#define EventInfo_h


#include "EventInfoUi.h"
#include "qfont.h"
#include <bitset>
#include <sstream>

using namespace std;


class EventInfo : public EventInfoUi {
 public:
  EventInfo( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 ) : EventInfoUi(parent, name, fl) { /* no-op */}

  public slots:
    void setRunNumber(const char* t) { run->setText(t); }
    void setEventNumber(const char* t) { event->setText(t); }
    void setEventCounter(const char* t) { count->setText(t); }
    void setTokenNumber(const char* t) { token->setText(t); }
    void setTriggerBits(const char* t) { triggerBits->setText(t); }
    void setDetectorBits(const char* t) { detectorBits->setText(t); }
    void setTriggerBitsRun(const char* t) { triggerBitsRun->setText(t); }
    void setDetectorBitsRun(const char* t) { detectorBitsRun->setText(t); }


    void setRunNumber(int t) { run->setText( itoa(t) ); }
    void setEventNumber(int t) { event->setText( itoa(t) ); }
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

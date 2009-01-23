#ifndef TriggerDetectorBitsInfo_h
#define TriggerDetectorBitsInfo_h


#include "TriggerDetectorBitsInfoUi.h"
#include "qfont.h"
#include <bitset>
#include <sstream>

using namespace std;


class TriggerDetectorBitsInfo : public TriggerDetectorBitsInfoUi {
 public:
  TriggerDetectorBitsInfo( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 ) : TriggerDetectorBitsInfoUi(parent, name, fl) { /* no-op */}

  public slots:

  virtual void setTriggerBitsRun(unsigned int v) { RunTrig->setText( utoa(v) ) ;}
  virtual void setTriggerBits(unsigned int v) { EventTrig->setText( utoa(v) ) ;}
  virtual void setDetectorBitsRun(unsigned int v) { RunDet->setText( utoa(v) );}
  virtual void setDetectorBits(unsigned int v) { EventDet->setText( utoa(v) );}

 private:
    string utoa(unsigned int t) {
      ostringstream os;
      bitset<8> bit;
      unsigned char*  p = (unsigned char*)&t;
      for (int i=sizeof(t)-1; i>=0; i--) {
	bit = *(p+i); 
	os << bit << " ";
      } 
      return os.str();
    }
};
    

#endif

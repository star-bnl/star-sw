#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include <daqFormats.h>
#include "gl3Event.h"
#include "l3Coordinates.h"
#include "l3EmcCalibration.h"
#include "gl3LMVertexFinder.h"
#include "FtfSl3.h"
#include "sizes.h"
#include "DAQ_L3/daq_l3.h"

// bField is a pain in the neck.
//
// Starting FY2005, we have bField in data file, but
// it is not 100% reliable, as we depend on RHIC feed & STAR db
// which have failed in past.
//
// The I use are:
//
//    If you use the EventTracker(float bField), constructor,
//    you override all intellegence.  The specified bField is used
//
//    If you use the EventTracker() constructor, the bField from the
//    file is used.  If the bField is not available from the file, I
//    use "defaultBField" which starts out .5, but can be set
//    using the setDefaultBField() function.

class EventTracker {
public:


  EventTracker()
  {
    bField = 1000;
    constructor_helper();
  }

  EventTracker(float magField) 
  { 
    bField = magField;
    constructor_helper();
  };

  void constructor_helper()
  {
    defaultBField = .5;
    transformer = new l3CoordinateTransformer();
    //    bemcCalib = new l3EmcCalibration(4800);
    //    eemcCalib = new l3EmcCalibration(720);
    lmv = new gl3LMVertexFinder();
    //     gl3 = new gl3Event(transformer,
    // 		       bemcCalib,
    // 		       eemcCalib,
    // 		       500000,
    // 		       20000);

    lmv->setParameters(10, 3, 250.0, 10.0, 3.0, 3.0);
    


    gl3 = new gl3Event(transformer,
		       NULL,
		       NULL);
    
    gl3->setLMVertexFinder(lmv);
    gl3->setVertexFinderMethod(3);

    //    tracker = new FtfSl3(transformer);
  }

  void setDefaultBField(float def)
  {
    defaultBField = def;
  }

  ~EventTracker()
  {
    // delete tracker;
    delete gl3;
    delete lmv;
    //    delete eemcCalib;
    //    delete bemcCalib;
    delete transformer;
  }


  // Does tracking and fills gtd given datap.
  //
  // suggested max_size = 0x800000
  //

#ifdef OLD_DAQ_READER
  int trackEvent(evpReader *evp, char *mem, L3_P *l3p, int max_size);
  int trackTPC(evpReader *evp, char *mem, L3_GTD *gtd, int max_size);
#else /* OLD_DAQ_READER */
  int trackEvent(daqReader *daq, char *mem, L3_P *l3p, int max_size);
  int trackTPC(daqReader *daq, char *mem, L3_GTD *gtd, int max_size);
#endif /* OLD_DAQ_READER */
    
#ifdef OBSOLETE
  int trackEvent(DATAP *datap, L3_P *l3p, int max_size);
  int trackTPC(DATAP *datap, L3_GTD *gtd, int max_size);
#endif

  void dumpGTD(L3_GTD *gtd);
  gl3Event *getL3Event() const { return gl3;}

  int copyl3_t(l3_t &l3, L3_P *l3p);

private:

  //int init();              // load the config files...automatic on first trackEvent() call
  int gl3Event_to_GTD(L3_GTD *gtd, u_int max_size);

  //int initialized;

  l3CoordinateTransformer *transformer;

  float bField;

  float defaultBField;
  int bFieldOverride;

  //  l3EmcCalibration *bemcCalib;
  //  l3EmcCalibration *eemcCalib;
  gl3LMVertexFinder *lmv;
  gl3Event *gl3;
  //FtfSl3 *tracker;
};

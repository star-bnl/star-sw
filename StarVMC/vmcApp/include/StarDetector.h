#ifndef STARDETECTOR_H
#define STARDETECTOR_H

/* $Id: StarDetector.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include "StarModule.h"

class StarGeometry;

// 
class StarDetector: public StarModule {

 public:
  StarDetector() {}
  StarDetector(const char* name_, const char *title_) {}
  StarDetector(const StarDetector &det_) {}
  virtual ~StarDetector() {}


  
 protected:

  // Probably won't do it this way
  //  StarGeometry* _geo;

 private:


  ClassDef(StarDetector,0)
    };
#endif //STARDETECTOR_H

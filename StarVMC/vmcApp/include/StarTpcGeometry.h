#ifndef STARTPCGEOMETRY_H
#define STARTPCGEOMETRY_H

/* $Id: StarTpcGeometry.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include "StarGeometry.h"

// 
class StarTpcGeometry: public StarGeometry {

 public:
  StarTpcGeometry();
  StarTpcGeometry(const char* name_, const char *title_);
  StarTpcGeometry(const StarTpcGeometry &mod);
  virtual ~StarTpcGeometry();

  static StarGeometry* GetInstance();

 protected:

 private:
  static StarTpcGeometry* _geo;	// pointer to the unique instance of the singleton

  ClassDef(StarTpcGeometry,0)
    };
#endif //STARTPCGEOMETRY_H

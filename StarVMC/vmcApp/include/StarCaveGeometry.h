#ifndef STARCAVEGEOMETRY_H
#define STARCAVEGEOMETRY_H

/* $Id: StarCaveGeometry.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include "StarGeometry.h"

// 
class StarCaveGeometry: public StarGeometry {

 public:
  StarCaveGeometry();
  StarCaveGeometry(const char* name_, const char *title_);
  StarCaveGeometry(const StarCaveGeometry &mod);
  virtual ~StarCaveGeometry();

  static StarGeometry* GetInstance();

 protected:

 private:
  static StarCaveGeometry* _geo;	// pointer to the unique instance of the singleton

  ClassDef(StarCaveGeometry,0)
    };
#endif //STARCAVEGEOMETRY_H

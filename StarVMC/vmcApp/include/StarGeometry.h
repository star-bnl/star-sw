#ifndef STARGEOMETRY_H
#define STARGEOMETRY_H

/* $Id: StarGeometry.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>

// 
class StarGeometry: public TNamed {

 public:
  StarGeometry();
  StarGeometry(const char* name_, const char *title_);
  StarGeometry(const StarGeometry &mod);
  virtual ~StarGeometry();

  static StarGeometry* GetInstance() {return 0;} // dummy

 protected:

 private:

  ClassDef(StarGeometry,0)
    };
#endif //STARGEOMETRY_H

#ifndef STARECAL_H
#define STARECAL_H

/* $Id: StarEcal.h,v 1.1 2004/07/16 22:56:12 potekhin Exp $ */
/* $Log: StarEcal.h,v $
 * Revision 1.1  2004/07/16 22:56:12  potekhin
 * Initial
 * */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include "StarDetector.h"

// 
class StarEcal: public StarDetector {

 public:
  StarEcal();
  StarEcal(const char* name_, const char *title_);
  virtual ~StarEcal();

  virtual void CreateGeometry(void);



  //  virtual StarGeometry* GetGeometry() const 
  //  {return StarEcalGeometry::GetInstance(GetTitle(),"") ;  }   

  
 protected:

 private:

  ClassDef(StarEcal,0)
    };
#endif //STARECAL_H

#ifndef STARTPC_H
#define STARTPC_H

/* $Id: StarTpc.h,v 1.1 2004/07/12 20:15:51 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include "StarDetector.h"

// 
class StarTpc: public StarDetector {

 public:
  StarTpc();
  StarTpc(const char* name_, const char *title_);
  virtual ~StarTpc();

  virtual void CreateGeometry(void);



  //  virtual StarGeometry* GetGeometry() const 
  //  {return StarTpcGeometry::GetInstance(GetTitle(),"") ;  }   

  
 protected:

 private:

  ClassDef(StarTpc,0)
    };
#endif //STARTPC_H

/**********************************************************************
 *
 * $Id: StEStructCentrality.h,v 1.1 2003/10/15 18:20:51 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  allows run-time definition of event class centrality
 *               this used to be persistent with dst...
 *
 ***********************************************************************/
#ifndef __STESTRUCTCENTRALITY_H
#define __STESTRUCTCENTRALITY_H

#include "TROOT.h"

class StEStructCentrality {

  int* mcentralities;
  int  mnumCentralities;

  static StEStructCentrality* mInstance;
  StEStructCentrality(): mcentralities(0), mnumCentralities(0) {};

 public:

  static StEStructCentrality* Instance();

  virtual ~StEStructCentrality();

  int centrality(int ncharge);
  void setCentralities(const int* centralities, const int num);


  ClassDef(StEStructCentrality,1)
};

#endif

/***********************************************************************
 *
 * $Log: StEStructCentrality.h,v $
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



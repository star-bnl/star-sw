/**********************************************************************
 *
 * $Id: StEStructCentrality.h,v 1.2 2004/02/27 02:28:03 prindle Exp $
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

  double* mcentralities;
  int  mnumCentralities;

  static StEStructCentrality* mInstance;
  StEStructCentrality(): mcentralities(0), mnumCentralities(0) {};

 public:

  static StEStructCentrality* Instance();

  virtual ~StEStructCentrality();

  int centrality(double impact);
  void setCentralities(const double* centralities, const int num);


  ClassDef(StEStructCentrality,1)
};

#endif

/***********************************************************************
 *
 * $Log: StEStructCentrality.h,v $
 * Revision 1.2  2004/02/27 02:28:03  prindle
 * Small modification to StEStructCentrality in EventMaker branch.
 * Many modifications to Fluctuations branch, although that branch is not
 * stable yet.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



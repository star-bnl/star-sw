/**********************************************************************
 *
 * $Id: StEStructCentrality.h,v 1.4 2004/06/25 03:13:41 porter Exp $
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

  double *mcentralities;
  int  mnumCentralities;
  double *mpts, *mptcents;
  int  mnumpts, mnumptcents;

  static StEStructCentrality* mInstance;
  StEStructCentrality(): mcentralities(0), mnumCentralities(0), mpts(0), mptcents(0),mnumpts(0), mnumptcents(0) {};

 public:

  static StEStructCentrality* Instance();

  virtual ~StEStructCentrality();

  double minCentrality(int id);
  double maxCentrality(int id);

  int centrality( double impact );
  int ptIndex(const double pt);
  int ptCentrality(const double cent);

  void setCentralities( const double* centralities, const int num );
  int numCentralities();
  double centralityLimit( const int index );

  void setPts( const double* ptRange,   const int numPt,
               const double* centRange, const int numPtCent );
  int numPts();
  int numPtCentralities();
  double ptLimit( const int ptIndex );
  double ptCentralityLimit( const int ptCent );

  ClassDef(StEStructCentrality,1)
};



inline double StEStructCentrality::minCentrality(int id){
  if(mcentralities && id>=0 && id<mnumCentralities ) return mcentralities[id];
  return -1;
}

inline double StEStructCentrality::maxCentrality(int id){
  if(mcentralities && id>=0 && id<(mnumCentralities-1) ) return mcentralities[id+1]-1;
  return -1;
}

#endif

/***********************************************************************
 *
 * $Log: StEStructCentrality.h,v $
 * Revision 1.4  2004/06/25 03:13:41  porter
 * updated centrality methods and put a test in StEStructEvent.cxx
 *
 * Revision 1.3  2004/06/09 22:39:09  prindle
 * Expanded centrality class.
 * Call to set centrality from event reader.
 *
 *
 * CVS :nded ----------------------------------------------------------------------
 *
 * Revision 1.2  2004/02/27 02:28:03  prindle
 *
 * Small modification to StEStructCentrality in EventMaker branch.
 * Many modifications to Fluctuations branch, although that branch is not
 * stable yet.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



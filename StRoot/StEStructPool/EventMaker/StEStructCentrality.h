/**********************************************************************
 *
 * $Id: StEStructCentrality.h,v 1.8 2010/03/02 21:47:18 prindle Exp $
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

enum StEStructCentType { ESNTracks=0, ESGenImpact }; // may be others from generators...

class StEStructCentrality {

  double mValue;
  double *mcentralities;
  int  mnumCentralities;
  double *mpts, *mptcents;
  int  mnumpts, mnumptcents;
  int warningCount;

  StEStructCentType  mCentType;

  static StEStructCentrality* mInstance;
  StEStructCentrality(): mcentralities(0), mnumCentralities(0), mpts(0), mptcents(0),mnumpts(0), mnumptcents(0), warningCount(0), mCentType(ESNTracks) {};

 public:

  static StEStructCentrality* Instance();

  virtual ~StEStructCentrality();

  double minCentrality(int id);
  double maxCentrality(int id);

  int centrality( double impact );
  double GetCentValue();
  int ptIndex(const double pt);
  int ptCentrality(const double cent);

  void setCentralities( const double* centralities, const int num );
  void setPtLimit( const int index, double pt );
  int numCentralities();
  double centralityLimit( const int index );

  void setPts( const double* ptRange,   const int numPt,
               const double* centRange, const int numPtCent );
  int numPts();
  int numPtCentralities();
  double ptLimit( const int ptIndex );
  double ptCentralityLimit( const int ptCent );

  void setCentTypeNTracks();
  void setCentTypeImpact();

  void Print();

  StEStructCentType getCentType();

  ClassDef(StEStructCentrality,1)
};



inline double StEStructCentrality::GetCentValue() {
  return mValue;
}

inline double StEStructCentrality::minCentrality(int id){
  if(mcentralities && id>=0 && id<mnumCentralities ) return mcentralities[id];
  return -1;
}

inline double StEStructCentrality::maxCentrality(int id){
  if(mcentralities && id>=0 && id<(mnumCentralities-1) ) return mcentralities[id+1]-1;
  return -1;
}


inline void StEStructCentrality::setCentTypeNTracks() { mCentType=ESNTracks ;};
inline void StEStructCentrality::setCentTypeImpact() { mCentType=ESGenImpact;};
inline StEStructCentType StEStructCentrality::getCentType(){ return mCentType;};

#endif

/***********************************************************************
 *
 * $Log: StEStructCentrality.h,v $
 * Revision 1.8  2010/03/02 21:47:18  prindle
 *   Support to retrieve track radius when it crosses endplate
 *   Add way to retrieve centrality
 *
 * Revision 1.7  2007/01/26 17:19:50  msd
 * Added Print function.
 *
 * Revision 1.6  2006/04/04 22:12:29  porter
 * Set up StEtructCentrality for use in event cut selection - includes impact para for generators
 *
 * Revision 1.5  2004/09/24 01:46:45  prindle
 * Added call for setPtLimit. I use this in fluctuations which prevented
 * a fresh CVS checkout from compiling
 *
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



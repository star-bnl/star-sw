/**********************************************************************
 *
 * $Id: StEStructEventCuts.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for event level quantities
 *
 *
 ***********************************************************************/
#ifndef __STEBYEEVENTCUTS__H
#define __STEBYEEVENTCUTS__H


#include "StEStructCuts.h"
#include "Stiostream.h"

class StEStructEventCuts : public StEStructCuts {

protected:

   CutName mtWordName;
   CutName mpVertexZName;
   CutName mcentralityName;
   CutName mnumTracksName;  


  unsigned int mtWord[2];  
  float        mpVertexZ[2]; 
  unsigned int mcentrality[2];
  unsigned int mnumTracks[2];
  
  void init();
  void initCuts();
  void initNames();

public:

  StEStructEventCuts();
  StEStructEventCuts(const char* cutFileName);
  virtual ~StEStructEventCuts();

  virtual bool loadBaseCuts(const char* name, const char** vals, int nvals);
  virtual void loadUserCuts(const char* name, const char** vals, int nvals);
  virtual void printCuts(ostream& ofs);
  virtual void printCuts(const char* fname) { StEStructCuts::printCuts(fname); };


  bool goodTrigger(unsigned int t);
  bool goodPrimaryVertexZ(float z);
  bool goodCentrality(unsigned int c);
  bool goodNumberOfTracks(unsigned int n);

  char* triggerWordName(){ return (char*)mtWordName.name; };
  char* primaryVertexZName() { return (char*) mpVertexZName.name; };
  char* centralityName() { return (char*) mcentralityName.name; };
  char* numTracksName() { return (char*) mnumTracksName.name; };


  ClassDef(StEStructEventCuts,1)

};

inline void StEStructEventCuts::loadUserCuts(const char* name, const char** vals, int nvals){}

inline bool StEStructEventCuts::goodTrigger(unsigned int t){
  mvalues[mtWordName.idx] = (float)t;
  return ( (mtWord[0]==mtWord[1] && mtWord[0]==0) ||
           (t>=mtWord[0] && t<=mtWord[1])  ) ;
}

inline bool StEStructEventCuts::goodPrimaryVertexZ(float z){
  mvalues[mpVertexZName.idx] = z;
  return ( (mpVertexZ[0]==mpVertexZ[1] && mpVertexZ[0]==0) ||
           (z>=mpVertexZ[0] && z<=mpVertexZ[1])  ) ;
}

inline bool StEStructEventCuts::goodCentrality(unsigned int c){
  mvalues[mcentralityName.idx] = (float)c;
  return (  (mcentrality[0]==mcentrality[1] && mcentrality[0]==0) ||
            (c>=mcentrality[0] && c<=mcentrality[1])  );
}

inline bool StEStructEventCuts::goodNumberOfTracks(unsigned int n){
  mvalues[mnumTracksName.idx] = (float)n;
  return (  (mnumTracks[0]==mnumTracks[1] && mnumTracks[0]==0) ||
            (n>=mnumTracks[0] && n<=mnumTracks[1]) );
}

#endif

/***********************************************************************
 *
 * $Log: StEStructEventCuts.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/









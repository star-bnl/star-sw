/**********************************************************************
 *
 * $Id: StEStructCuts.h,v 1.9 2012/11/16 21:19:06 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract class for cuts. It does implement reading 
 *               of a cut file and building histograms. Specific
 *               cuts are done in derived classes
 *
 ***********************************************************************/
#ifndef __STEBYECUTS__H
#define __STEBYECUTS__H


// -> for rootcint preprocessing
#include "TROOT.h"

class TH1;
class TFile;

// local name-id mapping
class CutName {

 public:
  CutName(): idx(99){};
  int idx;
  char name[32];
};


class StEStructCuts {

protected:

  char*  mcutFileName;//!

  int    mMaxStore; //!
  char** mvarName; //!
  float* mvalues; //!
  float* mminVal;
  float* mmaxVal;
  int    mnumVars; //!
  TH1**  mvarHistsNoCut; //!
  TH1**  mvarHistsCut; //!
  bool   mDoFillHists;  // If true we fill histograms. If false skip filling.

  char   mcutTypeName[64];

  void resize();
  void initVars();
  void deleteVars();

public:

  StEStructCuts(const char* cutFileName);
  StEStructCuts();
  virtual ~StEStructCuts();

  virtual void setCutFile(const char* cutFileName);
  virtual bool isLoaded();

  virtual bool loadCuts();
  virtual void dataValue(const char* name, float value);

  virtual int createCutHists(const char* name, float* range, int nvals=2);
  virtual int createCutHists(const char* name, double* range, int nvals=2);
  virtual int createCutHists(const char* name, int* range, int nvals=2);
  virtual int createCutHists(const char* name, unsigned int* range, int nvals=2);
  virtual void addCutHists(TH1* before, TH1* after, const char* name=NULL);
  virtual void setDoFillHists(bool filling);
  virtual bool doFillHists();


  virtual void fillHistogram(const char* name, float value, bool passed);
  virtual void fillHistogram(const char* name, float val1, float val2, bool passed);
  virtual void fillHistogram(const char* name, float val1, float val2, float val3, bool passed);
  virtual void fillHistograms(bool passed);
  virtual void writeCutHists(TFile* tf);
  virtual bool loadBaseCuts(const char* name,const char** vals,int nvals)=0;
  bool loadBaseCuts(const char* name,const char* val1,const char* val2,const char* val3="",const char* val4="");  // overloaded to accept strings
  virtual void loadUserCuts(const char* name,const char** vals,int nvals)=0; 
  void loadUserCuts(const char* name,const char* val1,const char* val2);  // overloaded to accept strings
  virtual void printCuts(std::ostream& of, int i=-1);
  virtual void printCuts(const char* fileName);
  virtual void printCutStats(std::ostream& of) = 0;
  virtual bool loadCutDB();   // Loads pre-compiled cuts from database

  virtual bool  isCut(const char* cutName);
  virtual int   cutID(const char* cutName);
  virtual float maxVal(const char* cutName);
  virtual float maxVal(int cutID);
  virtual float minVal(const char* cutName);
  virtual float minVal(int cutID);
  
  virtual void  setRange(const char* cutName, float xmin, float xmax);
  virtual void  setRange(int cutID, float xmin, float xmax);

  ClassDef(StEStructCuts,1)

};

inline void StEStructCuts::dataValue(const char* name,float value){

  int i = cutID(name); 
  if(i<0 || i==mnumVars) return;
  mvalues[i]=value;
}

inline int StEStructCuts::createCutHists(const char* name, double* range, int nvals){  
  float* tmp=new float[nvals]; 
  for(int i=0;i<nvals;i++)tmp[i]=(float)range[i];
  int retVal=createCutHists(name,tmp,nvals);
  delete [] tmp;
  return retVal;
}

inline int StEStructCuts::createCutHists(const char* name, int* range, int nvals){  
  float* tmp=new float[nvals]; 
  for(int i=0;i<nvals;i++)tmp[i]=(float)range[i];
  int retVal=createCutHists(name,tmp,nvals);
  delete [] tmp;
  return retVal;
}

inline int StEStructCuts::createCutHists(const char* name, unsigned int* range, int nvals){  
  float* tmp=new float[nvals]; 
  for(int i=0;i<nvals;i++)tmp[i]=(float)range[i];
  int retVal=createCutHists(name,tmp,nvals);
  delete [] tmp;
  return retVal;

}

inline bool StEStructCuts::isLoaded(){ return (mcutFileName) ? true : false ; }


inline int StEStructCuts::cutID(const char* cutName){
 
  if(mnumVars<0) return -1;
   int i; 
   for(i=0;i<mnumVars;i++)if(strstr(cutName,mvarName[i]))break;
   if(i==mnumVars)i=-1;
   return i;

}

inline bool StEStructCuts::isCut(const char* cutName){
  int id=cutID(cutName);
  if(id<0) return false;
  return true;
}

inline float StEStructCuts::minVal(int cutID){
  if(cutID<0 || cutID==mnumVars)return 0.;
  return mminVal[cutID];
}

inline float StEStructCuts::minVal(const char* cutName){
  return minVal(cutID(cutName));
}

inline float StEStructCuts::maxVal(int cutID){
  if(cutID<0 || cutID==mnumVars)return 0.;
  return mmaxVal[cutID];
}


inline float StEStructCuts::maxVal(const char* cutName){
  return maxVal(cutID(cutName));
}

inline void StEStructCuts::setRange(int cutID, float xmin, float xmax){
  if(cutID<0 || cutID==mnumVars) return;
  mminVal[cutID]=xmin;
  mmaxVal[cutID]=xmax;
}

inline void StEStructCuts::setRange(const char* cutName, float xmin, float xmax){
  setRange(cutID(cutName),xmin,xmax);
}

inline void StEStructCuts::setDoFillHists(bool filling) {
    mDoFillHists = filling;
}
inline bool StEStructCuts::doFillHists() {
    return mDoFillHists;
}


#endif


/***********************************************************************
 *
 * $Log: StEStructCuts.h,v $
 * Revision 1.9  2012/11/16 21:19:06  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.8  2012/06/11 14:35:32  fisyak
 * std namespace
 *
 * Revision 1.7  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.6  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.5  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.4  2005/09/14 17:08:32  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.3  2005/02/09 23:08:44  porter
 * added method to add histograms directly instead of under
 * the control of the class. Useful for odd 2D hists that don't
 * fit the current model.
 *
 * Revision 1.2  2004/08/23 19:12:13  msd
 * Added pre-compiled cut database, minor changes to cut base class
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

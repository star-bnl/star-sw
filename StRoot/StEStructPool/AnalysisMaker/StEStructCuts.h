/**********************************************************************
 *
 * $Id: StEStructCuts.h,v 1.2 2004/08/23 19:12:13 msd Exp $
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

class TH1F;
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
  int    mnumVars; //!
  TH1F**  mvarHistsNoCut; //!
  TH1F**  mvarHistsCut; //!

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

  virtual void fillHistogram(const char* name, float value, bool passed);
  virtual void fillHistograms(bool passed);
  virtual void writeCutHists(TFile* tf);
  virtual bool loadBaseCuts(const char* name,const char** vals,int nvals)=0;
  virtual bool loadBaseCuts(const char* name,const char* val1,const char* val2);  // overloaded to accept strings
  virtual void loadUserCuts(const char* name,const char** vals,int nvals)=0; 
  virtual void loadUserCuts(const char* name,const char* val1,const char* val2);  // overloaded to accept strings
  virtual void printCuts(ostream& of) = 0;
  virtual void printCuts(const char* fileName);
  virtual bool loadCutDB();   // Loads pre-compiled cuts from database
    
  ClassDef(StEStructCuts,1)

};

inline void StEStructCuts::dataValue(const char* name,float value){

  int i; for(i=0;i<mnumVars;i++)if(strstr(name,mvarName[i]))break;
  if(i==mnumVars) return;
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


#endif


/***********************************************************************
 *
 * $Log: StEStructCuts.h,v $
 * Revision 1.2  2004/08/23 19:12:13  msd
 * Added pre-compiled cut database, minor changes to cut base class
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

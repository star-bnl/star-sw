// $Id: StFgt1dClustMaker.h,v 1.1 2011/10/26 19:59:07 balewski Exp $


/* \class StFgt1dClustMaker        
\author Jan Balewski

1-dim CLuster finder for FGT 
  

*/
 
#ifndef STAR_StFgt1dClustMaker
#define STAR_StFgt1dClustMaker


#ifndef StMaker_H
#include "StMaker.h" 
#endif

#include "StFgtUtil/geometry/StFgtGeom.h"
#include "StFgtSimulator/StFgtContainers.h"

class StFgtNaiveDbMaker;

class StFgt1dClustMaker : public StMaker {   
 private:
  enum {mxH=16};
  TH1 *hA[mxH];
  TObjArray *HList;
  StFgtGeom *geom;
  StFgtNaiveDbMaker *fgtDb;

  int mInpEve;
  int     par_bx_valley; // # of strips, min separation between 2 clusters
  int     par_cl_width; // # of strips,min cluster width
  double  par_seedStripThres; //a.u.,  for valid cluster
  double  par_clusterMinAmpl;  // a.u., min ampl for a cluster

  int findClust1D(vector<fgt_strip> &sL, vector<fgt_cluster1D> &clL);
 public: 
  //  vector<fgt_cluster1D> mRadClustList[StFgtGeom::kFgtMxDisk]; // tmp for export
  // vector<fgt_cluster1D> mPhiClustList[StFgtGeom::kFgtMxDisk];


  StFgt1dClustMaker(const char *name="Fgt1dClustMk");
  virtual       ~StFgt1dClustMaker();
  virtual Int_t Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t Finish(); 
  virtual Int_t  Make();
  virtual void Clear(Option_t *option="");
  void  setHList(TObjArray * x){HList=x;}
  void  setFgtDb(StFgtNaiveDbMaker *x) { fgtDb=x;}
  void  setSeedStripThres(double x) {par_seedStripThres=x;}
  void  setClusterMinAmpl(double x) {par_clusterMinAmpl=x;}
  void  saveHisto(TString fname);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgt1dClustMaker.h,v 1.1 2011/10/26 19:59:07 balewski Exp $ built "__DATE__" "__TIME__ ; 

    return cvs;
  }
 private:
  
  ClassDef(StFgt1dClustMaker,0)   
};
    
#endif


// $Log: StFgt1dClustMaker.h,v $
// Revision 1.1  2011/10/26 19:59:07  balewski
// not working yet
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//
 

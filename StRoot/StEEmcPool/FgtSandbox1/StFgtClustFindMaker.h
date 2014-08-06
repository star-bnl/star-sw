// $Id: StFgtClustFindMaker.h,v 1.2 2014/08/06 11:42:57 jeromel Exp $


/* \class StFgtClustFindMaker        
\author Jan Balewski

CLuster finder for FGT 
  

*/
 
#ifndef STAR_StFgtClustFindMaker
#define STAR_StFgtClustFindMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtGeom.h"
#include "StFgtContainers.h"
class TRandom3;

class StFgtClustFindMaker : public StMaker {   
 private:
  enum {mxH=16};
  TH1 *hA[mxH];
  TObjArray *HList;
  StFgtGeom *geom;
  TRandom3* mRnd;

  int mInpEve;
  int     par_bx_valley; // # of strips, min separation between 2 clusters
  int     par_cl_width; // # of strips,min cluster width
  double  par_seedStripThres; //a.u.,  for valid cluster
  double  par_clusterMinAmpl;  // a.u., min ampl for a cluster
  double  par_stripNoiseSigma; // random noise sigma

  int findClust1D(vector<fgt_strip> &sL, vector<fgt_cluster1D> &clL);
 public: 
  vector<fgt_cluster1D> mRadClustList[kFgtMxDisk]; // tmp for export
  vector<fgt_cluster1D> mPhiClustList[kFgtMxDisk];


  StFgtClustFindMaker(const char *name="FgtClustFind");
  virtual       ~StFgtClustFindMaker();
  virtual Int_t Init();
  virtual Int_t Finish(); 
  virtual Int_t  Make();
  virtual void Clear(Option_t *option="");
  void  setHList(TObjArray * x){HList=x;}
  void  setStripNoiseSigma(double x) {par_stripNoiseSigma=x;}
  void  setSeedStripThres(double x) {par_seedStripThres=x;}
  void  setClusterMinAmpl(double x) {par_clusterMinAmpl=x;}
  void  saveHisto(TString fname);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtClustFindMaker.h,v 1.2 2014/08/06 11:42:57 jeromel Exp $ built " __DATE__ " " __TIME__ ; 

    return cvs;
  }
 private:
  
  ClassDef(StFgtClustFindMaker,0)   
};
    
#endif


// $Log: StFgtClustFindMaker.h,v $
// Revision 1.2  2014/08/06 11:42:57  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//
 

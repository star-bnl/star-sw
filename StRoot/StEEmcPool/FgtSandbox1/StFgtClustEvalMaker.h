// $Id: StFgtClustEvalMaker.h,v 1.2 2014/08/06 11:42:57 jeromel Exp $


/* \class StFgtClustEvalMaker        
\author Jan Balewski

CLuster finder for FGT 
  

*/
 
#ifndef STAR_StFgtClustEvalMaker
#define STAR_StFgtClustEvalMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtGeom.h"
#include "StFgtContainers.h"

 
class StFgtClustEvalMaker : public StMaker {   
 private:
  enum {mxH=16};
  TH1 *hA[mxH];
  TObjArray *HList;
  StFgtGeom *geom;

  int mInpEve;
  double par_minDelRad; // radial match cut off at 1 mm 
  double par_minRdPhi; //  match cut off at 1 mm 


  int matchRadClust1D( vector<fgt_g2t_auxil> *g2tTrL,  vector<fgt_cluster1D> &clL);
  int matchPhiClust1D( vector<fgt_g2t_auxil> *g2tTrL,  vector<fgt_cluster1D> &clL);
 public: 
  StFgtClustEvalMaker(const char *name="FgtClustEval");
  virtual       ~StFgtClustEvalMaker();
  virtual Int_t Init();
  virtual Int_t Finish(); 
  virtual Int_t  Make();
  virtual void Clear(Option_t *option="");
  void setHList(TObjArray * x){HList=x;}
  void saveHisto(TString fname);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtClustEvalMaker.h,v 1.2 2014/08/06 11:42:57 jeromel Exp $ built " __DATE__ " " __TIME__ ; 

    return cvs;
  }
 private:
  
  ClassDef(StFgtClustEvalMaker,0)   
};
    
#endif


// $Log: StFgtClustEvalMaker.h,v $
// Revision 1.2  2014/08/06 11:42:57  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//

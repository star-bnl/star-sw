// $Id: StarBASE.h,v 1.3 2015/08/04 21:24:02 jwebb Exp $

#ifndef STAR_StarBASE
#define STAR_StarBASE

/*!
 *                                                                     
 * \class  StarBASE
 * \author fisyak
 * \date   2005/04/22
 * \brief  virtual base class for Maker
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 */                                                                      

#include "StMaker.h"
#include "StVMCApplication.h"
#include "TGeant3TGeo.h"

class StMCStepping;

class StMCSimplePrimaryGenerator;

class StarBASE : public StMaker {
 public: 
  StarBASE(const char *name="GeoTest",const char *gy="y2009",int nTrig=100);
  virtual       ~StarBASE() {}
  int  Init();
  int  InitRun(int);
  int  Make();
  int  Finish();
  void SetDebug(int l);
  StMCStepping *steps(){ return mSteps; }

  void SetVertex( Double_t z, Double_t sigma=0.0 ){ mZ=z; mS=sigma; }

 private:
  // Private method declaration if any

  StMCSimplePrimaryGenerator *mGenerator;
 
 protected:
  int fNTrig;
  TString fGeo;

 public:
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StarBASE.h,v 1.3 2015/08/04 21:24:02 jwebb Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  StMCStepping *mSteps;

  Double_t mZ;
  Double_t mS;

  ClassDef(StarBASE,0)   // Makers
};

#endif



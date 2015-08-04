// $Id: GeoTestMaker.h,v 1.2 2015/08/04 21:13:40 jwebb Exp $

#ifndef STAR_GeoTestMaker
#define STAR_GeoTestMaker

/*!
 *                                                                     
 * \class  GeoTestMaker
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
class GeoTestMaker : public StMaker {
 public: 
  GeoTestMaker(const char *name="GeoTest",const char *gy="y2009",int nTrig=100);
  virtual       ~GeoTestMaker() {}
  int  Init();
  int  InitRun(int);
  int  Make();
  int  Finish();
  void SetDebug(int l);

 private:
  // Private method declaration if any
 
 protected:
  int fNTrig;
  TString fGeo;
 public:
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: GeoTestMaker.h,v 1.2 2015/08/04 21:13:40 jwebb Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(GeoTestMaker,0)   // Makers
};

#endif



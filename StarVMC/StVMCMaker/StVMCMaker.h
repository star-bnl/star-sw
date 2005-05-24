// $Id: StVMCMaker.h,v 1.1 2005/05/24 22:58:08 fisyak Exp $

#ifndef STAR_StVMCMaker
#define STAR_StVMCMaker

/*!
 *                                                                     
 * \class  StVMCMaker
 * \author fisyak
 * \date   2005/04/22
 * \brief  virtual base class for Maker
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StarVMCApplication.h"
#include "TGeant3TGeo.h"

class StVMCMaker : public StMaker {
 public: 
  StVMCMaker(const char *name="geant") : StMaker(name),fEventNo(0), fEvtHddr(0), fInputFile("") {}
  virtual       ~StVMCMaker() {}
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option="");
  virtual Int_t  Finish();

  virtual Int_t  InitRun  (int runumber);
  virtual Int_t  FinishRun(int runumber){return 0;}; 
  virtual void   SetDateTime(int idat=0,int itim=0);
  virtual void   Skip(Int_t nskip);
  virtual void   SetInputFile(const Char_t *fileName) {fInputFile = fileName;}
  const Char_t  *InputFile() const {return fInputFile.Data();}
  static StarVMCApplication* GetStarVMCApplication() {return fgStarVMCApplication;}
  static TGeant3TGeo*        GetGeant3()             {return fgGeant3;}
 private:
  // Private method declaration if any
 
 protected:
  // Protected method if any
  static StarVMCApplication* fgStarVMCApplication;
  static TGeant3TGeo*        fgGeant3;
  Int_t  fEventNo;
  StEvtHddr *fEvtHddr;//! pointer to Event Header
  TString    fInputFile;
 public:
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StVMCMaker.h,v 1.1 2005/05/24 22:58:08 fisyak Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StVMCMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StVMCMaker.h,v $
// Revision 1.1  2005/05/24 22:58:08  fisyak
// The first version
//

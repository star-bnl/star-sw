// $Id: StVMCMaker.h,v 1.1.1.2 2009/04/16 14:12:55 fisyak Exp $

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
class TTreeIter;
class StVMCMaker : public StMaker {
 public: 
  StVMCMaker(const char *name="geant") : StMaker(name),fEventNo(0), fRunNo(1), fEvtHddr(0), fInputFile(""), 
    fInitRun(0), fVolume(0), fMuDstIter(0) {fgGeantMk = this;}
  virtual       ~StVMCMaker() {}
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *option="");
  virtual Int_t  Finish();
  virtual Int_t  InitRun  (Int_t runumber = 0);
  virtual Int_t  FinishRun(Int_t runumber){return 0;}; 
  virtual void   SetDateTime(Int_t idat=0,Int_t itim=0);
  virtual void   SetRunNo(Int_t m ) {fRunNo = m < 1 || m >= 1000000 ? 1 : m;}
  virtual Int_t  Skip(Int_t nskip);
  virtual Int_t  SetInputFile(const Char_t *fileName);
  virtual void   SetDebug(Int_t l=1);          // *MENU*
  virtual Int_t  SetVertex();
  //  virtual void   SetInputMode(const Char_t *fileMode) {fInputMode = fileMode;}
  const Char_t  *InputFile() const {return fInputFile.Data();}
  static StarVMCApplication* GetStarVMCApplication() {return fgStarVMCApplication;}
  static TGeant3TGeo*        GetGeant3()             {return fgGeant3;}
  static StVMCMaker *instance() {return fgGeantMk;}
  TDataSet* GetVolume() { return fVolume; }
 private:
  // Private method declaration if any
 
 protected:
  // Protected method if any
  virtual TDataSet  *FindDataSet (const char* logInput,
				  const StMaker *uppMk=0,
				  const StMaker *dowMk=0) const ;
  static StarVMCApplication* fgStarVMCApplication; //!
  static TGeant3TGeo*        fgGeant3; //!
  static StVMCMaker*         fgGeantMk; //!
  Int_t                      fEventNo;
  Int_t                      fRunNo;
  StEvtHddr                 *fEvtHddr;//! pointer to Event Header
  TString                    fInputFile;
  Int_t                      fInitRun;
  TDataSet*                  fVolume;   //!
  TTreeIter*                 fMuDstIter; //! MuDst to select primary vertex for embedding
 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag $Name:  $ $Id: StVMCMaker.h,v 1.1.1.2 2009/04/16 14:12:55 fisyak Exp $ built " __DATE__ " " __TIME__  ; 
    return cvs;
  }

  ClassDef(StVMCMaker,0)   //
};

#endif


// $Log: StVMCMaker.h,v $
// Revision 1.1.1.2  2009/04/16 14:12:55  fisyak
// remove this version from STAR official repository
//
// Revision 1.8  2009/02/03 15:55:44  fisyak
// synchronize with .DEV2
//
// Revision 1.1.1.1  2008/12/10 20:45:49  fisyak
// Merge with macos version
//
// Revision 1.7  2008/03/05 13:15:56  fisyak
// comply Skip signuture with base class
//
// Revision 1.6  2007/04/07 19:33:00  perev
// Check for input file added
//
// Revision 1.5  2007/01/09 04:53:52  potekhin
// Add new methods
//
// Revision 1.4  2005/09/13 21:34:29  fisyak
// Move initialization from Init to InitRun, add conversion TGeoVolume to TVolume for StEventDisplayMaker and TofMatchers
//
// Revision 1.3  2005/08/29 22:49:31  fisyak
// add check for run no.
//
// Revision 1.2  2005/06/09 20:14:40  fisyak
// Set Run number (=1 D)
//
// Revision 1.1  2005/05/24 22:58:08  fisyak
// The first version
//

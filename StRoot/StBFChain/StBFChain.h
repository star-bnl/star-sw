// $Id: StBFChain.h,v 1.6 1999/10/28 01:57:17 fisyak Exp $
// $Log: StBFChain.h,v $
// Revision 1.6  1999/10/28 01:57:17  fisyak
// Add ParseString
//
// Revision 1.5  1999/10/12 23:13:31  fisyak
// Add AddBefore and AddAfter methods
//
// Revision 1.4  1999/09/24 01:22:51  fisyak
// Reduced Include Path
//
// Revision 1.3  1999/09/12 23:02:43  fisyak
// Add closing xdf and TFile
//
// Revision 1.2  1999/09/08 00:14:06  fisyak
// Add kReverseField option
//
// Revision 1.1  1999/09/02 16:14:43  fine
// new StBFChain library has been introduced to break dependences
//
// Revision 1.4  1999/08/31 00:26:42  fisyak
// Add TFile to BFChain
//
// Revision 1.3  1999/08/10 17:10:52  fisyak
// Exprot EChainOptions into rootcint
//
// Revision 1.2  1999/08/06 14:26:38  fisyak
// put back xdf out option
//
// Revision 1.1  1999/07/29 01:05:23  fisyak
// move bfc to StBFChain
//
#ifndef STAR_StBFChain
#define STAR_StBFChain

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StBFChain                                                            //
//                                                                      //
// Class to control "BFC" chain                                         //
//                                                                      //
// This class :                                                         //
//   - Initialises the run default parameters                           //
//   - Provides API to Set/Get run parameters                           //
//   - Creates the support lists (TClonesArrays) for the Event structure//
//   - Creates the physics objects makers                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StChain.h"
#include "TFile.h"
//_____________________________________________________________________
enum EChainOptions { 
  kFIRST   ,
  kSD97    ,kSD98    ,kY1a     ,kY1b     ,kY1c     ,          // time stamps
  kES99    ,kER99    ,kY1d     ,kY1e     ,kY2a     ,
  kEval    ,kOFF     ,kXIN     ,kXOUT    ,kGSTAR   ,          // Chains, options
  kTDAQ    ,kFZIN    ,kGEANT   ,kUTIL    ,
  kFieldOn ,kFieldOff,kHalfField,kReverseField     ,          // Magnetic Field
  kTPC     ,kTSS     ,kTRS     ,kMINIDAQ ,kTFS     ,kTCL     ,kTPT     ,// TPC
  kSVT     ,kSRS     ,kSTK     ,                              // SVT  
  kFTPC    ,kFSS     ,kFCL     ,kFPT     ,                    // FTPC
  kEMS     ,kEMC     ,                                        // EMC
  kTRG     ,kCTF     ,kMWC     ,kL3T     ,
  kRICH    ,                                                  // RICH
  kGLOBAL  ,kMATCH   ,kPRIMARY ,kV0      ,kXI      ,kKINK    ,// Global Chain
  kDST     ,kEVENT   ,kANALYSIS,kQA      ,                    // Dst
  kTREE    ,kAllEvent,kDISPLAY ,kLAST    ,                    // StEvent
  kDEFAULT ,
  kMakeDoc ,kDEBUG   ,kHIGZ
};

class St_XDFFile;

class StBFChain : public StChain {
 private:
  St_XDFFile          *xdf_out; //! xdf output file if any
  TFile               *m_TFile;         //TFile associated with the chain
 public:
                       StBFChain(const char *name="bfc");
   virtual            ~StBFChain();
   virtual Int_t       Load();      // *MENU*
   virtual Int_t       AddAB (const Char_t *after="",const StMaker *maker=0,const Int_t Opt=1); 
   virtual Int_t       AddAfter  (const Char_t *after, const StMaker *maker) {return AddAB (after,maker);} 
   virtual Int_t       AddBefore (const Char_t *before,const StMaker *maker) {return AddAB (before,maker,-1);} 
   virtual Int_t       ParseString (const TString &tChain, TString *Opt[]);
   void                SetFlags(const Char_t *Chain="gstar tfs"); // *MENU*
   void                Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0); // *MENU
   virtual void        SetXdfOut(St_XDFFile *xdf=0) {xdf_out = xdf;}
   virtual void        SetOption(Int_t k);
   virtual void        SetTFile(TFile *m) {m_TFile = m;}
   virtual Int_t       Finish();
   virtual TFile      *GetTFile() {return m_TFile;}
   virtual St_XDFFile *GetXdfOut() {return xdf_out;}
   virtual Bool_t      GetOption(Int_t k);
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBFChain.h,v 1.6 1999/10/28 01:57:17 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   ClassDef(StBFChain, 0)   //StBFChain control class
};
#endif

// $Id: StBFChain.h,v 1.9 1999/11/29 21:38:29 fisyak Exp $
// $Log: StBFChain.h,v $
// Revision 1.9  1999/11/29 21:38:29  fisyak
// Add Dave Hardtke corrections, clean up print outs
//
// Revision 1.8  1999/11/07 02:26:22  fisyak
// Clean ups
//
// Revision 1.7  1999/11/04 22:21:25  fisyak
// Reorganize chain as Table
//
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

class St_XDFFile;

class StBFChain : public StChain {
 private:
  St_XDFFile          *xdf_out; //! xdf output file if any
  TFile               *m_TFile;         //TFile associated with the chain
 public:
                       StBFChain(const char *name="bfc");
   virtual            ~StBFChain();
   virtual Int_t       Load();      // *MENU*
   virtual Int_t       Instantiate();      // *MENU*
   virtual Int_t       AddAB (const Char_t *after="",const StMaker *maker=0,const Int_t Opt=1); 
   virtual Int_t       AddAfter  (const Char_t *after, const StMaker *maker) {return AddAB (after,maker);} 
   virtual Int_t       AddBefore (const Char_t *before,const StMaker *maker) {return AddAB (before,maker,-1);} 
   virtual Int_t       ParseString (const TString &tChain, TString *Opt[]);
   void                SetFlags(const Char_t *Chain="gstar tfs", Bool_t Force=kFALSE); // *MENU*
   void                Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0); // *MENU
   virtual Int_t       kOpt(const TString *Tag) const; 
   virtual Int_t       kOpt(const Char_t  *Tag) const; 
   virtual void        SetXdfOut(St_XDFFile *xdf=0) {xdf_out = xdf;}
   virtual void        SetDbOptions();
   virtual void        SetDataBases(const Char_t *TimeStamp);
   virtual void        SetGeantOptions();
   virtual void        SetTreeOptions();
   virtual void        SetOption(const Int_t k);
   virtual void        SetOption(const Char_t*  Opt) {SetOption(kOpt(Opt));}
   virtual void        SetOption(const TString* Opt) {SetOption(kOpt(Opt));}
   virtual void        SetOptionOff(const Char_t*  Opt) {SetOption(-kOpt(Opt));}
   virtual void        SetOptionOff(const TString* Opt) {SetOption(-kOpt(Opt));}
   virtual void        SetTFile(TFile *m) {m_TFile = m;}
   virtual Int_t       Finish();
   virtual TFile      *GetTFile() {return m_TFile;}
   virtual St_XDFFile *GetXdfOut() {return xdf_out;}
   virtual Bool_t      GetOption(const Int_t k);
   virtual Bool_t      GetOption(const TString *Opt) {return GetOption(kOpt(Opt));}
   virtual Bool_t      GetOption(const Char_t *Opt)  {return GetOption(kOpt(Opt));}
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBFChain.h,v 1.9 1999/11/29 21:38:29 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   ClassDef(StBFChain, 0)   //StBFChain control class
};
#endif

// $Id: StBFChain.h,v 1.1 1999/07/29 01:05:23 fisyak Exp $
// $Log: StBFChain.h,v $
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

class St_XDFFile;

class StBFChain : public StChain {
 private:
  St_XDFFile         *xdf_out; //! xdf output file if any
 public:
                      StBFChain(const char *name="bfc");
   virtual           ~StBFChain();
   virtual Int_t      Load();      // *MENU*
   void               SetFlags(const Char_t *Chain="gstar tfs"); // *MENU*
   void               Set_IO_Files(const Char_t *infile=0, const Char_t *outfile=0); // *MENU
   St_XDFFile        *GetXdfOut() {return xdf_out;}
   void               SetOption(Int_t k);
   Bool_t             GetOption(Int_t k);
   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StBFChain.h,v 1.1 1999/07/29 01:05:23 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   ClassDef(StBFChain, 0)   //StBFChain control class
};
#endif

// $Id: StChain.h,v 1.26 1999/03/19 20:30:49 perev Exp $
// $Log: StChain.h,v $
// Revision 1.26  1999/03/19 20:30:49  perev
// GetCVSTag introduced
//
// Revision 1.25  1999/03/11 01:23:58  perev
// new schema StChain
//
// Revision 1.18  1998/12/21 19:42:50  fisyak
// Move ROOT includes to non system
//
// Revision 1.17  1998/11/25 21:58:21  fisyak
// Cleanup
//
// Revision 1.16  1998/11/22 18:28:06  fisyak
// Add name of tag
//
// Revision 1.15  1998/11/19 01:23:56  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.14  1998/10/31 00:21:31  fisyak
// Makers take care about branches
//
// Revision 1.13  1998/10/07 18:43:59  perev
// Add Spy classes for Farm Monitor
//
// Revision 1.12  1998/10/06 18:00:27  perev
// cleanup
//
// Revision 1.11  1998/09/18 14:35:29  fisyak
// Fix makers
//
// Revision 1.10  1998/09/08 22:43:09  fisyak
// Modify St_dst_Maker to account new calling sequence
//
// Revision 1.9  1998/09/08 13:42:00  love
// new St_tpctest_Maker module
//
// Revision 1.8  1998/08/18 14:05:02  fisyak
// Add to bfc dst
//
// Revision 1.7  1998/08/07 19:34:53  fisyak
// Add St_run_Maker
//
// Revision 1.6  1998/07/20 15:08:08  fisyak
// Add tcl and tpt
//

#ifndef STAR_StChain
#define STAR_StChain

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChain                                                              //
//                                                                      //
// Main base class to control chains for the different STAR "chains"    //
//                                                                      //
// This class :                                                         //
//   - Initialises the run default parameters                           //
//   - Provides API to Set/Get run parameters                           //
//   - Creates the support lists (TClonesArrays) for the Event structure//
//   - Creates the physics objects makers                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <stdlib.h>
#include <stdio.h>


#ifndef ROOT_TTree
#include "TTree.h"
#endif

#include "St_DataSet.h"

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include "TROOT.h"
#include "TChain.h"
#include "TTree.h"
#include "TBrowser.h"
#include "TClonesArray.h"
#include "TBenchmark.h"
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "StChain.h"
#include "StMaker.h"
#endif

class TBrowser;
class TChain;
class St_XDFFile; 
class StChain : public StMaker {
private:
   Int_t               m_Version;           //StChain version number
   Int_t               m_VersionDate;       //StChain version date
   St_DataSet         *m_RunSet;            //Run
   TTree              *m_Tree;              //Pointer to the Root tree
   St_XDFFile         *m_File;              //!Pointer to input file 
   St_XDFFile         *m_FileOut;           //!Pointer to output file 
public:
                      StChain();
                      StChain(const char *name);
   virtual           ~StChain();
//BaseUsed   virtual Int_t      Make(int number);
//BaseUsed   virtual Int_t      Make();
   Int_t              GetVersion() {return m_Version;}
   Int_t              GetVersionDate() {return m_VersionDate;}
   virtual void       Clear(Option_t *option="");
   virtual void       PrintInfo();

//		must be in .cxx
   static const char   *GetCVSIdC();

//		must be here in .h
   static const char   *GetCVSIdH()
    {static const char cvs[]="$Id: StChain.h,v 1.26 1999/03/19 20:30:49 perev Exp $";
     return cvs;};


   ClassDef(StChain, 0)   //StChain control class
};


#endif

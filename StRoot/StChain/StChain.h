// $Id: StChain.h,v 1.14 1998/10/31 00:21:31 fisyak Exp $
// $Log: StChain.h,v $
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
#include <TTree.h>
#endif

#include "St_DataSet.h"

#ifndef StMaker_H
#include "StMaker.h"
#endif

#ifndef __CINT__
#include <TROOT.h>
#include <TChain.h>
#include <TTree.h>
#include <TBrowser.h>
#include <TClonesArray.h>
#include <TBenchmark.h>
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "StChain.h"
#include "StMaker.h"
#endif

class TBrowser;
class TChain;
class St_XDFFile; 
//static Char_t      *m_VersionCVS="$Id: StChain.h,v 1.14 1998/10/31 00:21:31 fisyak Exp $";//StChain header CVS version

class StChain : public StMaker {
public:
typedef  enum {kNormal, kDebug} EDebugLevel;
private:
   Int_t               m_Version;           //StChain version number
   Int_t               m_VersionDate;       //StChain version date
   Int_t               m_Run;               //Run number 
   Int_t               m_Event;             //Event event number
   Int_t               m_Mode;              //Run mode
   EDebugLevel         m_DebugLevel;        //Debug level
   St_DataSet         *m_RunSet;            //Run
   St_DataSet         *m_EventSet;          //Event
   TTree              *m_Tree;              //Pointer to the Root tree
   TList              *m_Makers;            //List of Makers
   St_XDFFile         *m_File;              //!Pointer to input file 
   St_XDFFile         *m_FileOut;           //!Pointer to output file 
public:
                      StChain();
                      StChain(const char *name, const char *title="STAR Big Full Chain");
   virtual           ~StChain();
   virtual void       Browse(TBrowser *b);
   virtual void       Draw(Option_t *option="");  // *MENU*
   St_DataSet        *DataSet(){return StMaker::DataSet();}
   St_DataSet        *DataSet(Char_t *makername); // find the maker by name and return its dataset
   EDebugLevel        Debug(){return m_DebugLevel;}
   Int_t              GetVersion() {return m_Version;}
   Int_t              GetVersionDate() {return m_VersionDate;}
   virtual void       Clear(Option_t *option="");
   virtual void       FillClone();
   virtual void       FillXDF(St_XDFFile &file);
   virtual Int_t      Finish();
   virtual Int_t      GetEvent(Int_t event=1);  // *MENU*
   St_DataSet        *GetEventSet(){return m_EventSet;}
   St_XDFFile        *GetXDF_in(){return m_File;} 
   St_XDFFile        *GetXDF_out(){ return m_FileOut;} 
   virtual Int_t Init();
   Bool_t             IsFolder() {return kTRUE;}
   virtual Int_t      Make() {return 0;}
   virtual void       StartMaker(StMaker *mk);
   virtual Int_t      Make(Int_t i);
   virtual void       EndMaker  (StMaker *mk,Int_t iret);
   virtual void       Paint(Option_t *option="");
   virtual void       PrintInfo();
   void               SetDebug(EDebugLevel debug){m_DebugLevel = debug;}
   virtual void       SetDefaultParameters();
   virtual void       SetInputXDFile(St_XDFFile *file) {m_File = file;}
   virtual void       SetOutputXDFile(St_XDFFile *file) {m_FileOut = file;}
   virtual St_XDFFile *XDFFile() {return m_File;}
   virtual void	      Fatal(int Ierr, const char *Com);  

   TList             *Makers()    {return m_Makers;}
   StMaker           *Maker(const char *name) {return (StMaker*)m_Makers->FindObject(name);}
   TTree             *Tree() {return m_Tree;}

   Int_t             Run()   {return m_Run;}
   Int_t             Event() {return m_Event;}
   Int_t             Mode()  {return m_Mode;}

//    Setters for flags and switches

   virtual void   SetRun(Int_t run=1)     {m_Run=run;}
   virtual void   SetEvent(Int_t event=1) {m_Event=event;}
   
   virtual void   SetMode(Int_t mode=0)   {m_Mode=mode;}

   void           FillTree();
   void           InitChain(TChain *chain);
   virtual void   MakeBranch();   
   void           MakeTree(const char* name="T", const char*title="StChain tree");
   void           SortDown(Int_t n, Float_t *a, Int_t *index, Bool_t down=kTRUE);

   ClassDef(StChain, 1)   //StChain control class
};

EXTERN StChain *gStChain;

#endif

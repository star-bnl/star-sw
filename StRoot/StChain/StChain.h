// $Id: StChain.h,v 1.24 1999/03/04 02:26:10 fisyak Exp $
// $Log: StChain.h,v $
// Revision 1.24  1999/03/04 02:26:10  fisyak
// Add read Chain
//
// Revision 1.23  1999/03/04 00:01:14  fisyak
// Add access to Chain Version
//
// Revision 1.22  1999/02/22 02:21:49  fisyak
// Add GetGeometry
//
// Revision 1.21  1999/02/20 18:48:56  fisyak
// Add event/run information to Chain
//
// Revision 1.20  1999/01/20 23:44:47  fine
// The special Input/Output makers and the static variable StChain::g_Chain have been introduced
//
// Revision 1.19  1999/01/02 19:08:12  fisyak
// Add ctf
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
#include "TDatime.h"
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
class StHeader;
class StDisplay;
class StMagF;
class St_XDFFile; 

class StChain : public StMaker {
public:
typedef  enum {kNormal, kDebug} EDebugLevel;
private:
   const Char_t       *m_VersionCVS;        //StChain header CVS version
   const Char_t       *m_VersionTag;        //StChain tag
   Int_t               m_Version;           //StChain version number
   Int_t               m_VersionDate;       //StChain version date
   Int_t               m_Run;               //Run number 
   Int_t               m_Event;             //Event event number
   Int_t               m_Mode;              //Run mode -- Event generator accoringly Ron numenclature 
   TString             m_EvenType;          //Event type: Au+Au, p+p, laser..
   Float_t             m_BImpact;           //Impact parameter
   Float_t             m_PhImpact;          //Phi angle of impact
   TDatime             m_DateTime;          //Run date
   ULong_t             mAwest;
   ULong_t             mAeast;
   Float_t             mCenterOfMassEnergy; //
   ULong_t             mBunchCrossingNumber;//
   ULong_t             mTriggerMask;        //
   TDatime             mTimeStapm;          //time stamp for event
   TDatime             mProcessTime;        //time of event processing
   EDebugLevel         m_DebugLevel;        //Debug level
   TTree              *m_Tree;              //!Pointer to the Root tree
   TList              *m_Makers;            //!List of Makers
   St_XDFFile         *m_File;              //!Pointer to input file 
   St_XDFFile         *m_FileOut;           //!Pointer to output file 
   StHeader           *fHeader;             //!Header information
   Int_t               fNtrack;             //Number of tracks
   StDisplay          *fDisplay;            //!Pointer to event display
   TGeometry          *fGeometry;           //!Pointer to geometry
   TObjArray          *fDetectors;          //!List of Detectors
   StMagF             *fField;              //!Magnetic Field Map
public:
                      StChain();
                      StChain(const char *name, const char *title="STAR Big Full Chain");
   virtual           ~StChain();
   virtual void       Browse(TBrowser *b);
   virtual void       Draw(Option_t *option="");  // *MENU*
   St_DataSet        *DataSet()  const {return StMaker::DataSet();}
   St_DataSet        *DataSet(const Char_t *makername, const Char_t *path="") const ; // find the maker by name and return its dataset
   EDebugLevel        Debug(){return m_DebugLevel;}
   TGeometry         *GetGeometry();
   StHeader          *GetHeader() {return fHeader;} 
   Int_t              GetNtrack() {return fNtrack;}
   Int_t              GetVersion() {return m_Version;}
   Int_t              GetVersionDate() {return m_VersionDate;}
   virtual void       Clear(Option_t *option="");
   virtual void       CleanDetectors();
   TObjArray         *Detectors() const {return fDetectors;}
   virtual void       FillClone();
   virtual void       FillXDF(St_XDFFile &file);
   virtual Int_t      Finish();
   virtual Int_t      GetEvent(Int_t event=1);  
   St_XDFFile        *GetXDF_in(){return m_File;} 
   St_XDFFile        *GetXDF_out(){ return m_FileOut;} 
   virtual Int_t Init();
   Bool_t             IsFolder() {return kTRUE;}
   virtual Int_t      Make() {return 0;}
   virtual void       StartMaker(StMaker *mk);
   virtual Int_t      Make(Int_t i);
   virtual void       MakeDoc(const TString &stardir="$(afs)/rhic/star/packages/dev",const TString &outdir="$(star)/StRoot/html");
   virtual StMagF    *MagF() {return fField;}
   virtual void       EndMaker  (StMaker *mk,Int_t iret);
   virtual void       Paint(Option_t *option="");
   virtual void       PrintInfo();
   virtual void       ResetPoints();
   virtual void       ResetHits();
   virtual void       SetDebug(EDebugLevel debug){m_DebugLevel = debug;}
   virtual void       SetDefaultParameters();
   virtual void       SetDisplay(StDisplay *display) {fDisplay = display;}
   virtual void       SetInputXDFile(St_XDFFile *file) {m_File = file;}
   virtual void       SetOutputXDFile(St_XDFFile *file) {m_FileOut = file;}
   virtual void       SetMagF (StMagF *f){fField = f;}
   virtual St_XDFFile *XDFFile() {return m_File;}
   virtual void	      Fatal(int Ierr, const char *Com);  

   TList             *Makers()    {return m_Makers;}
   StMaker           *Maker(const char *name) {return (StMaker*)m_Makers->FindObject(name);}
   TTree             *Tree() {return m_Tree;}

   Int_t             Run()   {return m_Run;}
   Int_t             Event() {return m_Event;}
   TString          *EvenType() {return &m_EvenType;}
   Float_t           BImpact(){return m_BImpact;}
   Float_t           PhImpact(){return m_PhImpact;}
   Int_t             Mode()  {return m_Mode;}
   Float_t           CenterOfMassEnergy(){return mCenterOfMassEnergy;}
   Int_t             Date() {return m_DateTime.GetDate();}
   Int_t             Time() {return m_DateTime.GetTime();}
   Int_t             ProcessDate() {return mProcessTime.GetDate();}
   Int_t             ProcessTime() {return mProcessTime.GetTime();}
   ULong_t           Awest(){return mAwest;}
   ULong_t           Aeast() {return mAeast;}
   const Char_t     *VersionCVS() {return m_VersionCVS;}
   const Char_t     *VersionTag() {return m_VersionTag;}
//    Setters for flags and switches

   virtual void   SetBranches();
   virtual void   SetRun(Int_t run=1)     {m_Run=run;} // *MENU*
   virtual void   SetEvent(Int_t event=1) {m_Event=event;} // *MENU*
   virtual void   SetEvenType(Char_t const *type="Unknown") {m_EvenType = type;} // *MENU*
   virtual void   SetBImpact(Float_t b=0) {m_BImpact=b;} // *MENU*
   virtual void   SetPhImpact(Float_t Phi=0) {m_PhImpact=Phi;} // *MENU*
   virtual void   SetMode(Int_t mode=0)   {m_Mode=mode;} // *MENU*
   virtual void   SetCenterOfMassEnergy(Float_t s=0){mCenterOfMassEnergy=s;} // *MENU*
   virtual void   SetDateTime(Int_t date=0, Int_t time=0) {m_DateTime.Set(date,time);} // *MENU*
   virtual void   SetProcessDateTime(Int_t date, Int_t time) {mProcessTime.Set(date,time);} // *MENU*
   virtual void   SetProcessDateTime() {mProcessTime.Set();} 
   virtual void   SetAwest(ULong_t a=1) {mAwest = a;} // *MENU*
   virtual void   SetAeast(ULong_t a=1) {mAeast = a;} // *MENU*
 
   virtual void   SetTree(TTree *tree)   {m_Tree=tree;}
   
   Int_t          FillTree();
   void           InitChain(TChain *chain);
   virtual void   MakeBranch();   
   virtual TTree *MakeTree(const char* name="T", const char*title="StChain tree");
   void           SortDown(Int_t n, Float_t *a, Int_t *index, Bool_t down=kTRUE);

   ClassDef(StChain, 1)   //StChain control class
};

EXTERN StChain *gStChain;

#endif

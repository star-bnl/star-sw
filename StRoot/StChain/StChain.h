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

#ifndef ROOT_TTree
#include <TTree.h>
#endif

#include "St_DataSet.h"

#ifndef StMaker_H
#include "StMaker.h"
#endif


class TBrowser;
class TChain;
class St_XDFFile; 

class StChain : public TNamed {

private:
   Int_t               m_Version;           //StChain version number
   Int_t               m_VersionDate;       //StChain version date
   Int_t               m_Run;               //Run number
   Int_t               m_Event;             //Event number
   Int_t               m_Mode;              //Run mode
   St_DataSet         *m_DataSet;           //The main chain dataset structure
   St_DataSetIter     *m_RunIter;           //The parameters
   St_DataSetIter     *m_CalibIter;         //The calibration       
   St_DataSetIter     *m_EventIter;         //The Event (fruits)
   TTree              *m_Tree;              //Pointer to the Root tree
   TList              *m_Makers;            //List of Makers
   St_XDFFile         *m_File;              //!Pointer to input file 

public:
                      StChain();
                      StChain(const char *name, const char *title="STAR Big Full Chain");
   virtual           ~StChain();
   virtual void       Browse(TBrowser *b);
   virtual void       Draw(Option_t *option="");  // *MENU*
   St_DataSet        *DataSet(){return m_DataSet;}
   St_DataSet        *DataSet(Char_t *makername); // find the maker by name and return its dataset
   Int_t              GetVersion() {return m_Version;}
   Int_t              GetVersionDate() {return m_VersionDate;}
   virtual void       Clear(Option_t *option="");
   virtual void       FillClone();
   virtual void       FillXDF(St_XDFFile &file);
   virtual void       Finish();
   virtual void       GetEvent(Int_t event=1);  // *MENU*
   St_DataSetIter    *GetRunIter() {return m_RunIter;}
   St_DataSetIter    *GetCalibIter() {return m_CalibIter;}
   virtual void       Init();
   Bool_t             IsFolder() {return kTRUE;}
   virtual Int_t      Make(Int_t i=0);
   virtual void       Paint(Option_t *option="");
   virtual void       PrintInfo();
   virtual void       SetDefaultParameters();
   virtual void       SetInputXDFile(St_XDFFile *file) {m_File = file;}
   void               SetRunIter(St_DataSetIter *run) {m_RunIter = run;}
   void               SetCalibIter(St_DataSetIter *calib) {m_CalibIter = calib;}

   virtual St_XDFFile *XDFFile() {return m_File;}

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
   void           MakeTree(const char* name="T", const char*title="StChain tree");

   void           SortDown(Int_t n, Float_t *a, Int_t *index, Bool_t down=kTRUE);

   ClassDef(StChain, 1)   //StChain control class
};

EXTERN StChain *gStChain;

#endif

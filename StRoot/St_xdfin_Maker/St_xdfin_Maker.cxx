//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker class for Makers                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_xdfin_Maker.h"
#include "St_particle_Table.h"
#include "St_XDFFile.h"

ClassImp(St_xdfin_Maker)

//_____________________________________________________________________________
  St_xdfin_Maker::St_xdfin_Maker(const char *name, const char *inputFile):
    StIOInterFace(name),
    m_InitDone(0)
{
  if (inputFile && inputFile[0]) SetFile(inputFile);
  fEvtHddr = new StEvtHddr(m_ConstSet);
  SetOutput(fEvtHddr);	//Declare this "EvtHddr" for output
}
//_____________________________________________________________________________
St_xdfin_Maker::~St_xdfin_Maker(){
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Init()
{
// 		Get run parameters from input file
  return Open();  
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Open(const char*)
{
  if (m_InitDone) return 0;
  int iret = 0;
  m_InitDone=0;
  printf("*** St_xdfin_Maker::Init:  Open Input file %s ***\n",GetFile());
  iret = fXdfin.OpenXDF(GetFile(),"r");
  if (iret) return iret;
  while (!m_InitDone && !iret) iret = Make();
  return iret;
}
//_____________________________________________________________________________
void St_xdfin_Maker::Close(Option_t *)
{
  Clear();
  m_ConstSet->Delete();
  m_InitDone=0;
  fXdfin.CloseXDF();
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Make(){
  const char Skip[] = "ROSIE_RESET SLOW_CONTROL";
  const char Cons[] = "run BEGIN_RUN";
  const char Data[] = "event Event dst TPC_DATA";
  TString dsname, curdirname;
  const char *mkdir = 0;
  St_DataSet *curdir = 0, *set = 0;
  Bool_t      CONST;
  PrintInfo();

  St_DataSetIter local(m_DataSet);
  St_DataSetIter cons(m_ConstSet);

  while((set = fXdfin.ReadEvent())) {
    local.Reset();
    cons.Reset();
    dsname = set->GetName();

    if (strstr(Skip,dsname)) {//	Skip dataset
      printf("%s: Record %s is SKIPPED\n",GetName(),(const char*)dsname);
      delete set; continue;
    }
      
    mkdir = 0;
    CONST = kFALSE;
    if (!strcmp("dst",dsname)) {
      if (!m_InitDone) 
      {
        mkdir = "run";  CONST = kTRUE;
      }else {
        mkdir = "event/data/global"; 
        SetOutput("dst",".data/event/data/global/dst");
        SetOutput("DST",".data/event/data/global/dst");
      }  
    }
    if (!strcmp("run",dsname)) 		{                     CONST = kTRUE;}
    if (!strcmp("Run",dsname)) 		{mkdir = "run/geant"; CONST = kTRUE;}
    if (!strcmp("params",dsname)) 	{mkdir = "run";       CONST = kTRUE;}
    if (!strcmp("BEGIN_RUN",dsname))	{                     CONST = kTRUE;}
    if (!strcmp("Event",dsname)) 	 mkdir = "event/geant";
    if (CONST) {
      if (mkdir) {cons.Mkdir(mkdir); cons.Cd(mkdir);}
      curdir = cons.Mkdir(dsname);
    }
    else {
      if (mkdir) {local.Mkdir(mkdir); local.Cd(mkdir);}
      curdir = local.Mkdir(dsname);
    }
    if (GetDebug()) set->ls(0);
    curdir->Update(set); if(GetDebug()) curdir->ls(0);
    curdir->Purge();     if(GetDebug()) curdir->ls(0);
    SafeDelete(set);
    if (CONST) {if (!m_InitDone) {m_InitDone = CONST; return kStOK;} else continue;}
    if (strstr(Data,dsname)) return kStOK;
    SafeDelete(curdir);
  }
  return kStEOF;
}
//_____________________________________________________________________________
void St_xdfin_Maker::PrintInfo(){
  if (GetDebug()) printf("St_xdfin_Maker\n"); //  %s %s \n",GetName(), GetTitle());
}
//_____________________________________________________________________________
void St_xdfin_Maker::Skip(Int_t Nskip){
  St_DataSet *set;
  for (Int_t i =1; i<= Nskip; i++){
    if (GetDebug()) printf("St_xdfin_Maker skip record %i\n",i); 
    set = fXdfin.ReadEvent();
    SafeDelete (set);
  }
}

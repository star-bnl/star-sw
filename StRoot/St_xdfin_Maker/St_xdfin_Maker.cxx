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
St_xdfin_Maker::St_xdfin_Maker(const char *name, const char *inputFile):StMaker(name)
{fFileName = inputFile;m_InitDone=0;}
//_____________________________________________________________________________
St_xdfin_Maker::~St_xdfin_Maker(){
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Init(){
// Get run parameters from input file
  
  printf("*** St_xdfin_Maker::Init:  Open Input file %s ***\n",(const char*)fFileName);
  return fXdfin.OpenXDF(fFileName,"r");
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Make(){
  const char Skip[] = "ROSIE_RESET SLOW_CONTROL BEGIN_RUN";
  TString dsname;
  const char *mkdir = 0;
  St_DataSet *curdir,*set;
  
  PrintInfo();

  St_DataSetIter local(m_DataSet);
  Int_t  ret = kStErr;


  while((set = fXdfin.ReadEvent())) {
    local.Reset();
    dsname = set->GetName();

    if (strstr(Skip,dsname)) {//	Skip dataset
      printf("%s: Record %s is SKIPPED\n",GetName(),(const char*)dsname);
      delete set; continue;}
      
    mkdir = 0;
    if (!strcmp("dst",dsname)) 		mkdir = (m_InitDone) ? "run" :"event/data/global";
    if (!strcmp("Run",dsname)) 		mkdir = "run/geant";      
    if (!strcmp("BEGIN_RUN",dsname))	mkdir = "tpc";
    if (!strcmp("Event",dsname)) 	mkdir = "event/geant";
    
    if (mkdir) {local.Mkdir(mkdir); local.Cd(mkdir);}
    curdir = local.Mkdir(dsname);
    if (GetDebug()) set->ls(0);
    curdir->Update(set); if(GetDebug()) curdir->ls(0);
    curdir->Purge();     if(GetDebug()) curdir->ls(0);
    if (!m_InitDone) m_InitDone = (strstr("run Run BEGIN_RUN",dsname)!=0);
    delete set;
    if (strstr("event Event",dsname)) return kStOK;
  }
  return ret;
}
//_____________________________________________________________________________
void St_xdfin_Maker::PrintInfo(){
  if (GetDebug()) printf("St_xdfin_Maker\n"); //  %s %s \n",GetName(), GetTitle());
}

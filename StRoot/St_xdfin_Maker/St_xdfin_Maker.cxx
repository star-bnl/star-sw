//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "St_xdfin_Maker.h"
#include "St_particle_Table.h"
#include "StChain.h"
#include "St_XDFFile.h"
ClassImp(St_xdfin_Maker)

//_____________________________________________________________________________
St_xdfin_Maker::St_xdfin_Maker(){}
//_____________________________________________________________________________
St_xdfin_Maker::St_xdfin_Maker(const char *name, const char *title):StMaker(name,title){}
//_____________________________________________________________________________
St_xdfin_Maker::~St_xdfin_Maker(){
  m_DataSet = 0;//SafeDelete(m_DataSet); 
}
//_____________________________________________________________________________
void St_xdfin_Maker::Clear(Option_t *option){
  m_DataSet = 0;//SafeDelete(m_DataSet); 
}

//_____________________________________________________________________________
void St_xdfin_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_xdfin_Maker::Init(){
// Get run parameters from input file
  St_DataSet *set = gStChain->XDFFile()->NextEventGet(); 
  if (set) {
    if (strcmp(set->GetName(),"run")==0){ 
      St_DataSet *RunSet = gStChain->GetRun();
      SafeDelete(RunSet);
      gStChain->DataSet()->Add(set); 
    }
    else {// GEANT type of events
      if (strcmp(set->GetName(),"Run")==0){
        St_DataSetIter local( gStChain->DataSet());
        St_DataSet *RunSet = local.Mkdir("run");
        St_DataSet *geant = local.Mkdir("run/geant");
        geant->Add(set);
      }
      else {//Raw data format
        if (strcmp(set->GetName(),"BEGIN_RUN")==0){
          St_DataSetIter local(gStChain->GetParams());
          St_DataSet *tpc = local("tpc");
          if (!tpc) tpc = local.Mkdir("tpc");
          tpc->Add(set);
        }   
        else {
          Warning("Init","The first record has no \"run / Run\" dataset");
          delete set;
  	}
      }
    }
  }

// Create Histograms    
// Registrate the Maker
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Make(){
  PrintInfo();
   St_DataSet *set = gStChain->XDFFile()->NextEventGet();
   if (set){
     const Char_t *makertype = GetTitle();
     const Char_t *type = 0;
     St_DataSetIter    top(gStChain->DataSet());
     top.Cd(gStChain->GetName());
     if (makertype && strlen(makertype)&& strcmp(set->GetName(),"Event")==0) {

     }
     if (makertype && strlen(makertype)) {
       St_DataSet       *topset = top(makertype);
       if (!topset) {
         top.Mkdir(makertype); 
       }
       St_DataSetIter    parent(top(makertype));
       parent.Add(set);
     }
     else {
       if (strcmp(set->GetName(),"Event")==0){ 
         St_DataSetIter    top(gStChain->DataSet());
       }
     }
   }
  return set ? 0:-1;
}
//_____________________________________________________________________________
void St_xdfin_Maker::PrintInfo(){
  if (gStChain->Debug()) printf("St_xdfin_Maker\n"); //  %s %s \n",GetName(), GetTitle());
}


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
 if (m_DataSet) delete m_DataSet;
 m_DataSet = 0;
}
//_____________________________________________________________________________
void St_xdfin_Maker::Clear(Option_t *option){
  if (m_DataSet) {delete m_DataSet; m_DataSet = 0;}
}

//_____________________________________________________________________________
void St_xdfin_Maker::Finish(){ 
 Clear();
}
//_____________________________________________________________________________
void St_xdfin_Maker::Init(){
// Create tables
// Create Histograms    
  
// Registrate the Maker
   StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_xdfin_Maker::Make(){
  PrintInfo();
  m_DataSet = gStChain->XDFFile()->NextEventGet();
#if 0
  St_DataSet *set = gStChain->XDFFile()->NextEventGet();
  St_DataSetIter next(set);
  if (strlen(GetTitle()) )   m_DataSet = set;
  else if ( next(GetTitle())) m_DataSet = set;
  else {
    m_DataSet = new St_DataSet(GetTitle());
    m_DataSet->Add(set);
  }
#endif
  return m_DataSet ? 0:-1;
}
//_____________________________________________________________________________
void St_xdfin_Maker::PrintInfo(){
  if (gStChain->Debug()) printf("St_xdfin_Maker\n"); //  %s %s \n",GetName(), GetTitle());
}


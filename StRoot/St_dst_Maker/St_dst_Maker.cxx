// $Id: St_dst_Maker.cxx,v 1.14 1999/06/15 18:39:12 fisyak Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.14  1999/06/15 18:39:12  fisyak
// shunt particle to dst
//
// Revision 1.13  1999/05/04 21:00:43  fisyak
// Step back to MDC2 version
//
// Revision 1.12  1999/05/03 01:39:22  fisyak
// Remove tables from DST, add access to different makers
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TClass.h"
#include "St_dst_Maker.h"
static const char rcsid[] = "$Id: St_dst_Maker.cxx,v 1.14 1999/06/15 18:39:12 fisyak Exp $";
ClassImp(St_dst_Maker)
  
  //_____________________________________________________________________________
  St_dst_Maker::St_dst_Maker(const char *name):StMaker(name){
  fSelect = 0;
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}

//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
  static const char *todst[] = {
    
    "global:",	"dst",
    "geant:", 	"particle", "g2t_rch_hit",
    "trg:", 	"dst_TriggerDetectors",
    0};
  
  if (!fSelect) fSelect = todst;   
  
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){

  St_DataSet *ds=0,*mk=0, *dst=0;
  const char *name,*mkname;
  
  
  for (int idst=0; (name=fSelect[idst]); idst++) {
    
    if (strchr(name,':')) {
      mkname = name; mk = GetInputDS(name); continue;}
    
    if (!mk) continue;
    ds = mk->Find(name);
    if (!ds) continue;
    dst = m_DataSet;
    if (strcmp(name,"dst")) dst = m_DataSet->Find("dst");
    if (dst) ds->Shunt(dst);
    if (Debug()) printf("\n*** <%s::Make> *** selected %s%s\n",ClassName(),mkname,name);
  }
  return kStOK;
}
//_____________________________________________________________________________
void St_dst_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_dst_Maker.cxx,v 1.14 1999/06/15 18:39:12 fisyak Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

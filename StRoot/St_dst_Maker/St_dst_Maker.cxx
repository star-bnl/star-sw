// $Id: St_dst_Maker.cxx,v 1.9 1999/03/11 03:12:17 perev Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.9  1999/03/11 03:12:17  perev
// new schema
//
// Revision 1.8  1999/02/26 02:31:55  fisyak
// Replace emc hits by emc raw tables
//
// Revision 1.7  1999/02/23 02:09:02  fisyak
// Add emc hits to dst
//
// Revision 1.6  1999/02/20 18:49:16  fisyak
// Add event/run information
//
// Revision 1.5  1999/02/19 17:37:42  fisyak
// Add RICH hits to dst
//
// Revision 1.4  1999/02/19 17:35:47  fisyak
// Add RICH hits to dst
//
// Revision 1.2  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.1  1999/01/02 19:09:22  fisyak
// Add Clones
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TClass.h"
#include "St_dst_Maker.h"
#include "St_ObjectSet.h"
#include "St_DataSetIter.h"

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

  St_DataSet *ds=0,*mk=0;
  const char *name,*mkname;
  
 
  for (int idst=0; (name=fSelect[idst]); idst++) {
  
    if (strchr(name,':')) {
      mkname = name; mk = GetInputDS(name); continue;}

    if (!mk) continue;
    ds = mk->Find(name);
    if (!ds) continue;
    ds->Shunt(m_DataSet);
    if (Debug()) printf("\n*** <%s::Make> *** selected %s%s\n",ClassName(),mkname,name);
  }
  
  return kStOK;
}
//_____________________________________________________________________________
void St_dst_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_dst_Maker.cxx,v 1.9 1999/03/11 03:12:17 perev Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}


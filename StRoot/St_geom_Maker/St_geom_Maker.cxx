//*-- Author :    Valery Fine   29/06/99  (E-mail: fine@bnl.gov)
// $Id: St_geom_Maker.cxx,v 1.1 1999/06/29 20:50:33 fine Exp $
// $Log: St_geom_Maker.cxx,v $
// Revision 1.1  1999/06/29 20:50:33  fine
// Maker to provide a St_node geom structure for others
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geom_Maker class for Makers                                        //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "St_geom_Maker.h"

#include "TWebFile.h"
#include "TGeometry.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_Node.h"
ClassImp(St_geom_Maker)

//_____________________________________________________________________________
St_geom_Maker::St_geom_Maker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
St_geom_Maker::~St_geom_Maker(){
  
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Init() {
//--
//  reading STAR GEANT geometry database
//--
  TWebFile f(StrDup("http://www.star.bnl.gov/~fine/star.root"));
  // read STAR geometry database remotely
  TGeometry *star = (TGeometry *)f.Get("STAR");
  if (!star) {
    printf("Sorry, STAR was not found !\n");
    return kStErr;;
  }
//--
// Remove hall from the list of ROOT nodes
// to make it free of ROOT control
//--

  TList *listOfNode = gGeometry->GetListOfNodes();
  St_Node *hall =  (St_Node *)listOfNode->First();
  // Remove hall from the list of ROOT nodes to make it free of ROOT control
  listOfNode->Remove(hall);
  listOfNode->Remove(hall);
// Add "hall" into ".const" area of this maker
   AddConst(hall);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Make(){
//  PrintInfo();

 return kStOK;
}
//_____________________________________________________________________________
void St_geom_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_geom_Maker.cxx,v 1.1 1999/06/29 20:50:33 fine Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}


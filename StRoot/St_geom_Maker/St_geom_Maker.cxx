//*-- Author :    Valery Fine   29/06/99  (E-mail: fine@bnl.gov)
// $Id: St_geom_Maker.cxx,v 1.2 1999/06/30 02:50:12 fine Exp $
// $Log: St_geom_Maker.cxx,v $
// Revision 1.2  1999/06/30 02:50:12  fine
// Some comments
//
// Revision 1.1  1999/06/29 20:50:33  fine
//  Maker to provide a St_node geom structure for others
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geom_Maker class for Makers                                       //
//                                                                      //
// This maker is to initialize the STAR GEANT geometry description.     //
// "Geometry description" is an instance of the St_Node class derived   //
// from St_DataSet class.                                               //
//                                                                      //
// This means one can apply St_DataSetIter and TBrowser to navigate it  //
// One can get a pointer to the top level St_Node object named "HALL"   //
//                               via                                    //
// St_Node *hallNode = (St_Node *)StMaker::GetDataSet("HALL") method    //
//                                                                      //
// It is assumed the St_geom_Maker object was instantiated and          //
// initialized by  the the top level StChain object                     //
// (see $STAR/StRoot/macros/graphics/PadBrowser.C macro as a pattern    //
//                                                                      //
// To get the ROOT Browser and graphics view of the full structure      //
// one try:                                                             //
// ____________________________________________________________________ //
//                                                                      //
//  root [0] gSystem->Load("St_base");
//  root [1] gSystem->Load("StChain");
//  root [2] gSystem->Load("St_geom_Maker");
//  root [3] St_geom_Maker geomMaker;
//  root [4] geomMaker.Init();
//  
//  root [5] TBrowser b("HALL",geomMaker.GetDataSet("HALL"));
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/geomMakerBrowse.gif" ></P> end_html
//  root [6] ((St_Node *)geomMaker.GetDataSet("HALL"))->Draw();
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/geomMakerPad.gif" ></P> end_html
//  root [7] // One can two extra lines to get "Control Panel"
//  root [8] .x PadControlPanel.C
// begin_html  <P ALIGN=CENTER> <IMG SRC="gif/PadControlPanel.gif" ></P> end_html
//  root [9] // Plot 3D axice as follows:
//  root [10] St_PolyLine3D::Axis();
// ____________________________________________________________________ //
//
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
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
  printf("* $Id: St_geom_Maker.cxx,v 1.2 1999/06/30 02:50:12 fine Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}


//*-- Author :    Valery Fine   29/06/99  (E-mail: fine@bnl.gov)
// $Id: St_geom_Maker.cxx,v 1.14 2017/04/26 21:04:29 perev Exp $
// $Log: St_geom_Maker.cxx,v $
// Revision 1.14  2017/04/26 21:04:29  perev
// Cleanup
//
// Revision 1.13  2009/01/21 18:58:59  fine
// Draw the TGeoCompoisteShapes
//
// Revision 1.12  2007/04/19 23:27:58  fine
// replace printf with LOG macro
//
// Revision 1.11  2001/06/01 03:04:25  perev
// overloaded GetDataSet -> FindDataSet
//
// Revision 1.10  1999/11/21 01:40:48  fine
// Temporary mark TPC and SVT modes unless Pavel does it
//
// Revision 1.9  1999/11/12 18:25:51  fine
// Take in account GEANT maker
//
// Revision 1.8  1999/07/16 15:23:17  fisyak
// Switch TWebFile => TFile
//
// Revision 1.7  1999/07/15 13:58:10  perev
// cleanup
//
// Revision 1.6  1999/07/13 00:52:52  fine
// Some corrections
//
// Revision 1.5  1999/07/02 20:04:11  fine
// Init() can not be called from StDataSet since the last os const
//
// Revision 1.4  1999/07/02 20:01:21  fine
// The name of the maker is the geom file name
//
// Revision 1.3  1999/06/30 16:27:41  fine
// Comments make up
//
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
//  root [7] // One can add two extra lines to get "Control Panel"
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
#include "TFile.h"
#include "TGeometry.h"
#include "TSystem.h"
#include "TTUBE.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_Node.h"
ClassImp(St_geom_Maker)

//_____________________________________________________________________________
St_geom_Maker::St_geom_Maker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
St_geom_Maker::~St_geom_Maker()
{
}
//_____________________________________________________________________________
St_DataSet  *St_geom_Maker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const 
{
  St_DataSet *ds = StMaker::FindDataSet(logInput,uppMk,dowMk);
  return ds;
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Init() {
//--
//  reading STAR GEANT geometry database
//--
  PrintInfo();
#if 0
  TString fileName = "http://www.star.bnl.gov/~fine/";
  fileName += GetName();
  TWebFile *f =  new TWebFile(fileName.Data());
#endif
  // Check whether GEANT maker is present
  if (GetMaker("geant"))  return StMaker::Init();

  TString fileName = "$STAR/StDb/geometry/star.root.y1a";
  gSystem->ExpandPathName(fileName);
  TFile    *f = new TFile(fileName);
  // read STAR geometry database remotely
  TGeometry *star = (TGeometry *)f->Get("STAR");
  star->SetName("STARGEOMNODE");
  if (!star) {
    LOG_ERROR << "Sorry, STAR was not found !" << endm;
    return kStErr;
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
//  Artifact to be deleted just Pavel marks all volumes proeprly
// ---  Create "standard" TPC and SVT views ----

  // Create an iterator to navigate STAR geometry
  St_DataSetIter volume(hall,0);
  St_Node *sector = 0;
  const Char_t *volueNames[] = {"TPSS","SLDI","SFDM"};
  const Int_t lvolueNames = sizeof(volueNames)/sizeof(Char_t *);
  while ( (sector = ( St_Node *)volume()) ){
    Bool_t found = kFALSE;
    Int_t i;
    for (i =0; i < lvolueNames; i++) 
    if (strcmp(sector->GetName(),volueNames[i]) == 0 ) {found = kTRUE; break; }
    if (found) {
      sector->SetVisibility(St_Node::kSonUnvisible);
      sector->Mark();
      if (!i) {  // special case for TPSS sectors
        TTUBE *tubs = (TTUBE *)sector->GetShape();
        tubs->SetNumberOfDivisions(1);
      }
    }
  }

// Add "hall" into ".const" area of this maker
   AddConst(hall);
   if (Debug()) hall->ls(3);

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Make(){
//  PrintInfo();

 return kStOK;
}
//_____________________________________________________________________________


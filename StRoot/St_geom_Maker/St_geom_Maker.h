//*-- Author :    Valery Fine   29/06/99  (E-mail: fine@bnl.gov)
// $Id: St_geom_Maker.h,v 1.9 2014/08/06 11:43:55 jeromel Exp $
// $Log: St_geom_Maker.h,v $
// Revision 1.9  2014/08/06 11:43:55  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.8  2003/09/10 19:47:47  perev
// ansi corrs
//
// Revision 1.7  2001/06/01 03:04:25  perev
// overloaded GetDataSet -> FindDataSet
//
// Revision 1.6  2000/03/28 19:28:09  fine
// Adjusted to ROOT 2.24
//
// Revision 1.5  1999/07/15 13:58:10  perev
// cleanup
//
// Revision 1.4  1999/07/13 00:52:49  fine
// Some corrections
//
// Revision 1.3  1999/07/02 20:01:22  fine
// The name of the maker is the geom file name
//
// Revision 1.2  1999/06/30 16:27:43  fine
// Comments make up
//
// Revision 1.1  1999/06/29 20:50:34  fine
//  Maker to provide a St_node geom structure for others
//

#ifndef STAR_St_geom_Maker
#define STAR_St_geom_Maker

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

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "St_DataSet.h"

class St_geom_Maker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: St_geom_Maker.h,v 1.9 2014/08/06 11:43:55 jeromel Exp $";
 
 protected:
   virtual St_DataSet  *FindDataSet (const char* logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;
 public: 
                  St_geom_Maker(const char *name="star.root");
   virtual       ~St_geom_Maker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual void  SetGeomFileName(const Char_t *fileName){SetName(fileName);} 
   virtual const Char_t *GetGeomFileName(){ return GetName();}
// virtual void Set_mode       (Int_t   m =      2){m_mode       = m;} // *MENU*
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_geom_Maker.h,v 1.9 2014/08/06 11:43:55 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(St_geom_Maker,0)   //StAF chain virtual base class for Makers
};

#endif

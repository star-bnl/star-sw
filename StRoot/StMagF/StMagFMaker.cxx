// $Id: StMagFMaker.cxx,v 1.5 2000/06/26 22:11:52 fisyak Exp $
// $Log: StMagFMaker.cxx,v $
// Revision 1.5  2000/06/26 22:11:52  fisyak
// remove params
//
// Revision 1.4  2000/06/19 12:49:36  fisyak
// Resolve ambiguity between geometry/agufld and StMagF agufld by renaming agufld => lovefield
//
// Revision 1.3  2000/03/15 21:49:59  fisyak
// Change to RunLog
//
// Revision 1.2  2000/01/07 00:42:33  fisyak
// merge Make with Init
//
// Revision 1.1  2000/01/04 20:44:40  fisyak
// Add StMagFMaker
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMagFMaker class for Makers                                        //
//                                                                      //
// This commented block at the top of the source file is considered as  //
// this class description to be present on the this class Web page.     //
//  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html                    //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StMagFMaker.h"
#include "StChain.h"
#include "TDataSetIter.h"
#include "StMagF/StMagF.h"
#include "tables/St_MagFactor_Table.h"

ClassImp(StMagFMaker)
  
  //_____________________________________________________________________________
StMagFMaker::StMagFMaker(const char *name):StMaker(name),fMagFactor(0),fMagF(0),fScale(1){}
//_____________________________________________________________________________
StMagFMaker::~StMagFMaker(){}
//_____________________________________________________________________________
Int_t StMagFMaker::Init(){
  if (!fMagFactor) {
    TDataSet *RunLog = GetDataBase("RunLog");
    fMagFactor = (St_MagFactor *) RunLog->Find("MagFactor"); assert(fMagFactor);
  }
  Float_t Scale = (*fMagFactor)[0].ScaleFactor;
  if (Scale == fScale && fMagF) return kStOK;
  fScale = Scale;
  if (fMagF) {
    cout << "Reset STAR magnetic field with scale factor " << fScale << endl;
    fMagF->SetFactor(fScale);
    return kStOK;
  }
  if (!m_Mode) fMagF = new StMagFCM("Star Full Field",
				    "$STAR/StarDb/StMagF/bfp112.map",
				    kConMesh,fScale);
  else         fMagF = new StMagFC("Star Constant Field","Constant Field",fScale);
  cout << "Initialize STAR magnetic field with scale factor " << fScale << endl;
  return StMaker::Init();
}
//_____________________________________________________________________________











// $Id: StMagFMaker.cxx,v 1.1 2000/01/04 20:44:40 fisyak Exp $
// $Log: StMagFMaker.cxx,v $
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
#include "St_DataSetIter.h"
#include "StMagF/StMagF.h"
#include "tables/St_MagFactor_Table.h"

ClassImp(StMagFMaker)
  
  //_____________________________________________________________________________
StMagFMaker::StMagFMaker(const char *name):StMaker(name),fMagFactor(0),fMagF(0){}
//_____________________________________________________________________________
StMagFMaker::~StMagFMaker(){}
//_____________________________________________________________________________
Int_t StMagFMaker::Init(){
  fScale = 1.;
  cout << "Initialize STAR magnetic field with scale factor " << fScale << endl;
  if (!fMagF) {
    if (!m_Mode) fMagF = new StMagFCM("Star Full Field",
				       "$STAR/StDb/params/StMagF/bfp112.map",
				       kConMesh,fScale);
    else        fMagF = new StMagFC("Star Constant Field","Constant Field",fScale);
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMagFMaker::Make(){
  St_DataSet *magnet = GetDataBase("params/magnet"); assert(magnet);
  fMagFactor = (St_MagFactor *) magnet->Find("MagFactor"); assert(fMagFactor);
  Float_t Scale = (*fMagFactor)[0].ScaleFactor;
  if (Scale != fScale) {
    fScale = Scale;
    cout << "Reset STAR magnetic field with scale factor " << fScale << endl;
    fMagF->SetFactor(fScale);
  }
  return kStOK;
}











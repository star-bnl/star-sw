// $Id: StMagFMaker.cxx,v 1.10 2001/07/14 21:18:18 fisyak Exp $
// $Log: StMagFMaker.cxx,v $
// Revision 1.10  2001/07/14 21:18:18  fisyak
// Set scale=2.e-5 for FieldOff
//
// Revision 1.9  2001/05/29 21:59:41  fisyak
// Add StMagF class for back compartibility
//
// Revision 1.8  2001/05/23 22:52:08  fisyak
// Fix bug with scale factor
//
// Revision 1.7  2001/05/21 21:40:36  fisyak
// Merge geant and production mag. fields
//
// Revision 1.6  2001/05/17 20:38:26  fisyak
// Move check for mag. scale factor into InitRun
//
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
#include "StBFChain.h"
#include "St_geant_Maker/St_geant_Maker.h"
#include "tables/St_MagFactor_Table.h"
#include "StMagF.h"
#ifndef __CINT__
#include "StarCallf77.h"
#define    agdetp_new	 F77_NAME(agdetpnew,AGDETPNEW)
#define    agdetp_add	 F77_NAME(agdetpadd,AGDETPADD)
#define    mfldgeo	 F77_NAME(mfldgeo,MFLDGEO)
R__EXTERN  "C" {
 void type_of_call     agdetp_new (DEFCHARD DEFCHARL);
 void type_of_call     agdetp_add (DEFCHARD, Float_t*, Int_t*  DEFCHARL);
 void type_of_call     mfldgeo();
}
#endif

ClassImp(StMagFMaker)
ClassImp(StMagF)
  
  //_____________________________________________________________________________
StMagFMaker::StMagFMaker(const char *name):StMaker(name),fMagFactor(0),fMagF(kFALSE),fScale(1){}
//_____________________________________________________________________________
StMagFMaker::~StMagFMaker(){}
//_____________________________________________________________________________
Int_t StMagFMaker::InitRun(Int_t RunNo){
  StBFChain *chain = (StBFChain *) GetChain();
  if (chain) {
    if (chain->GetOption("FieldON"))      fScale = 1.0;
    else {if (chain->GetOption("FieldOff"))     fScale = 2.e-5;
    else {if (chain->GetOption("HalfField"))    fScale = 0.5;     
    else {if (chain->GetOption("ReverseField")) fScale = - fScale;
    else {
      if (!fMagFactor) {
	TDataSet *RunLog = GetDataBase("RunLog");
	fMagFactor = (St_MagFactor *) RunLog->Find("MagFactor"); assert(fMagFactor);
      }
      Float_t Scale = (*fMagFactor)[0].ScaleFactor;
      if (Scale == fScale && fMagF) return kStOK;
      fScale = Scale;
    }}}}
  }
  if (fMagF) cout << "Reset STAR magnetic field with scale factor " << fScale << endl;
  else       cout << "Initialize STAR magnetic field with scale factor " << fScale << endl;
  fMagF = kTRUE;
  agdetp_new (PASSCHARD("MFLD") PASSCHARL("MFLD"));
  Int_t One   = 1;
  Float_t Three   = 3;
  agdetp_add (PASSCHARD("MFLG(1).Version="),&Three,&One PASSCHARL("MFLG(1).Version="));
  Float_t Field = 5*fScale;
  agdetp_add (PASSCHARD("MFLG(1).Bfield="),&Field,&One PASSCHARL("MFLG(1).Bfield="));
  mfldgeo();
  return kStOK;
}
//_____________________________________________________________________________











// $Id: StMagFMaker.cxx,v 1.19 2020/01/15 02:01:26 perev Exp $
// $Log: StMagFMaker.cxx,v $
// Revision 1.19  2020/01/15 02:01:26  perev
// Option to change mag factor added
//
// Revision 1.18  2018/06/29 21:46:21  smirnovd
// Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
//
// Revert "NoDead option added"
// Revert "Fill mag field more carefully"
// Revert "Assert commented out"
// Revert "Merging with TPC group code"
// Revert "Remove too strong assert"
// Revert "Restore removed by mistake line"
// Revert "Remove not used anymore file"
// Revert "iTPCheckIn"
//
// Revision 1.16  2010/09/01 20:21:21  fisyak
// remove dependence on St_geant_Maker
//
// Revision 1.15  2009/11/10 20:50:46  fisyak
// Switch to TChair
//
// Revision 1.14  2007/03/21 17:12:58  fisyak
// Zero Field is 1G
//
// Revision 1.13  2005/08/29 22:54:26  fisyak
// switch to StarMagField
//
// Revision 1.12  2003/09/02 17:58:40  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2001/08/08 23:28:39  fisyak
// Fit Reverse Field option
//
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
#include <assert.h>
#include "TError.h"
#include <Stiostream.h>
#include "StMagFMaker.h"
#include "StDetectorDbMaker/St_MagFactorC.h"
#include "StDetectorDbMaker/St_starMagOnlC.h"
#include "StDetectorDbMaker/St_starMagAvgC.h"
#include "StMessMgr.h" 
#include "StMagF.h"
#include "StarMagField.h"
#include "StarCallf77.h"
#include "tables/St_Survey_Table.h"
#include "TMath.h"
#if 0
#define    agdetpnew	 F77_NAME(agdetpnew,AGDETPNEW)
#define    agdetpadd	 F77_NAME(agdetpadd,AGDETPADD)
#define    mfldgeo	 F77_NAME(mfldgeo,MFLDGEO)
R__EXTERN  "C" {
 void type_of_call     agdetpnew (DEFCHARD DEFCHARL);
 void type_of_call     agdetpadd (DEFCHARD, Float_t*, Int_t*  DEFCHARL);
 void type_of_call     mfldgeo();
}
#endif
ClassImp(StMagFMaker)
ClassImp(StMagF)
  
//_____________________________________________________________________________
StMagFMaker::~StMagFMaker(){}
//_____________________________________________________________________________
Int_t StMagFMaker::InitRun(Int_t RunNo)
{
  if (StarMagField::Instance() && StarMagField::Instance()->IsLocked()) {
    // Passive mode, do not change scale factor
    gMessMgr->Info() << "StMagFMaker::InitRun passive mode. Don't update Mag.Field from DB" << endm;
  } else {
    gMessMgr->Info() << "StMagFMaker::InitRun active mode " << endm;
    Float_t  fScale = St_MagFactorC::instance()->ScaleFactor();
    if (! St_starMagAvgC::instance()->Table()->IsMarked()) {
      fScale = St_starMagAvgC::instance()->ScaleFactor();
      gMessMgr->Info() << "StMagFMaker::InitRun use Scale Factor = " << fScale 
		       << " average for run " << St_starMagAvgC::instance()->runNumber();
      if (GetRunNumber() > 1000000) {// real data
	if (GetRunNumber() != (Int_t) St_starMagAvgC::instance()->runNumber()) {
	  gMessMgr->Error() << ", which does not matched with the current RunNo " << GetRunNumber();
	}
      }
      gMessMgr->Info() << endm;
    }
#if 0
    assert(TMath::Abs(fScale)>0.005);
#else 
    if (TMath::Abs(fScale)<=0.005) {
      gMessMgr->Info() << "StMagFMaker::InitRun reset scale factor from  " << fScale;
      fScale = 0.006;
      gMessMgr->Info() << " to " << fScale << 	endm;
    }
#endif
    gMessMgr->Info() << "StMagFMaker::InitRun from DB fScale = " << fScale << endm;
    if (*SAttr("magFactor")) {
      fScale = DAttr("magFactor");
      gMessMgr->Info() <<  "StMagFMaker::InitRun from Attr \"magFactor\" fScale = " << fScale << endm;
    }
    if (*SAttr("ScaleFactor")) {
      float myScale =  DAttr("ScaleFactor");
      if(myScale != fScale) {
	  gMessMgr->Info() <<  Form("StMagFMaker::InitRun Attr \"ScaleFactor\" %g is different from default %g",
				    myScale,fScale) << endm;
	  gMessMgr->Info() <<  "StMagFMaker::InitRun Attr \"ScaleFactor\" is assumed" << endm;
	  fScale = myScale;
      }
    }
    if (TMath::Abs(fScale) < 1e-3) fScale = 1e-3;
    if (fabs(fScale) < 1e-3) fScale = 1e-3;
    if (StarMagField::Instance())     {
      StarMagField::Instance()->SetFactor(fScale);
      gMessMgr->Info() << "Reset STAR magnetic field with scale factor " << fScale << endm;
    } else {
      new StarMagField(StarMagField::kMapped, fScale);
      gMessMgr->Info() << "Initialize STAR magnetic field with scale factor " << fScale << endm;
    }
#ifdef __RotateMagField__
    St_Survey *tableSet = (St_Survey *) GetDataBase("StMagF/MagFieldRotation");
    if (tableSet) {
      Survey_st *row = tableSet->GetTable();
      StarMagField::Instance()->SetStarMagFieldRotation(&row->r00);
    }
#endif
  }    
  double myX[3]={0},myB[3];
  StarMagField::Instance()->BField(myX,myB);
  gMessMgr->Info() << "StMagFMaker::InitRun Bz(0) = " << myB[2] << endm;

  return kStOK;
}
//_____________________________________________________________________________











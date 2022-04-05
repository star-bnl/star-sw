/***************************************************************************
 *
 * $Id: StSvtDbMaker.cxx,v 1.26 2010/09/01 21:07:07 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbMaker.cxx,v $
 * Revision 1.26  2010/09/01 21:07:07  fisyak
 * Disable simu flavor, now sim parameters is coming via DB associated with simulation time stamp
 *
 * Revision 1.25  2007/12/24 17:35:39  fisyak
 * spelling error StSvtRmsPedestal => StSvtRMSPedestal
 *
 * Revision 1.24  2007/07/31 16:38:11  fisyak
 * Make request for SvtGeometry from GetRotations
 *
 * Revision 1.23  2007/07/12 20:07:49  fisyak
 * Move to access on demand of Db tables
 *
 * Revision 1.22  2007/05/15 19:23:21  perev
 * Init local pointers by 0
 *
 * Revision 1.20  2007/04/28 17:57:09  perev
 * Redundant StChain.h removed
 *
 * Revision 1.19  2007/03/27 20:01:22  fisyak
 * remove senseless print outs
 *
 * Revision 1.18  2007/03/21 23:02:13  fisyak
 * add StSvtGeometry to const area
 *
 * Revision 1.17  2007/03/21 17:23:24  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.16  2006/02/14 17:49:25  perev
 * Add initialization =0
 *
 * Revision 1.15  2004/07/31 00:50:22  munhoz
 * adding anode drift veloc correction factor
 *
 * Revision 1.14  2004/07/29 01:36:00  caines
 * Changes for the drift curve usage
 *
 * Revision 1.13  2004/07/26 00:06:08  munhoz
 * read drift curve
 *
 * Revision 1.12  2004/03/30 21:16:17  caines
 * Get daq parameters
 *
 * Revision 1.10  2004/01/30 07:22:06  munhoz
 * adding rms and daq parameters reading
 *
 * Revision 1.9  2003/04/14 15:51:39  munhoz
 * reading t0 from DB
 *
 * Revision 1.8  2003/01/28 20:19:57  munhoz
 * including InitRun()
 *
 * Revision 1.7  2002/05/06 00:42:51  munhoz
 * adding bad anode list reading
 *
 * Revision 1.6  2002/02/28 20:41:37  caines
 * Fix filling of global coord from fortran
 *
 * Revision 1.5  2002/02/20 17:10:06  caines
 * Added fortran2c code from StDbUtilities so library depedancies removed
 *
 * Revision 1.4  2002/02/15 22:45:43  munhoz
 * introducing drift velocity reading capability
 *
 * Revision 1.3  2002/02/05 23:30:52  caines
 * fixing configuration bug
 *
 * Revision 1.2  2002/01/31 17:57:17  munhoz
 * removed incorrect line
 *
 * Revision 1.1  2001/10/29 18:53:13  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/
#include <assert.h>
#include "StSvtDbMaker.h"

#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtEnumerations.hh"

#include "StDbUtilities/StCoordinates.hh"  
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"

#include "TMath.h"
#include "TVector3.h"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridDriftCurve.hh"
#include "StSvtClassLibrary/StSvtHybridAnodeDriftCorr.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include "StSvtClassLibrary/StSvtDaq.hh"

#include "tables/St_svtConfiguration_Table.h"
#include "tables/St_svtDriftVelAvg_Table.h"
#include "tables/St_svtDriftCurve_Table.h"
#include "tables/St_svtBadAnodes_Table.h"
#include "tables/St_svtAnodeDriftCorr_Table.h"
#include "tables/St_svtPedestals_Table.h"
#include "tables/St_svtRms_Table.h"
#if 0
#include "tables/St_svtWafersPosition_Table.h"
#else
#include "tables/St_Survey_Table.h"
#endif
#include "tables/St_svtDimensions_Table.h"
#include "tables/St_svtElectronics_Table.h"
#include "tables/St_svtDaq_Table.h"
#include "tables/St_svtHybridDriftVelocity_Table.h"
#include "StDbUtilities/St_svtRDOstrippedC.h"
#include "StDbUtilities/St_svtHybridDriftVelocityC.h"
#include "StTpcDb/StTpcDb.h"

svtElectronics_st *electronic = NULL;
THashList *StSvtDbMaker::fRotList = 0;
StSvtDbMaker* gStSvtDbMaker = 0;
//C and fortran routines

//_______________________________________________________________________
int type_of_call SvtGtoL_(float *x,float *xp, int* index){

  StThreeVector<double> a(x[0], x[1], x[2]);
  StSvtCoordinateTransform transform;
  St_DataSet* dataSet;
  dataSet = gStSvtDbMaker->GetDataSet("StSvtGeometry");
  StSvtGeometry *GeomDataBase = (StSvtGeometry*)dataSet->GetObject();
  if(GeomDataBase)   transform.setParamPointers(GeomDataBase, NULL, NULL, NULL, NULL); // RW added 2 NULLs to remove ambiguity
  StSvtLocalCoordinate b;

  transform.GlobaltoLocal(a, b,*index, -1);

  xp[0] = b.position().x();
  xp[1] = b.position().y();
  xp[2] = b.position().z();
  
  return 0;
  
}
//____________________________________________________________________________
int type_of_call SvtLtoG_(float *xp, float *x, int* index){
  StSvtLocalCoordinate a;
  int layer,ladder,wafer;

  a.setPosition(StThreeVector<double>(xp[0],xp[1],xp[2]));
  StSvtCoordinateTransform transform;
  St_DataSet* dataSet;
  dataSet = gStSvtDbMaker->GetDataSet("StSvtGeometry");
  StSvtGeometry *GeomDataBase = (StSvtGeometry*)dataSet->GetObject();
  if(GeomDataBase)   transform.setParamPointers(GeomDataBase, NULL, NULL, NULL, NULL); // RW added 2 NULLs to remove ambiguity

  StThreeVector<double> b(0,0,0);
  StGlobalCoordinate c;

  layer = *index/1000;
  wafer = (*index -1000*layer)/100;
  ladder = *index -1000*layer -100*wafer;
  a.setLayer(layer);
  a.setLadder(ladder);
  a.setWafer(wafer);
  a.setHybrid(1);

  transform.LocaltoGlobal(a, b, -1);

  x[0] = b.x();
  x[1] = b.y();
  x[2] = b.z();
  
  return 0;
}

ClassImp(StSvtDbMaker)
//_____________________________________________________________________________
StSvtDbMaker::StSvtDbMaker(const char *name):StMaker(name) {  gStSvtDbMaker = this;}
//_____________________________________________________________________________
StSvtDbMaker::~StSvtDbMaker() { gStSvtDbMaker = NULL;}
//_____________________________________________________________________________
Int_t StSvtDbMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Init" << endm;
#if 0
  if( m_Mode == 1) {
    const Char_t *tabNames[3] = {"svtWafersPosition","svtDriftCorrection","svtRDOstripped"};
    for (Int_t i = 0; i < 3; i++) {
      gMessMgr->Message() << 
	"StSvtDbMaker::Init setting " << tabNames[i] << " to simu" << endm;
      SetFlavor("simu",tabNames[i]);   
    }
  }
#endif
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::InitRun(int runumber)
{
  gMessMgr->Info() << "StSvtDbMaker::InitRun" << endm;
  
  St_svtRDOstrippedC *svtRDOstrippedC = St_svtRDOstrippedC::instance();
  if (! svtRDOstrippedC) {
    St_svtRDOstripped *svtRDOstripped = (St_svtRDOstripped *) GetDataBase("Calibrations/svt/svtRDOstripped");
    if (svtRDOstripped) svtRDOstrippedC = new St_svtRDOstrippedC(svtRDOstripped);
  }
  assert(St_svtRDOstrippedC::instance());

  St_svtHybridDriftVelocityC *svtHybridDriftVelocityC = St_svtHybridDriftVelocityC::instance();
  if (! svtHybridDriftVelocityC) {
    St_svtHybridDriftVelocity *svtHybridDriftVelocity = 
      (St_svtHybridDriftVelocity *) GetDataBase("Calibrations/svt/svtHybridDriftVelocity");
    if (svtHybridDriftVelocity) svtHybridDriftVelocityC = new St_svtHybridDriftVelocityC(svtHybridDriftVelocity);
  }
  assert(St_svtHybridDriftVelocityC::instance());
  return kStOk;
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Make" << endm;
  St_svtRDOstrippedC *svtRDOstrippedC = St_svtRDOstrippedC::instance();
  if (svtRDOstrippedC) {
    UInt_t ut = GetDateTime().Convert();
    svtRDOstrippedC->SetDate(ut); // if you need to cut 1 hours after burn in has been finished
    if (Debug() > 2) svtRDOstrippedC->PrintRDOmap();
  }
  return kStOK;
}

//_____________________________________________________________________________
void StSvtDbMaker::Clear(const char*)
{
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Clear" << endm;

  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Finish" << endm;
  return kStOK;
}

//_____________________________________________________________________________
StSvtConfig* StSvtDbMaker::getConfiguration()
{
  gMessMgr->Info() << "StSvtDbMaker::getConfiguration" << endm;

  St_svtConfiguration *configuration = (St_svtConfiguration*) GetDataBase("Geometry/svt/svtConfiguration");
  if (!(configuration && configuration->HasData()) ){
    gMessMgr->Message("Error Finding SVT Configuration","E");
    return 0;
  }

  svtConfiguration_st* config = configuration->GetTable();

  gMessMgr->Info() << "numberOfBarrels = " << config->numberOfBarrels << endm;
  gMessMgr->Info() << "numberOfLadders = "  << config->numberOfLadders << endm;
  gMessMgr->Info() << "numberOfWafers = "  << config->numberOfWafers << endm;
  gMessMgr->Info() << "numberOfHybrids = "  << config->numberOfHybrids << endm;
  
  gMessMgr->Info() << "numberOfLaddersPerBarrel[0] = "  << config->numberOfLaddersPerBarrel[0] << endm;
  gMessMgr->Info() << "numberOfLaddersPerBarrel[1] = "  << config->numberOfLaddersPerBarrel[1] << endm;
  gMessMgr->Info() << "numberOfLaddersPerBarrel[2] = "  << config->numberOfLaddersPerBarrel[2] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[0] = "  << config->numberOfWafersPerLadder[0] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[1] = "  << config->numberOfWafersPerLadder[1] << endm;
  gMessMgr->Info() << "numberOfWafersPerLadder[2] = "  << config->numberOfWafersPerLadder[2] << endm;
  gMessMgr->Info() << "numberOfHybridsPerWafer = "  << config->numberOfHybridsPerWafer << endm;

  StSvtConfig *mSvtConfig = new StSvtConfig();
  mSvtConfig->setNumberOfBarrels(config->numberOfBarrels);

  for (int i=0; i<config->numberOfBarrels; i++) {
    mSvtConfig->setNumberOfLadders(i+1,config->numberOfLaddersPerBarrel[i]);
    mSvtConfig->setNumberOfWafers(i+1, config->numberOfWafersPerLadder[i]);
  }
  mSvtConfig->setNumberOfHybrids(config->numberOfHybridsPerWafer);
  mSvtConfig->setTotalNumberOfHybrids(config->numberOfHybrids);

  //temporary. Must read electronics db and fill these quantities
  mSvtConfig->setNumberOfAnodes(240);
  mSvtConfig->setNumberOfTimeBins(128);

  mSvtConfig->setConfiguration();

  return mSvtConfig;
}


//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbMaker::getDriftCurve()
{
  gMessMgr->Info() << "StSvtDbMaker::getDriftVelocityCurve" << endm;
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  StSvtHybridCollection *mSvtDriftCurve = (StSvtHybridCollection *) new StSvtHybridCollection(mSvtConfig);

  St_svtDriftCurve *driftVelocityCurve=0;

  svtDriftCurve_st *driftCurve=0;
  StSvtHybridDriftCurve* hybridDriftCurve=0;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
        for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {
	  
          index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);
	  
          if (index < 0) continue;
	  
	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtDriftCurve",ladder,wafer,hybrid);
	    break;
          }
	  
	  // get wafers position table
	  TString Path("Calibrations/svt/");
	  Path += path;
	  driftVelocityCurve = (St_svtDriftCurve*) GetDataBase(Path);
	  if (!(driftVelocityCurve && driftVelocityCurve->HasData()) ){
	    gMessMgr->Message("Error Finding SVT drift velocity curve","E");
	    return NULL;
	  }
	  
          driftCurve = driftVelocityCurve->GetTable();
	  
          hybridDriftCurve = (StSvtHybridDriftCurve*)mSvtDriftCurve->at(index);
          if (!hybridDriftCurve)
            hybridDriftCurve = new StSvtHybridDriftCurve(barrel,ladder,wafer,hybrid);
	  
          // loop over data
          for (int i=1; i<=3; i++)
	    for (int j=1; j<=10; j++) {
	      hybridDriftCurve->setParameter(i,j,driftCurve->driftCurve[i-1][j-1]);
	      // cout << "adc = " << i << ", parameter = " << j << ", value = " << driftCurve->driftCurve[i-1][j-1] << endl;
	    }
	  
          mSvtDriftCurve->put_at(hybridDriftCurve,index);
	  
        } // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels
  
  return mSvtDriftCurve;
}
    
//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbMaker::getAnodeDriftCorr()
{
  gMessMgr->Info() << "StSvtDbMaker::getAnodeDriftCorr" << endm;

  // get svt dimensions table
  St_svtAnodeDriftCorr *anodeDriftCorr;
  svtAnodeDriftCorr_st *driftCorr;

  // Create all pedestal objects
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  StSvtHybridCollection *mSvtAnodeDriftCorr = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridAnodeDriftCorr* hybridAnodeDriftCorr;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtAnodeDriftCorr",ladder,wafer,hybrid);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtAnodeDriftCorr",ladder,wafer,hybrid);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtAnodeDriftCorr",ladder,wafer,hybrid);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtAnodeDriftCorr",ladder,wafer,hybrid);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtAnodeDriftCorr",ladder,wafer,hybrid);
	    break;
	  }

	  // get wafers position table
	  TString Path("Calibrations/svt/");
	  Path += path;
	  anodeDriftCorr = (St_svtAnodeDriftCorr*) GetDataBase(Path);
	  if (!(anodeDriftCorr && anodeDriftCorr->HasData()) ){
	    gMessMgr->Message("Error Finding SVT bad anodes","E");
	    return 0;
	  }
	  driftCorr = anodeDriftCorr->GetTable();

	  hybridAnodeDriftCorr = (StSvtHybridAnodeDriftCorr*)mSvtAnodeDriftCorr->at(index);
	  if (!hybridAnodeDriftCorr)
	    hybridAnodeDriftCorr = new StSvtHybridAnodeDriftCorr(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    hybridAnodeDriftCorr->setValue(anode,driftCorr->driftVelocCorr[anode-1]);
	    //if (anode==120)
	    //  cout << "index = " << index << ", drift corr = " << driftCorr->driftVelocCorr[anode-1] << endl;
	  }

	  mSvtAnodeDriftCorr->put_at(hybridAnodeDriftCorr,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtAnodeDriftCorr;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbMaker::getPedestals()
{
  gMessMgr->Info() << "StSvtDbMaker::getPedestals" << endm;

  St_svtPedestals *pedestals = (St_svtPedestals*)GetDataBase("Calibrations/svt/svtPedestals");
  if (!(pedestals && pedestals->HasData()) ){
    gMessMgr->Message("Error Finding SVT Pedestals","E");
    return 0;
  }

  svtPedestals_st *pedestal = pedestals->GetTable();

  // Create all pedestal objects
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  StSvtHybridCollection *mSvtPed = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridPed* hybridPed;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridPed = (StSvtHybridPed*)mSvtPed->at(index);
	  if (!hybridPed)
	    hybridPed = new StSvtHybridPed(barrel,ladder,wafer,hybrid);
	  hybridPed->reset();

	  // loop over anodes
	  for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	    for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	      hybridPed->addToPixel(anode,time,pedestal[index].pedestal[anode-1][time]);
	      //gMessMgr->Info() << anode << "  " << time << "  " << pedestal[index].pedestal[anode-1][time] << endm;
	    }

	  mSvtPed->put_at(hybridPed,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtPed;
}


//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbMaker::getRms()
{
  gMessMgr->Info() << "StSvtDbMaker::getRms" << endm;

  St_svtRms *st_rms = (St_svtRms*)GetDataBase("Calibrations/svt/svtRms");
  if (!(st_rms && st_rms->HasData()) ){
    gMessMgr->Message("Error Finding SVT RMS","E");
    return 0;
  }

  svtRms_st *rms = st_rms->GetTable();

  // Create all pedestal objects
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  StSvtHybridCollection  *mSvtRms = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridPixels* hybridRms;
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  hybridRms = (StSvtHybridPixels*)mSvtRms->at(index);
	  if (!hybridRms)
	    hybridRms = new StSvtHybridPixels(barrel,ladder,wafer,hybrid);
	  hybridRms->reset();

	  // loop over anodes
	  for (int anode = 1; anode <= mSvtConfig->getNumberOfAnodes(); anode++)
	    for (int time = 0; time < mSvtConfig->getNumberOfTimeBins(); time++) {
	      hybridRms->addToPixel(anode,time,rms[index].rms[anode-1][time]);
	      //gMessMgr->Info() << anode << "  " << time << "  " << rms[index].rms[anode-1][time] << endm;
	    }

	  mSvtRms->put_at(hybridRms,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtRms;
}

//_____________________________________________________________________________
StSvtGeometry* StSvtDbMaker::getGeometry()
{
  gMessMgr->Info() << "StSvtDbMaker::getGeometry" << endm;

  // get svt dimensions table
  St_svtDimensions *dimensions = (St_svtDimensions*)GetDataBase("Geometry/svt/svtDimensions");
  if (!(dimensions && dimensions->HasData()) ){
    gMessMgr->Message("Error Finding SVT Dimensions","E");
    return 0;
  }

  svtDimensions_st *dimension = dimensions->GetTable();

  // Create all pedestal objects
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  StSvtGeometry *mSvtGeom = new StSvtGeometry(mSvtConfig);
  mSvtGeom->setBarrelRadius(dimension->barrelRadius);
  mSvtGeom->setWaferLength(dimension->waferLength);
  mSvtGeom->setWaferThickness(dimension->waferThickness);
  mSvtGeom->setWaferWidth(dimension->waferWidth);
  //mSvtGeom->setAnodePitch(dimension->anodePitch);
  mSvtGeom->setAnodePitch(0.025);
  mSvtGeom->setFocusRegionLength(dimension->focusRegionLength);
  mSvtGeom->setDistanceInjector(dimension->distanceInjector);
  mSvtGeom->setLaserPosition(dimension->laserPosition);
  fRotList = new THashList(100,0);
  fRotList->SetOwner(kFALSE);
  St_Survey *SvtOnGlobal = (St_Survey *) GetDataBase("Geometry/svt/SvtOnGlobal");
  if (! SvtOnGlobal)  {cout << "SvtOnGlobal has not been found"  << endl; return 0;}
  
  TGeoHMatrix GL, LS,SG,LA,WG;
  Survey_st *OnGlobal         = SvtOnGlobal->GetTable();        // SSD and SVT as whole 
  GL.SetRotation(&OnGlobal->r00);
  GL.SetTranslation(&OnGlobal->t0);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  
  // SVT  
  St_Survey *WaferOnLadder = (St_Survey *) GetDataBase("Geometry/svt/WaferOnLadder");
  St_Survey *LadderOnSurvey = (St_Survey *) GetDataBase("Geometry/svt/LadderOnSurvey");
  St_Survey *LadderOnShell = (St_Survey *) GetDataBase("Geometry/svt/LadderOnShell");
  St_Survey *ShellOnGlobal = (St_Survey *) GetDataBase("Geometry/svt/ShellOnGlobal");
  Int_t NW = WaferOnLadder->GetNRows();
  Int_t NL = LadderOnSurvey->GetNRows();
  Survey_st *waferOnLadder = WaferOnLadder->GetTable();
  Survey_st *shellOnGlobal0 = ShellOnGlobal->GetTable(0);
  Survey_st *shellOnGlobal1 = ShellOnGlobal->GetTable(1);
  TGeoHMatrix LSU, LSH, SHG[2];
  SHG[0].SetRotation(&shellOnGlobal0->r00);
  SHG[0].SetTranslation(&shellOnGlobal0->t0);
  SHG[1].SetRotation(&shellOnGlobal1->r00);
  SHG[1].SetTranslation(&shellOnGlobal1->t0);
  TGeoHMatrix *comb;
#if 0
  Double_t swap[9] = { 1, 0, 0, 0, 0, 1, 0, 1, 0};
  TGeoHMatrix SWAP("SWAP");
  SWAP.SetRotation(swap);
#endif 
  for (Int_t i = 0; i < NW; i++, waferOnLadder++)    {
    Int_t id = waferOnLadder->Id;
    Int_t wbarrel  = id/1000;
    Int_t wwafer  = (id - 1000*wbarrel)/100;
    Int_t wladder = id%100;
    Int_t wlayer = 2*wbarrel + wladder%2 - 1;
    //    Id = 1000* layer + 100* wafer +  ladder;
    Int_t Id = 1000*wlayer + 100*wwafer + wladder;
    Int_t index = mSvtGeom->getWaferIndex(wbarrel,wladder,wwafer);
    if (index < 0) continue;
    StSvtWaferGeometry *waferGeom = (StSvtWaferGeometry *) fRotList->FindObject(Form("R%04i",Id));
    if (waferGeom) continue;
    waferGeom = new StSvtWaferGeometry(wbarrel,wladder,wwafer);
    mSvtGeom->put_at(waferGeom,index);
    Int_t Found = 0;
    Survey_st *ladderOnSurvey = LadderOnSurvey->GetTable();
    for ( Int_t j = 0; j < NL; j++, ladderOnSurvey++)	{
      Int_t Idl =  ladderOnSurvey->Id;
      Int_t lbarrel = Idl/1000;
      Int_t lladder = Idl%100;
      if( wladder !=  lladder || wbarrel != lbarrel)	continue;
      Survey_st *ladderOnShell = LadderOnShell->GetTable();
      Int_t found = 0;
      for ( Int_t l = 0; l < NL; l++, ladderOnShell++) if (ladderOnShell->Id == ladderOnSurvey->Id) {found++; break;}
      if (! found) continue;
      LSH.SetRotation(&ladderOnShell->r00);
      LSH.SetTranslation(&ladderOnShell->t0);
      Int_t Shell = 1;
      if( (wbarrel == 1 && wladder <= 4) || (wbarrel == 2 && wladder <= 6) ||  (wbarrel == 3 && wladder <= 8) ) Shell = 0;
      // 	shellOnGlobal *	ladderOnShell * ladderOnSurvey * waferOnLadder 
      
      TGeoHMatrix wL;
      wL.SetRotation(&waferOnLadder->r00);
      wL.SetTranslation(&waferOnLadder->t0);
      LSU.SetRotation(&ladderOnSurvey->r00);
      LSU.SetTranslation(&ladderOnSurvey->t0);
	//    LSU - ladderOnSurvey     {1001,1.000000,-0.000923, 0.000074, 0.000923, 1.000000,-0.000036,-0.000074, 0.000036, 1, 0.0071,  7.1686, -1.2355
	//          wL - waferOnLadder {1101,0.999999, 0.001235, 0.000003,-0.001235, 0.999999,-0.000124,-0.000003, 0.000124, 1,-0.0005,  0.2404, 15.3065,
      TGeoHMatrix WLL = LSU * wL;
      
      TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%04i",Id));
      if (! WL) {
	WL = new  TGeoHMatrix(Form("WL%04i",Id)); 
	Double_t *r = WLL.GetRotationMatrix();   
	Double_t rot[9] = {r[0], r[2], r[1],   
			   r[3], r[5], r[4],   
			   r[6], r[8], r[7]};   
	// {7101,  1.000000,0, 0.000052,0, 1,0,-0.000052,0, 1.000000,-0.000600,0,-32.625900, 
	WL->SetRotation(rot);
	WL->SetTranslation(WLL.GetTranslation());
	fRotList->Add(WL);
      }
      //	WG = GL * SHG * LSH * LSU * (*WL); //  WG.Print();
      //             GL - SvtOnGlobal
      //                  SHG - ShellOnGlobal    {   0,  0.999850, 0.01733,  0.000019, -0.01733, 0.999850,-0.00071, -0.000019, 0.00071,  0.999999, -0.2105, -0.0160, -0.1244
      //                     LSH - ladderOnShell {1001,  0.707107,0.707107,  0.      , -0.707107,0.707107, 0.     ,  0       ,       0,  1       ,  0,       0     ,-23.5250
      // 
      if (Debug()) {
	cout << "Tpc2Global "; Tpc2Global.Print();
	cout << "GL "; GL.Print();
	TGeoHMatrix test =  Tpc2Global * GL; cout << "test "; test.Print();
      }
      WG = Tpc2Global * GL * SHG[Shell] * LSH * WLL;// * SWAP; //  WG.Print();
      Double_t *r = WG.GetRotationMatrix();
      Int_t fail = 0;
      for (int l = 0; l < 9; l++) {
	if (TMath::Abs(r[l]) >=  1.000001) fail++;
      }
      if (fail && Debug()) {
	cout << "===============" << waferOnLadder->Id << "  "<< id << " " <<  Id <<endl;
	cout << "WG\t"; WG.Print();
      }
      Double_t norm;
      TVector3 d(r[0],r[3],r[6]); norm = 1/d.Mag(); d *= norm;
      TVector3 t(r[2],r[5],r[8]); norm = 1/t.Mag(); t *= norm;
      TVector3 n(r[1],r[4],r[7]); norm = 1/n.Mag(); n *= norm;
      TVector3 c = d.Cross(t);
      if (c.Dot(n) < 0) c *= -1;
      
      Double_t *wgtr = WG.GetTranslation();
      Double_t rot[9] = {
	d[0], t[0], n[0],
	d[1], t[1], n[1],
	d[2], t[2], n[2]};
      waferGeom->SetRotation(rot);
      waferGeom->SetTranslation(wgtr);
      if (Debug()) waferGeom->print();
      fRotList->Add(waferGeom);
      Found++;
      break;
    }
    assert(Found);
  }
  TIter next(fRotList);
  Int_t fail = 0;
  //  TGeoHMatrix *comb;
  while ((comb = (TGeoHMatrix *) next())) {
    TString Name(comb->GetName());
    if (Name.BeginsWith("R")) {
      TGeoHMatrix *WL = (TGeoHMatrix *) fRotList->FindObject(Form("WL%s",Name.Data()+1));
      if (! WL) {
	cout << Form("WL%s",Name.Data()+1) << " has not been found" << endl;
	fail++;
      }
    }
  }
  assert(! fail);
  return mSvtGeom;
}

//_____________________________________________________________________________
StSvtHybridCollection* StSvtDbMaker::getBadAnodes()
{
  gMessMgr->Info() << "StSvtDbMaker::getBadAnodes" << endm;

  // get svt dimensions table
  St_svtBadAnodes *badAnodes;
  svtBadAnodes_st *badAnode;
  StSvtConfig *mSvtConfig = (StSvtConfig *) (((TObjectSet *) GetDataSet("StSvtConfig"))->GetObject());
  // Create all pedestal objects
  StSvtHybridCollection *mSvtBadAnodes = new StSvtHybridCollection(mSvtConfig);
  StSvtHybridBadAnodes* hybridBadAnodes;

  char path[100];
  int index;

  for (int barrel = 1;barrel <= mSvtConfig->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtConfig->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtConfig->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtConfig->getNumberOfHybrids();hybrid++) {

	  index = mSvtConfig->getHybridIndex(barrel,ladder,wafer,hybrid);

	  if (index < 0) continue;

	  switch (barrel) {
	  case 1:
	    sprintf(path,"InnerBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  case 2:
	    if (ladder < 10)
	      sprintf(path,"MiddleBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    else
	      sprintf(path,"MiddleBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  case 3:
	    if (ladder < 10)
	      sprintf(path,"OuterBarrel/Ladder_0%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    else
	      sprintf(path,"OuterBarrel/Ladder_%d/Wafer_0%d/Hybrid_0%d/svtBadAnodes",ladder,wafer,hybrid);
	    break;
	  }

	  // get wafers position table
	  TString Path("Calibrations/svt/");
	  Path += path;
	  badAnodes = (St_svtBadAnodes*) GetDataBase(Path);
	  if (!(badAnodes && badAnodes->HasData()) ){
	    gMessMgr->Message("Error Finding SVT bad anodes","E");
	    return 0;
	  }

	  badAnode = badAnodes->GetTable();

	  hybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	  if (!hybridBadAnodes)
	    hybridBadAnodes = new StSvtHybridBadAnodes(barrel,ladder,wafer,hybrid);
	  
	  // loop over anodes
	  for (int anode=1;anode<=mSvtConfig->getNumberOfAnodes();anode++) {
	    if (badAnode->isBadAnode[anode-1]) {
	      hybridBadAnodes->setBadAnode(anode);
	      //gMessMgr->Info() << "hybrid = "<< index << ", anode = " << anode << endm;
	    }
	  }

	  mSvtBadAnodes->put_at(hybridBadAnodes,index);

	} // end of loop over hybrids
      } // end of loop over wafers
    } // end of loop over ladders
  } // end of loop over barrels

  return mSvtBadAnodes;
}

//_____________________________________________________________________________
int StSvtDbMaker::getElectronics()
{
  // get svt electronics table
  St_svtElectronics *electronics = (St_svtElectronics*)GetDataBase("Calibrations/svt/svtElectronics");
  if (!(electronics && electronics->HasData()) ){
    gMessMgr->Message("Error Finding SVT Electronics","E");
    return kFALSE;
  }

  electronic = electronics->GetTable();
  return kTRUE;
}

//_____________________________________________________________________________
StSvtT0* StSvtDbMaker::getT0()
{
  StSvtT0 *mSvtT0 = new StSvtT0();

  if (getElectronics()) {
    for (int i=0;i<24;i++)
      mSvtT0->setT0(electronic->tZero[i],i+1);
    mSvtT0->setFsca(electronic->samplingFrequency);

    gMessMgr->Info() << "t0 = " << mSvtT0->getT0(1) << ", fsca =  " << mSvtT0->getFsca() << endm;

  }

  return mSvtT0;
}


//_____________________________________________________________________________
StSvtDaq* StSvtDbMaker::getDaqParameters()
{
  gMessMgr->Info() << "StSvtDbMaker::getDaqParameters" << endm;

  St_svtDaq *daq = (St_svtDaq*)GetDataBase("Calibrations/svt/svtDaq");
  if (!(daq && daq->HasData()) ){
    gMessMgr->Message("Error Finding SVT Daq","E");
    return 0;
  }

  svtDaq_st* daqParam = daq->GetTable();

  gMessMgr->Info() << "clearedTimeBins = " << daqParam->clearedTimeBins<< endm;
  gMessMgr->Info() << "pixelsBefore = " << daqParam->pixelsBefore << endm;
  gMessMgr->Info() << "pixelsAfter = " << daqParam->pixelsAfter << endm;
  gMessMgr->Info() << "pedOffset = " << daqParam->pedOffset << endm;
  gMessMgr->Info() << "seqLo = " << daqParam->seqLo << endm;
  gMessMgr->Info() << "seqHi = " << daqParam->seqHi << endm;
  gMessMgr->Info() << "threshLo = " << daqParam->threshLo << endm;
  gMessMgr->Info() << "threshHi = " << daqParam->threshHi << endm;

  StSvtDaq *mSvtDaq = new StSvtDaq();

  mSvtDaq->setClearedTimeBins(daqParam->clearedTimeBins);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[0],0);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[1],1);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[2],2);
  mSvtDaq->setSavedBlackAnodes(daqParam->savedBlackAnodes[2],3);
  mSvtDaq->setPixelsBefore(daqParam->pixelsBefore);
  mSvtDaq->setPixelsAfter(daqParam->pixelsAfter);
  mSvtDaq->setPedOffset(daqParam->pedOffset);
  mSvtDaq->setSeqLo(daqParam->seqLo);
  mSvtDaq->setSeqHi(daqParam->seqHi);
  mSvtDaq->setThreshLo(daqParam->threshLo);
  mSvtDaq->setThreshHi(daqParam->threshHi);

  return mSvtDaq;
}

//_____________________________________________________________________________
TDataSet  *StSvtDbMaker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const 
{
  TDataSet *ds = StMaker::FindDataSet(logInput,uppMk,dowMk); 
  if (ds) return ds;
  static const Char_t *SetNames[] = {"StSvtConfig",  "StSvtDriftVelocity", "StSvtDriftCurve", 
				     "StSvtAnodeDriftCorr", "StSvtPedestal","StSvtRMSPedestal", 
				     "StSvtGeometry", "StSvtBadAnodes", "StSvtT0", "StSvtDaq", 
				     "StSvtGeometry", 0};
  TString Input(logInput);
  if (! Input.Contains("StSvt")) return ds;
  St_ObjectSet *objs = 0;
  StSvtDbMaker *This = (StSvtDbMaker *) this;
  for (Int_t i = 0; SetNames[i]; i++) {
    if (Input.CompareTo(SetNames[i])) continue;
    objs = new St_ObjectSet(SetNames[i]);
    This->AddConst(objs);
    TObject *o = 0;
    if      (! Input.CompareTo("StSvtConfig"))        o = This->getConfiguration();
    else if (! Input.CompareTo("StSvtDriftVelocity")) o = 0;
    else if (! Input.CompareTo("StSvtDriftCurve"))    o = This->getDriftCurve(); 
    else if (! Input.CompareTo("StSvtAnodeDriftCorr"))o = This->getAnodeDriftCorr(); 
    else if (! Input.CompareTo("StSvtPedestal"))      o = This->getPedestals(); 
    else if (! Input.CompareTo("StSvtRMSPedestal"))   o = This->getRms(); 
    else if (! Input.CompareTo("StSvtGeometry"))      o = This->getGeometry(); 
    else if (! Input.CompareTo("StSvtBadAnodes"))     o = This->getBadAnodes(); 
    else if (! Input.CompareTo("StSvtT0"))            o = This->getT0(); 
    else if (! Input.CompareTo("StSvtDaq"))           o = This->getDaqParameters(); 
    else if (! Input.CompareTo("StSvtGeometry"))      o = This->getGeometry();  
    if (o) objs->SetObject(o);
    break;
  }
  return (TDataSet *) objs;  
}
//________________________________________________________________________________
THashList *StSvtDbMaker::GetRotations() {
  FindDataSet("StSvtGeometry");
  return fRotList;
}

/***************************************************************************
 *
 * $Id: StSvtDbMaker.cxx,v 1.11 2004/03/24 03:36:10 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbMaker.cxx,v $
 * Revision 1.11  2004/03/24 03:36:10  caines
 * Fix to allow to call INitRun twice as happens in MC data sometimes. Before we were deleting our own data by mistake
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

#include "StSvtDbMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtEnumerations.hh"
#include "St_SvtDb_Reader.hh"
#include "StSvtDbReader.hh"
#include "StSvtDbWriter.hh"
#include "St_db_Maker/St_db_Maker.h"

#include "StDbUtilities/StCoordinates.hh"  
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"

StSvtDbMaker* gStSvtDbMaker=NULL; 
St_ObjectSet *svtSetConfig;
St_ObjectSet *svtSetDrift;
St_ObjectSet *svtSetPed;
St_ObjectSet *svtSetRms;
St_ObjectSet *svtSetGeom;
St_ObjectSet *svtSetBad;
St_ObjectSet *svtSetT0;

//C and fortran routines

//_______________________________________________________________________
int type_of_call SvtGtoL_(float *x,float *xp, int* index){

  StThreeVector<double> a((double)x[0], (double)x[1], (double)x[2]);
  StSvtCoordinateTransform transform;
  St_DataSet* dataSet;
  dataSet = gStSvtDbMaker->GetDataSet("StSvtGeometry");
  StSvtGeometry *GeomDataBase = (StSvtGeometry*)dataSet->GetObject();
  if(GeomDataBase)   transform.setParamPointers(GeomDataBase, NULL, NULL);
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
  if(GeomDataBase)   transform.setParamPointers(GeomDataBase, NULL, NULL);

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
StSvtDbMaker::StSvtDbMaker(const char *name):StMaker(name)
{
  mTimeStamp = "1999-12-01 00:00:01";

  m_Reader = NULL;
  mReader = NULL;
  mWriter = NULL;
  gStSvtDbMaker = this;
}

//_____________________________________________________________________________
StSvtDbMaker::~StSvtDbMaker()
{

 gStSvtDbMaker = NULL;
  //delete m_Reader;
  //if (mReader) delete mReader;
  //if (mWriter) delete mWriter;
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Init" << endm;

  setSvtDb_Reader();
  //setSvtDbReader();
  //setSvtDbWriter();

  setSvtConfig();
  readSvtConfig();
  setSvtGeometry();
  setSvtDriftVelocity();
  setSvtBadAnodes();
  setSvtT0();

  if( m_Mode == 1) {
    gMessMgr->Message() << 
      "StSvtDbMaker::Init setting WaferPostions to simu" << endm;
    SetFlavor("simu","svtWafersPosition");   
  }

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::InitRun(int runumber)
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::InitRun" << endm;

  readSvtGeometry();
  readSvtDriftVelocity();
  readSvtBadAnodes();
  readSvtT0();
  return kStOk;
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDb_Reader()  
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::set_SvtDb_Reader" << endm;
  m_Reader = new St_SvtDb_Reader();

  Char_t *bases[2];
  //Char_t *bases[3];
  bases[kCalibration] = "Calibrations/";
  bases[kGeometry] = "Geometry/";
  //  bases[kConditions] = "Conditions/";
  int lBases = sizeof(bases)/sizeof(Char_t *);
  for (int i = 0;i<lBases;i++)
    {
      TString dbFullPath = "StDb/";
      TString dbPath = bases[i];
      dbPath += "svt";
      dbFullPath += dbPath;
      if (GetDataBase(dbPath))
	m_Reader->setDataBase(GetDataBase(dbPath),(dbSvtType)i);
      else if (GetDataBase(dbFullPath))
	m_Reader->setDataBase(GetDataBase(dbFullPath),(dbSvtType)i);
      else
	gMessMgr->Warning() << "Error Getting SVT database: " << bases[i] << "   " << endm;
    }
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDbReader(int unixTime)
{
  mUnixTimeStamp = unixTime;

  mReader = new StSvtDbReader(mUnixTimeStamp);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDbReader(Text_t *timestamp)
{
  mTimeStamp = timestamp;

  mReader = new StSvtDbReader(mTimeStamp);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDbWriter(int unixTime)
{
  mUnixTimeStamp = unixTime;

  mWriter = new StSvtDbWriter(mUnixTimeStamp);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDbWriter(Text_t *timestamp)
{
  mTimeStamp = timestamp;

  mWriter = new StSvtDbWriter(mTimeStamp);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtConfig()
{    
  svtSetConfig = new St_ObjectSet("StSvtConfig");
  AddConst(svtSetConfig);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtConfig()
{    
  if (mReader)
    svtSetConfig->SetObject((TObject*)mReader->getConfiguration(),kFALSE);
  else if (m_Reader)
    svtSetConfig->SetObject((TObject*)m_Reader->getConfiguration(),kFALSE);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtDriftVelocity()
{
  svtSetDrift = new St_ObjectSet("StSvtDriftVelocity");
  AddConst(svtSetDrift);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtDriftVelocity()
{
  if (mReader)
    svtSetDrift->SetObject((TObject*)mReader->getDriftVelocity(),kFALSE);
  else if (m_Reader)
    svtSetDrift->SetObject((TObject*)m_Reader->getDriftVelocity(),kFALSE);  
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtPedestals()
{
  svtSetPed = new St_ObjectSet("StSvtPedestal");
  AddConst(svtSetPed);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtPedestals()
{
  if (mReader)
    svtSetPed->SetObject((TObject*)mReader->getPedestals(),kFALSE);
  else if (m_Reader)
    svtSetPed->SetObject((TObject*)m_Reader->getPedestals(),kFALSE);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtRms()
{
  svtSetRms = new St_ObjectSet("StSvtRmsPedestal");
  AddConst(svtSetRms);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtRms()
{
  if (mReader)
    svtSetRms->SetObject((TObject*)mReader->getRms(),kFALSE);
  else if (m_Reader)
    svtSetRms->SetObject((TObject*)m_Reader->getRms(),kFALSE);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtGeometry()
{
  svtSetGeom = new St_ObjectSet("StSvtGeometry");
  AddConst(svtSetGeom);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtGeometry()
{
  if (mReader)
    svtSetGeom->SetObject((TObject*)mReader->getGeometry(),kFALSE);
  else if (m_Reader)
    svtSetGeom->SetObject((TObject*)m_Reader->getGeometry(),kFALSE);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtBadAnodes()
{
  svtSetBad = new St_ObjectSet("StSvtBadAnodes");
  AddConst(svtSetBad);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtBadAnodes()
{
  if (mReader)
    svtSetBad->SetObject((TObject*)mReader->getBadAnodes(),kFALSE);
  else if (m_Reader)
    svtSetBad->SetObject((TObject*)m_Reader->getBadAnodes(),kFALSE);
}

//_____________________________________________________________________________
void StSvtDbMaker::setSvtT0()
{    
  svtSetT0 = new St_ObjectSet("StSvtT0");
  AddConst(svtSetT0);  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtT0()
{    
  if (mReader)
    svtSetT0->SetObject((TObject*)mReader->getT0());
  else if (m_Reader)
    svtSetT0->SetObject((TObject*)m_Reader->getT0());
}

//_____________________________________________________________________________
void StSvtDbMaker::writeSvtDriftVelocity(StSvtHybridCollection* driftVeloc)
{
  if (!driftVeloc) {
    St_DataSet *dataSet = GetDataSet("StSvtDriftVelocity");
    assert(dataSet);
    driftVeloc = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(driftVeloc);
  }

  if (mWriter)
    mWriter->addDriftVelocity(driftVeloc);
}

//_____________________________________________________________________________
void StSvtDbMaker::writeSvtPedestals(StSvtHybridCollection* pedestals)
{
  if (!pedestals) {
    St_DataSet *dataSet = GetDataSet("StSvtPedestal");
    assert(dataSet);
    pedestals = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(pedestals);
  }
  
  if (mWriter)
    mWriter->addPedestals(pedestals);
}

//_____________________________________________________________________________
void StSvtDbMaker::writeSvtRms(StSvtHybridCollection* rms)
{
  if (!rms) {
    St_DataSet *dataSet = GetDataSet("StSvtRMSPedestal");
    assert(dataSet);
    rms = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(rms);
  }
  
  if (mWriter)
    mWriter->addRms(rms);
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Make" << endm;

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

/***************************************************************************
 *
 * $Id: StSvtDbMaker.cxx,v 1.1 2001/10/29 18:53:13 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbMaker.cxx,v $
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

ClassImp(StSvtDbMaker)

//_____________________________________________________________________________
StSvtDbMaker::StSvtDbMaker(const char *name):StMaker(name)
{
  mTimeStamp = "1999-12-01 00:00:01";

  m_Reader = NULL;
  mReader = NULL;
  mWriter = NULL;
}

//_____________________________________________________________________________
StSvtDbMaker::~StSvtDbMaker()
{
  delete m_Reader;
  delete mReader;
  delete mWriter;
}

//_____________________________________________________________________________
Int_t StSvtDbMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDbMaker::Init" << endm;

  setSvtDb_Reader();
  //setSvtDbReader();
  //setSvtDbWriter();

  ((St_db_Maker*) GetMaker("db"))->Make();
  readSvtConfig();
  readSvtGeometry();

  return StMaker::Init();
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
void StSvtDbMaker::readSvtConfig()
{    
  St_ObjectSet *svtSet = new St_ObjectSet("StSvtConfig");
  AddConst(svtSet);  

  if (mReader)
    svtSet->SetObject((TObject*)mReader->getConfiguration());
  else if (m_Reader)
    svtSet->SetObject((TObject*)m_Reader->getConfiguration());
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtDriftVelocity()
{
  St_ObjectSet *svtSet = new St_ObjectSet("StSvtDriftVelocity");
  AddConst(svtSet);  
    
  if (mReader)
    svtSet->SetObject((TObject*)mReader->getDriftVelocity());
  else if (m_Reader)
    svtSet->SetObject((TObject*)m_Reader->getDriftVelocity());  
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtPedestals()
{
  St_ObjectSet *svtSet = new St_ObjectSet("StSvtPedestal");
  AddConst(svtSet);  
    
  if (mReader)
    svtSet->SetObject((TObject*)mReader->getPedestals());
  else if (m_Reader)
    svtSet->SetObject((TObject*)m_Reader->getPedestals());
}

//_____________________________________________________________________________
void StSvtDbMaker::readSvtGeometry()
{
  St_ObjectSet *svtSet = new St_ObjectSet("StSvtGeometry");
  AddConst(svtSet);  
    
  if (mReader)
    svtSet->SetObject((TObject*)mReader->getGeometry());
  else if (m_Reader)
    svtSet->SetObject((TObject*)m_Reader->getGeometry());
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
  
  cout << "pedestals = " << pedestals << endl;

  if (mWriter)
    mWriter->addPedestals(pedestals);
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

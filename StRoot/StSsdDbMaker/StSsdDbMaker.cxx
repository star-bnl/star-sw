/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#include "StSsdDbMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"                         //

#include "StSsdDbWriter.hh"
#include "St_SsdDb_Reader.hh"
#include "St_db_Maker/St_db_Maker.h"
#include "StSsdUtil/StSsdGeometry.hh"
#include "StSsdUtil/StSsdEnumerations.hh"
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdDimensions_Table.h"
StSsdDbMaker* gStSsdDbMaker=NULL; 
St_ObjectSet *ssdSetConfig;
St_ObjectSet *ssdSetGeom;
St_ObjectSet *wafersPositionSetGeom;

ClassImp(StSsdDbMaker)
//_____________________________________________________________________________
StSsdDbMaker::StSsdDbMaker(const char *name):StMaker(name)
{
  //  mTimeStamp    = "1970-01-01 00:00:00";
  mTimeStamp    = "2004-07-10 00:00:00";

  m_Reader      = NULL;
  gStSsdDbMaker = this;
  mWriter       = NULL;

}

//_____________________________________________________________________________
StSsdDbMaker::~StSsdDbMaker()
{

 gStSsdDbMaker = NULL; 
  //delete m_Reader;
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Init - Start - " << endm;

  gMessMgr->Info() << " StSsdDbMaker :: accessing to databases "<<endm;
  setSsdDirectDb_Reader();
    
  setSsdConfig();
  readSsdDirectConfig();

  setSsdGeometry();
  // Call of readSsdDirectGeometry will be done in InitRun()
  //  readSsdDirectGeometry();

  return StMaker::Init();
  gMessMgr->Info() << "StSsdDbMaker :: Init() - Done - "<<endm;
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::InitTable()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::InitTable" << endm;
  
  //setSsdDbWriter(123456789);

  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);

  St_ssdWafersPosition* wafersPosition;
  wafersPosition  = (St_ssdWafersPosition    *) local("ssd/ssdWafersPosition");
  St_ssdConfiguration* ssdConfiguration;
  ssdConfiguration  = (St_ssdConfiguration    *) local("ssd/ssdConfiguration");
  St_ssdDimensions* ssdDimensions;
  ssdDimensions  = (St_ssdDimensions   *) local("ssd/ssdDimensions");
  
  if (!wafersPosition) 
    gMessMgr->Error() << "StSsdDbMaker :: No access to wafersPosition table " << endm;
  
  if (!ssdConfiguration) 
    gMessMgr->Error() << "StSsdDbMaker :: No access to ssdConfiguration table " << endm;

  if (!ssdDimensions) 
    gMessMgr->Error() << "StSsdDbMaker :: No access to ssdDimensions table " << endm;
 
    setSsdConfig();
  readSsdConfig();

  setSsdGeometry();
  readSsdGeometry();

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdDbMaker::InitRun(int runumber)
{
  gMessMgr->Info() << "StSsdDbMaker::InitRun" << endm;
  gMessMgr->Info() << "StSsdDbMaker::readSsdDirectGeometry done here " << endm;
  //  readSsdGeometry();
  readSsdDirectGeometry();
  return kStOk;
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdDirectDb_Reader()  
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::set_SsdDirectDb_Reader" << endm;
  m_Reader = new St_SsdDb_Reader(),
  mDbMgr = StDbManager::Instance();
  mDbMgr -> setVerbose(false);             // set Verbose mode for debug

  //-> connect to the db & get an empty container
  mConfigGeom  = mDbMgr -> initConfig(dbGeometry,dbSsd);
  mGeom    = mDbMgr -> initConfig(dbGeometry,dbSsd);

  StDbTable* ssdConfigTable = mConfigGeom -> addDbTable("ssdConfiguration");
  mDbMgr->fetchDbTable(ssdConfigTable);
  ssdConfiguration_st *config  = (ssdConfiguration_st*) ssdConfigTable->GetTable() ;

  StDbTable* ssdWafersPositionTable = mConfigGeom -> addDbTable("ssdWafersPosition");
  mDbMgr->fetchDbTable(ssdWafersPositionTable);
  ssdWafersPosition_st *geom  = (ssdWafersPosition_st*) ssdWafersPositionTable->GetTable() ;

  StDbTable* ssdDimensionsTable = mConfigGeom -> addDbTable("ssdDimensions");
  mDbMgr->fetchDbTable(ssdDimensionsTable);
  ssdDimensions_st *dimensions  = (ssdDimensions_st*) ssdDimensionsTable->GetTable() ;

  if (geom) 
      m_Reader->setWafersPosition(geom);
  else
      gMessMgr->Error() << "No  access to wafer position " << endm;

  if(config)
    m_Reader->setSsdConfiguration(config);
  else    
    gMessMgr->Error()   << "No  access to ssdConfiguration table " << endm;

  if(dimensions)
    m_Reader->setSsdDimensions(dimensions);
  else    
    gMessMgr->Error()   << "No  access to ssdDimensions table " << endm;

  
  if(!m_Reader)
    gMessMgr->Error() <<" m_Reader not defined........"<<endm;

  gMessMgr->Info() << " Finishing setSsdDirectDb_Reader..........."<<endm;
}


//_____________________________________________________________________________
void StSsdDbMaker::setSsdDb_Reader()  
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::set_SsdDb_Reader" << endm;
  m_Reader = new St_SsdDb_Reader();

  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  
  St_ssdWafersPosition* wafersPosition;
  wafersPosition  = (St_ssdWafersPosition    *) local("ssd/ssdWafersPosition");
  
  
  St_ssdConfiguration* ssdConfiguration;
  ssdConfiguration = (St_ssdConfiguration    *) local("ssd/ssdConfiguration");
 
  St_ssdDimensions* ssdDimensions;
  ssdDimensions = (St_ssdDimensions    *) local("ssd/ssdDimensions");
  
  if (wafersPosition) 
      m_Reader->setWafersPosition(wafersPosition);
  else
      gMessMgr->Error() << "No  access to wafer position table " << endm;

  if(ssdConfiguration)
    m_Reader->setSsdConfiguration(ssdConfiguration);
  else    
    gMessMgr->Error()   << "No  access to ssdConfiguration table " << endm;

  if(ssdDimensions)
    m_Reader->setSsdDimensions(ssdDimensions);
  else    
    gMessMgr->Error()   << "No  access to ssdDimensions table " << endm;
}
//_____________________________________________________________________________
void StSsdDbMaker::setSsdDbWriter(int unixTime)
{    
  mUnixTimeStamp = unixTime;
  mWriter        = new StSsdDbWriter(mUnixTimeStamp);
}

//_____________________________________________________________________________
void StSsdDbMaker::setSsdConfig()
{    
  ssdSetConfig = new St_ObjectSet("StSsdConfig");
  AddConst(ssdSetConfig);  
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdDirectConfig()
{      
  gMessMgr->Info() << " StSsdDbMaker::readSsdDirectConfig - Start - " <<endm;

  StDbTable* ssdConfigTable = mConfigGeom -> addDbTable("ssdConfiguration");
  mDbMgr->fetchDbTable(ssdConfigTable);
  ssdConfiguration_st *config  = (ssdConfiguration_st*) ssdConfigTable->GetTable() ;

  if (m_Reader)
    ssdSetConfig->SetObject((TObject*)m_Reader->getConfiguration(config));
  else
    gMessMgr->Error() << " Pb in readSsdDirectConfig....m_Reader Still Not Defined" <<endm;
  
  gMessMgr->Info() << " StSsdDbMaker::readSsdDirectConfig - End - " <<endm;

}
//_____________________________________________________________________________
void StSsdDbMaker::readSsdConfig()
{    
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  
  St_ssdConfiguration* ssdConfiguration;
  ssdConfiguration   = (St_ssdConfiguration    *) local("ssd/ssdConfiguration");
  if (m_Reader)
    ssdSetConfig->SetObject((TObject*)m_Reader->getConfiguration(ssdConfiguration));
}
//_____________________________________________________________________________
void StSsdDbMaker::setSsdGeometry()
{
  wafersPositionSetGeom = new St_ObjectSet("StSsdGeometry");
  AddConst(wafersPositionSetGeom);
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdDirectGeometry()
{
  gMessMgr->Info() <<" SsdDbMaker::readSsdDirectGeometry - Start - "<<endm;

  StDbTable* ssdGeomTable = mGeom -> addDbTable("ssdWafersPosition");
  mDbMgr->fetchDbTable(ssdGeomTable);
  ssdWafersPosition_st *geom  = (ssdWafersPosition_st*) ssdGeomTable->GetTable();

  if (m_Reader)
    wafersPositionSetGeom->SetObject((TObject*)m_Reader->getGeometry(geom));
  else
    gMessMgr->Error()<<" Pb in StSsdDbMaker::readSsdDirectGeometry : m_Reader Not Defined "<<endm;

  gMessMgr->Info() <<" SsdDbMaker::readSsdDirectGeometry - End - "<<endm;
}

//_____________________________________________________________________________
void StSsdDbMaker::readSsdGeometry()
{
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  
  St_ssdWafersPosition* wafersPosition;
  wafersPosition  = (St_ssdWafersPosition    *) local("ssd/ssdWafersPosition");
  
  St_ssdDimensions* ssdDimensions;
  ssdDimensions  = (St_ssdDimensions    *) local("ssd/ssdDimensions");

  if (m_Reader && wafersPosition)
    wafersPositionSetGeom->SetObject((TObject*)m_Reader->getGeometry(wafersPosition));
  else
      gMessMgr->Info()<<" SetGeom not done "<<endm;
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Make" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSsdDbMaker::Clear(const char*)
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Clear" << endm;
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Finish" << endm;
  return kStOK;
}

/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#include "StSsdDbMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

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
  mTimeStamp    = "2003-10-01 00:00:01";

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
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Init" << endm;


  // setSsdDbWriter(123456789);

  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);

  St_ssdWafersPosition* wafersPosition;
  wafersPosition  = (St_ssdWafersPosition    *) local("ssd/ssdWafersPosition");
  St_ssdConfiguration* ssdConfiguration;
  ssdConfiguration  = (St_ssdConfiguration    *) local("ssd/ssdConfiguration");
  St_ssdDimensions* ssdDimensions;
  ssdDimensions  = (St_ssdDimensions   *) local("ssd/ssdDimensions");
  
  if (wafersPosition) 
    cout<<"StSsdDbMaker::Init ----> ssdWafersPosition : Nber of Wafers = "<<wafersPosition->GetNRows()<<endl;
  else
    gMessMgr->Error() << "No  access to wafer position table " << endm;

  if (ssdConfiguration) 
    cout<<"StSsdDbMaker::Init ----> ssdConfiguration :  = "<<ssdConfiguration->GetNRows()<<endl;
  else
    gMessMgr->Error() << "No  access to ssdConfiguration table " << endm;


  // HIER WEITERMACHEN

  ssdConfiguration_st *config = ssdConfiguration->GetTable();
  cout << "TEST " << ssdConfiguration->GetNRows() << "   TEST 2 : " << config[0].nMaxWafers    <<endl;
 

  if (ssdDimensions) 
    cout<<"StSsdDbMaker::Init ----> ssdDimensions :  = "<<ssdDimensions->GetNRows()<<endl;
  else
    gMessMgr->Error() << "No  access to ssdDimensions table " << endm;
 
  
  setSsdDb_Reader();
    
  setSsdConfig();
  readSsdConfig();

  setSsdGeometry();
  readSsdGeometry();

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::InitRun(int runumber)
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::InitRun" << endm;
  cout << "StSsdDbMaker::InitRun" << endl;
  readSsdGeometry();
  return kStOk;
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
  if (Debug()) gMessMgr->Debug() << "StSsdDaqMaker::Clear" << endm;
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSsdDbMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSsdDbMaker::Finish" << endm;
  return kStOK;
}

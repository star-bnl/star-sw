/***************************************************************************
 * Author: Joerg Reinnarth joerg.reinnarth@subatech.in2p3.fr
 ***************************************************************************
 * Description: SSD DB access Maker
 ***************************************************************************
 * Revision 1.01  2004/05/19  Reinnarth
 * Getting started and testing
 **************************************************************************/

#include "StSsdDbWriter.hh"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StMessMgr.h"

#include "StDbLib/StDbManager.hh"                      // Database Libraries
#include "StDbLib/StDbConfigNode.hh"                   //
#include "StDbLib/StDbTable.h"                         //

#include "tables/St_ssdConfiguration_Table.h"

#ifdef __ROOT__
ClassImp(StSsdDbWriter)
#endif

//_____________________________________________________________________________
StSsdDbWriter::StSsdDbWriter(Text_t *timestamp)
{
  // set time stamp
  mTimeStamp = timestamp;
  mUnixTimeStamp = 0;

  cout << "TEST: In StSsdDbWriter.cxx Text_t" << endl;

  // set DB manager
  setDbManager();
  testwriting();
}

//_____________________________________________________________________________
StSsdDbWriter::StSsdDbWriter(Int_t timestamp)
{
  // set time stamp
  mTimeStamp = 0;
  mUnixTimeStamp = timestamp;

  cout << "TEST: In StSsdDbWriter.cxx Int_t" << endl;

  // set DB manager
  setDbManager();
  testwriting();
}

//_____________________________________________________________________________
StSsdDbWriter::~StSsdDbWriter()
{
  if (mConfigCalib)
    delete mConfigCalib;
  if (mConfigGeom)
    delete mConfigGeom;
  if (mConfigCond)
    delete mConfigCond;
}

//_____________________________________________________________________________
void StSsdDbWriter::setDbManager()
{
  //->get the singleton manager
  //
  mDbMgr = StDbManager::Instance();
  mDbMgr -> setVerbose(false);             // set Verbose mode for debug

  //-> connect to the db & get an empty container
  //
  // mConfigCalib = mDbMgr -> initConfig(dbCalibrations,dbSsd);
  mConfigGeom  = mDbMgr -> initConfig(dbGeometry,dbSsd);
  //  mConfigCond  = mDbMgr -> initConfig(dbConditions,dbSsd);

  // set the store time. (need to be only once for an ensemble of tables)
  //
  if (mTimeStamp)
    mDbMgr->setStoreTime(mTimeStamp);
  else if (mUnixTimeStamp)
    mDbMgr->setStoreTime(mUnixTimeStamp);
}

//_____________________________________________________________________________

void StSsdDbWriter::testwriting()
{

  // And now we try to write something somewhere ;)

  cout << "TEST testwriting1" << dbGeometry << endl;

  // Start: WRITING THE FILES
  StDbTable* ssdConfiguration_Table     = mConfigGeom->addDbTable("ssdConfiguration");
  ssdConfiguration_st *ssdConfiguration = new ssdConfiguration_st[0];
  ssdConfiguration[0].nMaxWafers        = 666;
  ssdConfiguration_Table->SetTable((char*)ssdConfiguration,1,0); 

  mDbMgr->storeDbTable(ssdConfiguration_Table);

  // End: WRITING INTO THE TABLE
}

//_____________________________________________________________________________

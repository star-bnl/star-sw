//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: generate trigger response based on ADC
// implemented BEMC,EEMC,....
// >StTriggerSimu/*SUB*/St*SUB*TriggerSimu.h
// >where *SUB* are the subsystems: Eemc, Bemc, Bbc,.... 
// > L2 is served by a separate maker
//
//////////////////////////////////////////////////////////////////////////

// $Id: StTriggerSimuMaker.cxx,v 1.26 2009/09/20 06:46:29 pibero Exp $


#include <Stiostream.h>
#include "StChain.h"
#include "TFile.h"
#include <math.h>

#include <fstream>
#include "StEmcUtil/database/StEmcDecoder.h"

#include "St_db_Maker/St_db_Maker.h" // just for time stamp
#include "StEEmcUtil/EEdsm/EMCdsm2Tree.h"// to access Etot

//StEvent
#include "StEvent/StEvent.h"

//get  EEMC
#include "Eemc/StEemcTriggerSimu.h"
#include "Eemc/EemcHttpInfo.h"
#include "StEEmcUtil/EEdsm/EEfeeTPTree.h" // for printouts only
#include "StEEmcUtil/EEdsm/EEfeeTP.h"  // for printouts only

//get BEMC
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

//get EMC
#include "StTriggerUtilities/Emc/StEmcTriggerSimu.h"

//get BBC
#include "Bbc/StBbcTriggerSimu.h"

//get L2
#include "L2Emulator/StL2TriggerSimu.h"

//get HEAD Maker
#include "StTriggerSimuMaker.h"
#include "StTriggerSimuResult.h"

// ROOT MySQL
#include "TMySQLServer.h"
#include "TMySQLResult.h"
#include "TMySQLRow.h"

ClassImp(StTriggerSimuMaker)

StTriggerSimuMaker::StTriggerSimuMaker(const char *name):StMaker(name) {
    mDbMk = dynamic_cast<St_db_Maker*> ( this->GetMakerInheritsFrom("St_db_Maker") );
    mYear=-1;
    mMCflag=0;
    eemc=0;
    bbc=0;
    bemc=0;
    lTwo=0;
    emc = new StEmcTriggerSimu;
    for (int a=0; a<numSimulators; a++){
      mSimulators[a]=0;
    }
}

StTriggerSimuMaker::~StTriggerSimuMaker() { /* no-op */ }


void StTriggerSimuMaker::useEemc(int flag){
    eemc=new StEemcTriggerSimu();
    eemc->setConfig(flag);
    mSimulators[0]=eemc;
}

void StTriggerSimuMaker::useBbc(){
    bbc=new StBbcTriggerSimu;
    mSimulators[1]=bbc;
}

void StTriggerSimuMaker::useBemc(){
    bemc=new StBemcTriggerSimu;
    bemc->setHeadMaker(this);
    mSimulators[2]=bemc;
}

void StTriggerSimuMaker::useL2(StGenericL2Emulator* L2Mk){
    lTwo=new StL2TriggerSimu(L2Mk);
    mSimulators[4]=lTwo;

}

Int_t StTriggerSimuMaker::Init() {
    LOG_INFO <<Form("StTriggerSimuMaker::Init(), MCflag=%d",mMCflag)<<endm;
    if(eemc) {
        eemc->setHList(mHList);
    }

    if(bemc) {
        bemc->setHList(mHList);
    }

    for(Int_t i=0; i<numSimulators; i++) {
      if(mSimulators[i]){  
        mSimulators[i]->setMC(mMCflag);
        mSimulators[i]->Init();
      }

    }


    return StMaker::Init();
}

void StTriggerSimuMaker::Clear(const Option_t*){
    LOG_DEBUG<<"StTriggerSimuMaker::Clear()"<<endm;
    
    for(Int_t i=0; i<numSimulators; i++) {
      if (mSimulators[i]){
        mSimulators[i]->Clear();
      }
    }
    
    mResults.clear();
}

Int_t StTriggerSimuMaker::InitRun(int runNumber) {
    assert(mDbMk);
    mYear = mDbMk->GetDateTime().GetYear();
    int yyyymmdd = mDbMk->GetDateTime().GetDate(); //form of 19971224 (i.e. 24/12/1997)
    int hhmmss = mDbMk->GetDateTime().GetTime(); //form of 123623 (i.e. 12:36:23)
    LOG_INFO << Form("InitRun() run=%d yyyymmdd=%d  hhmmss=%06d\n", runNumber, yyyymmdd, hhmmss) << endm;

    //Use unified EMC trigger for EEMC/BEMC triggers in y=2009 or later
    if (mYear >= 2009 && (mSimulators[0] || mSimulators[2])) {
      emc->setHeadMaker(this);
      emc->setBemc(bemc);
      emc->setEemc(eemc);
      mSimulators[3] = emc;

      // If MC, get real run number from database time stamp
      if (mMCflag) runNumber = get2009RunNumberFromTimestamp(GetDBTime());
    }

    for (Int_t i = 0; i < numSimulators; ++i)
      if (mSimulators[i])
	mSimulators[i]->InitRun(runNumber);

    return kStOK;
}

Int_t StTriggerSimuMaker::Make() {
   
  for(Int_t i=0; i<numSimulators; i++) {
    if (mSimulators[i]){
      mSimulators[i]->Make();
    }
  }
  return kStOK;
}

bool StTriggerSimuMaker::isTrigger(int trigId) {

  for(Int_t i=0; i<numSimulators; i++) {
    if (mSimulators[i]){  
      
      if(mSimulators[i]->triggerDecision(trigId) == kNo) return false;
    }
  }
  
  return true;
}


const StTriggerSimuResult& StTriggerSimuMaker::detailedResult(unsigned int trigId) {
    // first check if we already filled this result
    for(unsigned i=0; i<mResults.size(); i++) {
        if(mResults[i].triggerId() == trigId) return mResults[i];
    }
    
    StTriggerSimuResult result;
    result.setTriggerId(trigId);
    if(bbc) {
        result.setBbcDecision(bbc->triggerDecision(trigId));
    }
    if(bemc) {
        result.setBemcDecision(bemc->triggerDecision(trigId));

        if(bemc->triggerDecision(trigId)==1) {
            // Record HTs above Threshold
            vector< pair<int,int> > Towers=bemc->getTowersAboveThreshold(trigId);
            for(vector< pair<int,int> >::iterator itr=Towers.begin(); itr!=Towers.end(); itr++){
                result.addHighTower((*itr).first,(*itr).second);
            }
            // Record TPs above Threshold
            vector< pair<int,int> > tPatches=bemc->getTriggerPatchesAboveThreshold(trigId);
            for(vector< pair<int,int> >::iterator itr=tPatches.begin(); itr!=tPatches.end(); itr++){
                result.addTriggerPatch((*itr).first,(*itr).second);
            }
            // Record JPs above Threshold
            vector< pair<int,int> > jPatches=bemc->getJetPatchesAboveThreshold(trigId);
            for(vector< pair<int,int> >::iterator itr=jPatches.begin(); itr!=jPatches.end(); itr++){
                result.addJetPatch((*itr).first,(*itr).second);	    
            }
        }
    }
    if(eemc) {
        result.setEemcDecision(eemc->triggerDecision(trigId));
        // fill HT/TP/JP details here
    }
    if(lTwo) {
        result.setL2Decision(lTwo->triggerDecision(trigId));
        result.setL2Result(lTwo->result());
    }
    mResults.push_back(result);
    return mResults.back();
}

Int_t StTriggerSimuMaker::Finish() {
  return StMaker::Finish();
}

int StTriggerSimuMaker::get2009RunNumberFromTimestamp(const TDatime& timestamp) const
{
  LOG_INFO << "Open connection to RunLog database for Run 9" << endm;

  TString database = "mysql://dbbak.starp.bnl.gov:3408/RunLog";
  TString user = "";
  TString pass = "";

  LOG_INFO << "database:\t" << database << endm;
  LOG_INFO << "user:\t" << user << endm;
  LOG_INFO << "pass:\t" << pass << endm;
  
  TMySQLServer* mysql = (TMySQLServer*)TMySQLServer::Connect(database, user, pass);

  if (!mysql) {
    LOG_WARN << "Could not connect to Run 9 database" << endm;
    return kStWarn;
  }

  int runNumber = 0;

  TString query = Form("select runNumber from runDescriptor where (from_unixtime(startRunTime) <= '%s') and ('%s' <= from_unixtime(endRunTime))", timestamp.AsSQLString(), timestamp.AsSQLString());

  if (TMySQLResult* result = (TMySQLResult*)mysql->Query(query)) {
    while (TMySQLRow* row = (TMySQLRow*)result->Next()) {
      runNumber = atoi(row->GetField(0));
      LOG_INFO << Form("Found run number = %d for timestamp '%s'", runNumber, timestamp.AsSQLString()) << endm;
    }
  }
  else {
    LOG_WARN << Form("No run number found for database timestamp '%s'", timestamp.AsSQLString()) << endm;
  }

  LOG_INFO << "Close connection to database" << endm;

  mysql->Close();

  return runNumber;
}

/*****************************************************************************
 * $Log: StTriggerSimuMaker.cxx,v $
 * Revision 1.26  2009/09/20 06:46:29  pibero
 * Updates for Run 9
 *
 * Revision 1.25  2009/02/04 20:00:48  rfatemi
 * change includes for StEmcDecoder
 *
 * Revision 1.24  2009/02/03 15:40:38  rfatemi
 * Changed structure of mSimulators to accomodate 2009 EMC simulator update
 *
 * Revision 1.23  2009/01/17 13:09:02  pibero
 * Initial Version of EMC DSM algorithms for 2009
 *
 * Revision 1.22  2008/01/22 18:06:26  kocolosk
 * added detailedResult code for BEMC L0, courtesy Dave Staszak
 * fixed two bugs in vector accessors in result class (also thanks to Dave)
 *
 * Revision 1.21  2008/01/17 17:04:07  kocolosk
 * some revisions to StTriggerSimuResult structure to hopefully improve clarity and maintainability
 *
 * Revision 1.20  2008/01/17 01:58:25  kocolosk
 * StTriggerSimuResult makes detailed emulation results persistent
 *
 *
 *****************************************************************************/

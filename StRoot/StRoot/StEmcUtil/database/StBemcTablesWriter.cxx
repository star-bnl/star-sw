 /**************************************************************************
 *
 * $Id: StBemcTablesWriter.cxx,v 1.7 2015/03/02 20:28:26 jkadkins Exp $
 * Author:      Adam Kocoloski, MIT, kocolosk@mit.edu
 *
 ***************************************************************************/

#include "StBemcTablesWriter.h"

#include "TSystem.h"
#include "TFile.h"
#include "TDatime.h"

#include "TUnixTime.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include "StMessMgr.h"

#include "StEmcRawMaker/defines.h"

ClassImp(StBemcTablesWriter)

StBemcTablesWriter::StBemcTablesWriter() : StBemcTables() {
  mDbTables["bemcPed"]          = NULL;
  mDbTables["bprsPed"]          = NULL;
  mDbTables["bsmdePed"]         = NULL;
  mDbTables["bsmdpPed"]         = NULL;
  
  mDbTables["bemcStatus"]       = NULL;
  mDbTables["bprsStatus"]       = NULL;
  mDbTables["bsmdeStatus"]      = NULL;
  mDbTables["bsmdpStatus"]      = NULL;
  
  mDbTables["bemcCalib"]        = NULL;
  mDbTables["bprsCalib"]        = NULL;
  mDbTables["bsmdeCalib"]       = NULL;
  mDbTables["bsmdpCalib"]       = NULL;
  
  mDbTables["bemcGain"]         = NULL;
  mDbTables["bprsGain"]         = NULL;
  mDbTables["bsmdeGain"]        = NULL;
  mDbTables["bsmdpGain"]        = NULL;
  
  mDbTables["bemcTriggerPed"]   = NULL;
  mDbTables["bemcTriggerPed4"]  = NULL;
  mDbTables["bemcTriggerStatus"]= NULL;
  mDbTables["bemcTriggerLUT"]   = NULL;

  mDbHandler = new StEmcDbHandler();
}

StBemcTablesWriter::~StBemcTablesWriter(){
  for(map<string, StDbTable*>::iterator it=mDbTables.begin(); it!=mDbTables.end(); ++it){
    it->second = NULL;
    delete it->second;
  }
  
  mDbTables.clear();
  if(mDbHandler) delete mDbHandler;
}

void StBemcTablesWriter::loadTables(const char * sqlTime, const char * flavor) {
  TDatime dt(sqlTime);
  TUnixTime ut(dt, 1); //sqlTime must be in GMT
  
  if (mDecoder) mDecoder->SetDateTime(dt.GetDate(), dt.GetTime());
  
  // the BTOW map fix should be used *ONLY* for runs before 2006
  // For runs after 2006-01-01 the database is supposed to be fixed
  if(dt.GetDate() >= 20060101) mBtowMapFix = kFALSE;

  for(map<string, StDbTable*>::iterator it = mDbTables.begin(); it!=mDbTables.end(); it++) {
    string tableName = it->first;
    StDbTable * t = it->second;
    if( (!t) || (ut() < t->getBeginTime()) || (ut() >= t->getEndTime()) ) {
      if (mDbHandler) {
	mDbHandler->setTimeStamp(sqlTime);
	mDbHandler->setFlavor(flavor);
	mDbHandler->setTableName((char*)tableName.c_str());
	it->second = mDbHandler->getDbTable();
      }
      t = it->second;
      
      if (t) {
	string beginTime = StEmcDbHandler::timeToSqlTime(t->getBeginDateTime());
	string endTime   = StEmcDbHandler::timeToSqlTime(t->getEndDateTime());
	map<string, pair<string, string> >::iterator iter = mValidRanges.find(tableName);
	if(iter == mValidRanges.end()) {
	  mValidRanges[tableName] = make_pair(beginTime, endTime);
	  LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
	} else if( beginTime != (iter->second).first ) {
	  (iter->second).first    = beginTime;
	  (iter->second).second   = endTime;
	  LOG_INFO << Form("loaded a new %20s table with beginTime %s and endTime %s", tableName.c_str(), beginTime.c_str(), endTime.c_str()) << endm; 
	}
      }
    }
  }
  
  if(mBtowP) delete mBtowP;
  mBtowP = mDbTables["bemcPed"] ? (emcPed_st*) mDbTables["bemcPed"]->GetTableCpy() : 0;
  
  if(mBprsP) delete mBprsP;
  mBprsP = mDbTables["bprsPed"] ? (emcPed_st*) mDbTables["bprsPed"]->GetTableCpy() : 0;
  
  if(mSmdeP) delete mSmdeP;
  mSmdeP = mDbTables["bsmdePed"] ? (smdPed_st*) mDbTables["bsmdePed"]->GetTableCpy() : 0;
    
  if(mSmdpP) delete mSmdpP;
  mSmdpP = mDbTables["bsmdpPed"] ? (smdPed_st*) mDbTables["bsmdpPed"]->GetTableCpy() : 0;
        

  if(mBtowS) delete mBtowS;
  mBtowS = mDbTables["bemcStatus"] ? (emcStatus_st*) mDbTables["bemcStatus"]->GetTableCpy() : 0;
    
  if(mBprsS) delete mBprsS;
  mBprsS = mDbTables["bprsStatus"] ? (emcStatus_st*) mDbTables["bprsStatus"]->GetTableCpy() : 0;
    
  if(mSmdeS) delete mSmdeS;
  mSmdeS = mDbTables["bsmdeStatus"] ? (smdStatus_st*) mDbTables["bsmdeStatus"]->GetTableCpy() : 0;
    
  if(mSmdpS) delete mSmdpS;
  mSmdpS = mDbTables["bsmdpStatus"] ? (smdStatus_st*) mDbTables["bsmdpStatus"]->GetTableCpy() : 0;


  if(mBtowC) delete mBtowC;
  mBtowC = mDbTables["bemcCalib"] ? (emcCalib_st*) mDbTables["bemcCalib"]->GetTableCpy() : 0;
    
  if(mBprsC) delete mBprsC;
  mBprsC = mDbTables["bprsCalib"] ? (emcCalib_st*) mDbTables["bprsCalib"]->GetTableCpy() : 0;
    
  if(mSmdeC) delete mSmdeC;
  mSmdeC = mDbTables["bsmdeCalib"] ? (smdCalib_st*) mDbTables["bsmdeCalib"]->GetTableCpy() : 0;
    
  if(mSmdpC) delete mSmdpC;
  mSmdpC = mDbTables["bsmdpCalib"] ? (smdCalib_st*) mDbTables["bsmdpCalib"]->GetTableCpy() : 0;
    

  if(mBtowG) delete mBtowG;
  mBtowG = mDbTables["bemcGain"] ? (emcGain_st*) mDbTables["bemcGain"]->GetTableCpy() : 0;
    
  if(mBprsG) delete mBprsG;
  mBprsG = mDbTables["bprsGain"] ? (emcGain_st*) mDbTables["bprsGain"]->GetTableCpy() : 0;
    
  if(mSmdeG) delete mSmdeG;
  mSmdeG = mDbTables["bsmdeGain"] ? (smdGain_st*) mDbTables["bsmdeGain"]->GetTableCpy() : 0;
    
  if(mSmdpG) delete mSmdpG;
  mSmdpG = mDbTables["bsmdpGain"] ? (smdGain_st*) mDbTables["bsmdpGain"]->GetTableCpy() : 0;
    
    
  if(mTrigS) delete mTrigS;
  mTrigS = mDbTables["bemcTriggerStatus"] ? (emcTriggerStatus_st*) mDbTables["bemcTriggerStatus"]->GetTableCpy() : 0;
    
  if(mTrigP) delete mTrigP;
  mTrigP = mDbTables["bemcTriggerPed"] ? (emcTriggerPed_st*) mDbTables["bemcTriggerPed"]->GetTableCpy() : 0;

  if(mTrigP4) delete mTrigP4;
  mTrigP4 = mDbTables["bemcTriggerPed4"] ? (bemcTriggerPed4_st*) mDbTables["bemcTriggerPed4"]->GetTableCpy() : 0;
  
  if(mTrigL) delete mTrigL;
  mTrigL = mDbTables["bemcTriggerLUT"] ? (emcTriggerLUT_st*) mDbTables["bemcTriggerLUT"]->GetTableCpy() : 0;
}

void StBemcTablesWriter::setCalib(int det, int softId, int power, float val) {
  switch(det) {
  case BTOW:
    if (mBtowC) mBtowC->AdcToE[softId-1][power] = val;
    break;
        
  case BPRS:
    if (mBprsC) mBprsC->AdcToE[softId-1][power] = val;
    break;
        
  case BSMDE:
    if (mSmdeC) mSmdeC->AdcToE[softId-1][power] = val;
    break;
        
  case BSMDP:
    if (mSmdpC) mSmdpC->AdcToE[softId-1][power] = val;
    break;
  }
}

void StBemcTablesWriter::setPedestal(int det, int softId, int cap, float val) {
  short packedValue = static_cast<short>(val*100.0);
  switch(det) {
  case BTOW:
    if (mBtowP) mBtowP->AdcPedestal[softId-1] = packedValue;
    break;
        
  case BPRS:
    if (mBprsP) mBprsP->AdcPedestal[softId-1] = packedValue;
    break;
        
  case BSMDE:
    if (mSmdeP) mSmdeP->AdcPedestal[softId-1][cap] = packedValue;
    break;
            
  case BSMDP:
    if (mSmdpP) mSmdpP->AdcPedestal[softId-1][cap] = packedValue;
    break;
  }
}

void StBemcTablesWriter::setPedestalRMS(int det, int softId, int cap, float val) {
  short packedValue = static_cast<short>(val*100.0);
  switch(det) {
  case BTOW:
    if (mBtowP) mBtowP->AdcPedestalRMS[softId-1] = packedValue;
    break;
        
  case BPRS:
    if (mBprsP) mBprsP->AdcPedestalRMS[softId-1] = packedValue;
    break;
        
  case BSMDE:
    if (mSmdeP) mSmdeP->AdcPedestalRMS[softId-1][cap] = packedValue;
    break;
            
  case BSMDP:
    if (mSmdpP) mSmdpP->AdcPedestalRMS[softId-1][cap] = packedValue;
    break;
  }
}

void StBemcTablesWriter::setGain(int det, int softId, float val) {
  switch(det) {
  case BTOW:
    if (mBtowG) mBtowG->Gain[softId-1] = val;
    break;
        
  case BPRS:
    if (mBprsG) mBprsG->Gain[softId-1] = val;
    break;
        
  case BSMDE:
    if (mSmdeG) mSmdeG->Gain[softId-1] = val;
    break;
        
  case BSMDP:
    if (mSmdpG) mSmdpG->Gain[softId-1] = val;
    break;
  }
}

void StBemcTablesWriter::setStatus(int det, int softId, unsigned short val) {
  if(val > 255) {
    cout << "status must be between 0 and 255 -- you supplied " << val << endl;
    return;
  }
    
  switch(det) {
  case BTOW:
    if (mBtowS) mBtowS->Status[softId-1] = val;
    break;
        
  case BPRS:
    if (mBprsS) mBprsS->Status[softId-1] = val;
    break;
        
  case BSMDE:
    if (mSmdeS) mSmdeS->Status[softId-1] = val;
    break;
        
  case BSMDP:
    if (mSmdpS) mSmdpS->Status[softId-1] = val;
    break;
  }
}

void StBemcTablesWriter::setCalibStatus(int det, int softId, unsigned short val) {
  if(val > 255) {
    cout << "status must be between 0 and 255 -- you supplied " << val << endl;
    return;
  }
    
  switch(det) {
  case BTOW:
    if (mBtowC) mBtowC->Status[softId-1] = val;
    break;
        
  case BPRS:
    if (mBprsC) mBprsC->Status[softId-1] = val;
    break;
        
  case BSMDE:
    if (mSmdeC) mSmdeC->Status[softId-1] = val;
    break;
        
  case BSMDP:
    if (mSmdpC) mSmdpC->Status[softId-1] = val;
    break;
  }
}

void StBemcTablesWriter::setPedestalStatus(int det, int softId, unsigned short val) {
  if(val > 255) {
    cout << "status must be between 0 and 255 -- you supplied " << val << endl;
    return;
  }
    
  switch(det) {
  case BTOW:
    if (mBtowP) mBtowP->Status[softId-1] = val;
    break;
        
  case BPRS:
    if (mBprsP) mBprsP->Status[softId-1] = val;
    break;
        
  case BSMDE:
    if (mSmdeP) mSmdeP->Status[softId-1] = val;
    break;
        
  case BSMDP:
    if (mSmdpP) mSmdpP->Status[softId-1] = val;
    break;
  }
}

void StBemcTablesWriter::setGainStatus(int det, int softId, unsigned short val) {
  if(val > 255) {
    cout << "status must be between 0 and 255 -- you supplied " << val << endl;
    return;
  }
    
  switch(det) {
  case BTOW:
    if (mBtowG) mBtowG->Status[softId-1] = val;
    break;
        
  case BPRS:
    if (mBprsG) mBprsG->Status[softId-1] = val;
    break;
        
  case BSMDE:
    if (mSmdeG) mSmdeG->Status[softId-1] = val;
    break;
        
  case BSMDP:
    if (mSmdpG) mSmdpG->Status[softId-1] = val;
    break;
  }
}

void StBemcTablesWriter::writeToDb(const char * tableName, const char * timeStamp, const char * flavor) {
  if (!mDbHandler) return;

  mDbHandler->setTableName(tableName);
  mDbHandler->setTimeStamp(timeStamp);
  mDbHandler->setFlavor(flavor);
    
  if(!strcmp(tableName, "bemcCalib"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowC)); return; }
  if(!strcmp(tableName, "bprsCalib"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsC)); return; }
  if(!strcmp(tableName, "bsmdeCalib"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeC)); return; }
  if(!strcmp(tableName, "bsmdpCalib"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpC)); return; }
                                                                                                            
  if(!strcmp(tableName, "bemcPed"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowP)); return; }
  if(!strcmp(tableName, "bprsPed"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsP)); return; }
  if(!strcmp(tableName, "bsmdePed"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeP)); return; }
  if(!strcmp(tableName, "bsmdpPed"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpP)); return; }
                                                                                                            
  if(!strcmp(tableName, "bemcGain"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowG)); return; }
  if(!strcmp(tableName, "bprsGain"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsG)); return; }
  if(!strcmp(tableName, "bsmdeGain"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeG)); return; }
  if(!strcmp(tableName, "bsmdpGain"))   { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpG)); return; }
    
  if(!strcmp(tableName, "bemcStatus"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mBtowS)); return; }
  if(!strcmp(tableName, "bprsStatus"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mBprsS)); return; }
  if(!strcmp(tableName, "bsmdeStatus")) { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdeS)); return; }
  if(!strcmp(tableName, "bsmdpStatus")) { mDbHandler->writeToDb(reinterpret_cast<char*>(mSmdpS)); return; }
    
  if(!strcmp(tableName, "bemcTriggerPed"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigP));  return; }
  if(!strcmp(tableName, "bemcTriggerPed4"))    { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigP4)); return; }
  if(!strcmp(tableName, "bemcTriggerStatus"))  { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigS));  return; }
  if(!strcmp(tableName, "bemcTriggerLUT"))     { mDbHandler->writeToDb(reinterpret_cast<char*>(mTrigL));  return; }
    
    
    
  LOG_ERROR << "Can't upload a table with name = " << tableName << endm;
}

void StBemcTablesWriter::writeToFile(const char * fileName) {
  TFile *f = new TFile(fileName,"recreate");
  if (f) {
    string baseName = gSystem->BaseName(fileName);
    
    if(baseName.find("bemcCalib") == 0) {
      St_emcCalib* table = new St_emcCalib("bemcCalib",1);
      if (table && mBtowC) {
	table->AddAt(static_cast<void*>(mBtowC),0);
	table->Write();
      }
    }
    else if(baseName.find("bprsCalib") == 0) {
      St_emcCalib* table = new St_emcCalib("bprsCalib",1);
      if (table && mBprsC) {
	table->AddAt(static_cast<void*>(mBprsC),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdeCalib") == 0) {
      St_smdCalib* table = new St_smdCalib("bsmdeCalib",1);
      if (table && mSmdeC) {
	table->AddAt(static_cast<void*>(mSmdeC),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdpCalib") == 0) {
      St_smdCalib* table = new St_smdCalib("bsmdpCalib",1);
      if (table && mSmdpC) {
	table->AddAt(static_cast<void*>(mSmdpC),0);
	table->Write();
      }
    }
    
    else if(baseName.find("bemcPed") == 0) {
      St_emcPed* table = new St_emcPed("bemcPed",1);
      if (table && mBtowP) {
	table->AddAt(static_cast<void*>(mBtowP),0);
	table->Write();
      }
    }
    else if(baseName.find("bprsPed") == 0) {
      St_emcPed* table = new St_emcPed("bprsPed",1);
      if (table && mBprsP) {
	table->AddAt(static_cast<void*>(mBprsP),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdePed") == 0) {
      St_smdPed* table = new St_smdPed("bsmdePed",1);
      if (table && mSmdeP) {
	table->AddAt(static_cast<void*>(mSmdeP),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdpPed") == 0) {
      St_smdPed* table = new St_smdPed("bsmdpPed",1);
      if (table && mSmdpP) {
	table->AddAt(static_cast<void*>(mSmdpP),0);
	table->Write();
      }
    }    
    else if(baseName.find("bemcGain") == 0) {
      St_emcGain* table = new St_emcGain("bemcGain",1);
      if (table && mBtowG) {
	table->AddAt(static_cast<void*>(mBtowG),0);
	table->Write();
      }
    }
    else if(baseName.find("bprsGain") == 0) {
      St_emcGain* table = new St_emcGain("bprsGain",1);
      if (table && mBprsG) {
	table->AddAt(static_cast<void*>(mBprsG),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdeGain") == 0) {
      St_smdGain* table = new St_smdGain("bsmdeGain",1);
      if (table && mSmdeG) {
	table->AddAt(static_cast<void*>(mSmdeG),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdpGain") == 0) {
      St_smdGain* table = new St_smdGain("bsmdpGain",1);
      if (table && mSmdpG) {
	table->AddAt(static_cast<void*>(mSmdpG),0);
	table->Write();
      }
    }
    
    else if(baseName.find("bemcStatus") == 0) {
      St_emcStatus* table = new St_emcStatus("bemcStatus",1);
      if (table && mBtowS) {
	table->AddAt(static_cast<void*>(mBtowS),0);
	table->Write();
      }
    }
    else if(baseName.find("bprsStatus") == 0) {
      St_emcStatus* table = new St_emcStatus("bprsStatus",1);
      if (table && mBprsS) {
	table->AddAt(static_cast<void*>(mBprsS),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdeStatus") == 0) {
      St_smdStatus* table = new St_smdStatus("bsmdeStatus",1);
      if (table && mSmdeS) {
	table->AddAt(static_cast<void*>(mSmdeS),0);
	table->Write();
      }
    }
    else if(baseName.find("bsmdpStatus") == 0) {
      St_smdStatus* table = new St_smdStatus("bsmdpStatus",1);
      if (table && mSmdpS) {
	table->AddAt(static_cast<void*>(mSmdpS),0);
	table->Write();
      }
    }
    else if(baseName.find("bemcTriggerPed4") == 0) {
      St_bemcTriggerPed4* table = new St_bemcTriggerPed4("bemcTriggerPed4",1);
      if (table && mTrigP4) {
	table->AddAt(static_cast<void*>(mTrigP4),0);
	table->Write();
      }
    }
    else if(baseName.find("bemcTriggerPed") == 0) {
      St_emcTriggerPed* table = new St_emcTriggerPed("bemcTriggerPed",1);
      if (table && mTrigP) {
	table->AddAt(static_cast<void*>(mTrigP),0);
	table->Write();
      }
    }
    else if(baseName.find("bemcTriggerStatus") == 0) {
      St_emcTriggerStatus* table = new St_emcTriggerStatus("bemcTriggerStatus",1);
      if (table && mTrigS) {
	table->AddAt(static_cast<void*>(mTrigS),0);
	table->Write();
      }
    }
    else if(baseName.find("bemcTriggerLUT") == 0) {
      St_emcTriggerLUT* table = new St_emcTriggerLUT("bemcTriggerLUT",1);
      if (table && mTrigL) {
	table->AddAt(static_cast<void*>(mTrigL),0);
	table->Write();
      }
    }
    
    f->Close();
  }
}

void StBemcTablesWriter::loadTableFromFile(TFile *f) {
  string fileName = gSystem->BaseName(f->GetName());
    
  if(fileName.find("bemcPed") == 0) {
    if(mBtowP) delete mBtowP;
    mBtowP = f->Get("bemcPed") ? ((St_emcPed*)f->Get("bemcPed"))->GetTable() : 0;
  }
  else if(fileName.find("bprsPed") == 0) {
    if(mBprsP) delete mBprsP;
    mBprsP = f->Get("bprsPed") ? ((St_emcPed*)f->Get("bprsPed"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdePed") == 0) {
    if(mSmdeP) delete mSmdeP;
    mSmdeP = f->Get("bsmdePed") ? ((St_smdPed*)f->Get("bsmdePed"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdpPed") == 0) {
    if(mSmdpP) delete mSmdpP;
    mSmdpP = f->Get("bsmdpPed") ? ((St_smdPed*)f->Get("bsmdpPed"))->GetTable() : 0;
  }
  else if(fileName.find("bemcStatus") == 0) {
    if(mBtowS) delete mBtowS;
    mBtowS = f->Get("bemcStatus") ? ((St_emcStatus*)f->Get("bemcStatus"))->GetTable() : 0;
  }
  else if(fileName.find("bprsStatus") == 0) {
    if(mBprsS) delete mBprsS;
    mBprsS = f->Get("bprsStatus") ? ((St_emcStatus*)f->Get("bprsStatus"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdeStatus") == 0) {
    if(mSmdeS) delete mSmdeS;
    mSmdeS = f->Get("bsmdeStatus") ? ((St_smdStatus*)f->Get("bsmdeStatus"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdpStatus") == 0) {
    if(mSmdpS) delete mSmdpS;
    mSmdpS = f->Get("bsmdpStatus") ? ((St_smdStatus*)f->Get("bsmdpStatus"))->GetTable() : 0;
  }
  else if(fileName.find("bemcCalib") == 0) {
    if(mBtowC) delete mBtowC;
    mBtowC = f->Get("bemcCalib") ? ((St_emcCalib*)f->Get("bemcCalib"))->GetTable() : 0;
  }
  else if(fileName.find("bprsCalib") == 0) {
    if(mBprsC) delete mBprsC;
    mBprsC = f->Get("bprsCalib") ? ((St_emcCalib*)f->Get("bprsCalib"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdeCalib") == 0) {
    if(mSmdeC) delete mSmdeC;
    mSmdeC = f->Get("bsmdeCalib") ? ((St_smdCalib*)f->Get("bsmdeCalib"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdpCalib") == 0) {
    if(mSmdpC) delete mSmdpC;
    mSmdpC = f->Get("bsmdpCalib") ? ((St_smdCalib*)f->Get("bsmdpCalib"))->GetTable() : 0;
  }
  else if(fileName.find("bemcGain") == 0) {
    if(mBtowG) delete mBtowG;
    mBtowG = f->Get("bemcGain") ? ((St_emcGain*)f->Get("bemcGain"))->GetTable() : 0;
  }
  else if(fileName.find("bprsGain") == 0) {
    if(mBprsG) delete mBprsG;
    mBprsG = f->Get("bprsGain") ? ((St_emcGain*)f->Get("bprsGain"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdeGain") == 0) {
    if(mSmdeG) delete mSmdeG;
    mSmdeG = f->Get("bsmdeGain") ? ((St_smdGain*)f->Get("bsmdeGain"))->GetTable() : 0;
  }
  else if(fileName.find("bsmdpGain") == 0) {
    if(mSmdpG) delete mSmdpG;
    mSmdpG = f->Get("bsmdpGain") ? ((St_smdGain*)f->Get("bsmdpGain"))->GetTable() : 0;
  }
  else if(fileName.find("bemcTriggerStatus") == 0) {
    if(mTrigS) delete mTrigS;
    mTrigS = f->Get("bemcTriggerStatus") ? ((St_emcTriggerStatus*)f->Get("bemcTriggerStatus"))->GetTable() : 0;
  }
  else if(fileName.find("bemcTriggerPed4") == 0) {
    if(mTrigP4) delete mTrigP4;
    mTrigP4 = f->Get("bemcTriggerPed4") ? ((St_bemcTriggerPed4*)f->Get("bemcTriggerPed4"))->GetTable() : 0;
  }
  else if(fileName.find("bemcTriggerPed") == 0) {
    if(mTrigP) delete mTrigP;
    mTrigP = f->Get("bemcTriggerPed") ? ((St_emcTriggerPed*)f->Get("bemcTriggerPed"))->GetTable() : 0;
  }
  else if(fileName.find("bemcTriggerLUT") == 0) {
    if(mTrigL) delete mTrigL;
    mTrigL = f->Get("bemcTriggerLUT") ? ((St_emcTriggerLUT*)f->Get("bemcTriggerLUT"))->GetTable() : 0;
  }
}

void StBemcTablesWriter::setTable(const char * tableName, void * data) {
  if (!tableName || !data) return;
  string myName(tableName);
    
  if(myName.find("bemcPed") == 0) {
    if (mBtowP) memcpy(mBtowP, data, sizeof(emcPed_st));
  }
  else if(myName.find("bprsPed") == 0) {
    if (mBprsP) memcpy(mBprsP, data, sizeof(emcPed_st));
  }
  else if(myName.find("bsmdePed") == 0) {
    if (mSmdeP) memcpy(mSmdeP, data, sizeof(smdPed_st));
  }
  else if(myName.find("bsmdpPed") == 0) {
    if (mSmdpP) memcpy(mSmdpP, data, sizeof(smdPed_st));
  }
  else if(myName.find("bemcStatus") == 0) {
    if (mBtowS) memcpy(mBtowS, data, sizeof(emcStatus_st));
  }
  else if(myName.find("bprsStatus") == 0) {
    if (mBprsS) memcpy(mBprsS, data, sizeof(emcStatus_st));
  }
  else if(myName.find("bsmdeStatus") == 0) {
    if (mSmdeS) memcpy(mSmdeS, data, sizeof(smdStatus_st));
  }
  else if(myName.find("bsmdpStatus") == 0) {
    if (mSmdpS) memcpy(mSmdpS, data, sizeof(smdStatus_st));
  }
  else if(myName.find("bemcCalib") == 0) {
    if (mBtowC) memcpy(mBtowC, data, sizeof(emcCalib_st));
  }
  else if(myName.find("bprsCalib") == 0) {
    if (mBprsC) memcpy(mBprsC, data, sizeof(emcCalib_st));
  }
  else if(myName.find("bsmdeCalib") == 0) {
    if (mSmdeC) memcpy(mSmdeC, data, sizeof(smdCalib_st));
  }
  else if(myName.find("bsmdpCalib") == 0) {
    if (mSmdpC) memcpy(mSmdpC, data, sizeof(smdCalib_st));
  }
  else if(myName.find("bemcGain") == 0) {
    if (mBtowG) memcpy(mBtowG, data, sizeof(emcGain_st));
  }
  else if(myName.find("bprsGain") == 0) {
    if (mBprsG) memcpy(mBprsG, data, sizeof(emcGain_st));
  }
  else if(myName.find("bsmdeGain") == 0) {
    if (mSmdeG) memcpy(mSmdeG, data, sizeof(smdGain_st));
  }
  else if(myName.find("bsmdpGain") == 0) {
    if (mSmdpG) memcpy(mSmdpG, data, sizeof(smdGain_st));
  }
  else if(myName.find("bemcTriggerStatus") == 0) {
    if (mTrigS) memcpy(mTrigS, data, sizeof(emcTriggerStatus_st));
  }
  else if(myName.find("bemcTriggerPed4") == 0) {
    if (mTrigP4) memcpy(mTrigP4, data, sizeof(bemcTriggerPed4_st));
  }
  else if(myName.find("bemcTriggerPed") == 0) {
    if (mTrigP) memcpy(mTrigP, data, sizeof(emcTriggerPed_st));
  }
  else if(myName.find("bemcTriggerLUT") == 0) {
    if (mTrigL) memcpy(mTrigL, data, sizeof(emcTriggerLUT_st));
  }
}

/***************************************************************************
 *
 * $Log: StBemcTablesWriter.cxx,v $
 * Revision 1.7  2015/03/02 20:28:26  jkadkins
 * Updates to store "bemcTriggerPed4" values in database, updates to destructor
 *
 * Revision 1.6  2010/01/28 13:45:06  mattheww
 * update from Oleksandr to protect against NULL pointers
 *
 * Revision 1.4  2007/12/11 20:37:19  kocolosk
 * use memcpy to grab setTable data
 *
 * Revision 1.3  2007/12/11 19:54:46  kocolosk
 * allow direct setting of void * table
 *
 * Revision 1.2  2007/10/01 16:57:29  kocolosk
 * allow to uploaded bemcTrigger* tables too
 *
 * Revision 1.1  2007/09/08 01:22:37  kocolosk
 * StBemcTablesWriter provides common interface for inserting DB tables
 *
 *
 **************************************************************************/

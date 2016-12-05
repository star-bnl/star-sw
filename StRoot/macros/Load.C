// $Id: Load.C,v 1.23 2016/12/01 15:10:44 jeromel Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,St_Tables,StChain,StDetectorDbMaker,StBichsel,StEvent,StTpcDb,StUtilities,StDbLib,StDbBroker,St_db_Maker,StTriggerDataMaker,StEventUtilities,StBFChain"){
  TString opt(loadList);
  TString separator = "[^ ;,]+";
  TObjArray *array = opt.Tokenize(separator);
  TIter next(array);
  while ((objs = (TObjString *) next())) {gSystem->Load(objs->GetString());}
  delete array;
  if (gSystem->Load("liblog4cxx.so") >=  0) {             //  StMemStat::PrintMem("load log4cxx");
    cout << " + liblog4cxx.so";
    if(gSystem->Load("libStStarLogger.so") >= 0) {              //  StMemStat::PrintMem("load log4cxx");
      cout << " + libStStarLogger.so";
      //      gROOT->ProcessLine("StLoggerManager::StarLoggerInit();"); 
      StLoggerManager::StarLoggerInit();
      if (gROOT->IsBatch())  StLoggerManager::setColorEnabled(kFALSE);
    }
    cout << endl;
  }
}

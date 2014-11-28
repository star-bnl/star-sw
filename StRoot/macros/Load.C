// $Id: Load.C,v 1.22 2012/06/21 19:00:06 fisyak Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load(Char_t *loadList="St_base,StChain"){
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

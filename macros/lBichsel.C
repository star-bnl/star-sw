void lBichsel() {
#if defined(__CINT__) && ! defined(__MAKECINT__)
  if (gClassTable->GetID("StBichsel") < 0) {
    //    gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    //    m_Bichsel = Bichsel::Instance();
  // Look up for the logger option
    Bool_t needLogger  = kFALSE;
    if (gSystem->Load("liblog4cxx") >=  0) {             //  StMemStat::PrintMem("load log4cxx");
      cout << " + liblog4cxx";
      if(gSystem->Load("libStStarLogger") >= 0) {              //  StMemStat::PrintMem("load log4cxx");
	cout << " + libStStarLogger";
	//      gROOT->ProcessLine("StLoggerManager::StarLoggerInit();"); 
	StLoggerManager::StarLoggerInit();
	if (gROOT->IsBatch())  StLoggerManager::setColorEnabled(kFALSE);
      }
      cout << endl;
   }
  }
#else
  cout << "Please don't run it with AClick" << endl;
#endif
}

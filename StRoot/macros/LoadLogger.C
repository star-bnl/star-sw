// Author: V.Fine (fine@bnl.gov)
// ROOT macro to load and initialize the STAR messager with log4cxx implementation
void LoadLogger() {
   //  gSystem->Load("logger/lib/liblog4cxx.so");
  gSystem->Load("liblog4cxx.so");
  gSystem->Load("St_base.so");
  gSystem->Load("StStarLogger.so");
  StLoggerManager::StarLoggerInit();
}

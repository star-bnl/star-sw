testJprof(){

  // set period to a hair over 1ms
  gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001001");
  gSystem->Load("Jprof");

  // give jprof something to look at
  for(int i=0; i<100000;){ i++; }

} // testJprof

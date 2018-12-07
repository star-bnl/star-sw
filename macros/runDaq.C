void runDaq( 
      const Char_t *inputFile="/star/data03/daq/2016/126/17126051/st_physics_adc_17126051_raw_4000059.daq"
		){
  gROOT->LoadMacro("bfc.C");
  //TString _chain = "DbV20161018 P2016a emcAtoE trigSimu picoWrite PicoVtxVpd BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt";
  TString _chain = "DbV20161018 P2016a StiCA mtd mtdCalib btof PxlHit IstHit SstHit emcAtoE trigSimu picoWrite PicoVtxVpd BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt";
  bfc( 5, _chain, inputFile); 
}

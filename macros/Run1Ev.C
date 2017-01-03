void Run1Ev(Int_t NEvents=1, Int_t iD = 5, 
	    Double_t pTlow=1,Double_t pThigh=1,
	    Double_t Ylow=0.1, Double_t Yhigh=0.1,
	    Double_t Philow=0.0, Double_t Phihigh=0.0,
	    Double_t Zlow=2, Double_t Zhigh=2, Int_t Npart = 1) {
  if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
    if (gClassTable->GetID("St_geant_Maker") < 0) {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl; 
      return;
    }
    //    St_geant_Maker * geant = (  St_geant_Maker * ) chain->Maker("geant");
#if 0
    St_geant_Maker::instance()->SetAttr("phys_off",1);
#endif
    //    St_geant_Maker::instance()->InitRun(1);
#if 0
    St_geant_Maker::instance()->Do("subevent 0;");
#endif
    //                         NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
    //                         gkine 1  2    25     25  0.1   0.1      0       0   10    10
    //                         gkine 1  6    25     25  0.0   0.0      0       0  100   100
    TString kine(Form("gkine %i %i %f %f %f %f %f %f %f %f",Npart,iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh));
    cout << "Set kine : " << kine.Data() << endl;
    St_geant_Maker::instance()->Do(kine.Data());
    //    St_geant_Maker::instance()->Do("gspread 100. 100. 0.");
    //                              CUTS   CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
    //                          Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     10");
    //                          Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e-3");
    St_geant_Maker::instance()->Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e3");
    St_geant_Maker::instance()->Do("physi");

    /*
     * AGUSER/MODE Detector [ Flag Value ]
     
     Detector   C 'detector subsystem name' D='ALL'
     Flag       C 'control flag name'
     Value      I 'flag value'
     
     Set control flags for a given detector subsystem (or for ALL of them).  
     Possible flags and their default values are:  
     
     PNOW  (0)             - print level for current event
     PRIN  (0)             - normal print level
     DEBU  (0)             - debug print level
     GEOM  (1)             - geometry version
     HIST  (1)             - system histogram flag
     GRAP  (1)             - system graphics level
     SIMU  (1)             - store GEANT hits flag
     DIGI  (1)             - digitisation flag
     RECO  (1)             - reconstruction flag
     MFLD  (1)             - magnetic field flag
     ANAL  (0)             - user analysis level
     BACK  (0)             - number of pile-up bunchs to select (relative to the trigger one)
     
     To change default values use GSFLAG command.  */
#if 0
    St_geant_Maker::instance()->Do("mode  svtt simu 2");
    St_geant_Maker::instance()->Do("mode  tpce simu 2");
    St_geant_Maker::instance()->Do("mode  ftpc simu 2");
    St_geant_Maker::instance()->Do("detp  trac DCAY 210 210 0.1 0.01");
    St_geant_Maker::instance()->Do("mode  g2tm prin 1;");
#endif
#if 1
    St_geant_Maker::instance()->Do("debug on");
    St_geant_Maker::instance()->Do("swit 1 2");
    St_geant_Maker::instance()->Do("swit 2 2");
    St_geant_Maker::instance()->SetDebug(1);
#endif
  } else {
    StVMCMaker *geant = chain->Maker("geant");
    geant->SetDebug(1);
    // geant->SetDebug(2); // Print StarVMCApplication::MisalignGeometry()
#if 0
    StMaker *svtDb = chain->Maker("svtDb");
    if (svtDb) svtDb->SetDebug(2);
    StMaker *ssdDb = chain->Maker("ssdDb");
    if (ssdDb) ssdDb->SetDebug(2);
#endif
    chain->SetFlavor("simu","svtRDOstripped"); // disable time depended SVT activity status
#if 1
    if (StarVMCApplication::Instance()) {
      StarMCSimplePrimaryGenerator *gener = (StarMCSimplePrimaryGenerator *) StarVMCApplication::Instance()->GetPrimaryGenerator();
      if ( gener && ! gener->IsA()->InheritsFrom( "StarMCSimplePrimaryGenerator" ) ) {
	delete gener; gener = 0;
      }
      if (! gener) gener =  new 
      StarMCSimplePrimaryGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
      else
	gener->SetGenerator( Npart, iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh, "G");
      StarVMCApplication::Instance()->SetPrimaryGenerator(gener);
      cout << "Set StarMCSimplePrimaryGenerator" << endl;
      StarVMCApplication::Instance()->DoMisAlignment(kFALSE);
      //      geant->SetSetAttr("phys_off",kTRUE); // physics off
      geant->SetMode(100); // physics off
      if (gMC && gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
	TGeant3TGeo *geant3 = (TGeant3TGeo *)gMC;
	Gcflag_t* cflag = geant3->Gcflag();
	cflag->idebug = 1;
	cflag->idemax = 10000;
	cflag->iswit[0] = 2;
	cflag->iswit[1] = 2;
	cflag->iswit[2] = 2; 
	StGeanePropagator::instance()->SetDebug(1);
#if 0
	StiKalmanTrackNode::SetDebug(8);
	StiKalmanTrack::SetDebug(2);
#endif
      }
    } else {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl;
    }
#else
#endif
  }
#if 1
  // Old Sti
  StiKalmanTrackNode::setDebug(8+32+16);
  StiKalmanTrackFinder::setDebug(2);
  StiKalmanTrackFitter::setDebug(1);
  StiKalmanTrack::setDebug(2);
  StiTrackNodeHelper::setDebug(8);
#endif
#if 0
  if (chain->Maker("svt_hits")) chain->Maker("svt_hits")->SetDebug(2);
  if (chain->Maker("SsdFastSim")) chain->Maker("SsdFastSim")->SetDebug(2);
#endif
  chain->EventLoop(NEvents);
}

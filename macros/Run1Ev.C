void Run1Ev(Int_t NEvents=1, Int_t iD = 5, 
	    Double_t pTlow=100,Double_t pThigh=100,
	    Double_t Ylow=0.1, Double_t Yhigh=0.1,
	    //	    Double_t Ylow=-0.1, Double_t Yhigh=-0.1,
	    Double_t Philow=2.094, Double_t Phihigh=2.094,
	    Double_t Zlow=0, Double_t Zhigh=0, Int_t Npart = 1) {
  if ( gClassTable->GetID("TGiant3") >= 0) { // root4star
    if (gClassTable->GetID("St_geant_Maker") < 0) {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl; 
      return;
    }
    //    St_geant_Maker * geant = (  St_geant_Maker * ) chain->Maker("geant");
#if 0
    St_geant_Maker::instance()->SetAttr("phys_off",1);
    St_geant_Maker::instance()->Do("subevent 0;");
#endif
    //                         NTRACK  ID PTLOW PTHIGH YLOW YHIGH PHILOW PHIHIGH ZLOW ZHIGH
    //                         gkine 1  5     1      1  0.1   0.1      0       0    2     2
    //                         gkine 1  2    25     25  0.1   0.1      0       0   10    10
    //                         gkine 1  6    25     25  0.0   0.0      0       0  100   100
    if (NEvents) {
    TString kine(Form("gkine %i %i %f %f %f %f %f %f %f %f",Npart,iD, pTlow,pThigh,Ylow, Yhigh, Philow, Phihigh, Zlow, Zhigh));
    cout << "Set kine : " << kine.Data() << endl;
    St_geant_Maker::instance()->Do(kine.Data());
    //    St_geant_Maker::instance()->Do("gspread 100. 100. 0.");
    //                              CUTS   CUTGAM CUTELE CUTHAD CUTNEU CUTMUO BCUTE BCUTM DCUTE DCUTM PPCUTM TOFMAX GCUTS[5]
    //                          Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     10");
    //                          Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e-3");
    }
    St_geant_Maker::instance()->Do("CUTS     1e-5   1e-5   1e-3  1e-14   1e-3  1e-3  1e-3  1e-3  1e-3   1e-3     1e3");
    St_geant_Maker::instance()->Do("DCAY 0");
    St_geant_Maker::instance()->Do("ANNI 0");
    St_geant_Maker::instance()->Do("BREM 0");
    St_geant_Maker::instance()->Do("COMP 0");
    St_geant_Maker::instance()->Do("HADR 0");
    St_geant_Maker::instance()->Do("MUNU 0");
    St_geant_Maker::instance()->Do("PAIR 0");
    St_geant_Maker::instance()->Do("PFIS 0");
    St_geant_Maker::instance()->Do("PHOT 0");
    St_geant_Maker::instance()->Do("RAYL 0");
    St_geant_Maker::instance()->Do("LOSS 4"); // no fluctuations 
      //    St_geant_Maker::instance()->Do("LOSS 1"); // with delta electron above dcute
    St_geant_Maker::instance()->Do("DRAY 0");
    St_geant_Maker::instance()->Do("MULS 0");
    St_geant_Maker::instance()->Do("STRA 0");

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
    if (StarVMCApplication::Instance()) {
      if (NEvents) {
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
      }
      //      StarVMCApplication::Instance()->DoMisAlignment(kFALSE);
      //      geant->SetSetAttr("phys_off",kTRUE); // physics off
      gMC->SetProcess("DCAY", 0);
      gMC->SetProcess("ANNI", 0);
      gMC->SetProcess("BREM", 0);
      gMC->SetProcess("COMP", 0);
      gMC->SetProcess("HADR", 0);
      gMC->SetProcess("MUNU", 0);
      gMC->SetProcess("PAIR", 0);
      gMC->SetProcess("PFIS", 0);
      gMC->SetProcess("PHOT", 0);
      gMC->SetProcess("RAYL", 0);
      gMC->SetProcess("LOSS", 4); // no fluctuations 
      gMC->SetProcess("DRAY", 0);
      gMC->SetProcess("MULS", 0);
      gMC->SetProcess("STRA", 0);
      gMC->SetCut("CUTGAM",	1e-5  );
      gMC->SetCut("CUTELE", 	1e-5  );
      gMC->SetCut("CUTHAD", 	1e-3  );
      gMC->SetCut("CUTNEU", 	1e-14 );
      gMC->SetCut("CUTMUO", 	1e-3  );
      gMC->SetCut("BCUTE", 	1e-3  );
      gMC->SetCut("BCUTM", 	1e-3  );
      gMC->SetCut("DCUTE", 	1e-3  );
      gMC->SetCut("DCUTM", 	1e-3  );
      gMC->SetCut("PPCUTM", 	1e-3  );
      gMC->SetCut("TOFMAX", 	1e3);
      gMC->BuildPhysics();
      if (gMC && gMC->IsA()->InheritsFrom("TGeant3TGeo")) {
        TGeant3TGeo *geant3 = (TGeant3TGeo *)gMC;
        Gcflag_t* cflag = geant3->Gcflag();
        cflag->idebug = 1;
        cflag->idemax = 10000;
        cflag->iswit[0] = 2;
        cflag->iswit[1] = 2;
        cflag->iswit[2] = 2; 
        StGeanePropagator::instance()->SetDebug(1);
      }
    } else {
      cout << "You have to use root4star with St_geant_Maker already loaded" << endl;
    }
  }
#if 1
  if (gClassTable->GetID("StiMaker") >= 0) {
    // Old Sti
    StiKalmanTrackNode::setDebug(8+32+16);
    StiKalmanTrackFinder::setDebug(2);
    StiKalmanTrackFitter::setDebug(1);
    StiKalmanTrack::setDebug(2);
    StiTrackNodeHelper::setDebug(8);
  }
  if (gClassTable->GetID("StiVMCMaker") >= 0) {
    chain->Maker("StiVMC")->SetDebug(1);
    StiKalmanTrackNode::SetDebug(8+32+16);
    //    StiKalmanTrackFinder::SetDebug(2);
    //    StiKalmanTrackFitter::SetDebug(1);
    StiKalmanTrack::SetDebug(2);
    //    StiTrackNodeHelper::SetDebug(8);
  }  
#endif
  if (NEvents) {
    chain->EventLoop(NEvents);
  }
}

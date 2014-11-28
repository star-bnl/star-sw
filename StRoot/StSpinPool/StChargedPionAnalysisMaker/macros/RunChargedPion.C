// $Id: RunChargedPion.C,v 1.2 2008/12/29 16:12:49 kocolosk Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * main macro to generate StChargedPionEvent trees, both data and simu
 *****************************************************************************/

void RunChargedPion(const char *muPath, 
                    const char *outName,
                    const char *jetPath = NULL,
                    bool simu = true,
                    int year = 2005,
                    int nevents = 99999)
{
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("$STAR/StRoot/StSpinPool/StChargedPionAnalysisMaker/macros/LoadLibs.C");
    
    double pi = atan(1.0)*4.0;
    
    StChain chain;
    
    if(simu) {
        StIOMaker ioMk;
        TString muString(muPath);
        TString geantString(muString.ReplaceAll("MuDst", "geant"));
        ioMk.SetFile(geantString.Data());
        ioMk.SetIOMode("r");
        ioMk.SetBranch("*", 0, "0");
        ioMk.SetBranch("geantBranch", 0, "r");
    
        StMcEventMaker mcEventMk;
    }
    
    StMuDstMaker muMk(0, 0, "", muPath, "", 1e6);
    
    if(!simu) {
        StTriggerFilterMaker filterMk;
        filterMk.addTrigger(96011);
        filterMk.addTrigger(96201);
        filterMk.addTrigger(96211);
        filterMk.addTrigger(96221);
        filterMk.addTrigger(96233);
        
        filterMk.addTrigger(5);
        filterMk.addTrigger(8);
        filterMk.addTrigger(117001); // mb
        filterMk.addTrigger(117300); // zb
        filterMk.addTrigger(117402); // muon
        filterMk.addTrigger(137213); // bemc-ht2-mb-emul
        filterMk.addTrigger(137221); // bemc-jp1-mb
        filterMk.addTrigger(137222); // bemc-jp1-mb
        filterMk.addTrigger(137271); // eemc-jp1-mb
        filterMk.addTrigger(137272); // eemc-jp1-mb
        filterMk.addTrigger(137273); // eemc-jp1-mb
        filterMk.addTrigger(137501); // bemc-jp0-mb
        filterMk.addTrigger(137551); // eemc-jp0-mb
        filterMk.addTrigger(137571); // bemc-jp1
        filterMk.addTrigger(137575); // bemc-jp0-etot
        filterMk.addTrigger(137580); // eemc-http
        filterMk.addTrigger(137581); // eemc-http
        filterMk.addTrigger(137585); // bemc-jp2
        filterMk.addTrigger(137586); // bemc-http
        filterMk.addTrigger(137611); // bemc-http-mb-l2gamma
        filterMk.addTrigger(137622); // bemc-jp0-etot-mb-L2jet
        filterMk.addTrigger(137641); // eemc-http-mb-L2gamma
        filterMk.addTrigger(137652); // eemc-jp0-etot-mb-L2jet
    }
    
    St_db_Maker dbMk("StarDb", "MySQL:StarDb", "$STAR/StarDb");
    if(simu) {
        switch(year) {
            case 2005:
            dbMk.SetDateTime(20050506,214129);
            break;
            
            case 2006:
            dbMk.SetDateTime(20060522, 055000); // Alan's timestamp R7142018
            // dbMk.SetDateTime(20060522, 1128112); //R7142025 -- obviously wrong
            // dbMk.SetDateTime(20060603, 204549); //R7154063
            break;
        }
    }
    
    if(!simu) {
        StSpinDbMaker spDbMk("spinDb");
    }
    
    StEEmcDbMaker eemcDbMk("eemcDb");

    if(simu) {
        StEmcSimulatorMaker emcSimMk;
        emcSimMk.setCalibSpread(kBarrelEmcTowerId, 0.15);
    }
    else 
        StEmcADCtoEMaker adcMk;
    

    if(year == 2005) {
        StEmcTriggerMaker emcTrgMk("bemctrigger");
    }
    else {
        StTriggerSimuMaker trgSimuMk;
        trgSimuMk.setMC(simu);
        trgSimuMk.useBbc();
        trgSimuMk.useBemc();
        trgSimuMk.bemc->setConfig(StBemcTriggerSimu::kOffline);

        if(year == 2006) {
            StL2_2006EmulatorMaker simL2Mk;
            simL2Mk.setSetupPath("/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/");
            simL2Mk.setOutPath("./outL2");
            trgSimuMk.useL2(&simL2Mk);
        }
    }
    
    
    if(simu) StMCAsymMaker asymMk("MCAsym");
    
    if(simu) StPythiaFourPMaker pythia4pMk("StPythiaFourPMaker", &asymMk, &mcEventMk);
    
    bool doTowerSwapFix = true;
    StBET4pMaker bet4pMk("BET4pMaker", &muMk, doTowerSwapFix);
    switch(year) {
        case 2005:
        bet4pMk.setUse2005Cuts(true);
        break;
        
        case 2006:
        bet4pMk.setUseEndcap(true);
        bet4pMk.setUse2006Cuts(true);
        break;
    }
    
    StJetMaker emcJetMk("emcJetMaker", &muMk, "blah.jet.root");
    
    StppAnaPars anapars;
    switch(year) {
        case 2005:
        anapars.setFlagMin(0);
        anapars.setNhits(20);
        anapars.setAbsEtaMax(1.6);
        if(simu) anapars.setJetPtMin(3.0);
        else     anapars.setJetPtMin(5.0);
        break;
        
        case 2006:
        anapars.setFlagMin(0);
        anapars.setNhits(12);
        anapars.setAbsEtaMax(2.0);
        if(simu) anapars.setJetPtMin(3.5);
        else     anapars.setJetPtMin(5.0);
        break;
    }
    anapars.setCutPtMin(0.2);
    anapars.setJetEtaMax(100.0);
    anapars.setJetEtaMin(0);
    anapars.setJetNmin(0);
    
    StppAnaPars pypars;
    switch(year) {
        case 2005:        
        case 2006:
        pypars.setFlagMin(0);
        pypars.setNhits(0);
        pypars.setCutPtMin(0.0001);
        pypars.setAbsEtaMax(5.0);
        pypars.setJetPtMin(3.0);
        pypars.setJetEtaMax(5.0);
        pypars.setJetEtaMin(0);
        pypars.setJetNmin(0);
        break;
    }
    
    StConePars cpars;
    switch(year) {
        case 2005:
        cpars.setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
        cpars.setConeRadius(0.4);
        break;
        
        case 2006:
        cpars.setGridSpacing(105, -3.0, 3.0, 120, -pi, pi);
        cpars.setConeRadius(0.7);
        break;
    }
    cpars.setSeedEtMin(0.5);
    cpars.setAssocEtMin(0.1);
    cpars.setSplitFraction(0.5);
    cpars.setPerformMinimization(true);
    cpars.setAddMidpoints(true);
    cpars.setRequireStableMidpoints(true);
    cpars.setDoSplitMerge(true);
    cpars.setDebug(false);
    
    switch(year) {
        case 2005:
        emcJetMk.addAnalyzer(&anapars, &cpars, &bet4pMk, "ConeJets");
        break;
        
        case 2006:
        anapars.setNhits(5);
        emcJetMk.addAnalyzer(&anapars, &cpars, &bet4pMk, "ConeJets5");
    
        anapars.setNhits(12);
        emcJetMk.addAnalyzer(&anapars, &cpars, &bet4pMk, "ConeJets12");
    
        anapars.setNhits(10000000);
        emcJetMk.addAnalyzer(&anapars, &cpars, &bet4pMk, "ConeJetsEMC");
        break;
    }
    
    if(simu) {
        StConePars pycpars(cpars);
        if(year == 2005) pycpars.setGridSpacing(200, -5.0, 5.0, 120, -pi, pi);
        emcJetMk.addAnalyzer(&pypars, &pycpars, &pythia4pMk, "PythiaConeJets");
    }
    
    StChargedPionMaker cpMk("chargedPionMaker", outName);
    if(simu) {
        switch(year) {
            case 2005:
            cpMk.addTrigger(96011);
            cpMk.addTrigger(96201);
            cpMk.addTrigger(96211);
            cpMk.addTrigger(96221);
            cpMk.addTrigger(96233);
            break;
            
            case 2006:
            cpMk.addTrigger(5);
            cpMk.addTrigger(8);
            cpMk.addTrigger(117001); // mb
            cpMk.addTrigger(117300); // zb
            cpMk.addTrigger(117402); // muon
            cpMk.addTrigger(137213); // bemc-ht2-mb-emul
            cpMk.addTrigger(137221); // bemc-jp1-mb
            cpMk.addTrigger(137222); // bemc-jp1-mb
            cpMk.addTrigger(137271); // eemc-jp1-mb
            cpMk.addTrigger(137272); // eemc-jp1-mb
            cpMk.addTrigger(137273); // eemc-jp1-mb
            cpMk.addTrigger(137501); // bemc-jp0-mb
            cpMk.addTrigger(137551); // eemc-jp0-mb
            cpMk.addTrigger(137571); // bemc-jp1
            cpMk.addTrigger(137575); // bemc-jp0-etot
            cpMk.addTrigger(137580); // eemc-http
            cpMk.addTrigger(137581); // eemc-http
            cpMk.addTrigger(137585); // bemc-jp2
            cpMk.addTrigger(137586); // bemc-http
            cpMk.addTrigger(137611); // bemc-http-mb-l2gamma
            cpMk.addTrigger(137622); // bemc-jp0-etot-mb-L2jet
            cpMk.addTrigger(137641); // eemc-http-mb-L2gamma
            cpMk.addTrigger(137652); // eemc-jp0-etot-mb-L2jet
            break;
        }
    }
    
    TStopwatch total;
    TStopwatch timer;
    TMemStat memory;
    
    chain.Init();
    
    int i=0;
    while(i<nevents && chain.Make()==kStOk) {
        if(i % 500 == 0) {
            cout << "done with event " << i 
                 << "\tcpu: " << timer.CpuTime() 
                 << "\treal: " << timer.RealTime() 
                 << "\tratio: " << timer.CpuTime()/timer.RealTime() << endl;
            timer.Start();
            memory.PrintMem(NULL);
        }
        ++i;
        chain.Clear();
    }
    
    chain.Finish();
    cout << "my macro processed " << i << " events"
         << "\tcpu: " << total.CpuTime()
         << "\treal: " << total.RealTime()
         << "\tratio: " << total.CpuTime()/total.RealTime() << endl;
}

/*****************************************************************************
 * $Log: RunChargedPion.C,v $
 * Revision 1.2  2008/12/29 16:12:49  kocolosk
 * added $Id$/$Log$ as needed
 *
 * Revision 1.1  2008/07/17 17:07:02  kocolosk
 * cleanup all those old macros and XML files
 *
 *****************************************************************************/

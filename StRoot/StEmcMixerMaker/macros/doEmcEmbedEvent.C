////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn doEmcEmbedEvent
\author Alexandre Suaide
*/
class StChain;
StChain *chain=0;
void doEmcEmbedEvent(int nevents = 10,char* file="*.event.root",Bool_t print = kTRUE)
{
    gROOT->Macro("LoadLogger.C");
    gROOT->Macro("loadMuDst.C");
    gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StEEmcUtil");
    gSystem->Load("StMcEvent"); 
    gSystem->Load("StMcEventMaker"); 
    gSystem->Load("StDaqLib");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StMcAnalysisMaker");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");     
    gSystem->Load("StEmcMixerMaker");

    // create chain    
    chain = new StChain("bfc");  
    if(print) chain->SetDebug(1);

    StIOMaker* io = new StIOMaker("IO");
    io->SetFile(file);
    io->SetIOMode("r"); 
    io->SetBranch("*",0,"0");           //deactivate all branches
    io->SetBranch("eventBranch",0,"r");
    io->SetBranch("geantBranch",0,"r");

    St_db_Maker *db1 = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
    db1->SetFlavor("sim", "bprsCalib");

    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    // this line is important to propagate all the hits into StEvent
    // so, even the pedestals are propagated. In this case
    // the second AdcToEMaker will be responsible for making the
    // cuts (after the simulated hits are embedded)
    adc->saveAllStEvent(kTRUE);
    if(!print) adc->setPrint(kFALSE);

    StEmcPreMixerMaker *preMixer = new StEmcPreMixerMaker("preEmbed");

    StMcEventMaker *mcEvent = new StMcEventMaker();

    StEmcSimulatorMaker *emcSim = new StEmcSimulatorMaker();

    StEmcMixerMaker *emb = new StEmcMixerMaker();
    // include the next line if you want to embedd all simuated hits
    // even the ones that do not have a hit in the real data
    //emb->setEmbedAll(kTRUE);
    if(!print) emb->setPrint(kFALSE);

    StEmcADCtoEMaker *adc1 = new StEmcADCtoEMaker("EReadEmbed");      
    adc1->setEmbeddingMode(kTRUE);
    if(!print) adc1->setPrint(kFALSE);

    StPreEclMaker *pre = new StPreEclMaker();
    if(!print) pre->setPrint(kFALSE);

    StEpcMaker *epc = new StEpcMaker();
    if(!print) epc->setPrint(kFALSE);

    StAssociationMaker    *association = new StAssociationMaker();       // TPC association maker
    StEmcAssociationMaker *emcAssociation = new StEmcAssociationMaker(); // EMC association maker
    emcAssociation->setPrint(print);

    ///////////////////////////////////////////////////////////////
    //
    // put your analysis maker here
    //
    ///////////////////////////////////////////////////////////////

    chain->Init();
    int iev = 0;
    int istat = 0; 

    // do the event loop    
    while ( istat!=2 && istat!=3 && istat!=4 && iev<=nevents ) {
        chain->Clear();
        istat = chain->Make();
        emcAssociation->printMaps();
        if(iev%20==0) cout << "Finished processing event number "<<iev <<endl;
        iev++;
    }
    chain->Finish();

}

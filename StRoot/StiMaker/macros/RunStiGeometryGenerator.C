// RunStiGeometryGenerator.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiGeometryGenerator()
{
    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");

    gSystem->Load("StarClassLibrary");
    //We have to activate this to run in dev
    //gSystem->Load("StarRoot");
    

    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");

    gSystem->Load("Sti"); 
    gSystem->Load("StiGui");
    gSystem->Load("StiMaker"); 
    
    // create a new instance of the chain
    chain = new StChain("StChain"); 
    chain->SetDebug();
    
    // add makers to the chain

    const char* calibDB = "MySQL:StarDb";
    const char *paramsDB = "$STAR/StarDb";
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,paramsDB);
    calibMk->SetDateTime("year_1h");
//    calibMk->SetDebug();
    
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  
    //StiGeometryGenerator (MLM)
    StiGeometryGenerator* anaMk = new StiGeometryGenerator();
    
    // now execute the chain member functions    
    chain->PrintInfo();

    chain->Init();
    chain->Clear();
    int iev=0;
    int istat = chain->Make(iev);
    //chain->Finish();
    
}


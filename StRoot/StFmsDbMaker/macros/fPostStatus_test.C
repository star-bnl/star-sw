void fPostStatus_test(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_Tables");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");

    const Int_t MAX_DB_INDEX = 241;
    fpostStatus_st in[MAX_DB_INDEX];
    
    fpostStatus_st* t=new fpostStatus_st;
    delete t;

}

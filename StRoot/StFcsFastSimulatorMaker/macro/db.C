class StFcsDbMaker;
class StFcsDb;
StFcsDbMaker* mFcsDbMkr=0;
StFcsDb* mFcsDb=0;

void db(){
    gSystem->Load("libPhysics");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StFcsDbMaker");
    mFcsDbMkr = new StFcsDbMaker();
    mFcsDb = dynamic_cast<StFcsDb*>(mFcsDbMkr->GetDataSet("fcsDb"));
    mFcsDb->setDebug(1);
    mFcsDb->Init();
}

void bfcread(const char *MainFile="psc0049_08_40evts.DST.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");


    TFile *tf = new TFile(MainFile,"read");
    StTree *st;
    st = StTree::GetTree(tf,"bfc");
    st->Open();
    ULong_t UUU = 0;
    st->NextEvent(UUU);

    st->ls(9);    
    
    
}

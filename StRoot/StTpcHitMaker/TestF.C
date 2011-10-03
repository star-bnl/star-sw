void TestF()
{
   gROOT->ProcessLine(".L bfc.C");

   TString opt,inp;

   opt ="LanaDV,ITTF,display";

   inp="/star/data03/daq/2008/006/9006012/st_laser_9006012_raw_2130001.daq";
   gSystem->Load("StarRoot");
   bfc(-1,opt,inp);
  // return;

  printf(" Loading ...  StTpcHitMaker    %d\n", gSystem->Load("StTpcHitMaker.so"));
  printf(" Loading ...  StTpxReaderMaker %d\n", gSystem->Load("StTpxReaderMaker.so"));
  StMaker::lsMakers(chain);
   // Load RTS
   delete  StMaker::GetChain()->GetMaker("tpc_hits");
   StMaker *tpcChain = StMaker::GetChain()->GetMaker("tpcChain");
   StMaker *tpcHitMover = StMaker::GetChain()->GetMaker("tpc_hit_mover");
   tpcHitMover->Shunt(0);
   // tpcChain->AddMaker(new StTpcHitMaker("tpc_hits"));
    StTpxReaderMaker *tpxMk = new  StTpxReaderMaker("tpx_hits");
    tpxMk->SetFileName(inp);
    tpcChain->AddMaker(tpxMk);
    tpcChain->AddMaker(tpcHitMover);
   StMaker::lsMakers(chain);
   chain->Init();
   chain->Make();
//  chain->EventLoop(1,1);

}

{ 
 gSystem.Load("St_base.so");
 gSystem.Load("St_Tables.so");
 TFile f("/star/tof/fine/auau_ce_b0-2_4041_4060.root");
 gBenchmark->Start("read"); 
 f.ReadAll();
 gBenchmark->Show("read");
}


{
gSystem.Exec("rm *.log");
St_XDFFile xdffile("StChain.xdf","w");
StChain chain("StChain");
St_mev_Maker mev("MevSim","test");
chain.PrintInfo();
chain.Init();
chain.Make(1);
  }

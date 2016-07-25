void doMiniMcPlots(
#if 0
		   const Char_t *file1 = "tpt/rcf1249_19_500evts.minimc.Plots.root",
		   const Char_t *file2 = "ittf/rcf1249_19_500evts.minimc.Plots.root"
#else
		   const Char_t *file1 = "tpt/gstar_9.minimc.Plots.root",
		   const Char_t *file2 = "ittf/gstar_9.minimc.Plots.root"
#endif
		   ) { 
  gROOT->LoadMacro("MiniMcPlots.C+"); 
  Draw(file1,file2);
}

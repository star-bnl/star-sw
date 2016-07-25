void ErrorAnalysis(const Char_t *treeName = "SpX",
		   Int_t sec = 1, Int_t row = 1, Int_t p = 3, Int_t t = 3,Int_t border=1, 		  
		   const Char_t *fn="") {
  gROOT->LoadMacro("TpcT.C+");
  AnalysisSparse(treeName,sec,row,p,t,border,fn);
}

void hxp(){
  gROOT->LoadMacro("Chain.C");
  ChainDeDx();
  TFile *f = new TFile("hxp3.root","RECREATE");
  hxp = new TH2D("hxp","(z - <z>)*sqrt(x) versus log10(pmag)",20,-1.,1.,150,-7.5,7.5);
  DeDxTree->Draw("(log(m_de/m_dx)-m_avgz)*sqrt(m_dx):log10(m_pmag)>>hxp",
		 "m_dx <10 && m_de < 2e-4 && m_pmag>0.1 && m_pmag<10 && m_flag0points > 36");
  hxp->Write();
}

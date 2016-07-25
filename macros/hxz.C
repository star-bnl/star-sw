TFile *f = 0;
void hxz(){
  gROOT->LoadMacro("Chain.C");
  ChainDeDx();
  f = new TFile("hxz4.root","RECREATE");
  hxz = new TH2D("hxz","(z - <z>)*sqrt(x) versus log10(x)",20,0,1.,150,-7.5,7.5);
  DeDxTree->Draw("(log(m_de/m_dx)-m_avgz)*sqrt(m_dx):log10(m_dx)>>hxz",
		 "m_dx <10 && m_de < 2e-4 && m_pmag>0.1 && m_pmag<10 && m_flag0points > 36");
  hxz->Write();
}

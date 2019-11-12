#ifdef __CLING__
class StBFChain;        

StBFChain * bfc(Int_t First, Int_t Last,const Char_t *Chain = "", // + ",Display",
	 const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName=0);
StBFChain *bfc(Int_t First, const Char_t *Chain = "MC2016,20Muons,vmc,Rung.1",
 	       const Char_t *infile=0, const Char_t *outfile=0, const Char_t *TreeFile=0, const Char_t *chainName = "");
#endif
void lDb(const Char_t *date = 0) {
#ifdef __CLING__  
  gSystem->Load("St_base");
  gSystem->Load("libmysqlclient");
  gSystem->Load("StStarLogger");
  gSystem->Load("StChain");
  gSystem->Load("StBFChain");
  gROOT->LoadMacro("bfc.C+");
#else
  gROOT->LoadMacro("bfc.C");
#endif
  TString Chain("mysql,tpcDb,detDb,magF,TpcHitMover,CorrX,LaserIT,nodefault");
  if (date) {Chain += ","; Chain += date;}
  bfc(-1,Chain,0,0,0);
}

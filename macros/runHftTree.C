/*
  root.exe -q -b -x runHftTree.C >& runHftTree.log &
  root.exe -q -b -x Hft.tree.root makeHftPlots.C >& makeHftPlots.log &
  root.exe Out.plot.root HFTDraw.C+
 */
void runHftTree(Int_t N = 10000000, const Char_t *input="./*event.root", const Char_t *output=0) {
  gROOT->LoadMacro("bfc.C");
  Load();
  TString Input(input);
  TString Chain("in,StEvent,mysql,libPhysics,db,StarMagField,MagF,pxlDb,istDb,HftMatTree,nodefault,corrX");
  if (Input.Contains("rcf") || Input.Contains("gstar")) Chain += ",y2014a,simu";
  cout << "Chain used:\t" << Chain << endl;
  TString Output(output);
  if (Output == "") {
    Output = Input;
    Output.ReplaceAll("event.root","tree.root");
    if (Output.Contains("*")) Output = "Hft.tree.root";
    if (Output == Input) {
      cout << "Input = " << Input.Data() << " and Output = " << Output.Data() << " are the same" << endl;
      return;
    }
  }
  bfc(-1,Chain.Data(),Input,0,Output);
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  HftMatchedTree *mtree  = (HftMatchedTree *) chain->Maker("HftMatTree");
  if (! mtree) return;
  mtree->SetpCut(1.0);
  if (N < 0) return;
  chain->Init();
  if (N <= 0) return;
  chain->EventLoop(N);
}

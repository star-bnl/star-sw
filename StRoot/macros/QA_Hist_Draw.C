//read root histogram file and send all histograms to a postscript file
void QA_Hist_Draw(const Char_t
*fileName="/diskA/star/kathy/output/psc0049_08_40evts_3EV.root",
const Char_t *psFile="QA_hist.ps")
{
   printf(" Usage: QA_Hist_Draw( const Char_t *fileName = \"%s\",\n\t\tconst Char_t *psFile=\"%s\")\n\n\n"
          , fileName, psFile);
  TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root");  
  // file1.ls();
  TList *keys = file1.GetListOfKeys();
  if (keys) {
    TPostScript p5(psFile);
    // Create an itertor
    TIter nextKey(keys);
    TKey *key = 0; 
    Int_t histCounter = 0;
    while (key = (TKey *)nextKey()) {
      TObject *obj = key->ReadObj();
      if (obj && obj->InheritsFrom("TH1")) {
          printf(" Drawing ... %s::%s; Title=\"%s\"\n", obj->ClassName(),obj->GetName(), obj->GetTitle());
          obj->Draw(); 
          histCounter++;
          if (gPad) {
            gPad->Modified();
            gPad->Update();
          }
      }
    } 
  }
  p5.Close();
  printf(" ---------------------------------------------------------------------------\n");
  printf(" %d histograms have been plotted from file: \"%s\"\n", histCounter,fileName);
  printf(" The postscript copy of them have been created: \"%s\"\n",psFile);
  printf(" ---------------------------------------------------------------------------\n");
  
}

//read root histogram file and send all histograms to a postscript file
void QA_Hist_Draw(
     const Char_t *firstHistName="*",const Char_t *lastHistName="*",
     const Char_t
     *fileName="/disk00001/star/auau200/hijing135/jetq_on/b3_6/year_1b/hadronic_on/tfs/set0050_04_24evts.root",
     const Char_t *psFile="QA_hist.ps")
{ 
    cout << endl   
         << " Usage:  QA_Hist_Draw( " << endl
         << "                        const Char_t *firstHistName=\"" << firstHistName << "\","   << endl
         << "                        const Char_t *lastHistName=\""  << lastHistName  << "\""    << endl
         << "                        const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                        const Char_t *psFile=\""        << psFile        << "\","   << endl
         << "                      );" << endl 
         << " Note: firstHistName = \"*\" is by default and means ALL histograms from the file should be drawn" << endl
         << " ----- lastHistName  = \"*\" is by default and means ALL histograms by the end of file should be drawn" << endl
         << endl ;

  TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root");  
  // file1.ls();
  TList *keys = file1.GetListOfKeys();
  if (keys) {
    TPostScript p5(psFile);
    // Create an itertor
    TIter nextKey(keys);
    TKey *key = 0; 
    Int_t histCounter = 0;
    Int_t histReadCounter = 0;
    Bool_t started = kFALSE;
    while (key = (TKey *)nextKey()) {
      TObject *obj = key->ReadObj();
      if (obj && obj->InheritsFrom("TH1")) { 
          histReadCounter++;
          printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
          if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;
          if (started) {
            if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
            histCounter++;
            printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
            obj->Draw();   
            if (gPad) {
              gPad->Modified();
              gPad->Update();
           }
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

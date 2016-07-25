//______________________________________________________________________
void HCheck(const Char_t *topDir = "/star/rcf/disk1/star/fisyak/Hist212/")
{
  //open all input files and insert them in the list of files
  TFileSet dirs(topDir);
  TDataSetIter next(&dirs,0);
  TDataSet *set = 0; 
  Int_t nfiles = 0;
  while ( (set = next()) ) {           
    if (strcmp(set->GetTitle(),"file") || 
	!strstr(set->GetName(),".root")) continue;
    TString Fname(gSystem->ConcatFileName(topDir,set->Path()));
    cout << "Open " <<   Fname.Data();
    TFile *afile = new TFile(Fname.Data());
    if (!afile) continue;
    TList *list = gDirectory->GetListOfKeys();
    if (list->GetSize() <= 1) {
      delete afile; continue;
      cout << "   ========================= Empty" << endl;
    }
    TH2 *ZRow = (TH2 *) afile->Get("ZRow");
    Int_t no  = ZRow->ProjectionX("bin",13,13)->Integral();
    if (no > 0) cout << " Padrow 13 has " << no << " Entries" << endl;
    else        cout << endl;
    nfiles++;
  }
}

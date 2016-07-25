TChain *chain = 0;
void ChainTags(const Char_t *topDir = "/star/",
	       const Char_t *TreeName = "Tag")
{
  if (gClassTable->GetID("TDataSet")) gSystem->Load("libStar");
#ifdef P00hg
  Char_t *DirsNames[] = {"/star/data04/reco/P00hg/2000/",
			 "/star/data05/reco/P00hg/2000/",
			 "/star/data07/reco/P00hg/2000/",
			 "/star/data08/reco/P00hg/2000/",
			 "/star/data09/reco/P00hg/2000/",
			 "/star/data10/reco/P00hg/2000/",
			 0};
#else
  Char_t *DirsNames[] = {"/star/data03/reco/P00hi/2000/",
			 "/star/data02/disk00001/star/reco/P00hi/2000",
			 0};
#endif
  chain = new TChain(TreeName);
  for (Int_t f=0; DirsNames[f]; f++) {
    Char_t *DirName = DirsNames[f];
    printf("DirName %s\n",DirName);
    TFileSet dirs(DirName);
    TDataSetIter next(&dirs,0);
    TDataSet *set = 0; 
    while ( (set = next()) ) {           
      if (strcmp(set->GetTitle(),"file")) continue; 
      Char_t *Name = set->GetName();// printf("Name = %\n",Name);
      if (!strstr(Name,".root")) continue;
      if (!strstr(Name,".tags")) continue; 
      TString File(DirName);
      File += set->Path();
      chain->Add(File.Data());
      cout << "chained " <<  File.Data() << endl;
    }
  }
  UInt_t nEvents = chain->GetEntries();
  cout<<"chained "<<nEvents<<" events "<<endl;

  TObjArray *files = chain->GetListOfFiles();
  UInt_t nFiles = files->GetEntriesFast();
  cout << "chained " << nFiles << " files from " << topDir << endl;
  Tag->Draw("PCollTag.numberOfPrimaryTracks","PCollTag.primaryVertexZ > -2000");
}

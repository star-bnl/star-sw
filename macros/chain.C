TChain *tchain = 0;
void chain(const Char_t *topDir = "/star/data14/reco/P00hm/2000/08/",
	   const Char_t *TreeName = "Tag") 
{
  const Char_t *Files[] = {
"st_physics_1241016_raw_0001.tags.root",
"st_physics_1241016_raw_0003.tags.root",
"st_physics_1241016_raw_0004.tags.root",
"st_physics_1241016_raw_0005.tags.root",
"st_physics_1241016_raw_0006.tags.root",
"st_physics_1241016_raw_0007.tags.root",
"st_physics_1241016_raw_0008.tags.root",
"st_physics_1241016_raw_0009.tags.root",
"st_physics_1241016_raw_0010.tags.root",
"st_physics_1241016_raw_0011.tags.root",
"st_physics_1241016_raw_0012.tags.root",
"st_physics_1241016_raw_0013.tags.root",
//"st_physics_1241016_raw_0014.tags.root",
"st_physics_1241016_raw_0015.tags.root",
"st_physics_1241016_raw_0016.tags.root",
"st_physics_1241016_raw_0017.tags.root",
"st_physics_1241016_raw_0018.tags.root",
"st_physics_1241016_raw_0019.tags.root",
"st_physics_1241016_raw_0020.tags.root",
"st_physics_1241016_raw_0021.tags.root",
"st_physics_1241016_raw_0022.tags.root",
"st_physics_1241016_raw_0023.tags.root",
"st_physics_1241016_raw_0024.tags.root",
"st_physics_1241016_raw_0025.tags.root",
"st_physics_1241016_raw_0026.tags.root",
"st_physics_1241016_raw_0027.tags.root",
"st_physics_1241016_raw_0028.tags.root",
"st_physics_1241016_raw_0029.tags.root",
"st_physics_1241016_raw_0030.tags.root",
"st_physics_1241016_raw_0031.tags.root",
"st_physics_1241016_raw_0032.tags.root",
"st_physics_1241016_raw_0033.tags.root",
"st_physics_1241016_raw_0034.tags.root",
"st_physics_1241016_raw_0035.tags.root",
"st_physics_1241016_raw_0036.tags.root",
"st_physics_1241016_raw_0037.tags.root",
"st_physics_1241016_raw_0038.tags.root",
"st_physics_1241016_raw_0039.tags.root",
"st_physics_1241016_raw_0040.tags.root",
"st_physics_1241016_raw_0041.tags.root",
"st_physics_1241016_raw_0042.tags.root",
"st_physics_1241016_raw_0043.tags.root",
"st_physics_1241016_raw_0044.tags.root",
"st_physics_1241016_raw_0045.tags.root",
"st_physics_1241016_raw_0046.tags.root",
"st_physics_1241016_raw_0047.tags.root",
0};

  if (gClassTable->GetID("TDataSet")) gSystem->Load("libStar");
  const Char_t *name;
  tchain = new TChain(TreeName);
  Int_t NFiles = 0;
  for (Int_t t=0; name=Files[t];t++) {
    TString path(topDir);
    path += name;
    tchain->Add(path.Data());
    cout << "chained " <<   path << endl;
    NFiles++;
    //    if (NFiles > 20) break;
  }
  UInt_t nEvents = tchain->GetEntries();
  cout<<"chained "<<nEvents<<" events "<<endl;

  TObjArray *files = tchain->GetListOfFiles();
  UInt_t nFiles = files->GetEntriesFast();
  cout << "chained " << nFiles << " files from " << topDir << endl;
  
}

void writeTextFile(const char* inRootFile=
		   "links/P00hk.nofield.refitOS.undoPTCME.slice/dip5.typec.slice.root",
		   const char* outDir=
		   "links/P00hk.nofield.refitOS.undoPTCME.slice")
{
  cout << "in root=" << inRootFile << endl
       << "out dir=" << outDir << endl;

  TFile inRoot(inRootFile);
  if(!inRoot.IsOpen()){
    cout << "Cannot open " << inRootFile << endl;
    return;
  }
  TIterator* iterator = inRoot.GetListOfKeys()->MakeIterator();
  TKey* key;

  TString outText = inRootFile;
  outText.Replace(0,outText.Last('/')+1,"");
  outText.ReplaceAll(".root",".txt");
  outText.Prepend("/"); outText.Prepend(outDir);

  ofstream os(outText.Data());
  if(!os){cout << "huh?" << endl; return; }

  char buf[500];

  int count(0),nLimit(5);
  while( (key=dynamic_cast<TKey*>(iterator->Next())) != 0){
    //    cout << key->GetName() << endl;
    TH1* h = (TH1*)inRoot.Get(key->GetName());
    if(h->GetDimension()!=1) continue;

    //if(++count>10) break;

    int nBin = h->GetNbinsX();
    os << "name: " << h->GetName() << endl
       << "title: " << h->GetTitle() << endl
       << "bins: " << h->GetNbinsX() << endl;
    TArrayD* ary=0;
    if(ary=h->GetXaxis()->GetXbins()){
      os << "isarray" << endl;
      for(int i=0; i<ary->GetSize();i++){
	os << ary->At(i) << " ";
      }
      os << endl;
    } else{
      os << "notarray" << endl;
       << "min: " << h->GetXaxis()->GetBinLowEdge(1) << endl
       << "max: " << h->GetXaxis()->GetBinUpEdge(h->GetNbinsX()) << endl;
    }
      
    for(int i=1; i<=nBin; i++){
      os << "bin=" << i << " value: " << h->GetBinContent(i)
	 << " error: " << h->GetBinError(i) << endl;
      // cout << h->GetBinContent(i) << endl;
    }
  }
   
}

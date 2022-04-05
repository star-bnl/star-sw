#include <fstream>

void writeFile(const char* inRootFile)
{
  TFile inRoot(inRootFile);
  if(!inRoot.IsOpen()){
    cout << "Cannot open " << inRootFile << endl;
    return;
  }
  TIterator* iterator = inRoot.GetListOfKeys()->MakeIterator();
  TKey* key;

  TString outText = inRootFile;
  outText.Replace(0,outText.Last('/')+1,"");

  ofstream os(outText.Data());

  char buf[500];

  int count(0);
  while( (key=dynamic_cast<TKey*>(iterator->Next())) != 0){
    cout << key->GetName() << endl;
    TH1* h = (TH1*)inRoot.Get(key->GetName());
    if(h->GetDimension()!=1) continue;

    if(++count>1) break;

    int nBin = h->GetNbinsX();
    os << "name: " << h->GetName() << endl
       << "title: " << h->GetTitle() << endl
       << "bins: " << h->GetNbinsX() << endl
       << "min: " << h->GetXaxis()->GetBinLowEdge(1) 
       << ", max: " << h->GetXaxis()->GetBinUpEdge(h->GetNbinsX()) << endl;

    for(int i=1; i<=nBin; i++){
      os << "bin: " << i << " value: " << (float)h->GetBinContent(i)
	 << " error: " << (float)h->GetBinError(i) << endl;
    }
  }
   
}


void readFile(const char* textFile)
{
  

}


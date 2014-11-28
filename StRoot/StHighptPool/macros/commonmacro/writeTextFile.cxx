#include "writeTextFile.h"

const char* inrootfile="HIST/finish_central_cut1.hist.root";
const char* outdir="REAL";
 
int main(int argc,char** argv)
{
  char* argvZ[] = {"-b"}; // batch mode, no gui
  Int_t argcZ = 1;
  TApplication r00t("r00t", &argcZ, argvZ);
  
  extern char* optarg;
  const char* options = "i:o:m:";
  Int_t chr, i=0,o=0;
  char inRootFile[300],outDir[300];
  TString match;
  while ((chr = getopt(argc, argv, options)) >= 0){
    switch(chr){
    case 'i': strcpy(inRootFile,optarg); i=1; break;
    case 'o': strcpy(outDir,optarg); o=1; break;
    case 'm': match=optarg; break; 
    }
  }
  if(!(i*o)){
    cout << "-i infile -o outdir [-m <match>]" << endl;
    exit(-1);
  }


  cout << "in root=" << inRootFile << endl
       << "out dir=" << outDir << endl;
  if(match!="") cout << "match=" << match << endl;


  TFile inRoot(inRootFile);
  if(!inRoot.IsOpen()){
    cout << "Cannot open " << inRootFile << endl;
    return -1;
  }
  TIterator* iterator = inRoot.GetListOfKeys()->MakeIterator();
  TKey* key;

  TString outText = inRootFile;
  outText.Replace(0,outText.Last('/')+1,"");
  outText.ReplaceAll(".root",".txt");
  outText.Prepend("/"); outText.Prepend(outDir);

  cout << "out txt file=" << outText.Data() << endl;

  ofstream os(outText.Data());
  if(!os){cout << "huh?" << endl; return -1; }

  //  char buf[500];
  int count(0),nLimit(99999999);
  while( (key=dynamic_cast<TKey*>(iterator->Next())) != 0){
    //    cout << key->GetName() << endl;
    TObject* t = inRoot.Get(key->GetName());
    if(!t) continue;
    if(!(t->IsA()->InheritsFrom("TH1"))) continue;

    TH1* h=(TH1*)t;
    //    cout << h->GetName() << endl;
    TString name=h->GetName();
    if(match!="" && !strstr(name,match.Data())) continue;

    if(++count>nLimit) break;

    if(h->GetXaxis()->GetXbins()->GetSize()) continue;

    os << "name: " << h->GetName() << endl
       << "title: " << h->GetTitle() << endl
       << "dim: " << h->GetDimension() << endl
       << "ary: " << (h->GetXaxis()->GetXbins()->GetSize()!=0) << endl;
    for(int iDim=1; iDim<=h->GetDimension(); iDim++){   
      TAxis* axis = 0;
      switch(iDim){
      case 1: axis = h->GetXaxis(); break;
      case 2: axis = h->GetYaxis(); break;
      case 3: axis = h->GetZaxis(); break;
      default: cout << "wrong dimension" << endl;exit(0);
      }

      int nBin = axis->GetNbins();

      os << "bins: " << nBin << endl;

      TArrayD* ary=0;
      if(axis->GetXbins()->GetSize()){
	ary=axis->GetXbins();
	for(int i=0; i<ary->GetSize();i++){
	  os << ary->At(i) << " ";
	}
	os << endl;
     
      } else{
	os << "min: " << axis->GetBinLowEdge(1) << endl
	   << "max: " << axis->GetBinUpEdge(nBin) << endl;
      }
    }
    // list the contents
    for(int ix=0; ix<h->GetNbinsX()+2; ix++){
      for(int iy=0; iy<h->GetNbinsY()+2; iy++){
	for(int iz=0; iz<h->GetNbinsZ()+2; iz++){

	  int bin = h->GetBin(ix,iy,iz);
	  os << "bin=" << bin << " " << h->GetBinContent(bin) << " "
	     << h->GetBinError(bin) << endl;
	}
      }
    }
             
  }
   
  cout << "done" << endl; 

  return 0;
}

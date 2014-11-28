#include "subtract.h"

/* 
const char* infileA="links/P01hf.nofield.refitOS.slice/dip5lim.typea.slice.root";
const char* infileB="links/P01hf.nofield.2230026.noExB.refitOS.slice/dip5lim.typea.slice.root";
const char* outfile="links/P01hf.nofield.refitOS.slice/dip5lim.typea.slice.ExBsubNoExB.root";
*/
/*
// P01hf 2001data,2001V (2230026), noExB - DEV 2001data, 2000V, noExB (2255032)
const char* infileA="links/P01hf.nofield.2230026.noExB.refitOS.slice/dip5lim.typec.slice.root";
const char* infileB="links/DEV.nofield.2255032.noExB.refitOS.slice/dip5.typec.slice.root";
const char* outfile="links/P01hf.nofield.2230026.noExB.refitOS.slice/dip5.typec.2230026minus2255032.slice.root";
*/
// DEV 2000 data noExB - P00hk 2000 data noExB 
const char* infileA="links/DEV.nofield.1244036.noExB.refitOS.slice/dip5.slice.root";
const char* infileB="links/P00hk.nofield.refitOS.0917.slice/dip5.typec.slice.new.root";
const char* outfile="links/DEV.nofield.1244036.noExB.refitOS.slice/dip5.typec.DEVminusP00hk.slice.root";


int main(int argc,char** argv)
{
  char* argvZ[] = {"-b"}; // batch mode, no gui
  Int_t argcZ = 1;
  TApplication r00t("r00t", &argcZ, argvZ);

  extern char* optarg;
  const char* options = "a:b:o:";
  Int_t chr, a=0,b=0,o=0;
  char inFileA[300],inFileB[300],outFile[300];
  while ((chr = getopt(argc, argv, options)) >= 0){
    switch(chr){
    case 'a': strcpy(inFileA,optarg); a=1; break;
    case 'b': strcpy(inFileB,optarg); b=1; break;
    case 'o': strcpy(outFile,optarg); o=1; break;
    }
  }
  if(!(a*b*o)){
    cout << "-a infileA -b infileB -o outFile" << endl;
    exit(-1);
  }

  //const char* inFileA=infileA;
  //const char* inFileB=infileB;
  //const char* outFile=outfile;

  cout << "infileA=" << inFileA << endl
       << "infileB=" << inFileB << endl
       << "outFile=" << outFile << endl;

  TFile inRootA(inFileA); if(!inRootA.IsOpen()) return -1;
  TFile inRootB(inFileB); if(!inRootB.IsOpen()) return -1;
  TFile outRoot(outFile,"RECREATE"); if(!outRoot.IsOpen()) return -1;

  TIterator* iterator = inRootA.GetListOfKeys()->MakeIterator();
  TKey* key;
  //int count(0);
  while( (key=dynamic_cast<TKey*>(iterator->Next())) != 0){
    //    cout << key->GetName() << endl;
    TH1 *hA=0,*hB=0,*hOut=0;
    hA = (TH1*)inRootA.Get(key->GetName());
    if(hA->GetDimension()!=1) continue;

    hB = (TH1*)inRootB.Get(key->GetName());
    if(!hB) {
      cout << "Cannot find " << key->GetName()
	   << " in file " << inFileB << endl;
      continue;
    }
    if(hA->GetNbinsX()!=hB->GetNbinsX() || 
       hA->GetXaxis()->GetBinLowEdge(1) != hB->GetXaxis()->GetBinLowEdge(1)){
      cout << "Different bins " << key->GetName() << endl;
      continue;
    }
    if(hA->GetXaxis()->GetXbins()){
      hOut = new TH1D(hA->GetName(),hA->GetTitle(),
		      hA->GetNbinsX(),hA->GetXaxis()->GetXbins()->GetArray());
    }
    else{
      hOut = new TH1D(hA->GetName(),hA->GetTitle(),
		      hA->GetNbinsX(),
		      hA->GetXaxis()->GetBinLowEdge(1),
		      hA->GetXaxis()->GetBinUpEdge(hA->GetNbinsX()));
    }

    for(int i=1; i<=hA->GetNbinsX(); i++){
      double val=hA->GetBinContent(i)-hB->GetBinContent(i);
      double err=sqrt(hA->GetBinError(i)*hA->GetBinError(i)+
		      hB->GetBinError(i)*hB->GetBinError(i));
      hOut->SetBinContent(i,val);
      hOut->SetBinError(i,err);
    }
    hOut->Write();

    //if(++count>1) break;
  }
  outRoot.Close();

  cout << "done" << endl;

  //return 0;
  exit(0);
}

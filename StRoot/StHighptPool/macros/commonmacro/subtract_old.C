const char* infileA="links/P01hf.nofield.refitOS.slice/dip5.typec.slice.root";
const char* infileB="links/P01hf.nofield.2230026.noExB.refitOS.slice/dip5.typec.slice.root";
const char* outfile="links/P01hf.nofield.refitOS.slice/dip5.typec.slice.ExBsubNoExB.root";
void subtract(const char* inFileA=infileA,
	      const char* inFileB=infileB,
	      const char* outFile=outfile)
{
  cout << "infileA=" << inFileA << endl
       << "infileB=" << inFileB << endl
       << "outFile=" << outFile << endl;

  TFile inRootA(inFileA); if(!inRootA.IsOpen()) return;
  TFile inRootB(inFileB); if(!inRootB.IsOpen()) return;
  TFile outRoot(outFile,"RECREATE"); if(!outRoot) return;

  TIterator* iterator = inRootA.GetListOfKeys()->MakeIterator();
  TKey* key;
  int count(0);
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
  outRoot->Close();
}

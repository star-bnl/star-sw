int loaded=0;

void Load();

void CompareInvX0(const char *m1, const char *m2)
{
  Load();
  const TProfile2D *P[3]={0};
  const char *M[2]={m1,m2};
  
  for (int i=0;i<2;i++) {
    int ierr=0;
    { gROOT->Macro(M[i],&ierr);}
    if (ierr) return;
    TString myTs(M[i]); 
    myTs = gSystem->BaseName(myTs);
    assert(myTs(0,2) == "C_");
    myTs.Replace(0,2,"");
    int j=0;
    for (j=1; j<myTs.Length(); j++) {
      if ( isdigit(myTs[j])) 	continue;
      if (!isalpha(myTs[j])) 	break;
      if ( isupper(myTs[j])) 	break;
    }
    myTs.Replace(j,999,"");
    myTs.Insert(0,"P2_");
    myTs+="_ZR";
    P[i] = (TProfile2D*)gROOT->FindObject(myTs);
    if (!P[i]) {printf("***ERROR: Histogram %s NOT FOUND***\n",myTs.Data());return;}
    
    printf("File %s && hist %s OK\n",M[i],myTs.Data());
  }

  int nX = P[0]->GetXaxis()->GetNbins(); 
  assert(nX==P[1]->GetXaxis()->GetNbins());
  
  int nY = P[0]->GetYaxis()->GetNbins(); 
  assert(nY==P[1]->GetYaxis()->GetNbins());

  double xLow=P[0]->GetXaxis()->GetXmin();
  double xUpp=P[0]->GetXaxis()->GetXmax();
  double xWid=P[0]->GetXaxis()->GetBinWidth(1);
  double yLow=P[0]->GetYaxis()->GetXmin();
  double yUpp=P[0]->GetYaxis()->GetXmax();
  double yWid=P[0]->GetYaxis()->GetBinWidth(1);

  TString myTs("Compare_"); 	myTs += P[0]->GetName();
  myTs+="_And_"; 		myTs += P[1]->GetName();
  P[2] = new StTProfile2D(myTs,myTs,nX,xLow,xUpp,nY,yLow,yUpp);
  for (int iX=1;iX<=nX;iX++) {
    for (int iY=1;iY<=nY;iY++) {
      double cont[2];
      cont[0] = P[0]->GetBinContent(iX,iY);
      cont[1] = P[1]->GetBinContent(iX,iY);
      double sum = cont[0] + cont[1];
      if (sum <1e-4) 			continue; 
      double del = cont[1] - cont[0];
      double err = pow(P[0]->GetBinError(iX,iY),2) 
                 + pow(P[1]->GetBinError(iX,iY),2);

//      if (del*del < 9*err) 		continue;
//      if (fabs(del)< 0.005*sum) 	continue;
      if (fabs(del)< 0.001*sum) 	continue;
      P[2]->Fill(xLow+0.5*xWid*(iX-1),yLow+0.5*yWid*(iY-1),del);
  } }

  TString ts("C_"); ts +=P[2]->GetName();
  TCanvas *CC = new TCanvas(ts,ts,600,800);
  P[2]->Draw("colz");
  CC->Print(".C");
  CC->Print(".png");
}

void Load()
{

TH1D h1d; h1d.GetPainter();
gSystem->Load("libVMC.so");
gSystem->Load("St_base.so");
gSystem->Load("St_Tables.so");
gSystem->Load("StUtilities.so");
gSystem->Load("StChain.so");
gSystem->Load("StarVMCApplication.so");
gSystem->Load("libStarMiniCern.so");
gSystem->Load("libgeant3.so");
gSystem->Load("GeoTestMaker.so");
loaded=1;

}
    

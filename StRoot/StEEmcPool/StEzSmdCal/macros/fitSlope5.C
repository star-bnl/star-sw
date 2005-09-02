FILE *fdo=0;
TFile *fdA=0;

fitSlope5(char *core="07TD03") {
  // fit slope to 5 tiles at the same eta and in the same phi 
  int sec=atoi(core);
  int eta=atoi(core+4);
  printf("core=%s, sect=%d eta=%d\n",core,sec,eta);

  char txt[200];
  char *path="/star/data05/scratch/balewski/2005-eemcCal/day49-hist/iter2-out/";
  path="./in/";
  sprintf(txt,"%ssum-sect%d.hist.root",path,sec);
  
  fdA=new TFile(txt);  assert(fdA->IsOpen());

  sprintf(txt,"%s.slope",core);
  fdo=fopen(txt,"w"); 
  assert(fdo);
  fitOneSl(core);
  fclose(fdo);
  return;
}


//======================================================
void fitOneSl(char *core, float x1=20, float x2=70){
  c=new TCanvas(core,core,600,600);
  c->Divide(2,3);

  int k=0;
  for(k=0;k<5;k++) {
    core[3]='A'+k;
    TString name="a"; name+=core;
    TH1F *h=(TH1F*) fdA->Get(name); assert(h);
    c->cd(k+1);
    h->Draw();
    h->Fit("expo","R","",x1,x2);
    TF1*ff=h->GetFunction("expo");
    ff->SetLineColor(kRed);
    ff->SetLineWidth(1);
    float sl=ff->GetParameter(1);
    float slEr=ff->GetParError(1);
    fprintf(fdo,"%s %.4f %.4f\n",name.Data()+1,sl,slEr); 
    gPad->SetLogy();
  }
  c->Print();
}

newSmdGain() {
 
  int sectID=6;
  char planeUV='V';
  TString inPath="/star/data05/scratch/balewski/outD1/";

  TString plCore;
  enum {mxS=288};

  char tt1[100];
  sprintf(tt1,"%02d%c",sectID,planeUV);
  plCore=tt1;

  //..... input gain corrections
  TFile *fd1=new TFile("smd"+plCore+".hist.root"); 
  assert(fd1->IsOpen());
  TGraphErrors  *grGc=fd1->Get("gc"+plCore);
  assert(grGc);
  printf("Found:%s:\n",grGc->GetTitle());

  //.... old gains
  TFile *fd1=new TFile(inPath+"R5107008.hist.root"); 
  assert(fd1->IsOpen());
  TH1F  *hug=fd1->Get("ug"+plCore);
  assert(hug);
  printf("Found:%s:\n",hug->GetTitle());

  //...... new gains
  FILE *fd2=fopen(("gains"+plCore+"smd.dat").Data(),"w");
  assert(fd2);

  //... another output for finding extreems
  TGraph *gr2=new TGraph;

  //.....calc new gains
  int i;
  int ns=hug->GetNbinsX();
  int ncor=grGc->GetN();

  printf("aa %d %d\n", ns,ncor);

  for(i=1;i<=mxS;i++) {
    assert(i<ns);
    float g1=hug->GetBinContent(i);
    float egc=0;
    double strip=i,gc=1;
    if(i<ncor) {
      grGc->GetPoint(i-1,strip,gc);
      egc=grGc->GetErrorY(i);
    }

    // tmp 
    if(i>260) { gc=0.8; egc=0.3;}

    float g2=g1*gc;
    float eg2=g1*egc;
    float err=100*egc/gc;
     
    printf("str=%d %f %f %f %f -->%.2f %.2f \n",i,strip,g1,gc,egc,g2,eg2);
    assert(i==(int) (strip+0.5));
    fprintf(fd2,"%s%03d %.2f %.2f  (%.1f%c)\n",plCore.Data(),i,g2,eg2,err,37);
    //break;

    int n=gr2->GetN();
    gr2->SetPoint(n,g2,strip);
  }
  fclose(fd2);

  //  gr2->Sort(); gr2->Print();


}


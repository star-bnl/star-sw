ReadTree(Int_t runnum=0)
{
  gStyle->SetPalette(1);
  gStyle->SetOptFit(1111);
  gStyle->SetCanvasColor(0);
  gStyle->SetPadColor(0);
  gStyle->SetFillColor(0);
  gStyle->SetStatBorderSize(0); 

  Int_t adcth=600;
  Int_t intth=1000;
  Float_t t0min=1.5;
  Float_t t0max=2.5;
  Float_t ch2th=2.;
  
  char tname[100];
  sprintf(tname,"run%d.*tree.root",runnum);//point to the output tree from StFgtTimeShapeMaker

  TChain* tFgt=new TChain("tFgt");
  tFgt->Add(tname);

  Int_t iEvt;
  Int_t rdo;
  Int_t arm;
  Int_t apv;
  Int_t chn;
  Short_t disk;
  Short_t quad;
  Short_t strip;
  Short_t stat;//0:good event, bad if anything else
  Double_t ordinate;
  Double_t lowerSpan;
  Double_t upperSpan;
  Char_t layer;
  Double_t ped;//pedestal
  Double_t pedSig;//pedestal sigma
  Int_t adc[7];
  Int_t adcmax;//maximum adc for the event
  Int_t mmax;//time bin of the maximum adc
  Int_t mmin;//time bin of the minimum adc
  Float_t chi2;//chi squared of the fit
  Float_t fmax;//maximum time of the fit function
  Float_t norm;//normalization for the fit function
  Float_t tau;//width of the fit function
  Float_t t0;//rise time of the fit function
  Float_t offset;//y-offset 
  Int_t errCode;//0:good event, 1:oscillation, 2:saturation
  
  tFgt->SetBranchAddress("iEvt",&iEvt);
  tFgt->SetBranchAddress("rdo",&rdo);
  tFgt->SetBranchAddress("arm",&arm);
  tFgt->SetBranchAddress("apv",&apv);
  tFgt->SetBranchAddress("chn",&chn);
  tFgt->SetBranchAddress("disk",&disk);
  tFgt->SetBranchAddress("quad",&quad);
  tFgt->SetBranchAddress("strip",&strip);
  tFgt->SetBranchAddress("stat",&stat);
  tFgt->SetBranchAddress("ordinate",&ordinate);
  tFgt->SetBranchAddress("lowerSpan",&lowerSpan);
  tFgt->SetBranchAddress("upperSpan",&upperSpan);
  tFgt->SetBranchAddress("layer",&layer);
  tFgt->SetBranchAddress("adc",adc);
  tFgt->SetBranchAddress("ped",&ped);
  tFgt->SetBranchAddress("pedSig",&pedSig);
  tFgt->SetBranchAddress("adcmax",&adcmax);
  tFgt->SetBranchAddress("mmin",&mmin);
  tFgt->SetBranchAddress("mmax",&mmax);
  tFgt->SetBranchAddress("chi2",&chi2);
  tFgt->SetBranchAddress("fmax",&fmax);
  tFgt->SetBranchAddress("norm",&norm);
  tFgt->SetBranchAddress("tau",&tau);
  tFgt->SetBranchAddress("t0",&t0);
  tFgt->SetBranchAddress("offset",&offset);
  tFgt->SetBranchAddress("errCode",&errCode);
   

  Int_t nevents=tFgt->GetEntries();
  printf("nevents=%d \n",nevents);
  for(Int_t i=0;i<nevents;i++)
    {
      tFgt->GetEntry(i);


    };
};


class St_DataSet;
St_DataSet *evgen = 0;
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("St_g2x");
}
void g2x(Int_t N=1){
  Load();
  St_DataSetIter dui;
  dui.Mkdir("dui"); dui.Cd("dui");
  dui.Mkdir("evgen"); evgen = dui.Cd("evgen");
  St_XDFFile *xdf_out = new St_XDFFile("genoutput","wb");
  St_particle *particle = new St_particle("particle",30000);
  evgen->Add(particle);
  for (Int_t iev=1;iev<=N;iev++){
    Int_t res = gst2xdf(particle);
    if (res != 1) break;
    particle->Print(0,5);
    xdf_out->NextEventPut(evgen);
  }
}

class TCanvas;
TCanvas *c1 = 0;
#ifdef TNAIL
TCanvas *c2 = 0;
TPad *selold = 0;
#endif
class Millepede;
Millepede *m = 0;
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);

void Millepede(Int_t iopt=0, Int_t NEvents = 999999, const Char_t *File = "EventLadderNew_uv_250um_w_250um_rot_1000urad.root", const Char_t *opt="K2Miht0.5GeV_80umN100TracksParSig0.03") {
  gROOT->LoadMacro("bfc.C");
  if (! iopt ) {// write                                   
    bfc(0,"in,StEvent,mysql,db,svtDb,ssdDb,ry2005d,simu,nodefault",
	"/star/data07/calib/fisyak/SvtSsdAlignment/rcf1263_TpcSvtSsd/gstar_1.event.root"); // new 
    //	"/star/data06/ITTF/cucu62_ssd/evgen_*.event.root"); // old, 4, 5
    gSystem->Load("StiUtilities"); 
    gSystem->Load("Millepede"); 
    m = new Millepede();  
    m->SetFitCase(Millepede::kLadder);
    //    m->SetFitCase(Millepede::kLayer);
    m->SetSigmaUV(0.025);
    m->SetSigmaW(0.025);
    m->SetSigmaRot(0.001);
    m->SetRecSigmaUV(250e-4);
    if (NEvents > 0) m->MakeTree(File,NEvents);
  } else { 
    bfc(0,"StEvent,nodefault");
    gSystem->Load("Millepede"); 
    m = new Millepede();  
    //    m->SetFitCase(Millepede::kLayer);
    m->SetRecSigmaUV(0.0080);
    if (iopt == 1) {
      m->SetFitCase(Millepede::kLadder);
      m->SetParSig(0,0,0.03);
#ifdef 0
      m->FixAlpha();
      m->FixBeta();
      m->FixGamma();
      m->FixW();
#endif
      
      if (NEvents > 0) m->FillMatrix(File,NEvents,2,0.5,opt);
    } else         {
      m->SetFitCase(Millepede::kPlaneTest);
#if 1
      //      m->SetParSig(0,0,0.1);
      m->SetSigmaUV(0.03);
      m->SetSigmaW(0.0);  m->FixW();
      m->SetSigmaRot(0.003);// m->FixAlpha(); m->FixBeta(); m->FixGamma();
      m->FixAlpha();
      //      m->FixBeta();
      m->FixSums();
      m->SetRecSigmaUV(80e-4);
      //	m->FixPlane(0);
      //       m->SetDebug(4);
#else
      m->SetSigmaUV(0.0);
      m->SetSigmaW(0.0);
      m->SetSigmaRot(0.0);
      m->SetRecSigmaUV(1e-4);
      m->SetDebug(4);
#endif
      if (NEvents > 0) m->PlaneTest(NEvents);
    }
    if (NEvents > 0) m->Fit();
  }
}
#ifdef TNAIL
//________________________________________________________________________________
void tnail() {
   TPad *sel = (TPad*)gPad->GetSelectedPad();
   int px = gPad->GetEventX();
   int py = gPad->GetEventY();
   if (sel && sel != c1 && sel != c2) {
      if (selold) delete selold;
      c2->cd();
      TPad *newpad = (TPad*)sel->Clone();
      c2->GetListOfPrimitives()->Add(newpad);
      newpad->SetPad(0,0,1,1);
      selold = newpad;
      c2->Update();
   }
}
#endif
//________________________________________________________________________________
void DrawResults() {
  static const Int_t NVars = 6;
  static const Char_t *vars[NVars] = {"u","v","w","alpha","beta","gamma"};
  TH1D *hists[2][NVars];
  Int_t Nhists = 0;
  for (Int_t i = 0; i < NVars; i++) {
    hists[0][i] = (TH1D*) gDirectory->Get(Form("Rec%s",vars[i]));
    hists[1][i] = (TH1D*) gDirectory->Get(Form("Sim%s",vars[i]));
    if (hists[0][i]) Nhists++;
  }
  if (! c1) c1 = new TCanvas();
  c1->Clear();
  if (Nhists <= 3)  c1->Divide(Nhists,1);
  else              c1->Divide((Nhists+1)/2,2);
  Int_t iPad  = 1;
  for (Int_t i = 0; i < NVars; i++) {
    c1->cd(iPad);
    TH1D *Rec = hists[0][i];
    TH1D *Sim = hists[1][i];
    if (! Rec) continue;
    Double_t min = 9999, max = -9999;
    if (Sim) {
      //      Sim->Fit("pol0","e"); 
      //      Sim->GetFunction("pol0")->SetLineColor(2);
      min = Sim->GetMinimum(); 
      max = Sim->GetMaximum();
    }
    if (min < Rec->GetMinimum()) Rec->SetMinimum(min);
    if (max > Rec->GetMaximum()) Rec->SetMaximum(max);
    //    Rec->Fit("pol0","e");
    Rec->Draw();
    if (Sim) Sim->Draw("same");
    iPad++;
  }
#ifdef TNAIL
  c1->AddExec("tnail","tnail()");
  c2 = new TCanvas("c2","c2",650,10,800,600);
#endif
}

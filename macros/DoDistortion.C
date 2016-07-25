//________________________________________________________________________________
Double_t xPrime(Double_t *x,Double_t *par) {
  StThreeVectorF pos(60,x[0],100);
  StThreeVectorF posMoved;
  StMagUtilities::Instance()->UndoDistortion(pos.xyz(),posMoved.xyz(), 3); 
  //  return posMoved.phi() - pos.phi();
  return posMoved.x() - pos.x();
}
//________________________________________________________________________________
Double_t yPrime(Double_t *x,Double_t *par) {
  StThreeVectorF pos(60,x[0],100);
  StThreeVectorF posMoved;
  StMagUtilities::Instance()->UndoDistortion(pos.xyz(),posMoved.xyz(), 3); 
  //  return posMoved.phi() - pos.phi();
  return posMoved.y() - pos.y();
}
//________________________________________________________________________________
TF1 *x() {
  TF1 *f = (TF1 *) gROOT->GetFunction("xPrime");
  if (f) delete f;
  f = new TF1("xPrime",xPrime, -40, 40, 0);
  return f;
}
//________________________________________________________________________________
TF1 *y() {
  TF1 *f = (TF1 *) gROOT->GetFunction("yPrime");
  if (f) delete f;
  f = new TF1("yPrime",yPrime, -40, 40, 0);
  return f;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void DoIterate() {
  StThreeVectorF pos(120,20,100);
  cout << "pos        :" << pos << endl;
  StThreeVectorF posMoved;
  StMagUtilities::Instance()->DoDistortion(pos.xyz(),posMoved.xyz(), 3);  
  cout << "PosMoved    :" << posMoved << endl;
  StThreeVectorF posMovedC = posMoved;
  for (Int_t iter = 0; iter < 10; iter++) {
    StThreeVectorF posMovedb;
    cout << "PosMovedC   :" << posMovedC << endl;
    StMagUtilities::Instance()->UndoDistortion(posMovedC.xyz(),posMovedb.xyz(), 3);  
    StThreeVectorF dif = posMovedb - pos;
    cout << "dif     :" << dif << endl;
    if (dif.mag() < 0.0010) break;
    posMovedC = posMovedC - dif; //+ (posMovedb-pos);
  }
  cout << "PosMovedC   :" << posMovedC << endl;
  StMagUtilities::Instance()->UndoDistortion(posMovedC.xyz(),posMoved.xyz(), 3);
  cout << "PosMoved    :" << posMoved << endl;
}
#if 0
//________________________________________________________________________________
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag) {
  f = 0.;
  StThreeVectorD posR(&par[0]);
  StThreeVectorD pos(&par[3]);
  StThreeVectorD posMoved;
  StMagUtilities::Instance()->DoDistortion(posMoved.xyz(),posMovedb.xyz());  
}
//________________________________________________________________________________
void DoFit(){
  TVirtualFitter::SetDefaultFitter("Minuit");
  TVirtualFitter * minuit = TVirtualFitter::Fitter(0,6);
  StThreeVectorF pos(60,x[0],100);
  const Char_t *parname[6] = {"x_ideal","y_ideal","z_ideal","x_distorted","y_distorted","z_distorted"};
  for (int i = 0; i < 6; ++i) {  
    if (i < 3) minuit->SetParameter(i, parname[i], pos.xyz()[i], 0.00, 0,0);
    else       minuit->SetParameter(i, parname[i], pos.xyz()[i-3], 0.01, 0,0);
  }
  Double_t arglist[10];
  Int_t ierflg = 0;
  minuit->SetFCN(fcn);
  //    minuit->SetPrintLevel(-1);
  if (Debug() < 2) {
    arglist[0] = -1;
    minuit->mnexcm("set print",arglist, 1, ierflg);
  }
  minuit->mnexcm("set NOW",arglist, 0, ierflg);
  minuit->mnexcm("CLEAR",arglist, 0, ierflg);
  arglist[0] = 0.5;
  minuit->mnexcm("SET ERR", arglist ,1,ierflg);
  arglist[0] = 500;
  arglist[1] = 1.;
  minuit->mnexcm("MIGRAD", arglist ,2,ierflg);
  minuit->mnexcm("HESSE  ",arglist,0,ierflg);
}
#endif
//________________________________________________________________________________
void DoDistortion(Double_t z = 100) {
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  Int_t nR = 140;
  Int_t nPhi = 90;
  TH2D *dR   = new TH2D("dR"  ,Form("dR   (UndoDistortion) versus R and Phi @ z = %4.0f",z),nR,50,190,nPhi,-15*TMath::DegToRad(), +15*TMath::DegToRad());
  TH2D *dPhi = new TH2D("dPhi",Form("dPhi (UndoDistortion) versus R and Phi @ z = %4.0f",z),nR,50,190,nPhi,-15*TMath::DegToRad(), +15*TMath::DegToRad());
  Double_t phiS = 15*TMath::DegToRad();
  Double_t dxS  = 190*TMath::Sin(phiS);
  Double_t y1   =  50*TMath::Cos(phiS);
  Double_t y2   = 190*TMath::Cos(phiS);
  StThreeVectorF posMoved;
  for (Int_t r = 1; r <= nR; r++) {
    Double_t R = dR->GetXaxis()->GetBinCenter(r);
    for (Int_t p = 1; p <= nPhi; p++) {
      Double_t phiD = dPhi->GetYaxis()->GetBinCenter(p);
      Double_t phi  = phiD;
      StThreeVectorF pos(R*TMath::Sin(phi),R*TMath::Cos(phi),z);
      StMagUtilities::Instance()->UndoDistortion(pos.xyz(),posMoved.xyz(), 3);
      Double_t dr = posMoved.perp() - pos.perp();
      Double_t dphi = posMoved.phi() - pos.phi();
      dR->Fill(R,phiD,dr);
      dPhi->Fill(R,phiD,dphi);
    }
  }
  if (c1) c1->Clear();
  else    c1 = new TCanvas();
  c1->Divide(1,2);
  c1->cd(1);
  dR->Draw("colz");
  c1->cd(2);
  dPhi->Draw("colz");
  Int_t nX = 100;
  Int_t nY = 100;
  TH2D *dX   = new TH2D("dX",Form("dX (UndoDistortion) versus x and y @ z = %4.0f",z),nX,-dxS, dxS,nY, y1,y2);
  TH2D *dY   = new TH2D("dY",Form("dY (UndoDistortion) versus x and y @ z = %4.0f",z),nX,-dxS, dxS,nY, y1,y2);
  for (Int_t r = 1; r <= nX; r++) {
    Double_t X = dX->GetXaxis()->GetBinCenter(r);
    for (Int_t p = 1; p <= nY; p++) {
      Double_t Y = dY->GetYaxis()->GetBinCenter(p);
      StThreeVectorF pos(X,Y,z);
      StMagUtilities::Instance()->UndoDistortion(pos.xyz(),posMoved.xyz(), 3);
      StThreeVectorF dif = posMoved - pos;
      dX->Fill(X,Y,dif.x());
      dY->Fill(X,Y,dif.y());
    }
  }
  TCanvas *c2 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c2");
  if (c2) c2->Clear();
  else    c2 = new TCanvas("c2","c2");
  c2->Divide(1,2);
  c2->cd(1);
  dX->Draw("colz");
  c2->cd(2);
  dY->Draw("colz");
}

/* J.Thomas, 09/18/12
   Various permutations of (pad pitch versus length)

   6.7  5.0  3.35  
   20   16   12  
   
   It would be useful to know which works best for STAR.
          padP x rowP (cm x cm => No.Inner Rows)

   devTR 0.335 x 1.15 => 13 => Total no. of pads per inner sector      1750
   devTA 0.670 x 2.00 => 32 => Total no. of pads per inner sector      2162
   devTB 0.670 x 1.60 => 40 => Total no. of pads per inner sector      2572
   devTC 0.500 x 1.60 => 40 => Total no. of pads per inner sector      3496
   devTD 0.500 x 2.00 => 32 => Total no. of pads per inner sector      2762
   devTE 0.335 x 1.28 => 50 => Total no. of pads per inner sector      6494
   devTF 0.400 x 2.00 => 32 => Total no. of pads per inner sector      3456
*/
//________________________________________________________________________________
void Print(const Char_t *conf = "devTA", Int_t nRows=32, Double_t pitch=0.67, Double_t width=2, 
	   Int_t *Npads=0, Double_t *Rpads=0, Int_t vers = 0) {
  cout << "   Fill TPRS                    ! " << conf << ": sector of padrows  
      sec    = " << 10*vers + 1 << "               ! sector number: 1 for inner, 2 for outer  
      nRow   = " << nRows << "               ! number of padrows in the sector  
      pitch  = " << pitch << "             ! tpc padrow pitch width  
      width  = " << width - 0.05 << "             ! tpc padrow thickness 
      super  = 0                ! number of padraws in a superpadrow  
      dAnode = 0.2              ! distance to anode wire from pad plane" << endl;  
  Int_t nopads = 0;
  if (Npads) {
    Int_t KR = nRows/10;
    if (10*KR != nRows) KR++;
    cout << "      Npads  = {";
    for (Int_t k = 0; k < KR; k++) {
      if (k != 0) cout << "                ";
      for (Int_t r = 10*k; r < TMath::Min(nRows,10*(k+1)); r++) {
	cout << Form("%3i",Npads[r]);
	nopads += Npads[r];
	if (r != nRows - 1) cout << ",";
      }
      if (k != KR - 1) cout << endl;
    }
    cout  << "} ! number of pads in row" << endl;
  }
  if (Rpads) {
    cout << "      Rpads  = {";
    for (Int_t k = 0; k < KR; k++) {
      if (k != 0) cout << "                ";
      for (Int_t r = 10*k; r < TMath::Min(nRows,10*(k+1)); r++) {
	cout << Form("%5.1f",Rpads[r]);
	if (r != nRows - 1) cout << ",";
      }
      if (k != KR - 1) cout << endl;
    }
    cout  << "} ! tpc padrow radii" << endl;
  }
  cout << "   EndFill" << endl;
  cout << conf << "\tTotal no. of pads per inner sector\t" << nopads << endl;
}
//________________________________________________________________________________
Int_t NPads(Double_t R = 60, Double_t pitch = 0.335) {
  Double_t x1 =  60.000;  // Numbers from http://www.star.bnl.gov/public/tpc/hard/figs/pads.gif
  Double_t x2 = 127.195;
  Double_t y1 = 291.45/20.;
  Double_t y2 = 649.90/20;
  Double_t k = (y2 - y1)/(x2 - x1);
  Double_t x0 = x1 - y1/k;
  Double_t a = -1.2;
  Double_t x = k*(R - x0);
#if 0
  Double_t dPhi = 14.93;
  Double_t b = TMath::Tan(15*TMath::DegToRad());
  Double_t x = a + b*R;
#endif
  Int_t Npads = 2*((int) (x/pitch));
  return TMath::Min(182,Npads);
}
//________________________________________________________________________________
TGraph *Pads(){ // 13 rows 0.335 pitch 
  Double_t heights[2] = {1.2, 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.335, 0.670};
  const Int_t NR = 45;
  const Int_t NrInner = 13;
  static Double_t Rpads[NR] = {
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8, 104.0,109.2,114.4,119.6, // inner Centres
    127.195, 129.195, 131.195, 133.195, 135.195, //Outer 
    137.195, 139.195, 141.195, 143.195, 145.195, 
    147.195, 149.195, 151.195, 153.195, 155.195, 
    157.195, 159.195, 161.195, 163.195, 165.195, 
    167.195, 169.195, 171.195, 173.195, 175.195, 
    177.195, 179.195, 181.195, 183.195, 185.195, 
    187.195, 189.195};
  static Int_t nPads[NR] = {
    88 ,     96,    104,    112,    118,    126,    134,    142,    150,    158,   
    166,    174,    182, //Inner 
    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
    114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
    130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
    144,    144}; //Outer
  Double_t xEdge[NR];
  Int_t n;
  for (Int_t i = 0; i < NR; i++) {
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    if (i < NrInner) n = NPads(Rpads[i]-heights[0]/2,pitch);
    else             n = NPads(Rpads[i]-heights[1]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << "\t" << n << endl;
  }
  TGraph *gr = new TGraph(NR,Rpads,xEdge);
  Print("default",NrInner,pitchs[0],heights[0],nPads,Rpads);
  return gr;
}//________________________________________________________________________________
TGraph *PadsdevTA(){// 32 inner padrow, height 2 cm, pitch 0.67
  Double_t heights[2] = {2., 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.670, 0.670};
  const Int_t NrInner = 32;
  static Double_t Rpads[NrInner] =
    { 57.2, 59.2, 61.2, 63.2, 65.2, 67.2, 69.2, 71.2, 73.2, 75.2,
      77.2, 79.2, 81.2, 83.2, 85.2, 87.2, 89.2, 91.2, 93.2, 95.2,
      97.2, 99.2,101.2,103.2,105.2,107.2,109.2,111.2,113.2,115.2,
      117.2,119.2};
  static Int_t nPads[NrInner] = 
    {42,44,46,48,50,50,52,54,56,58,
     58,60,62,64,66,66,68,70,72,74,
     74,76,78,80,82,82,84,86,88,90,
     90,92};
  Double_t xEdge[NrInner];
  for (Int_t i = 0; i < NrInner; i++) {
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    Int_t n = NPads(Rpads[i]-heights[0]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << "\t" << n << endl;
  }
  Print("devTA",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}
//________________________________________________________________________________
TGraph *PadsdevTB(){// 40 inner padrow, height 1.6 cm, pitch 0.67
  Double_t heights[2] = {1.6, 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.670, 0.670};
  Double_t RImin =  56.2;
  Double_t RImax = 120.2;
  const Int_t NrInner = (RImax - RImin)/heights[0];
  TArrayD RpadsD(NrInner); Double_t *Rpads = RpadsD.GetArray();
  TArrayI nPadsI(NrInner); Int_t    *nPads = nPadsI.GetArray();
  TArrayD xEdgeD(NrInner); Double_t *xEdge = xEdgeD.GetArray();
  for (Int_t i = 0; i < NrInner; i++) {
    Rpads[i] = RImin + heights[0]*(i + 0.5);
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    nPads[i] = NPads(Rpads[i]-heights[0]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << endl;
  }
  Print("devTB",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}
//________________________________________________________________________________
TGraph *PadsdevTC(){// 40 inner padrow, height 1.6 cm, pitch 0.50
  Double_t heights[2] = {1.6, 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.500, 0.670};
  Double_t RImin =  56.2;
  Double_t RImax = 120.2;
  const Int_t NrInner = (RImax - RImin)/heights[0];
  TArrayD RpadsD(NrInner); Double_t *Rpads = RpadsD.GetArray();
  TArrayI nPadsI(NrInner); Int_t    *nPads = nPadsI.GetArray();
  TArrayD xEdgeD(NrInner); Double_t *xEdge = xEdgeD.GetArray();
  for (Int_t i = 0; i < NrInner; i++) {
    Rpads[i] = RImin + heights[0]*(i + 0.5);
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    Int_t n = NPads(Rpads[i]-heights[0]/2,pitch);
    nPads[i] = NPads(Rpads[i],pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << endl;
  }
  Print("devTC",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}
//________________________________________________________________________________
TGraph *PadsdevTD(){// 32 inner padrow, height 2.0 cm, pitch 0.50
  Double_t heights[2] = {2., 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.500, 0.670};
  Double_t RImin =  56.2;
  Double_t RImax = 120.2;
  const Int_t NrInner = (RImax - RImin)/heights[0];
  TArrayD RpadsD(NrInner); Double_t *Rpads = RpadsD.GetArray();
  TArrayI nPadsI(NrInner); Int_t    *nPads = nPadsI.GetArray();
  TArrayD xEdgeD(NrInner); Double_t *xEdge = xEdgeD.GetArray();
  for (Int_t i = 0; i < NrInner; i++) {
    Rpads[i] = RImin + heights[0]*(i + 0.5);
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    nPads[i] = NPads(Rpads[i]-heights[0]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << endl;
  }
  Print("devTD",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}
//________________________________________________________________________________
TGraph *PadsdevTE(){// 50 inner padrow, height 1.28 cm, pitch 0.335
  Double_t heights[2] = {1.28, 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.335, 0.670};
  Double_t RImin =  56.2;
  Double_t RImax = 120.2;
  const Int_t NrInner = (RImax - RImin)/heights[0];
  TArrayD RpadsD(NrInner); Double_t *Rpads = RpadsD.GetArray();
  TArrayI nPadsI(NrInner); Int_t    *nPads = nPadsI.GetArray();
  TArrayD xEdgeD(NrInner); Double_t *xEdge = xEdgeD.GetArray();
  for (Int_t i = 0; i < NrInner; i++) {
    Rpads[i] = RImin + heights[0]*(i + 0.5);
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    nPads[i] = NPads(Rpads[i]-heights[0]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << endl;
  }
  Print("devTE",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}
//________________________________________________________________________________
TGraph *PadsdevTF(){// 32 inner padrow, height 2.00 cm, pitch 0.400
  Double_t heights[2] = {2.0, 2.}; // pad length + 0.5
  Double_t pitchs[2] = {0.4, 0.670};
  Double_t RImin =  56.2;
  Double_t RImax = 120.2;
  const Int_t NrInner = (RImax - RImin)/heights[0];
  TArrayD RpadsD(NrInner); Double_t *Rpads = RpadsD.GetArray();
  TArrayI nPadsI(NrInner); Int_t    *nPads = nPadsI.GetArray();
  TArrayD xEdgeD(NrInner); Double_t *xEdge = xEdgeD.GetArray();
  for (Int_t i = 0; i < NrInner; i++) {
    Rpads[i] = RImin + heights[0]*(i + 0.5);
    Double_t pitch = pitchs[0];
    if (i >= NrInner) pitch = pitchs[1];
    xEdge[i] = nPads[i]/2.*pitch;
    nPads[i] = NPads(Rpads[i]-heights[0]/2,pitch);
    cout << "Row " << i+1 << "\tRadius\t" << Rpads[i] << "\tNpads\t" << nPads[i] << endl;
  }
  Print("devTF",NrInner,pitchs[0],heights[0],nPads,Rpads);
  TGraph *gr = new TGraph(NrInner,Rpads,xEdge);
  return gr;
}

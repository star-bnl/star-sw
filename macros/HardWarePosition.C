#ifndef  __HardwarePosition__
#define  __HardwarePosition__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "Rtypes.h"
#include "TVector3.h"
#include "TMath.h"
#endif
//________________________________________________________________________________
UInt_t bits(UInt_t mHardwarePosition, UInt_t bit, UInt_t nbits=1) {return (mHardwarePosition>>bit) & ~(~0UL<<nbits);};
//________________________________________________________________________________
UInt_t sector(UInt_t mHardwarePosition) {return bits(mHardwarePosition, 4, 5);}      // TPC
//________________________________________________________________________________
UInt_t padrow(UInt_t mHardwarePosition) {return bits(mHardwarePosition, 9, 7);}      // TPC
//________________________________________________________________________________
UInt_t index(UInt_t mHardwarePosition)  {return (mHardwarePosition>>4) &((1L<<9)-1);}// SVT Hybrid indexunsigned int
//________________________________________________________________________________
UInt_t barrel(UInt_t mHardwarePosition) {
  Int_t Index = index(mHardwarePosition);
  if( Index < 0) return 0; // Something wrong
  if( Index < 64) return 1;        // Index starts at 0 for hybrid1,wafer1, 
  else if( Index < 208) return 2;  // ladder1,barrel1 and moves out 
  else if( Index < 432) return 3;  // Index=431 is the largest value
  return 0;  // Something wrong
}
//________________________________________________________________________________
UInt_t ladder(UInt_t mHardwarePosition) {
  Int_t Index = index(mHardwarePosition);
  Int_t mLadder;
  static Int_t mHybrid[3]={8,12,14};  // Hybrids on each ladder
  static Int_t mLadderTot[2]={8,12};  // Ladders on each barrel
  switch( barrel(mHardwarePosition)){
  case 1:
    mLadder = Index/mHybrid[0];
    return mLadder+1;
    break;
  case 2:
    Index -= mHybrid[0]*mLadderTot[0];  // Subtract off hybrids from previous
    mLadder = Index/mHybrid[1];         // layers the div. by hybrids per lay 
    return mLadder+1;
    break;
  case 3:
    Index -= mHybrid[0]*mLadderTot[0]; // Subtract off hybrids from previous
    Index -= mHybrid[1]*mLadderTot[1]; // layers the div. by hybrids per lay
    mLadder = Index/mHybrid[2];
    return mLadder+1;
    break;
  default:
    return 0; //Something Wrong
  }
}
//________________________________________________________________________________
UInt_t layer(UInt_t mHardwarePosition) {
  Int_t Barrel = barrel(mHardwarePosition);
  Int_t Ladder = ladder(mHardwarePosition);
  if( Ladder%2){
    switch( Barrel){
    case 1:
      return 2;    // Outer layers are the odd numbered ladders
      break;
    case 2:
      return 4;
      break;
    case 3:
      return 6;
      break;
    default:
      return 0;
    }
  }
   else{
     switch( Barrel){
     case 1: 
       return 1;  // Inner layers are the even ladders
       break;
     case 2:
       return 3;
       break;
     case 3:
       return 5;
       break;
     default:
       return 0;
     }
   }

  return 0;
}
//________________________________________________________________________________
UInt_t wafer(UInt_t mHardwarePosition) {
  Int_t Index = index(mHardwarePosition);
  Int_t Barrel = barrel(mHardwarePosition)-1;
  Int_t Ladder = ladder(mHardwarePosition)-1;
  static Int_t mHybrid[3] ={8,12,14};  // Number of hybrids per ladder
  static Int_t mLadderTot[3] ={8,12,16}; // Number of ladders per barrel
  for( Int_t B=0; B<Barrel; B++){
    Index -= mHybrid[B]*mLadderTot[B]; // Sub. the hybrids from prev. barrels
  }
  for( Int_t L=0; L<Ladder; L++){
    Index -= mHybrid[Barrel]; // Sub. hybrids from previous ladders on  barrel
  }
  return (Index/2)+1;  // Two hybrids per wafer start counting from 1

}
//________________________________________________________________________________
UInt_t hybrid(UInt_t mHardwarePosition) {return (index(mHardwarePosition)%2)+1; }
//________________________________________________________________________________
Double_t Phi(Double_t px, Double_t py) {
  TVector3 xyz(px,py,1);
  return TMath::RadToDeg()*xyz.Phi();
}
//________________________________________________________________________________
Double_t Dip(Double_t px, Double_t py, Double_t pz) {
  TVector3 xyz(px,py,pz);
  return TMath::RadToDeg()*TMath::ATan2(pz,TMath::Sqrt(px*px+py*py));
}
//________________________________________________________________________________
Double_t Eta(Double_t px, Double_t py, Double_t pz) {
  TVector3 xyz(px,py,pz);
  return xyz.PseudoRapidity();
}
//________________________________________________________________________________
Double_t LocalPhi(Double_t px, Double_t py, Int_t volid) {
  Int_t sec = volid;
  if (sec > 100) {
    sec = (volid/100)%100;
  }
  Double_t phi = Phi(px,py);
  Double_t phi0 = ((sec > 12) ? 30*(sec -  21) 
		   :            30*(3   - sec));
  Double_t dphi = phi0 - phi;
  if (dphi >  180) dphi -= 360;
  if (dphi < -180) dphi += 360;
  return dphi;
}
//________________________________________________________________________________
Int_t secE2W(Int_t sector) {
  if (sector <= 12) return sector;
  Int_t secW = 24 - sector;
  if (secW == 0) secW = 12;
  return secW;
}
//________________________________________________________________________________
Int_t SectorNumber(Float_t x, Float_t y, Float_t z) {
  Double_t phi = TMath::RadToDeg()*TMath::ATan2(y,x);
  Int_t iphi = TMath::Nint(phi/30.);
  Int_t Sector;
  if (z > 0) {
    Sector = 3 - iphi;
    if (Sector <=  0) Sector += 12;
  } else {
    Sector = 21 + iphi;
    if (Sector > 24) Sector -= 12;
  }
  return Sector;
}
//________________________________________________________________________________
Float_t PadRowRadius(Float_t x) {
  static Float_t Rpads[45] = 
    { 60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8,
      104.0,109.2,114.4,119.6, //    ! tpc padrow radii
      127.195, 129.195, 131.195, 133.195, 135.195,
      137.195, 139.195, 141.195, 143.195, 145.195,
      147.195, 149.195, 151.195, 153.195, 155.195,
      157.195, 159.195, 161.195, 163.195, 165.195,
      167.195, 169.195, 171.195, 173.195, 175.195,
      177.195, 179.195, 181.195, 183.195, 185.195,
      187.195, 189.195 }; //       ! tpc padrow radii
  Int_t row = TMath::Nint(x);
  if (row < 1 || row > 45) return -1.;
  else                     return Rpads[row-1];
}
//________________________________________________________________________________
Float_t rowSigned(Int_t row, Int_t sector) {
  return (sector <= 12) ? row : -row;
}
//________________________________________________________________________________
Float_t SignedY(Double_t x, Double_t y) {
  return (x > 0) ? y : -y;
}
//________________________________________________________________________________
void HardWarePosition() {}
//________________________________________________________________________________
UInt_t getSectorMask(UInt_t sec=1, UInt_t MASK=65535) {
   if( sec % 2 == 0){ // if its even relevent bits are 8-13
     MASK = MASK >> 8; // with iTPC 8 bits otherwise 6
   }
   return MASK &= 255;
}
//________________________________________________________________________________
UInt_t isRdoOn(UInt_t sec=1, Int_t rdo = 1, UInt_t MASK=65535) {
  Int_t m =  getSectorMask(sec, MASK);
  Int_t b = m >> (rdo - 1);
  Int_t c = b & 1;
  //  cout << MASK << "\t" << m  << "\t" << b << "\t" << c << endl;
  return c; // (getSectorMask(sec, MASK) >> (rdo-1)) & 1;
}
//________________________________________________________________________________
void isRdosOn(UInt_t sec=1,  UInt_t MASK=65535) {
  //  cout << "sec = " << sec << "\tMASK = " << MASK << endl;
  for (Int_t rdo = 1; rdo <= 8; rdo++) 
    cout << "rdo " << rdo << " is " << isRdoOn(sec, rdo, MASK) << endl;
}
//________________________________________________________________________________
UInt_t SwitchOffRDO(UInt_t MASK=65535, UInt_t sec = 1, Int_t rdo1 = 1, Int_t rdo2 = -1) {
  UInt_t mask = MASK;
  if (rdo1 > 0) mask &= ~(1 << (8*((sec-1)%2) + rdo1 - 1));
  if (rdo2 > 0) mask &= ~(1 << (8*((sec-1)%2) + rdo2 - 1));
  return mask;
}
//________________________________________________________________________________
Double_t xPad(Double_t pad, Int_t row) {
  Int_t nPads[72] = { //J.Thomas, 05/31/2016
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
    104,106,108,110,110,112,114,116,118,120,
    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
    114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
    130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
    144,    144};
  Double_t x =  -2;
  if (row > 0 && row < 73) {
    x = 2*pad / ( nPads[row-1] + 1.0) - 1;
  }
  return x;
}
#endif

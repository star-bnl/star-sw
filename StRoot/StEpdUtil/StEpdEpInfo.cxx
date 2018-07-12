#include "StEpdEpInfo.h"
#include "TMath.h"

StEpdEpInfo::StEpdEpInfo(){
  for (int iorder=0; iorder<_EpOrderMax; iorder++){
    for (int ew=0; ew<2; ew++){
      for (int xy=0; xy<2; xy++){
	QrawOneSide[ew][iorder][xy] = 0.0;
	QphiWeightedOneSide[ew][iorder][xy] = 0.0;
	for (int ring=0; ring<16; ring++){
	  QringRaw[ew][iorder][xy][ring] = 0.0;
	  QringPhiWeighted[ew][iorder][xy][ring] = 0.0;
	}
      }
    }
  }

  for (int iorder=0; iorder<_EpOrderMax; iorder++){
    for (int ewfull=0; ewfull<3; ewfull++){
      PsiRaw[ewfull][iorder] = -999.0;
      PsiPhiWeighted[ewfull][iorder] = -999.0;
      PsiPhiWeightedAndShifted[ewfull][iorder] = -999.0;
    }
  }
}


// ===================== Access to Q-vectors ==========================

//------------------------------ Raw Q --------------------------------
TVector2 StEpdEpInfo::RawQ(int ew, int order){
  TVector2 q(QrawOneSide[ew][order-1][0],QrawOneSide[ew][order-1][1]);
  return q;
}
TVector2 StEpdEpInfo::EastRawQ(int order){return RawQ(0,order);}
TVector2 StEpdEpInfo::WestRawQ(int order){return RawQ(1,order);}
//-----------------------------------------------------------------------

//------------------------ phi-weighted Q -------------------------------
TVector2 StEpdEpInfo::PhiWeightedQ(int ew, int order){
  TVector2 q(QphiWeightedOneSide[ew][order-1][0],QphiWeightedOneSide[ew][order-1][1]);
  return q;
}
TVector2 StEpdEpInfo::EastPhiWeightedQ(int order){return PhiWeightedQ(0,order);}
TVector2 StEpdEpInfo::WestPhiWeightedQ(int order){return PhiWeightedQ(1,order);}
//-----------------------------------------------------------------------

//---------------------- Raw Q for a single ring -----------------------
TVector2 StEpdEpInfo::RingRawQ(int ew, int order, int ring){
  TVector2 q(QringRaw[ew][order-1][0][ring-1],QringRaw[ew][order-1][1][ring-1]);
  return q;
}
TVector2 StEpdEpInfo::EastRingRawQ(int order, int ring){return RingRawQ(0,order,ring);}
TVector2 StEpdEpInfo::WestRingRawQ(int order, int ring){return RingRawQ(1,order,ring);}
//-----------------------------------------------------------------------

//---------------- phi-weighted Q for a single ring----------------------
TVector2 StEpdEpInfo::RingPhiWeightedQ(int ew, int order, int ring){
  TVector2 q(QringPhiWeighted[ew][order-1][0][ring-1],QringPhiWeighted[ew][order-1][1][ring-1]);
  return q;
}
TVector2 StEpdEpInfo::EastRingPhiWeightedQ(int order, int ring){return RingPhiWeightedQ(0,order,ring);}
TVector2 StEpdEpInfo::WestRingPhiWeightedQ(int order, int ring){return RingPhiWeightedQ(1,order,ring);}
//-----------------------------------------------------------------------


// ===================== Access to Event-plane angles ====================

//------------------------- raw EP angles --------------------------------
double StEpdEpInfo::RawPsi(int ewfull, int order){
  return Range(PsiRaw[ewfull][order-1],order);
}
double StEpdEpInfo::EastRawPsi(int order){return RawPsi(0,order);}
double StEpdEpInfo::WestRawPsi(int order){return RawPsi(1,order);}
double StEpdEpInfo::FullRawPsi(int order){return RawPsi(2,order);}
//-----------------------------------------------------------------------

//-------------------- phi-weighted EP angles ---------------------------
double StEpdEpInfo::PhiWeightedPsi(int ewfull, int order){
  return Range(PsiPhiWeighted[ewfull][order-1],order);
}
double StEpdEpInfo::EastPhiWeightedPsi(int order){return PhiWeightedPsi(0,order);}
double StEpdEpInfo::WestPhiWeightedPsi(int order){return PhiWeightedPsi(1,order);}
double StEpdEpInfo::FullPhiWeightedPsi(int order){return PhiWeightedPsi(2,order);}
//-----------------------------------------------------------------------

//------------------- phi-weighted and shifted EP angles ----------------
double StEpdEpInfo::PhiWeightedAndShiftedPsi(int ewfull, int order){
  return Range(PsiPhiWeightedAndShifted[ewfull][order-1],order);
}
double StEpdEpInfo::EastPhiWeightedAndShiftedPsi(int order){return PhiWeightedAndShiftedPsi(0,order);}
double StEpdEpInfo::WestPhiWeightedAndShiftedPsi(int order){return PhiWeightedAndShiftedPsi(1,order);}
double StEpdEpInfo::FullPhiWeightedAndShiftedPsi(int order){return PhiWeightedAndShiftedPsi(2,order);}
//-----------------------------------------------------------------------

//--------------------- single-ring raw EP angles -----------------------
double StEpdEpInfo::RingRawPsi(int ew, int order, int ring){
  return Range(PsiRingRaw[ew][order-1][ring-1],order);
}
double StEpdEpInfo::EastRingRawPsi(int order, int ring){return RingRawPsi(0,order,ring);}
double StEpdEpInfo::WestRingRawPsi(int order, int ring){return RingRawPsi(1,order,ring);}
//-----------------------------------------------------------------------

//--------------------- single-ring raw EP angles -----------------------
double StEpdEpInfo::RingPhiWeightedPsi(int ew, int order, int ring){
  return Range(PsiRingPhiWeighted[ew][order-1][ring-1],order);
}
double StEpdEpInfo::EastRingPhiWeightedPsi(int order, int ring){return RingPhiWeightedPsi(0,order,ring);}
double StEpdEpInfo::WestRingPhiWeightedPsi(int order, int ring){return RingPhiWeightedPsi(1,order,ring);}
//-----------------------------------------------------------------------

//----- Simple method to put angles in a convenient range: (0,2pi/n) ----
double StEpdEpInfo::Range(double psi,int order){
  double wrap = 2.0*TMath::Pi()/(double)order;
  if (psi<0.0) psi += (1.0+(int)(fabs(psi)/wrap))*wrap;
  else{ if (psi>wrap) psi -= ((int)(psi/wrap))*wrap;}
  return psi;
}
  

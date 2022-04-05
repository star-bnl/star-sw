/***********************************************************************
 *
 * Author: Gene Van Buren, BNL, 24-May-2005
 *
 ***********************************************************************
 *
 * Description:  Code to patch incorrect data from using the wrong
 *               Twist distortion correction.
 *               Default is to patch P05ia production.
 *
 ***********************************************************************/

#include "TwistPatch.h"
#include "TMath.h"
#include "StMessMgr.h"


ClassImp(TwistPatch)

TwistPatch::TwistPatch() {
  magf = -1; // int
  
  vtxpars = 0; // Double_t*
  etapars = 0; // Double_t*
  ptpars  = 0; // Double_t*
  phipars = 0; // Double_t*
  
  CorrectP05ia();

}

TwistPatch::~TwistPatch() {}

//////////////////////

void TwistPatch::SetMagF(double mag) {
  // Require full strength mag field (+/- 10%)
  if (TMath::Abs(TMath::Abs(mag/4.98) - 1) > 0.1) {
    gMessMgr->Error() << "TwistPatch: Invalid magnetic field: " << mag
      << "\n  Need +/- 4.98 (+/- 10%)" << endm;
    return;
  }
  // Neg field -> 0, Pos field -> 1
  magf = (int) (0.5 + TMath::Sign(0.5,mag));
  SetVtxPars();
  SetEtaPars();
}

void TwistPatch::SetVtxPars() {
  Double_t* arr = vtxparsFull.GetArray();
  int offset = magf*3*2;
  vtxpars = &(arr[offset]);
}

void TwistPatch::SetEtaPars() {
  Double_t* arr = etaparsFull.GetArray();
  int offset = magf*2;
  etapars = &(arr[offset]);
}

void TwistPatch::SetPtPars(int ew, int ch) {
  Double_t* arr = ptparsFull.GetArray();
  int offset = ((magf*2 + ew)*2 + ch)*4*2;
  ptpars = &(arr[offset]);
}

void TwistPatch::SetPhiPars(int ew, int ch) {
  Double_t* arr = phiparsFull.GetArray();
  int offset = ((magf*2 + ew)*2 + ch)*5*2;
  phipars = &(arr[offset]);
}

//////////////////////

void TwistPatch::PrimaryVertex(StThreeVectorF& vtx, double mag) {
  double vx = vtx.x(); double vy = vtx.y(); double vz = vtx.z();
  PrimaryVertex(vx,vy,vz,mag);
  vtx.setX(vx); vtx.setY(vy); vtx.setZ(vz);
}

void TwistPatch::PrimaryVertex(double& vx, double& vy, double& vz, double mag) {
  if (mag != -999.) SetMagF(mag);
  if (magf == -1) {
    gMessMgr->Error() << "TwistPatch: unassigned magnetic field." << endm;
    return;
  }
  if (TMath::Abs(vz)>40.) {
    gMessMgr->Warning() << "TwistPatch: corrections invalid outsize |vz|<40cm." << endm;
    vx = -999.; vy = -999.; vz = -999.;
    return;
  }
  vx += vtxpars[0] + vtxpars[1]*vz;
  vy += vtxpars[2] + vtxpars[3]*vz;
  vz += vtxpars[4] + vtxpars[5]*vz;
}

void TwistPatch::PrimaryTrack(double& pt, double& phi, double& eta,
			      int ch, double vz, double mag) {
  if (mag != -999.) SetMagF(mag);
  if (magf == -1) {
    gMessMgr->Error() << "TwistPatch: unassigned magnetic field." << endm;
    return;
  }
  if (TMath::Abs(vz)>40.) {
    gMessMgr->Warning() << "TwistPatch: corrections invalid outsize |vz|<40cm."
      << endm;
    pt = -999.; phi = 0.; eta = -999.;
    return;
  }

  // Corrections are invalid (and unimportant?) for low-pt tracks:
  if (pt < 0.5) return;

  int ewi = -1;
  double seta = TMath::SinH(eta);
  if (seta < TMath::Min(-vz/60.0,-vz/200.0)) {
    ewi = 0; // east TPC tracks
  } else if (seta > TMath::Max(-vz/60.0,-vz/200.0)) {
    ewi = 1; // west TPC tracks
  } else {
    // Invalid: track passes through Central Membrane in TPC
    pt = -999.; phi = 0.; eta = -999.;
    return;
  }
  int chi = ( (ch>0) ? 1 : 0 );
  SetPtPars(ewi,chi);
  SetPhiPars(ewi,chi);
  
  double Amp, Phase;
  
  Amp = etapars[0];
  Phase = etapars[1];
  eta += Amp*sin(phi-Phase);

  Amp = ptpars[0] + ptpars[1]*eta + ptpars[2]*vz + ptpars[3]*eta*vz;
  Phase = ptpars[4] + ptpars[5]*eta + ptpars[6]*vz + ptpars[7]*eta*vz;
  pt += pt*pt*Amp*sin(phi-Phase);

  Amp = phipars[0] + phipars[1]*eta + phipars[2]*vz +
    phipars[3]*eta*vz + phipars[4]*eta*eta;
  Phase = phipars[5] + phipars[6]*eta + phipars[7]*vz +
    phipars[8]*eta*vz + phipars[9]*eta*eta;
  phi += Amp*sin(phi-Phase);
  
}

//////////////////////

void TwistPatch::CorrectP05ia() {
  double vtxparsP05ia[12] = {
    // RFF:
		  //  x:
     2.30466e-02, // +/- 9.88135e-06
    -1.32527e-03, // +/- 6.14399e-07
		  //  y:
    -9.30086e-02, // +/- 9.66327e-06
     7.34537e-04, // +/- 5.66952e-07
		  //  z:
    -1.35367e-02, // +/- 1.75835e-05
    -1.58680e-05, // +/- 6.25426e-07

    // FF:
		  //  x:
     2.37662e-02, // +/- 4.07841e-06
    -3.26507e-04, // +/- 2.52854e-07
		  //  y:
    -9.14897e-02, // +/- 4.06082e-06
     4.78500e-04, // +/- 2.48710e-07
		  //  z:
    -1.30379e-02, // +/- 1.13662e-05
     6.15786e-06  // +/- 5.71219e-07
  };
  vtxparsFull.Set(12,vtxparsP05ia);
  
  double etaparsP05ia[4] = {
    // RFF:
    -5.00077e-04, // +/- 5.40643e-07
     1.31853e+00, // +/- 1.03448e-03

    // FF:
     3.89157e-04, // +/- 2.78473e-07
    -1.82777e+00  // +/- 7.13594e-04
  };
  etaparsFull.Set(4,etaparsP05ia);
  
  double ptparsP05ia[64] ={
    // RFF:
                  //  east neg:
     2.32846e-03, // +/- 1.99903e-05
    -3.40209e-04, // +/- 3.37835e-05
     6.60694e-05, // +/- 1.43984e-06
     9.85024e-06, // +/- 2.41392e-06
    -4.95131e-01, // +/- 9.73165e-03
     4.82614e-02, // +/- 1.64392e-02
    -2.38376e-03, // +/- 7.51324e-04
    -1.56343e-03, // +/- 1.17801e-03
		  //  east pos:
     2.25788e-03, // +/- 1.86541e-05
     3.33810e-04, // +/- 3.15036e-05
     5.91728e-05, // +/- 1.21662e-06
    -2.54143e-06, // +/- 2.04576e-06
     2.61730e+00, // +/- 9.82629e-03
    -1.06788e-01, // +/- 1.85022e-02
    -4.76870e-03, // +/- 7.19356e-04
    -4.34837e-04, // +/- 1.25031e-03
		  //  west neg:
     2.32846e-03, // +/- 1.99903e-05
    -3.40209e-04, // +/- 3.37835e-05
     6.60694e-05, // +/- 1.43984e-06
     9.85024e-06, // +/- 2.41392e-06
    -4.95131e-01, // +/- 9.73165e-03
     4.82614e-02, // +/- 1.64392e-02
    -2.38376e-03, // +/- 7.51324e-04
    -1.56343e-03, // +/- 1.17801e-03
		  //  west pos:
     2.54657e-03, // +/- 1.78081e-05
    -4.67158e-04, // +/- 3.20610e-05
    -6.49651e-05, // +/- 1.20041e-06
     6.93050e-06, // +/- 2.15511e-06
    -6.42205e-01, // +/- 6.89840e-03
     7.98875e-02, // +/- 1.37710e-02
     2.84334e-04, // +/- 5.50864e-04
     8.89173e-04, // +/- 1.02173e-03    
    
    // FF:
                  //  east neg:
     8.88408e-04, // +/- 1.23449e-05
     1.05058e-03, // +/- 2.29742e-05
     2.30728e-05, // +/- 8.66520e-07
     2.93934e-06, // +/- 1.60371e-06
     1.41523e+00, // +/- 1.80226e-02
    -1.58434e-01, // +/- 5.03514e-02
     4.30552e-03, // +/- 1.50170e-03
     1.22534e-02, // +/- 3.13251e-03
                  //  east pos:
     8.01270e-04, // +/- 1.22456e-05
    -9.31071e-04, // +/- 2.24259e-05
     2.02355e-05, // +/- 8.61748e-07
    -4.55626e-07, // +/- 1.56680e-06
    -1.64989e+00, // +/- 1.08019e-02
    -3.21806e-01, // +/- 1.79223e-02
     1.81404e-04, // +/- 8.76902e-04
     2.80395e-03, // +/- 1.41348e-03
                  //  west neg:
     8.88408e-04, // +/- 1.23449e-05
     1.05058e-03, // +/- 2.29742e-05
     2.30728e-05, // +/- 8.66520e-07
     2.93934e-06, // +/- 1.60371e-06
     1.41523e+00, // +/- 1.80226e-02
    -1.58434e-01, // +/- 5.03514e-02
     4.30552e-03, // +/- 1.50170e-03
     1.22534e-02, // +/- 3.13251e-03
                  //  west pos:
     1.03332e-03, // +/- 1.10946e-05
     7.57373e-04, // +/- 1.94757e-05
    -2.41674e-05, // +/- 7.29351e-07
     1.02034e-06, // +/- 1.34588e-06
     1.57296e+00, // +/- 8.47273e-03
     2.25255e-01, // +/- 1.41731e-02
     1.34373e-05, // +/- 7.06803e-04
     1.96348e-03  // +/- 1.14546e-03
    
  };
  ptparsFull.Set(64,ptparsP05ia);
  
  double phiparsP05ia[80] ={
    // RFF:
                  //  east neg:
     4.93131e-04, // +/- 4.01012e-06
    -4.23908e-05, // +/- 1.76226e-05
     1.31741e-05, // +/- 2.10474e-07
    -7.03829e-08, // +/- 3.64895e-07
     1.51012e-04, // +/- 1.76204e-05
     2.89801e+00, // +/- 9.55759e-03
    -1.45601e-01, // +/- 3.97013e-02
    -2.42827e-03, // +/- 5.07492e-04
     4.78493e-03, // +/- 7.68787e-04
     9.33780e-03, // +/- 3.74458e-02
                  //  east pos:
     4.52643e-04, // +/- 4.28775e-06
    -2.45860e-05, // +/- 1.78107e-05
     1.20074e-05, // +/- 2.36478e-07
    -1.90426e-06, // +/- 3.97117e-07
     1.28262e-04, // +/- 1.76248e-05
     2.47569e+00, // +/- 9.99750e-03
    -1.19158e-01, // +/- 4.35742e-02
    -2.04744e-05, // +/- 5.71813e-04
     8.56369e-03, // +/- 8.99748e-04
     1.85302e-01, // +/- 4.24722e-02
                  //  west neg:
     4.93131e-04, // +/- 4.01012e-06
    -4.23908e-05, // +/- 1.76226e-05
     1.31741e-05, // +/- 2.10474e-07
    -7.03829e-08, // +/- 3.64895e-07
     1.51012e-04, // +/- 1.76204e-05
     2.89801e+00, // +/- 9.55759e-03
    -1.45601e-01, // +/- 3.97013e-02
    -2.42827e-03, // +/- 5.07492e-04
     4.78493e-03, // +/- 7.68787e-04
     9.33780e-03, // +/- 3.74458e-02
                  //  west pos:
     5.08061e-04, // +/- 3.74512e-06
     9.58800e-06, // +/- 1.53306e-05
    -1.27918e-05, // +/- 1.95678e-07
    -4.98383e-07, // +/- 3.25354e-07
     1.32884e-04, // +/- 1.50385e-05
    -7.77448e-01, // +/- 7.14028e-03
     2.44479e-01, // +/- 3.12072e-02
     3.67018e-04, // +/- 4.18872e-04
     5.51353e-03, // +/- 6.46282e-04
     5.67082e-02, // +/- 3.02489e-02

    // FF:
                  //  east neg:
     1.74816e-04, // +/- 2.68167e-06
     1.33818e-04, // +/- 1.11078e-05
     4.61758e-06, // +/- 1.43821e-07
     3.63453e-06, // +/- 2.33195e-07
     3.29694e-04, // +/- 1.08453e-05
     1.20753e+00, // +/- 1.53169e-02
    -2.84916e+00, // +/- 6.42459e-02
    -1.17895e-02, // +/- 9.24726e-04
     5.26084e-03, // +/- 1.26590e-03
    -1.32828e+00, // +/- 5.98208e-02
                  //  east pos:
     1.59700e-04, // +/- 2.52246e-06
    -1.00637e-04, // +/- 1.07372e-05
     4.01605e-06, // +/- 1.30586e-07
     1.43594e-06, // +/- 2.19201e-07
     1.92839e-04, // +/- 1.04302e-05
     1.78569e+00, // +/- 1.47063e-02
    -2.14438e+00, // +/- 5.74426e-02
    -9.44847e-03, // +/- 6.96743e-04
     7.87320e-04, // +/- 9.87320e-04
    -1.15795e+00, // +/- 5.06799e-02
                  //  west neg:
     1.74816e-04, // +/- 2.68167e-06
     1.33818e-04, // +/- 1.11078e-05
     4.61758e-06, // +/- 1.43821e-07
     3.63453e-06, // +/- 2.33195e-07
     3.29694e-04, // +/- 1.08453e-05
     1.20753e+00, // +/- 1.53169e-02
    -2.84916e+00, // +/- 6.42459e-02
    -1.17895e-02, // +/- 9.24726e-04
     5.26084e-03, // +/- 1.26590e-03
    -1.32828e+00, // +/- 5.98208e-02
                  //  west pos:
     1.93766e-04, // +/- 2.35454e-06
     5.79827e-05, // +/- 9.32671e-06
    -4.49880e-06, // +/- 1.19739e-07
     1.40342e-06, // +/- 1.95492e-07
     2.00904e-04, // +/- 8.90481e-06
    -1.40318e+00, // +/- 1.13584e-02
     1.86508e+00, // +/- 4.34266e-02
     7.21917e-03, // +/- 5.34498e-04
     2.70015e-03, // +/- 7.54637e-04
    -8.63279e-01  // +/- 3.81182e-02
    
  };
  phiparsFull.Set(80,phiparsP05ia);

}

/***********************************************************************
 * $Id: TwistPatch.cxx,v 1.2 2005/05/26 17:25:55 genevb Exp $
 * $Log: TwistPatch.cxx,v $
 * Revision 1.2  2005/05/26 17:25:55  genevb
 * Fixed typo with ptpars,phipars
 *
 * Revision 1.1  2005/05/24 19:05:02  genevb
 * Introduce TwistPatch
 *
 *
 ***********************************************************************/

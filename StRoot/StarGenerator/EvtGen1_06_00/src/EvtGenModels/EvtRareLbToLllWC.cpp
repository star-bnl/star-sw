// Include files 



// local

#include "EvtGenModels/EvtRareLbToLllWC.hh"

//-----------------------------------------------------------------------------
// Implementation file for class : EvtRareLbToLllWC
//
// 2013-11-27 : Thomas Blake
//-----------------------------------------------------------------------------

//=============================================================================
// Standard constructor, initializes variables
//=============================================================================

EvtRareLbToLllWC::EvtRareLbToLllWC(  ) {}

//=============================================================================
// Destructor
//=============================================================================

EvtRareLbToLllWC::~EvtRareLbToLllWC() {}

EvtComplex EvtRareLbToLllWC::GetC7Eff( const double q2 ) const 
{

  static double mbeff = 4.8;
  double shat = q2/mbeff/mbeff;
  double logshat;
  logshat = log(shat);
  
  double muscale;
  muscale = 2.5;
  double alphas;
  alphas = 0.267;
  double A7;
  A7 = -0.353 + 0.023;
  double A8;
  A8 = -0.164;
  double C1;
  C1 = -0.697;
  double C2;
  C2 = 1.046;
  
  double Lmu;
  Lmu = log(muscale/mbeff);

  EvtComplex uniti(0.0,1.0);

  EvtComplex c7eff;
  if (shat > 0.25)
  { 
   c7eff = A7;
   return c7eff;
  }

  // change energy scale to 5.0 for full NNLO calculation below shat = 0.25
  muscale = 5.0;
  alphas = 0.215;
  A7 = -0.312 + 0.008;
  A8 = -0.148;
  C1 = -0.487;
  C2 = 1.024;
  Lmu = log(muscale/mbeff);

  EvtComplex F71;
  EvtComplex f71;
  EvtComplex k7100(-0.68192,-0.074998);
  EvtComplex k7101(0.0,0.0);
  EvtComplex k7110(-0.23935,-0.12289);
  EvtComplex k7111(0.0027424,0.019676);
  EvtComplex k7120(-0.0018555,-0.175);
  EvtComplex k7121(0.022864,0.011456);
  EvtComplex k7130(0.28248,-0.12783);
  EvtComplex k7131(0.029027,-0.0082265);
  f71 = k7100 + k7101*logshat + shat*(k7110 + k7111*logshat) +
        shat*shat*(k7120 + k7121*logshat) + 
        shat*shat*shat*(k7130 + k7131*logshat); 
  F71 = (-208.0/243.0)*Lmu + f71;

  EvtComplex F72;
  EvtComplex f72;
  EvtComplex k7200(4.0915,0.44999);
  EvtComplex k7201(0.0,0.0);
  EvtComplex k7210(1.4361,0.73732);
  EvtComplex k7211(-0.016454,-0.11806);
  EvtComplex k7220(0.011133,1.05);
  EvtComplex k7221(-0.13718,-0.068733);
  EvtComplex k7230(-1.6949,0.76698);
  EvtComplex k7231(-0.17416,0.049359);
  f72 = k7200 + k7201*logshat + shat*(k7210 + k7211*logshat) +
        shat*shat*(k7220 + k7221*logshat) + 
        shat*shat*shat*(k7230 + k7231*logshat); 
  F72 = (416.0/81.0)*Lmu + f72;
  
  EvtComplex F78;
  F78 = (-32.0/9.0)*Lmu + 8.0*EvtConst::pi*EvtConst::pi/27.0 + (-44.0/9.0) 
        + (-8.0*EvtConst::pi/9.0)*uniti +
        (4.0/3.0*EvtConst::pi*EvtConst::pi - 40.0/3.0)*shat +
        (32.0*EvtConst::pi*EvtConst::pi/9.0 - 316.0/9.0)*shat*shat +
        (200.0*EvtConst::pi*EvtConst::pi/27.0 - 658.0/9.0)*shat*shat*shat +
    (-8.0*logshat/9.0)*(shat + shat*shat + shat*shat*shat);
        
  c7eff = A7 - alphas/(4.0*EvtConst::pi)*(C1*F71 + C2*F72 + A8*F78);

  return c7eff;
}

EvtComplex EvtRareLbToLllWC::GetC9Eff( const double q2, const bool btod ) const 
{
  
  static double mbeff = 4.8;
  double shat = q2/mbeff/mbeff;
  double logshat;
  logshat = log(shat);
  double mchat = 0.29;

  
  double muscale;
  muscale = 2.5;
  double alphas;
  alphas = 0.267;
  double A8;
  A8 = -0.164;
  double A9;
  A9 = 4.287 + (-0.218);
  double C1;
  C1 = -0.697;
  double C2;
  C2 = 1.046;
  double T9;
  T9 = 0.114 + 0.280;
  double U9;
  U9 = 0.045 + 0.023;
  double W9;
  W9 = 0.044 + 0.016;
  
  double Lmu;
  Lmu = log(muscale/mbeff);


  EvtComplex uniti(0.0,1.0);

  EvtComplex hc;
  double xarg;
  xarg = 4.0*mchat/shat;
  hc = -4.0/9.0*log(mchat*mchat) + 8.0/27.0 + 4.0*xarg/9.0;

if (xarg < 1.0)
  {
    hc = hc - 2.0/9.0*(2.0 + xarg)*sqrt(fabs(1.0 - xarg))*
      (log(fabs((sqrt(1.0 - xarg)+1.0)/(sqrt(1.0 - xarg) - 1.0))) -
       uniti*EvtConst::pi);
  }
  else
  {
    hc = hc - 2.0/9.0*(2.0 + xarg)*sqrt(fabs(1.0 - xarg))*
      2.0*atan(1.0/sqrt(xarg - 1.0));
  }
                                                                                                                                                             
  EvtComplex h1;
  xarg = 4.0/shat;
  h1 = 8.0/27.0 + 4.0*xarg/9.0;
  if (xarg < 1.0)
  {
    h1 = h1 - 2.0/9.0*(2.0 + xarg)*sqrt(fabs(1.0 - xarg))*
      (log(fabs((sqrt(1.0 - xarg)+1.0)/(sqrt(1.0 - xarg) - 1.0))) -
       uniti*EvtConst::pi);
  }
  else
  {
    h1 = h1 - 2.0/9.0*(2.0 + xarg)*sqrt(fabs(1.0 - xarg))*
      2.0*atan(1.0/sqrt(xarg - 1.0));
  }


  EvtComplex h0;
  h0 = 8.0/27.0 - 4.0*log(2.0)/9.0 + 4.0*uniti*EvtConst::pi/9.0;


  // X=V_{ud}^* V_ub / V_{td}^* V_tb * (4/3 C_1 +C_2) * (h(\hat m_c^2, hat s)-
  // h(\hat m_u^2, hat s))
  EvtComplex Vudstar(1.0 - 0.2279*0.2279/2.0, 0.0);
  EvtComplex Vub((0.118+0.273)/2.0, -1.0*(0.305+0.393)/2.0);
  EvtComplex Vtdstar(1.0 - (0.118+0.273)/2.0,(0.305+0.393)/2.0);
  EvtComplex Vtb(1.0,0.0);

  EvtComplex Xd;
  Xd = (Vudstar * Vub / Vtdstar * Vtb) * (4.0/3.0*C1 + C2) * (hc - h0);


  EvtComplex c9eff=4.344;
  if (shat > 0.25)
  { 
   c9eff =  A9 + T9*hc + U9*h1 + W9*h0;
   if (btod)
   {
    c9eff += Xd; 
   }

   return c9eff;
  }

  // change energy scale to 5.0 for full NNLO calculation below shat = 0.25
  muscale = 5.0;
  alphas = 0.215;
  A9 = 4.174 + (-0.035);
  C1 = -0.487;
  C2 = 1.024;
  A8 = -0.148;
  T9 = 0.374 + 0.252;
  U9 = 0.033 + 0.015;
  W9 = 0.032 + 0.012;
  Lmu = log(muscale/mbeff);

  EvtComplex F91;
  EvtComplex f91;
  EvtComplex k9100(-11.973,0.16371);
  EvtComplex k9101(-0.081271,-0.059691);
  EvtComplex k9110(-28.432,-0.25044);
  EvtComplex k9111(-0.040243,0.016442);
  EvtComplex k9120(-57.114,-0.86486);
  EvtComplex k9121(-0.035191,0.027909);
  EvtComplex k9130(-128.8,-2.5243);
  EvtComplex k9131(-0.017587,0.050639);
  f91 = k9100 + k9101*logshat + shat*(k9110 + k9111*logshat) +
        shat*shat*(k9120 + k9121*logshat) + 
        shat*shat*shat*(k9130 + k9131*logshat); 
  F91 = (-1424.0/729.0 + 16.0*uniti*EvtConst::pi/243.0 
         + 64.0/27.0*log(mchat))*Lmu - 16.0*Lmu*logshat/243.0 +
        (16.0/1215.0 - 32.0/135.0/mchat/mchat)*Lmu*shat +
        (4.0/2835.0 - 8.0/315.0/mchat/mchat/mchat/mchat)*Lmu*shat*shat +
    (16.0/76545.0 - 32.0/8505.0/mchat/mchat/mchat/mchat/mchat/mchat)*
    Lmu*shat*shat*shat -256.0*Lmu*Lmu/243.0 + f91;

  EvtComplex F92;
  EvtComplex f92;
  EvtComplex k9200(6.6338,-0.98225);
  EvtComplex k9201(0.48763,0.35815);
  EvtComplex k9210(3.3585,1.5026);
  EvtComplex k9211(0.24146,-0.098649);
  EvtComplex k9220(-1.1906,5.1892);
  EvtComplex k9221(0.21115,-0.16745);
  EvtComplex k9230(-17.12,15.146);
  EvtComplex k9231(0.10552,-0.30383);
  f92 = k9200 + k9201*logshat + shat*(k9210 + k9211*logshat) +
        shat*shat*(k9220 + k9221*logshat) + 
        shat*shat*shat*(k9230 + k9231*logshat); 
  F92 = (256.0/243.0 - 32.0*uniti*EvtConst::pi/81.0 
         - 128.0/9.0*log(mchat))*Lmu + 32.0*Lmu*logshat/81.0 +
        (-32.0/405.0 + 64.0/45.0/mchat/mchat)*Lmu*shat +
        (-8.0/945.0 + 16.0/105.0/mchat/mchat/mchat/mchat)*Lmu*shat*shat +
    (-32.0/25515.0 + 64.0/2835.0/mchat/mchat/mchat/mchat/mchat/mchat)*
    Lmu*shat*shat*shat + 512.0*Lmu*Lmu/81.0 + f92;
  
  EvtComplex F98;
  F98 = 104.0/9.0 - 32.0*EvtConst::pi*EvtConst::pi/27.0 + 
        (1184.0/27.0 - 40.0*EvtConst::pi*EvtConst::pi/9.0)*shat +
        (14212.0/135.0 - 32.0*EvtConst::pi*EvtConst::pi/3.0)*shat*shat +
    (193444.0/945.0 - 560.0*EvtConst::pi*EvtConst::pi/27.0)*shat*shat*shat +
        16.0*logshat/9.0*(1.0 + shat + shat*shat + shat*shat*shat);

  Xd = (Vudstar * Vub / Vtdstar * Vtb) * (4.0/3.0*C1 + C2) * (hc - h0);

  c9eff = A9 + T9*hc + U9*h1 + W9*h0 -             
    alphas/(4.0*EvtConst::pi)*(C1*F91 + C2*F92 + A8*F98);
  if (btod)
  {
   c9eff += Xd; 
  }

  return c9eff;
}

/*
 Calculate C10 coefficient 
 
 C10 is scale (and q^2) independent 
*/
EvtComplex EvtRareLbToLllWC::GetC10Eff( double /*q2*/ ) const 
{
  double A10;
  A10 = -4.592 + 0.379;

  EvtComplex c10eff;
  c10eff = A10;

  return c10eff;
}

//=============================================================================

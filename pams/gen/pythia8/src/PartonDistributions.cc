// PartonDistributions.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the PDF,
// GRV94L, CTEQ5L, LHAPDF  and Lepton classes.

#include "PartonDistributions.h"
#include "LHAPDFInterface.h"

namespace Pythia8 {
 
//**************************************************************************

// Base class for parton distribution functions.

//*********

// Standard parton densities.

double PDF::xf(int id, double x, double Q2) { 

  // Need to update if flavour, x or Q2 changed.
  // Use idSav = 9 to indicate that ALL flavours are up-to-date.
  // Assume that flavour and antiflavour always updated simultaneously.
  if ( (abs(idSav) != abs(id) && idSav != 9) || x != xSav || Q2 != Q2Sav) 
    {idSav = id; xfUpdate(id, x, Q2); xSav = x; Q2Sav = Q2;}

  // Baryon beam.
  if (abs(idBeam) > 100) { 
    int idNow = (idBeam > 0) ? id : -id;
    int idAbs = abs(id);
    if (idNow == 0 || idAbs == 21) return max(0., xg);  
    if (idNow == 1) return max(0., xd);
    if (idNow == -1) return max(0., xdbar);
    if (idNow == 2) return max(0., xu);
    if (idNow == -2) return max(0., xubar);
    if (idAbs == 3) return max(0., xs);
    if (idAbs == 4) return max(0., xc);
    if (idAbs == 5) return max(0., xb);
    return 0.;

  // Lepton beam.
  } else {
    if (id == idBeam ) return max(0., xlepton);
    if (abs(id) == 22) return max(0., xgamma);
    return 0.;
  }
   
}

//*********

// Only valence part of parton densities.

double PDF::xfVal(int id, double x, double Q2) { 

  // Need to update if flavour, x or Q2 changed.
  // Use idSav = 9 to indicate that ALL flavours are up-to-date.
  // Assume that flavour and antiflavour always updated simultaneously.
  if ( (abs(idSav) != abs(id) && idSav != 9) || x != xSav || Q2 != Q2Sav) 
    {idSav = id; xfUpdate(id, x, Q2); xSav = x; Q2Sav = Q2;}

  // Baryon beam.
  if (abs(idBeam) > 100) { 
    int idNow = (idBeam > 0) ? id : -id;
    if (idNow == 1) return max(0., xdVal);
    if (idNow == 2) return max(0., xuVal);
    return 0.;

  // Lepton beam.
  } else {
    if (id == idBeam) return max(0., xlepton);
    return 0.;
  }
   
}

//*********

// Only sea part of parton densities.

double PDF::xfSea(int id, double x, double Q2) { 

  // Need to update if flavour, x or Q2 changed.
  // Use idSav = 9 to indicate that ALL flavours are up-to-date.
  // Assume that flavour and antiflavour always updated simultaneously.
  if ( (abs(idSav) != abs(id) && idSav != 9) || x != xSav || Q2 != Q2Sav) 
    {idSav = id; xfUpdate(id, x, Q2); xSav = x; Q2Sav = Q2;}

  // Baryon beam.
  if (abs(idBeam) > 100) { 
    int idNow = (idBeam > 0) ? id : -id;
    int idAbs = abs(id);
    if (idNow == 0 || idAbs == 21) return max(0., xg);  
    if (idNow == 1) return max(0., xdSea);
    if (idNow == -1) return max(0., xdbar);
    if (idNow == 2) return max(0., xuSea);
    if (idNow == -2) return max(0., xubar);
    if (idAbs == 3) return max(0., xs);
    if (idAbs == 4) return max(0., xc);
    if (idAbs == 5) return max(0., xb);
    if (idAbs == 5) return max(0., xb);
    return 0.;

  // Lepton beam.
  } else {
    if (abs(id) == 22) return max(0., xgamma);
    return 0.;
  }
   
}
 
//**************************************************************************

// Gives the GRV 94 L (leading order) parton distribution function set
// in parametrized form. Authors: M. Glueck, E. Reya and A. Vogt.
// Ref: M. Glueck, E. Reya and A. Vogt, Z.Phys. C67 (1995) 433.

void GRV94L::xfUpdate(int id, double x, double Q2) {
 
  // Common expressions.
  double mu2  = 0.23;
  double lam2 = 0.2322 * 0.2322;
  double s    = log (log(Q2/lam2) / log(mu2/lam2));
  double ds   = sqrt(s);
  double s2   = s * s;
  double s3   = s2 * s;
 
  // uv :
  double nu  =  2.284 + 0.802 * s + 0.055 * s2;
  double aku =  0.590 - 0.024 * s;
  double bku =  0.131 + 0.063 * s;
  double au  = -0.449 - 0.138 * s - 0.076 * s2;
  double bu  =  0.213 + 2.669 * s - 0.728 * s2;
  double cu  =  8.854 - 9.135 * s + 1.979 * s2;
  double du  =  2.997 + 0.753 * s - 0.076 * s2;
  double uv  = grvv (x, nu, aku, bku, au, bu, cu, du);

  // dv :
  double nd  =  0.371 + 0.083 * s + 0.039 * s2;
  double akd =  0.376;
  double bkd =  0.486 + 0.062 * s;
  double ad  = -0.509 + 3.310 * s - 1.248 * s2;
  double bd  =  12.41 - 10.52 * s + 2.267 * s2;
  double cd  =  6.373 - 6.208 * s + 1.418 * s2;
  double dd  =  3.691 + 0.799 * s - 0.071 * s2;
  double dv  = grvv (x, nd, akd, bkd, ad, bd, cd, dd);
  
  // udb :
  double alx =  1.451;
  double bex =  0.271;
  double akx =  0.410 - 0.232 * s;
  double bkx =  0.534 - 0.457 * s;
  double agx =  0.890 - 0.140 * s;
  double bgx = -0.981;
  double cx  =  0.320 + 0.683 * s;
  double dx  =  4.752 + 1.164 * s + 0.286 * s2;
  double ex  =  4.119 + 1.713 * s;
  double esx =  0.682 + 2.978 * s;
  double udb = grvw (x, s, alx, bex, akx, bkx, agx, bgx, cx,
    dx, ex, esx);

  // del :
  double ne  =  0.082 + 0.014 * s + 0.008 * s2;
  double ake =  0.409 - 0.005 * s;
  double bke =  0.799 + 0.071 * s;
  double ae  = -38.07 + 36.13 * s - 0.656 * s2;
  double be  =  90.31 - 74.15 * s + 7.645 * s2;
  double ce  =  0.;
  double de  =  7.486 + 1.217 * s - 0.159 * s2;
  double del = grvv (x, ne, ake, bke, ae, be, ce, de);
 
  // sb :
  double sts =  0.;
  double als =  0.914;
  double bes =  0.577;
  double aks =  1.798 - 0.596 * s;
  double as  = -5.548 + 3.669 * ds - 0.616 * s;
  double bs  =  18.92 - 16.73 * ds + 5.168 * s;
  double dst =  6.379 - 0.350 * s  + 0.142 * s2;
  double est =  3.981 + 1.638 * s;
  double ess =  6.402;
  double sb  = grvs (x, s, sts, als, bes, aks, as, bs, dst, est, ess);
 
  // cb :
  double stc =  0.888;
  double alc =  1.01;
  double bec =  0.37;
  double akc =  0.;
  double ac  =  0.;
  double bc  =  4.24  - 0.804 * s;
  double dct =  3.46  - 1.076 * s;
  double ect =  4.61  + 1.49  * s;
  double esc =  2.555 + 1.961 * s;
  double chm = grvs (x, s, stc, alc, bec, akc, ac, bc, dct, ect, esc);
 
  // bb :
  double stb =  1.351;
  double alb =  1.00;
  double beb =  0.51;
  double akb =  0.;
  double ab  =  0.;
  double bb  =  1.848;
  double dbt =  2.929 + 1.396 * s;
  double ebt =  4.71  + 1.514 * s;
  double esb =  4.02  + 1.239 * s;
  double bot = grvs (x, s, stb, alb, beb, akb, ab, bb, dbt, ebt, esb);
 
  // gl :
  double alg =  0.524;
  double beg =  1.088;
  double akg =  1.742 - 0.930 * s;
  double bkg =                     - 0.399 * s2;
  double ag  =  7.486 - 2.185 * s;
  double bg  =  16.69 - 22.74 * s  + 5.779 * s2;
  double cg  = -25.59 + 29.71 * s  - 7.296 * s2;
  double dg  =  2.792 + 2.215 * s  + 0.422 * s2 - 0.104 * s3;
  double eg  =  0.807 + 2.005 * s;
  double esg =  3.841 + 0.316 * s;
  double gl  = grvw (x, s, alg, beg, akg, bkg, ag, bg, cg,
    dg, eg, esg);

  // Update values
  xg    = gl;
  xu    = uv + 0.5*(udb - del);
  xd    = dv + 0.5*(udb + del); 
  xubar = 0.5*(udb - del); 
  xdbar = 0.5*(udb + del);
  xs    = sb;
  xc    = chm;
  xb    = bot;

  // Subdivision of valence and sea.
  xuVal = uv;
  xuSea = xubar;
  xdVal = dv;
  xdSea = xdbar;

  // idSav = 9 to indicate that all flavours reset. id change dummy. 
  idSav = 9;
  id   = 0;

} 

//*********

double GRV94L::grvv (double x, double n, double ak, double bk, double a, 
   double b, double c, double d) {

  double dx = sqrt(x);
  return n * pow(x, ak) * (1. + a * pow(x, bk) + x * (b + c * dx)) *
    pow(1. - x, d);

} 

//*********

double GRV94L::grvw (double x, double s, double al, double be, double ak, 
  double bk, double a, double b, double c, double d, double e, double es) {
 
  double lx = log(1./x);
  return (pow(x, ak) * (a + x * (b + x * c)) * pow(lx, bk) + pow(s, al)
    * exp(-e + sqrt(es * pow(s, be) * lx))) * pow(1. - x, d);

}

//*********
  
double GRV94L::grvs (double x, double s, double sth, double al, double be, 
  double ak, double ag, double b, double d, double e, double es) {
 
  if(s <= sth) {
    return 0.;
  } else {
    double dx = sqrt(x);
    double lx = log(1./x);
    return pow(s - sth, al) / pow(lx, ak) * (1. + ag * dx + b * x) *
      pow(1. - x, d) * exp(-e + sqrt(es * pow(s, be) * lx));
  }
 
}
 
//**************************************************************************

// Gives the CTEQ 5 L (leading order) parton distribution function set
// in parametrized form. Parametrization by J. Pumplin.
// Ref: CTEQ Collaboration, H.L. Lai et al., Eur.Phys.J. C12 (2000) 375.

// The range of (x, Q) covered by this parametrization of the QCD
// evolved parton distributions is 1E-6 < x < 1, 1.1 GeV < Q < 10 TeV. 
// In the current implementation, densities are frozen at borders.

void CTEQ5L::xfUpdate(int id, double x, double Q2) {

  // Constrain x and Q2 to range for which parametrization is valid.
  double Q = sqrt( max( 1., min( 1e8, Q2) ) );
  x = max( 1e-6, min( 1.-1e-10, x) ); 

  // Derived kinematical quantities.
  double y = - log(x);
  double u = log( x / 0.00001);
  double x1 = 1. - x;
  double x1L = log(1. - x);
  double sumUbarDbar = 0.; 

  // Parameters of parametrizations.
  const double Qmin[8] = { 0., 0., 0., 0., 0., 0., 1.3, 4.5};
  const double alpha[8] = { 0.2987216, 0.3407552, 0.4491863, 0.2457668,
    0.5293999, 0.3713141, 0.03712017, 0.004952010 };
  const double ut1[8] = { 4.971265, 2.612618, -0.4656819, 3.862583,
    0.1895615, 3.753257, 4.400772, 5.562568 };
  const double ut2[8] = { -1.105128, -1.258304e5, -274.2390, -1.265969,
    -3.069097, -1.113085, -1.356116, -1.801317 };
  const double am[8][9][3] = { 
    // d.
    { {  0.5292616E+01, -0.2751910E+01, -0.2488990E+01 },
      {  0.9714424E+00,  0.1011827E-01, -0.1023660E-01 },
      { -0.1651006E+02,  0.7959721E+01,  0.8810563E+01 },
      { -0.1643394E+02,  0.5892854E+01,  0.9348874E+01 },
      {  0.3067422E+02,  0.4235796E+01, -0.5112136E+00 },
      {  0.2352526E+02, -0.5305168E+01, -0.1169174E+02 },
      { -0.1095451E+02,  0.3006577E+01,  0.5638136E+01 },
      { -0.1172251E+02, -0.2183624E+01,  0.4955794E+01 },
      {  0.1662533E-01,  0.7622870E-02, -0.4895887E-03 } },
    // u.
    { {  0.9905300E+00, -0.4502235E+00,  0.1624441E+00 },
      {  0.8867534E+00,  0.1630829E-01, -0.4049085E-01 },
      {  0.8547974E+00,  0.3336301E+00,  0.1371388E+00 },
      {  0.2941113E+00, -0.1527905E+01,  0.2331879E+00 },
      {  0.3384235E+02,  0.3715315E+01,  0.8276930E+00 },
      {  0.6230115E+01,  0.3134639E+01, -0.1729099E+01 },
      { -0.1186928E+01, -0.3282460E+00,  0.1052020E+00 },
      { -0.8545702E+01, -0.6247947E+01,  0.3692561E+01 },
      {  0.1724598E-01,  0.7120465E-02,  0.4003646E-04 } },
    // g.
    { {  0.1193572E+03, -0.3886845E+01, -0.1133965E+01 },
      { -0.9421449E+02,  0.3995885E+01,  0.1607363E+01 },
      {  0.4206383E+01,  0.2485954E+00,  0.2497468E+00 },
      {  0.1210557E+03, -0.3015765E+01, -0.1423651E+01 },
      { -0.1013897E+03, -0.7113478E+00,  0.2621865E+00 },
      { -0.1312404E+01, -0.9297691E+00, -0.1562531E+00 },
      {  0.1627137E+01,  0.4954111E+00, -0.6387009E+00 },
      {  0.1537698E+00, -0.2487878E+00,  0.8305947E+00 },
      {  0.2496448E-01,  0.2457823E-02,  0.8234276E-03 } },
    // ubar + dbar.
    { {  0.2647441E+02,  0.1059277E+02, -0.9176654E+00 },
      {  0.1990636E+01,  0.8558918E-01,  0.4248667E-01 },
      { -0.1476095E+02, -0.3276255E+02,  0.1558110E+01 },
      { -0.2966889E+01, -0.3649037E+02,  0.1195914E+01 },
      { -0.1000519E+03, -0.2464635E+01,  0.1964849E+00 },
      {  0.3718331E+02,  0.4700389E+02, -0.2772142E+01 },
      { -0.1872722E+02, -0.2291189E+02,  0.1089052E+01 },
      { -0.1628146E+02, -0.1823993E+02,  0.2537369E+01 },
      { -0.1156300E+01, -0.1280495E+00,  0.5153245E-01 } },
    // dbar/ubar.
    { { -0.6556775E+00,  0.2490190E+00,  0.3966485E-01 },
      {  0.1305102E+01, -0.1188925E+00, -0.4600870E-02 },
      { -0.2371436E+01,  0.3566814E+00, -0.2834683E+00 },
      { -0.6152826E+01,  0.8339877E+00, -0.7233230E+00 },
      { -0.8346558E+01,  0.2892168E+01,  0.2137099E+00 },
      {  0.1279530E+02,  0.1021114E+00,  0.5787439E+00 },
      {  0.5858816E+00, -0.1940375E+01, -0.4029269E+00 },
      { -0.2795725E+02, -0.5263392E+00,  0.1290229E+01 },
      {  0.0000000E+00,  0.0000000E+00,  0.0000000E+00 } },
    // sbar.
    { {  0.1580931E+01, -0.2273826E+01, -0.1822245E+01 },
      {  0.2702644E+01,  0.6763243E+00,  0.7231586E-02 },
      { -0.1857924E+02,  0.3907500E+01,  0.5850109E+01 }, 
      { -0.3044793E+02,  0.2639332E+01,  0.5566644E+01 },
      { -0.4258011E+01, -0.5429244E+01,  0.4418946E+00 },
      {  0.3465259E+02, -0.5532604E+01, -0.4904153E+01 },
      { -0.1658858E+02,  0.2923275E+01,  0.2266286E+01 },
      { -0.1149263E+02,  0.2877475E+01, -0.7999105E+00 },
      {  0.0000000E+00,  0.0000000E+00,  0.0000000E+00 } },
    // cbar.
    { { -0.8293661E+00, -0.3982375E+01, -0.6494283E-01 },
      {  0.2754618E+01,  0.8338636E+00, -0.6885160E-01 },
      { -0.1657987E+02,  0.1439143E+02, -0.6887240E+00 },
      { -0.2800703E+02,  0.1535966E+02, -0.7377693E+00 },
      { -0.6460216E+01, -0.4783019E+01,  0.4913297E+00 },
      {  0.3141830E+02, -0.3178031E+02,  0.7136013E+01 },
      { -0.1802509E+02,  0.1862163E+02, -0.4632843E+01 },
      { -0.1240412E+02,  0.2565386E+02, -0.1066570E+02 },
      {  0.0000000E+00,  0.0000000E+00,  0.0000000E+00 } },
    // bbar.
    { { -0.6031237E+01,  0.1992727E+01, -0.1076331E+01 },
      {  0.2933912E+01,  0.5839674E+00,  0.7509435E-01 },
      { -0.8284919E+01,  0.1488593E+01, -0.8251678E+00 },
      { -0.1925986E+02,  0.2805753E+01, -0.3015446E+01 },
      { -0.9480483E+01, -0.9767837E+00, -0.1165544E+01 },
      {  0.2193195E+02, -0.1788518E+02,  0.9460908E+01 },
      { -0.1327377E+02,  0.1201754E+02, -0.6277844E+01 },
      {  0.0000000E+00,  0.0000000E+00,  0.0000000E+00 },
      {  0.0000000E+00,  0.0000000E+00,  0.0000000E+00 } } };

  // Loop over 8 different parametrizations. Check if inside allowed region.
  for (int i = 0; i < 8; ++i) {
    double answer = 0.;
    if (Q > max(Qmin[i], alpha[i])) {

      // Evaluate answer.
      double tmp = log(Q / alpha[i]);  
      double sb = log(tmp);   
      double sb1 = sb - 1.2;
      double sb2 = sb1*sb1; 
      double af[9];
      for (int j = 0; j < 9; ++j) 
        af[j] = am[i][j][0] + sb1 * am[i][j][1] + sb2 * am[i][j][2]; 
      double part1 = af[1] * pow( y, 1. + 0.01 * af[4]) * (1. + af[8] * u);
      double part2 = af[0] * x1 + af[3] * x;
      double part3 = x * x1 * (af[5] + af[6] * x1 + af[7] * x * x1);
      double part4 = (ut2[i] < -100.) ? ut1[i] * x1L + af[2] * x1L
                   : ut1[i] * x1L + af[2] * log(x1 + exp(ut2[i]));
      answer       = x * exp( part1 + part2 + part3 + part4);   
      answer      *= 1. - Qmin[i] / Q;
    }

    // Store results. 
    if (i == 0) xd = x * answer;
    else if (i == 1) xu = x * answer;
    else if (i == 2) xg = x * answer;
    else if (i == 3) sumUbarDbar = x * answer;
    else if (i == 4) { xubar = sumUbarDbar / (1. + answer);
      xdbar = sumUbarDbar * answer / (1. + answer); }
    else if (i == 5) xs = x * answer;
    else if (i == 6) xc = x * answer;
    else if (i == 7) xb = x * answer;
  }

  // Subdivision of valence and sea.
  xuVal = xu - xubar;
  xuSea = xubar;
  xdVal = xd - xdbar;
  xdSea = xdbar;

  // idSav = 9 to indicate that all flavours reset. id change is dummy here. 
  idSav = 9;
  id    = 0;

}
 
//**************************************************************************

// Interface to the LHAPDF library.

//*********
 
// Definitions of static variables.

string LHAPDF::latestSetName = " ";
int    LHAPDF::latestMember  = -1;
int    LHAPDF::latestNSet    = 0;

//*********

// Initialize a parton density function from LHAPDF.

void LHAPDF::init(string setName, int member, Info* infoPtr) {

  // If already initialized then need not do anything.
  if (setName == latestSetName && member == latestMember 
    && nSet == latestNSet) return;

  // Initialize set. If first character is '/' then assume that name 
  // is given with path, else not.
  if (setName[0] == '/') LHAPDFInterface::initPDFsetM( nSet, setName);
  else LHAPDFInterface::initPDFsetByNameM( nSet, setName);

  // Check that not dummy library was linked and put nSet negative.
  isSet = (nSet >= 0); 
  if (!isSet) {
    if (infoPtr > 0) infoPtr->errorMsg("Error from LHAPDF::init: "
      "you try to use LHAPDF but did not link it");  
    else cout << "Error from LHAPDF::init: you try to use LHAPDF "
      << "but did not link it" << endl;  
  }

  // Initialize member.
  LHAPDFInterface::initPDFM(nSet, member);

  // Do not collect statistics on under/overflow to save time and space.
   LHAPDFInterface::setPDFparm( "NOSTAT" );
   LHAPDFInterface::setPDFparm( "LOWKEY" );

  // Save values to avoid unnecessary reinitializations.
  latestSetName = setName;
  latestMember  = member;
  latestNSet    = nSet;

}

//*********

// Allow optional extrapolation beyond boundaries.

void LHAPDF::setExtrapolate(bool extrapol) {

   LHAPDFInterface::setPDFparm( (extrapol) ? "EXTRAPOLATE" : "18" );

}

//*********

// Give the parton distribution function set from LHAPDF.

void LHAPDF::xfUpdate(int , double x, double Q2) {

  // Let LHAPDF do the evaluation of parton densities.
  double Q = sqrt( max( 0., Q2));
  LHAPDFInterface::evolvePDFM( nSet, x, Q, xfArray);

  // Update values.
  xg    = xfArray[6];
  xu    = xfArray[8]; 
  xd    = xfArray[7]; 
  xubar = xfArray[4]; 
  xdbar = xfArray[5]; 
  xs    = xfArray[9]; 
  xc    = xfArray[10]; 
  xb    = xfArray[11];

  // Subdivision of valence and sea.
  xuVal = xu - xubar; 
  xuSea = xubar; 
  xdVal = xd - xdbar; 
  xdSea = xdbar;

  // idSav = 9 to indicate that all flavours reset. 
  idSav = 9; 

}
 
//**************************************************************************

// Gives electron (or muon, or tau) parton distribution.

// Constant. alphaEM(0) could be taken from settings but safer this way.
const double Lepton::ALPHAEM = 0.00729735;
 
void Lepton::xfUpdate(int id, double x, double Q2) {
 
  // Squared mass of lepton species: electron, muon, tau. 
  if (!isInit) {
    m2Lep = pow2( ParticleDataTable::m0(idBeam) );
    isInit = true;
  }

  // Electron inside electron, see R. Kleiss et al., in Z physics at
  // LEP 1, CERN 89-08, p. 34
  double xLog = log(max(1e-10,x));
  double xMinusLog = log( max(1e-10, 1. - x) );
  double Q2Log = log( max(3., Q2/m2Lep) );
  double beta = (ALPHAEM / M_PI) * (Q2Log - 1.);
  double delta = 1. + (ALPHAEM / M_PI) * (1.5 * Q2Log + 1.289868) 
    + pow2(ALPHAEM / M_PI) * (-2.164868 * Q2Log*Q2Log 
    + 9.840808 * Q2Log - 10.130464);
  double fPrel =  beta * pow(1. - x, beta - 1.) * sqrtpos( delta )
     - 0.5 * beta * (1. + x) + 0.125 * beta*beta * ( (1. + x) 
     * (-4. * xMinusLog + 3. * xLog) - 4. * xLog / (1. - x) - 5. - x); 

  // Zero distribution for very large x and rescale it for intermediate.
  if (x > 1. - 1e-10) fPrel = 0.;
  else if (x > 1. - 1e-7) fPrel *= pow(1000.,beta) / (pow(1000.,beta) - 1.); 
  xlepton = x * fPrel; 

  // Photon inside electron (one possible scheme - primitive).
  xgamma = (0.5 * ALPHAEM / M_PI) * Q2Log * (1. + pow2(1. - x));

  // idSav = 9 to indicate that all flavours reset. id change is dummy here. 
  idSav = 9;
  id    = 0;

}
 
//**************************************************************************

} // end namespace Pythia8

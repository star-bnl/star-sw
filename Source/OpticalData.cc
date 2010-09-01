#include <iostream>
#include <iomanip>
#include <cmath>

#include "OpticalData.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

OpticalData::OpticalData() :
  emin(0.), emax(0.),
  ionmin(0.), ionmax(0.), 
  debug(false) {
  
}

OpticalData::~OpticalData() {

}

bool
OpticalData::IsAvailable(const std::string material) const {

  if (material == "Ar") return true;
  
  if (material == "CH4") return true;
  if (material == "C2H2") return true;
  if (material == "C2H4") return true;
  if (material == "C2H6") return true;
  if (material == "iC4H10") return true;
  
  return false;
  
}  

bool
OpticalData::GetPhotoabsorptionCrossSection(const std::string material,
                                            const double e, 
                                            double& cs, double& eta) {

  if (material == "Ar") return PhotoAbsorptionCsArgon(e, cs, eta);
  if (material == "CH4") return PhotoAbsorptionCsMethane(e, cs, eta);
  if (material == "C2H2") return PhotoAbsorptionCsAcetylene(e, cs, eta);
  // if (material == "C2H4") return PhotoAbsorptionCsEthylene(e, cs, eta);
  // if (material == "C2H6") return PhotoAbsorptionCsEthane(e, cs, eta);
  // 
 
  return false;

}

bool
OpticalData::PhotoAbsorptionCsArgon(const double e, 
                                    double& cs, double& eta) {

  // Sources:
  // J. Berkowitz, Atomic and Molecular Photoabsorption (2002)
  // N. Sakamoto et al., NIFS-DATA 109
  
  const double ip32 = 15.7596103;
  const double ip12 = 15.9371039;
  
  if (e < ip32) {
    cs = eta = 0.;
    return true;
  } else if (e < ip12) {
    // Continuum contribution between IP 3/2 and IP 1/2
    cs = 20.75e-18;
    eta = 1.;
    return true;
  }
  
  if (e >= 243. && e <= 336.) {
    // L23 edge
    const int nEntries = 130;
    const double xAr[nEntries] = {
      2.4300e02, 2.4400e02, 2.4450e02, 2.4500e02, 2.4550e02, 
      2.4600e02, 2.4650e02, 2.4700e02, 2.4750e02, 2.4800e02,
      2.4850e02, 2.4900e02, 2.4950e02, 2.5000e02, 2.5050e02,
      2.5100e02, 2.5150e02, 2.5200e02, 2.5250e02, 2.5300e02,
      2.5350e02, 2.5400e02, 2.5450e02, 2.5500e02, 2.5550e02,
      2.5600e02, 2.5650e02, 2.5700e02, 2.5750e02, 2.5800e02,
      2.5850e02, 2.5900e02, 2.5950e02, 2.6000e02, 2.6050e02,
      2.6100e02, 2.6150e02, 2.6200e02, 2.6250e02, 2.6300e02,
      2.6350e02, 2.6400e02, 2.6450e02, 2.6500e02, 2.6550e02,
      2.6600e02, 2.6650e02, 2.6700e02, 2.6750e02, 2.6800e02,
      2.6850e02, 2.6900e02, 2.6950e02, 2.7000e02, 2.7050e02,
      2.7100e02, 2.7150e02, 2.7200e02, 2.7250e02, 2.7300e02,
      2.7350e02, 2.7400e02, 2.7450e02, 2.7500e02, 2.7550e02,
      2.7600e02, 2.7650e02, 2.7700e02, 2.7750e02, 2.7800e02,
      2.7850e02, 2.7900e02, 2.7950e02, 2.8000e02, 2.8100e02,
      2.8200e02, 2.8300e02, 2.8400e02, 2.8500e02, 2.8600e02,
      2.8700e02, 2.8800e02, 2.8900e02, 2.9000e02, 2.9100e02,
      2.9200e02, 2.9300e02, 2.9400e02, 2.9500e02, 2.9600e02,
      2.9700e02, 2.9800e02, 2.9900e02, 3.0000e02, 3.0100e02,
      3.0200e02, 3.0300e02, 3.0400e02, 3.0500e02, 3.0600e02,
      3.0700e02, 3.0800e02, 3.0900e02, 3.1000e02, 3.1100e02,
      3.1200e02, 3.1300e02, 3.1400e02, 3.1500e02, 3.1600e02,
      3.1700e02, 3.1800e02, 3.1900e02, 3.2000e02, 3.2100e02,
      3.2200e02, 3.2300e02, 3.2400e02, 3.2500e02, 3.2600e02,
      3.2700e02, 3.2800e02, 3.2900e02, 3.3000e02, 3.3100e02,
      3.3200e02, 3.3300e02, 3.3400e02, 3.3500e02, 3.3600e02};
    
    const double yAr[nEntries] = {
      4.1053e-01, 5.7040e-01, 8.3784e-01, 7.6382e-01, 5.8520e-01,
      6.0099e-01, 1.0352,     1.5938,     1.8138,     1.9737,
      2.2698,     2.6546,     3.0888,     3.3750,     3.5428,
      3.7204,     3.8092,     3.8882,     3.8882,     3.9178,
      4.3657,     4.3323,     4.3212,     4.2324,     4.1990,
      4.1768,     4.0991,     4.0546,     4.0324,     3.9769,
      3.9658,     3.9435,     3.8658,     3.8769,     3.8102,
      3.7769,     3.8213,     3.7658,     3.7547,     3.7325,
      3.7214,     2.7106,     2.6945,     2.7106,     2.6945,
      2.6945,     2.7026,     2.7026,     2.7106,     2.6945,
      2.6864,     2.6380,     2.5977,     2.5735,     2.5332,
      3.3806,     3.3697,     3.3697,     3.3367,     3.3258,
      3.3148,     3.3148,     3.3038,     3.2819,     3.2489,
      3.2599,     3.2489,     3.2489,     3.1940,     3.2050,
      3.1940,     3.1831,     3.2050,     3.1721,     3.1611,
      3.1501,     3.1282,     3.1501,     3.1172,     3.1282,
      3.1062,     3.1062,     3.1062,     3.0843,     3.0623,
      3.0623,     3.0404,     3.0294,     3.0075,     3.0294,
      3.0184,     3.0075,     2.9745,     2.9745,     2.9965,
      2.9635,     2.9306,     2.9306,     2.9306,     2.9196, 
      2.9306,     2.8977,     2.8757,     2.8867,     2.8867,
      2.8757,     2.8757,     2.8867,     2.8428,     2.8209,
      2.8428,     2.8428,     2.8209,     2.8428,     2.8428,
      2.8538,     2.9196,     2.9635,     3.0075,     3.0075,
      2.9635,     3.0075,     2.9526,     2.9526,     2.9087,
      2.8648,     2.8977,     2.8757,     2.8648,     2.8318};
    
    int iLow = 0;
    int iUp = nEntries - 1;
    int iM;
    while (iUp - iLow > 1) {
      iM = (iUp + iLow) >> 1;
      if (e >= xAr[iM]) {
        iLow = iM;
      } else {
        iUp = iM;
      }
    }

    // Linear interpolation.
    cs = yAr[iLow] + (e - xAr[iLow]) * 
         (yAr[iUp] - yAr[iLow]) / (xAr[iUp] - xAr[iLow]);
    // Convert from Mbarn to cm2.
    cs *= 1.e-18;
    eta = 1.;
    return true;
  }
  
  double a, b, c, d;
  if (e < 29.2395) {
    a =  -25.4281;
    b =  170.7881;
    c = -247.886;
    d =  106.5586;
  } else if (e < 48.) {
    a =   76.97689;
    b = -573.622;
    c = 1358.922;
    d = -976.888;
  } else if (e < 79.3) {
    a =   14.43074;
    b =  -40.8325;
    c = -115.985;
    d =  347.5945;
  } else if (e < 243.) {
    a =     5.617571;
    b =   128.2189;
    c = -1203.47;
    d =  2660.151;
  } else if (e < 500.) {
    a =      -11.8768;
    b =     8371.694;
    c =  -109963;
    d = 0.;
  } else if (e < 929.7) {
    a =       35.65584;
    b =     4922.702;
    c =     8315.576;
    d = -1757750;
  } else if (e < 3206.) {
    a =       20.59692;
    b =     6151.107;
    c =    -2513.47;
    d = -2337467;
  } else if (e < 6199.3) {
    a =       -1004.53;
    b =      954912.7;
    c =  -220652027;
    d = 17883565552;
  } else {
    a =         12.26308;
    b =     132886.9;
    c =   -5911229;
    d = -624237063;
  }
  
  const double y = ip12 / e;
  const double f = a * pow(y, 2) + b * pow(y, 3) + 
                   c * pow(y, 4) + d * pow(y, 5);
  // Conversion from oscillator strength to photoabsorption cs
  cs = 8.067283e-18 * f;
  eta = 1.;
  return true;
  
}

bool 
OpticalData::PhotoAbsorptionCsMethane(const double e, 
                                      double& cs, double& eta) {

  // Sources:
  // Photoabsorption cross-section:
  // J. Berkowitz, Atomic and Molecular Photoabsorption (2002)
  // N. Sakamoto et al., NIFS-DATA 109
  // Photoionization yield:
  // K. Kameta et al., J. El. Spectr. Rel. Phen. 123 (2002), 225-238

  
  if (e < 8.61) {
    cs = eta = 0.;
    return true;
  }
  
  if (e < 150.) {
    
    // Photoabsorption cross-section
    const int nPacsEntries = 134;
    
    const double xCH4[nPacsEntries] = {
      8.6100,    9.0000,    9.2130,    9.5000,    9.6900,
      1.0050e01, 1.0425e01, 1.0700e01, 1.0913e01, 1.1270e01,
      1.1500e01, 1.1713e01, 1.1900e01, 1.2125e01, 1.2375e01,
      1.2610e01, 1.3000e01, 1.3325e01, 1.3620e01, 1.4000e01,
      1.4500e01, 1.5000e01, 1.5500e01, 1.6000e01, 1.6500e01,
      1.7000e01, 1.7500e01, 1.8000e01, 1.8500e01, 1.9000e01,
      1.9500e01, 2.0000e01, 2.0500e01, 2.1000e01, 2.1500e01,
      2.2000e01, 2.2500e01, 2.3000e01, 2.3500e01, 2.4000e01,
      2.4500e01, 2.5000e01, 2.5500e01, 2.6000e01, 2.6500e01,
      2.7000e01, 2.7500e01, 2.8000e01, 2.8500e01, 2.9000e01,
      2.9500e01, 3.0000e01, 3.0500e01, 3.1000e01, 3.1500e01,
      3.2000e01, 3.2500e01, 3.3000e01, 3.3500e01, 3.4000e01,
      3.4500e01, 3.5000e01, 3.5500e01, 3.6000e01, 3.6500e01,
      3.7000e01, 3.7500e01, 3.8000e01, 3.8500e01, 3.9000e01,
      3.9500e01, 4.0000e01, 4.1000e01, 4.2000e01, 4.3000e01,
      4.4000e01, 4.5000e01, 4.6000e01, 4.7000e01, 4.8000e01,
      4.9000e01, 5.0000e01, 5.1000e01, 5.2000e01, 5.3000e01,
      5.4000e01, 5.5000e01, 5.6000e01, 5.7000e01, 5.8000e01, 
      5.9000e01, 6.0000e01, 6.1000e01, 6.2000e01, 6.3000e01,
      6.4000e01, 6.5000e01, 6.6000e01, 6.7000e01, 6.8000e01,
      6.9000e01, 7.0000e01, 7.1000e01, 7.2000e01, 7.3000e01, 
      7.4000e01, 7.5000e01, 7.6000e01, 7.7000e01, 7.8000e01, 
      7.9000e01, 8.0000e01, 8.2000e01, 8.4000e01, 8.6000e01, 
      8.8000e01, 9.0000e01, 9.2000e01, 9.4000e01, 9.6000e01, 
      9.8000e01, 1.0000e02, 1.0200e02, 1.0400e02, 1.0600e02, 
      1.0800e02, 1.1000e02, 1.1200e02, 1.1270e02, 1.1900e02, 
      1.2500e02, 1.3291e02, 1.4286e02, 1.5000e02};

    const double yCH4[nPacsEntries] = {
      0.,        3.9896,    9.2248,    1.6727e01, 1.8644e01,
      1.7399e01, 1.9316e01, 1.8119e01, 1.6679e01, 2.0792e01,
      2.8706e01, 3.0086e01, 2.9689e01, 3.1516e01, 3.3929e01,
      3.9598e01, 4.5208e01, 4.8369e01, 4.9053e01, 4.9090e01,
      4.8742e01, 4.8255e01, 4.7111e01, 4.5877e01, 4.4643e01,
      4.3260e01, 4.1429e01, 3.9947e01, 3.8415e01, 3.6982e01,
      3.5649e01, 3.4355e01, 3.2674e01, 3.1480e01, 3.0296e01,
      2.9152e01, 2.7719e01, 2.6535e01, 2.5441e01, 2.4306e01,
      2.3001e01, 2.1999e01, 2.1576e01, 2.0181e01, 1.9134e01,
      1.8481e01, 1.7642e01, 1.6825e01, 1.5854e01, 1.5364e01,
      1.4885e01, 1.4252e01, 1.3609e01, 1.3217e01, 1.2200e01,
      1.1381e01, 1.0879e01, 1.0716e01, 1.0269e01, 1.0040e01,
      9.5267,    9.0567,    8.8718,    8.4345,    8.1288,
      7.9107,    7.6596,    7.4084,    6.9486,    6.9827, 
      6.6434,    6.4139,    6.0315,    5.6927,    5.2109,
      4.9047,    4.6753,    4.3687,    4.1610,    3.9201, 
      3.8223,    3.5040,    3.3288,    3.1755,    3.0000, 
      2.8907,    2.6709,    2.4951,    2.4076,    2.3200, 
      2.1771,    2.0452,    2.0237,    1.9248,    1.8701, 
      1.7600,    1.6499,    1.6283,    1.5623,    1.4520,
      1.4303,    1.3975,    1.3868,    1.2652,    1.2101,
      1.1772,    1.1220,    1.0890,    1.0449,    1.0230,    
      9.8999e-1, 9.4580e-1, 9.0188e-1, 8.2448e-1, 7.8041e-1, 
      7.3630e-1, 6.9212e-1, 6.4789e-1, 6.2595e-1, 5.8161e-1, 
      5.5961e-1, 5.2637e-1, 5.1550e-1, 4.8219e-1, 4.7128e-1, 
      4.4913e-1, 4.2695e-1, 4.0474e-1, 3.8734e-1, 3.4841e-1, 
      3.0851e-1, 2.7834e-1, 2.3357e-1, 2.0263e-1};
    

    // Photoionization yield
    const int nYieldEntries = 11;
    const double xIon[nYieldEntries] = {
      12.65, 12.87, 13.02, 13.24, 13.88, 
      14.66, 15.01, 15.44, 15.67, 15.87, 
      16.22};
                 
    const double yIon[nYieldEntries] = {
      0.,     0.01226, 0.02900, 0.08853, 0.3693, 
      0.7024, 0.8190,  0.9262,  0.9715,  0.9883, 
      1.};
      
    // Locate the requested energy in the tables.
    // First the photoabsorption cross-section.
    int iLow = 0;
    int iUp = nPacsEntries - 1;
    int iM;
    while (iUp - iLow > 1) {
      iM = (iUp + iLow) >> 1;
      if (e >= xCH4[iM]) {
        iLow = iM;
      } else {
        iUp = iM;
      }
    }

    // Linear interpolation.
    cs = yCH4[iLow] + (e - xCH4[iLow]) * 
         (yCH4[iUp] - yCH4[iLow]) / (xCH4[iUp] - xCH4[iLow]);
    // Convert from Mbarn to cm2.
    cs *= 1.e-18;
    
    if (e < xIon[0]) {
      eta = 0.;
    } else if (e >= xIon[nYieldEntries - 1]) {
      eta = 1.;
    } else {
      // Linear interpolation.
      // Same procedure as for photoabsorption cross-section.
      iLow = 0;
      iUp = nYieldEntries - 1;
      while (iUp - iLow > 1) {
        iM = (iUp + iLow) >> 1;
        if (e >= xIon[iM]) {
          iLow = iM;
        } else {
          iUp = iM;
        }
      }
      eta = yIon[iLow] + (e - xIon[iLow]) * 
            (yIon[iUp] - yIon[iLow]) / (xIon[iUp] - xIon[iLow]);
    }
    
    return true;
    
  }
  
  if (e >= 285. && e <= 340.) {
    // Carbon K edge
  
    const int nEntries = 134;
    
    const double xCH4[nEntries] = {
      2.8500e02, 2.8591e02, 2.8640e02, 2.8671e02, 2.8695e02, 
      2.8701e02, 2.8704e02, 2.8708e02, 2.8712e02, 2.8716e02, 
      2.8725e02, 2.8734e02, 2.8739e02, 2.8759e02, 2.8768e02, 
      2.8776e02, 2.8781e02, 2.8786e02, 2.8790e02, 2.8793e02, 
      2.8795e02, 2.8798e02, 2.8801e02, 2.8803e02, 2.8805e02, 
      2.8808e02, 2.8810e02, 2.8812e02, 2.8815e02, 2.8817e02, 
      2.8823e02, 2.8827e02, 2.8832e02, 2.8836e02, 2.8839e02, 
      2.8843e02, 2.8849e02, 2.8853e02, 2.8855e02, 2.8860e02, 
      2.8865e02, 2.8868e02, 2.8876e02, 2.8881e02, 2.8883e02, 
      2.8888e02, 2.8894e02, 2.8901e02, 2.8908e02, 2.8912e02, 
      2.8920e02, 2.8926e02, 2.8933e02, 2.8936e02, 2.8941e02, 
      2.8944e02, 2.8949e02, 2.8952e02, 2.8956e02, 2.8958e02, 
      2.8964e02, 2.8968e02, 2.8971e02, 2.8975e02, 2.8979e02, 
      2.8984e02, 2.8989e02, 2.8993e02, 2.8996e02, 2.9003e02,     
      2.9011e02, 2.9015e02, 2.9018e02, 2.9022e02, 2.9026e02,  
      2.9029e02, 2.9034e02, 2.9037e02, 2.9048e02, 2.9054e02,
      2.9063e02, 2.9071e02, 2.9076e02, 2.9085e02, 2.9103e02,
      2.9126e02, 2.9150e02, 2.9223e02, 2.9366e02, 2.9590e02, 
      2.9776e02, 3.0025e02, 3.0224e02, 3.0342e02, 3.0479e02,
      3.0628e02, 3.0821e02, 3.0908e02, 3.1120e02, 3.1275e02,
      3.1580e02, 3.1934e02, 3.2451e02, 3.2961e02, 3.3495e02, 
      3.4000e02};
     
    const double yCH4[nEntries] = {
      4.1692e-02, 8.4191e-02, 1.5756e-01, 3.8643e-01, 5.0083e-01, 
      1.0526,     1.2561,     1.0539,     7.0664e-01, 4.1755e-01, 
      3.3202e-01, 3.6272e-01, 4.7949e-01, 4.5392e-01, 5.7151e-01, 
      7.7579e-01, 1.1537,     1.8502,     3.1844,     6.1130,
      1.0201e01,  1.4028e01,  1.2318e01,  9.9124,     7.4197,
      5.5937,     4.4055,     4.1161,     4.4645,     4.5808,
      4.2918,     4.3506,     4.2065,     4.4100,     4.6715,
      3.9185,     2.6439,     2.1228,     2.0652,     2.3849,
      3.3134,     3.6909,     2.8225,     2.3885,     2.2150,
      1.7230,     1.2312,     1.0585,     1.2047,     1.3214,
      1.1487,     1.0338,     1.1799,     1.5574,     2.9208,
      3.4142,     2.5164,     1.7921,     1.5609,     1.7352,
      2.2580,     1.9689,     1.5056,     1.3613,     1.5649,
      2.0586,     2.0595,     2.4080,     2.5825,     1.9750,
      1.3964,     1.2813,     1.3977,     1.8333,     1.8629,     
      1.8054,     2.0963,     2.1838,     1.6639,     1.5200,    
      1.7824,     1.7258,     1.8138,     1.6993,     1.7025,     
      1.6774,     1.6526,     1.5320,     1.4318,     1.3409,     
      1.2958,     1.2874,     1.2789,     1.2609,     1.1881,     
      1.1245,     1.1068,     1.0796,     1.0711,     1.0258,     
      9.9013e-01, 9.4546e-01, 8.6468e-01, 8.0217e-01, 7.3974e-01,
      6.9461e-01};
    
    // Linear interpolation.
    int iLow = 0;
    int iUp = nEntries - 1;
    int iM;
    while (iUp - iLow > 1) {
      iM = (iUp + iLow) >> 1;
      if (e >= xCH4[iM]) {
        iLow = iM;
      } else {
        iUp = iM;
      }
    }

    cs = yCH4[iLow] + (e - xCH4[iLow]) * 
         (yCH4[iUp] - yCH4[iLow]) / (xCH4[iUp] - xCH4[iLow]);
    // Convert from Mbarn to cm2.
    cs *= 1.e-18;
    eta = 1.;
    return true;
        
  }
  
  double a, b, c, d;
  if (e < 285.) {
    a =    -4.03133;
    b =   261.0982;
    c = -3005.43;
    d = 11572.96;
  } else if (e < 1740.) {
    a =    -13.0225;
    b =   4303.263;
    c = -77622.4;
    d = 446724.9;
  } else {
    a =      -1.11677;
    b =    3478.699;
    c =  -48076.5;
    d = -364234;
  }
  
  const double y = 12.61 / e;
  const double f = a * pow(y, 2) + b * pow(y, 3) + 
                   c * pow(y, 4) + d * pow(y, 5);
  // Conversion from oscillator strength to photoabsorption cs
  cs = 8.067283e-18 * f;
  eta = 1.;
  return true;
  
}

bool 
OpticalData::PhotoAbsorptionCsAcetylene(const double e, 
                                        double& cs, double& eta) {

  // Sources:

  // Photoabsorption cross-section:
  // J. Berkowitz, Atomic and Molecular Photoabsorption (2002)
  // N. Sakamoto et al., NIFS-DATA 109
  // Photoionization yield:
  // M. Ukai et al., J. Chem. Phys. 95 (1991), 4142-4153
  
  
  if (e < 62.) {
    
    // Photoabsorption cross-section
    const int nPacsEntries = 4;
    
    const double xC2H2[nPacsEntries] = {
      1.2500e02, 1.3291e02, 1.4286e02, 1.5000e02};

    const double yC2H2[nPacsEntries] = {
      3.0851e-1, 2.7834e-1, 2.3357e-1, 2.0263e-1};
    
    // Photoionization yield
    const int nYieldEntries = 40;
    double xIon[nYieldEntries] = {
      11.05, 11.06, 11.38, 11.56, 11.99, 12.56, 12.88, 13.23, 13.65, 13.82,
      14.13, 14.35, 14.49, 14.63, 14.84, 15.06, 15.23, 15.37, 15.48, 15.51,
      15.72, 15.83, 16.00, 16.21, 16.56, 16.67, 16.74, 16.95, 17.13, 17.27,
      17.41, 17.62, 17.87, 18.33, 18.68, 19.00, 19.35, 19.67, 19.98, 20.16
    };
                 
    double yIon[nYieldEntries] = {
      0.009, 0.153, 0.290, 0.486, 0.700, 0.794, 0.794, 0.780, 0.716, 0.678, 
      0.632, 0.662, 0.702, 0.750, 0.783, 0.826, 0.817, 0.814, 0.799, 0.789,
      0.796, 0.780, 0.769, 0.751, 0.766, 0.787, 0.812, 0.841, 0.883, 0.896,
      0.910, 0.924, 0.940, 0.940, 0.949, 0.967, 0.981, 0.988, 0.997, 1.
    };
    
      
    // Locate the requested energy in the tables.
    // First the photoabsorption cross-section.
    int iLow = 0;
    int iUp = nPacsEntries - 1;
    int iM;
    while (iUp - iLow > 1) {
      iM = (iUp + iLow) >> 1;
      if (e >= xC2H2[iM]) {
        iLow = iM;
      } else {
        iUp = iM;
      }
    }

    // Linear interpolation.
    cs = yC2H2[iLow] + (e - xC2H2[iLow]) * 
         (yC2H2[iUp] - yC2H2[iLow]) / (xC2H2[iUp] - xC2H2[iLow]);
    // Convert from Mbarn to cm2.
    cs *= 1.e-18;
    
    if (e < xIon[0]) {
      eta = 0.;
    } else if (e >= xIon[nYieldEntries - 1]) {
      eta = 1.;
    } else {
      // Linear interpolation.
      // Same procedure as for photoabsorption cross-section.
      iLow = 0;
      iUp = nYieldEntries - 1;
      while (iUp - iLow > 1) {
        iM = (iUp + iLow) >> 1;
        if (e >= xIon[iM]) {
          iLow = iM;
        } else {
          iUp = iM;
        }
      }
      eta = yIon[iLow] + (e - xIon[iLow]) * 
            (yIon[iUp] - yIon[iLow]) / (xIon[iUp] - xIon[iLow]);
    }
    
    return true;
    
  }
  
  eta = 1.;
  return true;

}

void
OpticalData::Isobutane() {

  const int nEntries = 32;

  const double xiso[nEntries] = {
    235, 258, 271, 278, 292, 302, 310, 318, 330, 344,
    355, 362, 369, 382, 391, 402, 411, 420, 430, 438,
    448, 457, 468, 481, 491, 506, 514, 524, 540, 556,
    578, 596 
  };

  const double yiso[nEntries] = {
    451, 428, 411, 398, 390, 382, 367, 341, 312, 283,
    268, 260, 261, 271, 274, 268, 251, 231, 211, 204,
    209, 214, 209, 194, 176, 170, 156, 143, 129, 121,
    110, 107
  };

  energy.clear(); energy.resize(nEntries);
  eps1.clear(); eps1.resize(nEntries);
  eps2.clear(); eps2.resize(nEntries);
  pacs.clear(); pacs.resize(nEntries);

  for (int i = nEntries; i--;) {
    energy[i] = 0.1240 * (50. + (xiso[i] - 80.) * 36. / (606. - 80.));
    pacs[i] = 1.e-18 * (451. - yiso[i]) * 129.1 / (451. - 203.);
  }
  
  emin = energy[0];
  emax = energy.back();

  energyIon.clear();
  yieldIon.clear();
  ionmin = 10.67;
  ionmax = 10.66;


}

}


#include <iostream>
#include <iomanip>

#include "OpticalData.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

bool
OpticalData::SetMaterial(std::string material) {

  if (material == "Ar") {
    Argon();
  } else if (material == "CH4") {
    Methane();
  } else if (material == "C2H2") {
    Acetylene();
  } else if (material == "iC4H10") {
    Isobutane();
  } else {
    std::cerr << "OpticalData::SetMaterial:\n";
    std::cerr << "    No data for material " << material 
              << " available.\n";
    return false;
  }

  if (debug) {
    std::cout << "OpticalData::SetMaterial:\n";
    std::cout << "    Energy [eV]    PACS [cm2]\n";
    const int nEntries = energy.size();
    for (int j = 0; j < nEntries; ++j) {
      std::cout << energy[j] << "    " << pacs[j] << "\n";
    }
  }
  hasData = true;
  return true;

}

bool
OpticalData::GetPhotoabsorptionCrossSection(const double e, double& cs) {

  if (!hasData) {
    std::cerr << "OpticalData::GetPhotoabsorptionCrossSection:\n";
    std::cerr << "    No material set.\n";
    cs = 0.;
    return false;
  }

  if (e < emin || e > emax) {
    if (debug) {
      std::cerr << "OpticalData::GetPhotoabsorptionCrossSection:\n";
      std::cerr << "    Requested energy is outside the range.\n";
      std::cerr << "    " << emin << " < E [eV] < " << emax << "\n";
    }
    cs = 0.;
    return false;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = energy.size() - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (e >= energy[iM]) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }

  // Linear interpolation
  cs = pacs[iLow] + (e - energy[iLow]) * 
      (pacs[iUp] - pacs[iLow]) / (energy[iUp] - energy[iLow]);
  return true;

}

bool
OpticalData::GetPhotoionisationYield(const double e, double& eta) {

  if (!hasData) return false;
  if (e <= ionmin) {
    eta = 0.; 
    return true;
  }
  if (e >= ionmax) {
    eta = 1.;
    return true;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = energyIon.size() - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (e >= energyIon[iM]) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }

  // Linear interpolation
  eta = yieldIon[iLow] + (e - energyIon[iLow]) * 
      (yieldIon[iUp] - yieldIon[iLow]) / (energyIon[iUp] - energyIon[iLow]);
  return true;

}

void
OpticalData::Argon() {

  // const int nEntries1 = 92;
  const int nEntries1 = 0;
  /*
  // Detailed measurement
  const double xar1[nEntries1] = {  
     91,  98, 103, 104, 105, 106, 107, 110, 112, 113,
    114, 116, 119, 123, 125, 127, 128, 129, 130, 132,
    133, 134, 135, 136, 137, 139, 147, 363, 367, 369,
    371, 374, 375, 377, 380, 381, 383, 387, 389, 390,
    391, 393, 395, 396, 397, 398, 401, 409, 444, 447,
    448, 449, 451, 452, 454, 455, 456, 460, 464, 465,
    465, 466, 467, 469, 470, 471, 472, 478, 481, 484,
    485, 486, 488, 489, 491, 496, 501, 502, 503, 505,
    507, 509, 511, 517, 520, 521, 524, 527, 531, 535,
    539, 540
  };

  const double yar1[nEntries1] = {
    536, 536, 532, 520, 505, 483, 458, 449, 463, 488,
    511, 522, 534, 531, 518, 477, 421, 323, 207, 159,
    233, 351, 439, 483, 509, 526, 536, 536, 521, 503,
    495, 489, 463, 414, 465, 513, 523, 513, 494, 476,
    442, 395, 423, 463, 497, 514, 530, 536, 536, 521,
    509, 488, 465, 455, 490, 509, 527, 532, 525, 517,
    503, 491, 487, 493, 503, 516, 527, 531, 527, 509,
    493, 484, 501, 515, 528, 533, 528, 519, 493, 484,
    499, 516, 527, 521, 525, 516, 508, 517, 520, 515,
    518, 518
  };
  //*/
  const int nEntries2 = 17;

  const double xar2[nEntries2] = {
    157, 158, 159, 160, 162, 165, 168, 171, 175, 181,
    204, 232, 253, 275, 309, 337, 358
  };
  
  const double yar2[nEntries2] = {
    206, 204, 202, 200, 195, 188, 183, 176, 170, 162,
    141, 130, 131, 146, 177, 235, 294
  };

  energy.clear(); energy.resize(nEntries1 + nEntries2);
  eps1.clear(); eps1.resize(nEntries1 + nEntries2);
  eps2.clear(); eps2.resize(nEntries1 + nEntries2);
  pacs.clear(); pacs.resize(nEntries1 + nEntries2);
  /*
  for (int i = 0; i < nEntries1; ++i) {
    energy[i] = 11. + (xar1[i] - 44.) * 7. / (783. - 44.);
    pacs[i] = 1.e-18 * (536. - yar1[i]) * 500. / (536. - 206.);
  }
  //*/
  for (int i = nEntries1; i < nEntries1 + nEntries2; ++i) {
    energy[i] = 10. + (xar2[i] - 76.) * 30. / (501. - 76.);
    pacs[i] = 1.e-18 * (548. - yar2[i]) * 30. / (548. - 167.);
  }
  
  emin = energy[0];
  emax = energy.back();

  energyIon.clear();
  yieldIon.clear();
  ionmin = 15.937;
  ionmax = 15.936;

}

void 
OpticalData::Methane() {

  const int nEntries = 75;

  const double xch4[nEntries] = {
    105, 113, 115, 119, 121, 123, 125, 128, 130, 131,
    133, 135, 138, 143, 146, 149, 153, 158, 163, 167,
    168, 172, 177, 185, 188, 191, 197, 200, 203, 207,
    208, 211, 215, 217, 220, 225, 229, 233, 237, 239,
    243, 245, 248, 252, 256, 259, 262, 265, 271, 275, 
    280, 285, 293, 303, 310, 323, 336, 351, 367, 391, 
    413, 424, 441, 459, 479, 501, 535, 549, 560, 575, 
    587, 609, 629, 655, 683 
  };

  const double ych4[nEntries] = {
    539, 533, 530, 515, 506, 494, 480, 471, 461, 451,
    425, 411, 394, 373, 365, 363, 363, 368, 371, 369,
    363, 358, 357, 361, 371, 377, 377, 370, 355, 350,
    343, 316, 283, 268, 263, 257, 260, 261, 255, 249,
    239, 232, 227, 199, 181, 167, 157, 143, 126, 113,
     98,  83,  77,  77,  79,  80,  85,  87,  96, 111,
    127, 133, 149, 164, 178, 194, 216, 229, 237, 245,
    251, 267, 283, 298, 315
  };

  energy.clear(); energy.resize(nEntries);
  eps1.clear(); eps1.resize(nEntries);
  eps2.clear(); eps2.resize(nEntries);
  pacs.clear(); pacs.resize(nEntries);

  for (int i = nEntries; i--;) {
    energy[i] = 10. + (xch4[i] - 159.) * 10. / (534. - 159.);
    pacs[i] = 1.e-18 * (541. - ych4[i]) * 50. / (541. - 71.);
  } 

  emin = energy[0];
  emax = energy.back(); 

  // Photoionization yield
  // Source: 
  // K. Kameta et al., J. El. Spectr. Rel. Phen. 123 (2002), 225-238
  const int nYieldEntries = 11;
  double xion[nYieldEntries] = {12.65,
    12.87, 13.02, 13.24, 13.88, 14.66, 
    15.01, 15.44, 15.67, 15.87, 16.22};
                 
  double yion[nYieldEntries] = {0.,
    0.01226, 0.02900, 0.08853, 0.3693, 0.7024, 
    0.8190,  0.9262,  0.9715,  0.9883, 1.};

  energyIon.clear(); energyIon.resize(nYieldEntries);
  yieldIon.clear();  yieldIon.resize(nYieldEntries);
  for (int i = nYieldEntries; i--;) {
    energyIon[i] = xion[i];
    yieldIon[i] = yion[i];
  }
  ionmin = energyIon[0];
  ionmax = energyIon.back();

}

void 
OpticalData::Acetylene() {

  const int nEntriesA = 27;

  const double xc2h2A[nEntriesA] = {
    446, 437, 434, 428, 425, 419, 418, 408, 400, 394,
    388, 380, 372, 366, 358, 352, 346, 309, 230, 160,
    118, 108,  99,  88,  79,  69,  59
  };    

  const double yc2h2A[nEntriesA] = {
    103, 383, 288, 314, 212, 312, 170, 293, 243, 239,
    252, 266, 230, 239, 245, 251, 345, 202, 175, 115,
     77,  60,  58,  55,  59,  66,  89
  };
 
  const int nEntriesB = 41;

  const double xc2h2B[nEntriesB] = {
    371, 364, 361, 359, 356, 355, 353, 351, 349, 347,
    343, 339, 331, 325, 321, 319, 316, 314, 311, 308,
    305, 303, 301, 299, 296, 293, 291, 286, 281, 277,
    273, 269, 256, 248, 231, 203, 175, 150, 134, 126,
    101
  };

  const double yc2h2B[nEntriesB] = {
    241, 253, 256, 252, 243, 236, 242, 253, 267, 261,
    253, 257, 247, 229, 208, 191, 191, 186, 178, 177,
    173, 179, 186, 179, 171, 179, 187, 188, 184, 199,
    195, 203, 219, 229, 235, 241, 256, 264, 266, 269,
    286
  };

  energy.clear(); energy.resize(nEntriesA + nEntriesB);
  eps1.clear(); eps1.resize(nEntriesA + nEntriesB);
  eps2.clear(); eps2.resize(nEntriesA + nEntriesB);
  pacs.clear(); pacs.resize(nEntriesA + nEntriesB);

  for (int i = 0; i < nEntriesA; ++i) {
    energy[i] = TwoPi * HbarC / 
                ((90. + (xc2h2A[i] - 46.) * 10. / (239. - 84.)) * 1.e-7);
    pacs[i] = 1.e-18 * 50. * (514. - yc2h2A[i]) / (514. - 23.);
  }

  for (int i = 0; i < nEntriesB; ++i) { 
    energy[nEntriesA + i] = TwoPi * HbarC / 
                ((50. + (xc2h2B[i] - 84.) * 30. / (298. - 84.)) * 1.e-7);
    pacs[nEntriesA + i] = 1.e-18 * 50. * (389. - yc2h2B[i]) / (389. - 207.);
  }

  emin = energy[0];
  emax = energy.back(); 

  // Photoionization yield
  // Source:
  // M. Ukai et al., J. Chem. Phys. 95 (1991), 4142-4153
  const int nYieldEntries = 40;
  double xion[nYieldEntries] = {
    11.05, 11.06, 11.38, 11.56, 11.99, 12.56, 12.88, 13.23, 13.65, 13.82,
    14.13, 14.35, 14.49, 14.63, 14.84, 15.06, 15.23, 15.37, 15.48, 15.51,
    15.72, 15.83, 16.00, 16.21, 16.56, 16.67, 16.74, 16.95, 17.13, 17.27,
    17.41, 17.62, 17.87, 18.33, 18.68, 19.00, 19.35, 19.67, 19.98, 20.16
  };
                 
  double yion[nYieldEntries] = {
    0.009, 0.153, 0.290, 0.486, 0.700, 0.794, 0.794, 0.780, 0.716, 0.678, 
    0.632, 0.662, 0.702, 0.750, 0.783, 0.826, 0.817, 0.814, 0.799, 0.789,
    0.796, 0.780, 0.769, 0.751, 0.766, 0.787, 0.812, 0.841, 0.883, 0.896,
    0.910, 0.924, 0.940, 0.940, 0.949, 0.967, 0.981, 0.988, 0.997, 1.
  };

  energyIon.clear(); energyIon.resize(nYieldEntries);
  yieldIon.clear();  yieldIon.resize(nYieldEntries);
  for (int i = nYieldEntries; i--;) {
    energyIon[i] = xion[i];
    yieldIon[i] = yion[i];
  }
  ionmin = energyIon[0];
  ionmax = energyIon.back();

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


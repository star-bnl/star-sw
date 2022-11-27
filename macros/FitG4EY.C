static const Int_t Npart4EY = 6;
//________________________________________________________________________________
Double_t gf4EYFunc(Double_t *x, Double_t *par) {
  Double_t XX[1] = {x[0]};
  // par[0] - norm
  // par[1] - pion position wrt Z_pion (Bichsel prediction)
  // par[2] - sigma 
  // par[3] - proton signal  1
  // par[4] - Kaon    -"-    2
  // par[5] - electorn -"-   3
  // par[6] - deuteron -"-   4
  // par[7] - muon           5
  // par[8] - triton         6
  // par[9] - He3            7
  // par[10]- alpha          8
  // par[11] - Total
  // par[12] - case (-1 all, >-0 hyp no.)
  // par[13] - occupancy = probability to have 2d hits in the cluster 
  // par[14] - IO
  // par[15] - sign
  // par[16] - eLoss || Scale
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t occupancy = par[13];
  Double_t scale = 1;
  //#define __ELOSS__
#ifdef __ELOSS__
  Double_t eLoss = par[16];
#else 
  //#define __SCALE__
#ifdef __SCALE__
  Double_t scale = 1 + par[16];
  //  XX[0] *= scale;
#endif /* __SCALE__ */
#endif /* __ELOSS__ */
  Int_t IO = par[14];
  IO = TMath::Max(0, TMath::Min(1, IO));
  Int_t sign = par[15];
  Double_t frac[9];
  Double_t ff[9] = {0};
  for (Int_t i = 1; i < Npart4EY; i++) {
    ff[i] = TMath::Sin(par[2+i]);
    ff[i] *= ff[i];
  }
  frac[1] = ff[1];
  //  frac[0] = (1 - frac[1])/(1. + ff[2] + ff[3] + ff[4]);
  frac[0] = (1 - ff[1]*(1 + ff[4] + ff[6] + ff[7] + ff[8]))/(1 + ff[2] + ff[3] + ff[5]);
  frac[2] = frac[0]*ff[2];
  frac[3] = frac[0]*ff[3];
  frac[4] = ff[1]*ff[4];
  frac[5] = frac[0]*ff[5];
  frac[6] = ff[1]*ff[6];
  frac[7] = ff[1]*ff[7];
  frac[8] = ff[1]*ff[8];
  if (frac[0] < 0.1) return 0;
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
#if 0
  // 11/03/2022 /hlt/cephfs/fisyak/TpcRS_2021.COL/dEdx/Fit/FiTGG
  static MIPFitParX_t parMIPs[27][3][2] = {
    {{ // particle, norml, mu, sigma, alpha
	{ 0,-1, 0, 0,"zIpionN"                       ,    8.87416,   -0.18328,    0.31251,    0.69199},
	{ 0,-1, 0, 1,"zOpionN"                       ,    9.27278,   -0.20263,    0.29407,    0.80488} },{
	{ 0,-1, 1, 0,"zIpionP"                       ,    9.00311,   -0.22109,    0.31727,    0.71221},
	{ 0,-1, 1, 1,"zOpionP"                       ,    9.28935,   -0.22444,    0.29757,    0.80836} },{
	{ 0,-1, 2, 0,"zIpion"                        ,    9.63291,   -0.20025,    0.31756,    0.71756},
	{ 0,-1, 2, 1,"zOpion"                        ,    9.97435,   -0.21317,    0.29645,    0.80915} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1,-1, 0, 0,"zIprotonN"                     ,    9.49624,    0.89721,    0.25712,    1.02688},
	{ 1,-1, 0, 1,"zOprotonN"                     ,    9.27711,    0.97538,    0.24224,    1.31196} },{
	{ 1,-1, 1, 0,"zIprotonP"                     ,    9.48544,    0.89182,    0.25704,    1.00724},
	{ 1,-1, 1, 1,"zOprotonP"                     ,    9.24728,    0.97935,    0.24158,    1.27985} },{
	{ 1,-1, 2, 0,"zIproton"                      ,   10.18393,    0.89492,    0.25734,    1.02144},
	{ 1,-1, 2, 1,"zOproton"                      ,    9.95568,    0.97717,    0.24183,    1.29140} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2,-1, 0, 0,"zIkaonN"                       ,    9.40743,    0.16954,    0.28372,    0.74882},
	{ 2,-1, 0, 1,"zOkaonN"                       ,    9.56678,    0.20207,    0.26922,    0.87722} },{
	{ 2,-1, 1, 0,"zIkaonP"                       ,    9.42199,    0.14436,    0.28366,    0.74689},
	{ 2,-1, 1, 1,"zOkaonP"                       ,    9.49138,    0.19374,    0.26951,    0.87505} },{
	{ 2,-1, 2, 0,"zIkaon"                        ,   10.10761,    0.15804,    0.28466,    0.75458},
	{ 2,-1, 2, 1,"zOkaon"                        ,   10.22326,    0.19778,    0.26929,    0.87344} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3,-1, 0, 0,"zIelectronN"                   ,    9.23395,    0.06415,    0.28901,    0.77957},
	{ 3,-1, 0, 1,"zOelectronN"                   ,    9.42279,    0.09136,    0.27332,    0.90388} },{
	{ 3,-1, 1, 0,"zIelectronP"                   ,    9.26732,    0.03658,    0.28661,    0.77656},
	{ 3,-1, 1, 1,"zOelectronP"                   ,    9.36402,    0.07940,    0.27576,    0.91919} },{
	{ 3,-1, 2, 0,"zIelectron"                    ,    9.94402,    0.05032,    0.28819,    0.77943},
	{ 3,-1, 2, 1,"zOelectron"                    ,   10.08722,    0.08557,    0.27467,    0.91107} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4,-1, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4,-1, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4,-1, 1, 0,"zIdeuteronP"                   ,    8.07059,    1.89157,    0.18219,    2.40412},
	{ 4,-1, 1, 1,"zOdeuteronP"                   ,    6.51591,    1.88577,    0.13634,    3.17867} },{
	{ 4,-1, 2, 0,"zIdeuteron"                    ,    8.07059,    1.89157,    0.18219,    2.40411},
	{ 4,-1, 2, 1,"zOdeuteron"                    ,    6.51591,    1.88577,    0.13634,    3.17867} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5,-1, 0, 0,"zImuonN"                       ,    8.88300,   -0.16888,    0.31967,    0.73087},
	{ 5,-1, 0, 1,"zOmuonN"                       ,    9.26417,   -0.19513,    0.29321,    0.81657} },{
	{ 5,-1, 1, 0,"zImuonP"                       ,    8.97478,   -0.21283,    0.31732,    0.72263},
	{ 5,-1, 1, 1,"zOmuonP"                       ,    9.25995,   -0.21710,    0.29686,    0.82006} },{
	{ 5,-1, 2, 0,"zImuon"                        ,    9.62141,   -0.18838,    0.32076,    0.74474},
	{ 5,-1, 2, 1,"zOmuon"                        ,    9.95531,   -0.20532,    0.29591,    0.82278} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 0, 0, 0, 0,"zIpionN+zIpionN"               ,    8.86640,    0.60309,    0.25412,    0.69806},
	{ 0, 0, 0, 1,"zOpionN+zOpionN"               ,    9.26943,    0.55800,    0.23384,    0.80701} },{
	{ 0, 0, 1, 0,"zIpionP+zIpionP"               ,    8.99606,    0.56301,    0.25826,    0.72391},
	{ 0, 0, 1, 1,"zOpionP+zOpionP"               ,    9.28438,    0.53521,    0.23405,    0.79928} },{
	{ 0, 0, 2, 0,"zIpion+zIpion"                 ,    9.62321,    0.58874,    0.26059,    0.74840},
	{ 0, 0, 2, 1,"zOpion+zOpion"                 ,    9.96974,    0.54703,    0.23382,    0.80591} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1, 0, 0, 0,"zIprotonN+zIpionN"             ,    9.49336,    1.25335,    0.22881,    0.95996},
	{ 1, 0, 0, 1,"zOprotonN+zOpionN"             ,    9.28035,    1.28799,    0.21230,    1.18652} },{
	{ 1, 0, 1, 0,"zIprotonP+zIpionP"             ,    9.48424,    1.23266,    0.22601,    0.92806},
	{ 1, 0, 1, 1,"zOprotonP+zOpionP"             ,    9.24978,    1.28437,    0.21047,    1.15924} },{
	{ 1, 0, 2, 0,"zIproton+zIpion"               ,   10.17995,    1.24672,    0.22951,    0.98475},
	{ 1, 0, 2, 1,"zOproton+zOpion"               ,    9.95845,    1.28447,    0.21113,    1.16424} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1, 1, 0, 0,"zIprotonN+zIprotonN"           ,    9.49571,    1.63364,    0.19938,    1.08554},
	{ 1, 1, 0, 1,"zOprotonN+zOprotonN"           ,    9.28137,    1.69989,    0.18766,    1.50346} },{
	{ 1, 1, 1, 0,"zIprotonP+zIprotonP"           ,    9.48316,    1.63047,    0.19988,    1.07087},
	{ 1, 1, 1, 1,"zOprotonP+zOprotonP"           ,    9.25176,    1.70409,    0.18709,    1.42341} },{
	{ 1, 1, 2, 0,"zIproton+zIproton"             ,   10.18218,    1.63154,    0.19963,    1.08358},
	{ 1, 1, 2, 1,"zOproton+zOproton"             ,    9.95969,    1.70267,    0.18745,    1.45636} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 0, 0, 0,"zIkaonN+zIpionN"               ,    9.39822,    0.79371,    0.24559,    0.77500},
	{ 2, 0, 0, 1,"zOkaonN+zOpionN"               ,    9.56313,    0.77291,    0.22220,    0.85333} },{
	{ 2, 0, 1, 0,"zIkaonP+zIpionP"               ,    9.41622,    0.75197,    0.24127,    0.74717},
	{ 2, 0, 1, 1,"zOkaonP+zOpionP"               ,    9.48856,    0.75935,    0.22381,    0.85825} },{
	{ 2, 0, 2, 0,"zIkaon+zIpion"                 ,   10.09704,    0.77389,    0.24380,    0.77987},
	{ 2, 0, 2, 1,"zOkaon+zOpion"                 ,   10.21982,    0.76645,    0.22348,    0.85808} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 1, 0, 0,"zIkaonN+zIprotonN"             ,    9.40721,    1.34994,    0.21728,    0.95715},
	{ 2, 1, 0, 1,"zOkaonN+zOprotonN"             ,    9.56234,    1.39637,    0.20196,    1.19396} },{
	{ 2, 1, 1, 0,"zIkaonP+zIprotonP"             ,    9.41951,    1.33608,    0.21651,    0.95035},
	{ 2, 1, 1, 1,"zOkaonP+zOprotonP"             ,    9.48740,    1.39612,    0.20140,    1.17214} },{
	{ 2, 1, 2, 0,"zIkaon+zIproton"               ,   10.10591,    1.34415,    0.21743,    0.96447},
	{ 2, 1, 2, 1,"zOkaon+zOproton"               ,   10.21889,    1.39584,    0.20166,    1.19004} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 2, 0, 0,"zIkaonN+zIkaonN"               ,    9.40211,    0.93993,    0.23077,    0.77767},
	{ 2, 2, 0, 1,"zOkaonN+zOkaonN"               ,    9.56370,    0.95073,    0.21218,    0.89741} },{
	{ 2, 2, 1, 0,"zIkaonP+zIkaonP"               ,    9.41654,    0.90909,    0.22678,    0.75783},
	{ 2, 2, 1, 1,"zOkaonP+zOkaonP"               ,    9.48818,    0.94099,    0.21140,    0.88340} },{
	{ 2, 2, 2, 0,"zIkaon+zIkaon"                 ,   10.09990,    0.93000,    0.23142,    0.80011},
	{ 2, 2, 2, 1,"zOkaon+zOkaon"                 ,   10.22001,    0.94642,    0.21222,    0.89642} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 0, 0, 0,"zIelectronN+zIpionN"           ,    9.22884,    0.72874,    0.24699,    0.76717},
	{ 3, 0, 0, 1,"zOelectronN+zOpionN"           ,    9.41978,    0.70743,    0.22290,    0.85700} },{
	{ 3, 0, 1, 0,"zIelectronP+zIpionP"           ,    9.26357,    0.69278,    0.24483,    0.77306},
	{ 3, 0, 1, 1,"zOelectronP+zOpionP"           ,    9.36133,    0.69155,    0.22466,    0.87577} },{
	{ 3, 0, 2, 0,"zIelectron+zIpion"             ,    9.93630,    0.71492,    0.24788,    0.80079},
	{ 3, 0, 2, 1,"zOelectron+zOpion"             ,   10.08417,    0.70016,    0.22455,    0.86614} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 1, 0, 0,"zIelectronN+zIprotonN"         ,    9.23427,    1.31412,    0.21865,    0.98583},
	{ 3, 1, 0, 1,"zOelectronN+zOprotonN"         ,    9.41905,    1.36283,    0.20496,    1.26748} },{
	{ 3, 1, 1, 0,"zIelectronP+zIprotonP"         ,    9.26811,    1.30231,    0.21897,    0.98417},
	{ 3, 1, 1, 1,"zOelectronP+zOprotonP"         ,    9.36074,    1.36016,    0.20370,    1.23517} },{
	{ 3, 1, 2, 0,"zIelectron+zIproton"           ,    9.94459,    1.30733,    0.21822,    0.97826},
	{ 3, 1, 2, 1,"zOelectron+zOproton"           ,   10.08420,    1.36213,    0.20465,    1.24950} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 2, 0, 0,"zIelectronN+zIkaonN"           ,    9.22966,    0.89111,    0.23268,    0.79503},
	{ 3, 2, 0, 1,"zOelectronN+zOkaonN"           ,    9.41990,    0.89682,    0.21342,    0.91190} },{
	{ 3, 2, 1, 0,"zIelectronP+zIkaonP"           ,    9.26416,    0.85847,    0.22819,    0.77433},
	{ 3, 2, 1, 1,"zOelectronP+zOkaonP"           ,    9.36237,    0.88572,    0.21398,    0.91420} },{
	{ 3, 2, 2, 0,"zIelectron+zIkaon"             ,    9.93922,    0.87532,    0.23185,    0.80834},
	{ 3, 2, 2, 1,"zOelectron+zOkaon"             ,   10.08520,    0.88926,    0.21202,    0.89738} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 3, 0, 0,"zIelectronN+zIelectronN"       ,    9.22757,    0.83513,    0.23428,    0.82319},
	{ 3, 3, 0, 1,"zOelectronN+zOelectronN"       ,    9.41942,    0.83828,    0.21370,    0.91254} },{
	{ 3, 3, 1, 0,"zIelectronP+zIelectronP"       ,    9.26297,    0.80216,    0.23008,    0.79237},
	{ 3, 3, 1, 1,"zOelectronP+zOelectronP"       ,    9.36047,    0.82650,    0.21575,    0.94180} },{
	{ 3, 3, 2, 0,"zIelectron+zIelectron"         ,    9.93961,    0.81681,    0.23100,    0.79272},
	{ 3, 3, 2, 1,"zOelectron+zOelectron"         ,   10.08438,    0.83354,    0.21578,    0.93866} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 0, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 0, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 0, 1, 0,"zIdeuteronP+zIpionP"           ,    8.07109,    2.03495,    0.17359,    1.58649},
	{ 4, 0, 1, 1,"zOdeuteronP+zOpionP"           ,    6.19625,    2.00375,    0.05734,    0.55092} },{
	{ 4, 0, 2, 0,"zIdeuteron+zIpion"             ,    8.07134,    2.03917,    0.17414,    1.67285},
	{ 4, 0, 2, 1,"zOdeuteron+zOpion"             ,    6.51986,    2.02066,    0.13615,    1.98079} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 1, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 1, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 1, 1, 0,"zIdeuteronP+zIprotonP"         ,    8.07135,    2.22718,    0.15927,    1.59972},
	{ 4, 1, 1, 1,"zOdeuteronP+zOprotonP"         ,    6.52201,    2.23891,    0.12960,    1.84937} },{
	{ 4, 1, 2, 0,"zIdeuteron+zIproton"           ,    8.07151,    2.22946,    0.16054,    1.67014},
	{ 4, 1, 2, 1,"zOdeuteron+zOproton"           ,    6.52121,    2.23993,    0.12863,    1.97146} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 2, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 2, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 2, 1, 0,"zIdeuteronP+zIkaonP"           ,    8.07103,    2.08126,    0.16850,    1.57256},
	{ 4, 2, 1, 1,"zOdeuteronP+zOkaonP"           ,    6.51906,    2.07589,    0.13186,    1.66651} },{
	{ 4, 2, 2, 0,"zIdeuteron+zIkaon"             ,    8.07134,    2.08499,    0.17016,    1.58621},
	{ 4, 2, 2, 1,"zOdeuteron+zOkaon"             ,    6.29231,    2.08952,    0.08246,    0.98966} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 3, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 3, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 3, 1, 0,"zIdeuteronP+zIelectronP"       ,    8.07151,    2.06413,    0.17039,    1.78828},
	{ 4, 3, 1, 1,"zOdeuteronP+zOelectronP"       ,    6.51867,    2.05938,    0.13189,    2.01076} },{
	{ 4, 3, 2, 0,"zIdeuteron+zIelectron"         ,    8.07131,    2.06720,    0.17055,    1.74713},
	{ 4, 3, 2, 1,"zOdeuteron+zOelectron"         ,    6.51926,    2.05955,    0.13175,    1.92409} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 4, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 4, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 4, 1, 0,"zIdeuteronP+zIdeuteronP"       ,    8.07129,    2.59226,    0.13102,    2.30376},
	{ 4, 4, 1, 1,"zOdeuteronP+zOdeuteronP"       ,    6.51356,    2.58279,    0.09917,    3.96216} },{
	{ 4, 4, 2, 0,"zIdeuteron+zIdeuteron"         ,    8.07099,    2.59279,    0.13203,    2.93974},
	{ 4, 4, 2, 1,"zOdeuteron+zOdeuteron"         ,    6.51208,    2.58357,    0.09959,    3.32026} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 0, 0, 0,"zImuonN+zIpionN"               ,    8.87430,    0.61936,    0.26307,    0.75714},
	{ 5, 0, 0, 1,"zOmuonN+zOpionN"               ,    9.26064,    0.56150,    0.23204,    0.81108} },{
	{ 5, 0, 1, 0,"zImuonP+zIpionP"               ,    8.96818,    0.56652,    0.25786,    0.73470},
	{ 5, 0, 1, 1,"zOmuonP+zOpionP"               ,    9.25475,    0.53906,    0.23409,    0.81113} },{
	{ 5, 0, 2, 0,"zImuon+zIpion"                 ,    9.61636,    0.59105,    0.26134,    0.74399},
	{ 5, 0, 2, 1,"zOmuon+zOpion"                 ,    9.95087,    0.55151,    0.23477,    0.81939} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 1, 0, 0,"zImuonN+zIprotonN"             ,    8.88467,    1.25286,    0.22653,    0.95822},
	{ 5, 1, 0, 1,"zOmuonN+zOprotonN"             ,    9.26148,    1.28978,    0.21297,    1.22443} },{
	{ 5, 1, 1, 0,"zImuonP+zIprotonP"             ,    8.97217,    1.23801,    0.22741,    0.95831},
	{ 5, 1, 1, 1,"zOmuonP+zOprotonP"             ,    9.25555,    1.28529,    0.21141,    1.18034} },{
	{ 5, 1, 2, 0,"zImuon+zIproton"               ,    9.62262,    1.24651,    0.22898,    0.97128},
	{ 5, 1, 2, 1,"zOmuon+zOproton"               ,    9.95174,    1.28799,    0.21266,    1.20641} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 2, 0, 0,"zImuonN+zIkaonN"               ,    8.87746,    0.79040,    0.24285,    0.74482},
	{ 5, 2, 0, 1,"zOmuonN+zOkaonN"               ,    9.26142,    0.77666,    0.22228,    0.86517} },{
	{ 5, 2, 1, 0,"zImuonP+zIkaonP"               ,    8.96844,    0.75499,    0.24107,    0.75565},
	{ 5, 2, 1, 1,"zOmuonP+zOkaonP"               ,    9.25640,    0.76261,    0.22370,    0.87089} },{
	{ 5, 2, 2, 0,"zImuon+zIkaon"                 ,    9.61669,    0.77522,    0.24307,    0.76871},
	{ 5, 2, 2, 1,"zOmuon+zOkaon"                 ,    9.95171,    0.76981,    0.22256,    0.86836} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 3, 0, 0,"zImuonN+zIelectronN"           ,    8.87855,    0.72985,    0.24438,    0.77014},
	{ 5, 3, 0, 1,"zOmuonN+zOelectronN"           ,    9.26022,    0.71224,    0.22397,    0.87965} },{
	{ 5, 3, 1, 0,"zImuonP+zIelectronP"           ,    8.96795,    0.69181,    0.24190,    0.76021},
	{ 5, 3, 1, 1,"zOmuonP+zOelectronP"           ,    9.25495,    0.69423,    0.22482,    0.88132} },{
	{ 5, 3, 2, 0,"zImuon+zIelectron"             ,    9.61558,    0.71360,    0.24561,    0.78231},
	{ 5, 3, 2, 1,"zOmuon+zOelectron"             ,    9.95130,    0.70455,    0.22380,    0.88122} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 4, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 5, 4, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 5, 4, 1, 0,"zImuonP+zIdeuteronP"           ,    8.97929,    2.03603,    0.17454,    1.63866},
	{ 5, 4, 1, 1,"zOmuonP+zOdeuteronP"           ,    9.25379,    2.02255,    0.13540,    1.92364} },{
	{ 5, 4, 2, 0,"zImuon+zIdeuteron"             ,    9.62888,    2.03893,    0.17419,    1.62344},
	{ 5, 4, 2, 1,"zOmuon+zOdeuteron"             ,    9.94975,    2.02354,    0.13484,    1.87674} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 5, 0, 0,"zImuonN+zImuonN"               ,    8.87616,    0.61590,    0.25874,    0.73767},
	{ 5, 5, 0, 1,"zOmuonN+zOmuonN"               ,    9.25954,    0.56551,    0.23347,    0.82580} },{
	{ 5, 5, 1, 0,"zImuonP+zImuonP"               ,    8.96932,    0.57138,    0.25956,    0.73270},
	{ 5, 5, 1, 1,"zOmuonP+zOmuonP"               ,    9.25478,    0.54185,    0.23172,    0.81559} },{
	{ 5, 5, 2, 0,"zImuon+zImuon"                 ,    9.61508,    0.60025,    0.26425,    0.77273},
	{ 5, 5, 2, 1,"zOmuon+zOmuon"                 ,    9.95068,    0.55337,    0.23333,    0.82203} }},
  };
#else
// 11/27/2022 /hlt/cephfs/fisyak/TpcRS_2021.COL.NoPad/Fit2/
  static MIPFitParX_t parMIPs[27][3][2] = {
    {{ // particle, norml, mu, sigma, alpha
	{ 0,-1, 0, 0,"zIpionN"                       ,   10.55697,   -0.33349,    0.31430,    0.82637},
	{ 0,-1, 0, 1,"zOpionN"                       ,   10.26820,   -0.27693,    0.31165,    0.87240} },{
	{ 0,-1, 1, 0,"zIpionP"                       ,   10.53649,   -0.34145,    0.31789,    0.83232},
	{ 0,-1, 1, 1,"zOpionP"                       ,   10.25151,   -0.28663,    0.31219,    0.86099} },{
	{ 0,-1, 2, 0,"zIpion"                        ,   11.23971,   -0.33655,    0.31659,    0.83519},
	{ 0,-1, 2, 1,"zOpion"                        ,   10.95303,   -0.28153,    0.31208,    0.86817} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1,-1, 0, 0,"zIprotonN"                     ,   10.38947,    0.86350,    0.25422,    0.87902},
	{ 1,-1, 0, 1,"zOprotonN"                     ,   10.06501,    0.95926,    0.23128,    0.84635} },{
	{ 1,-1, 1, 0,"zIprotonP"                     ,   10.38031,    0.86587,    0.25382,    0.86185},
	{ 1,-1, 1, 1,"zOprotonP"                     ,   10.06217,    0.96047,    0.23002,    0.82423} },{
	{ 1,-1, 2, 0,"zIproton"                      ,   11.07824,    0.86444,    0.25392,    0.86813},
	{ 1,-1, 2, 1,"zOproton"                      ,   10.75685,    0.95980,    0.23064,    0.83443} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2,-1, 0, 0,"zIkaonN"                       ,   10.51381,    0.08053,    0.28862,    0.81467},
	{ 2,-1, 0, 1,"zOkaonN"                       ,   10.19077,    0.16450,    0.26818,    0.80084} },{
	{ 2,-1, 1, 0,"zIkaonP"                       ,   10.50486,    0.07647,    0.28987,    0.81067},
	{ 2,-1, 1, 1,"zOkaonP"                       ,   10.18158,    0.16157,    0.26949,    0.80591} },{
	{ 2,-1, 2, 0,"zIkaon"                        ,   11.20215,    0.07956,    0.28981,    0.82003},
	{ 2,-1, 2, 1,"zOkaon"                        ,   10.87921,    0.16354,    0.26912,    0.80688} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3,-1, 0, 0,"zIelectronN"                   ,   10.60108,   -0.03044,    0.29184,    0.86265},
	{ 3,-1, 0, 1,"zOelectronN"                   ,   10.25540,    0.04944,    0.27353,    0.86694} },{
	{ 3,-1, 1, 0,"zIelectronP"                   ,   10.59369,   -0.03561,    0.29459,    0.86004},
	{ 3,-1, 1, 1,"zOelectronP"                   ,   10.25972,    0.04563,    0.27553,    0.86756} },{
	{ 3,-1, 2, 0,"zIelectron"                    ,   11.29052,   -0.03282,    0.29335,    0.86271},
	{ 3,-1, 2, 1,"zOelectron"                    ,   10.95074,    0.04757,    0.27457,    0.86750} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4,-1, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4,-1, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4,-1, 1, 0,"zIdeuteronP"                   ,    9.93472,    1.99429,    0.22046,    1.21525},
	{ 4,-1, 1, 1,"zOdeuteronP"                   ,    9.75096,    2.14656,    0.21822,    1.21399} },{
	{ 4,-1, 2, 0,"zIdeuteron"                    ,    9.93472,    1.99429,    0.22046,    1.21525},
	{ 4,-1, 2, 1,"zOdeuteron"                    ,    9.75096,    2.14656,    0.21822,    1.21399} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5,-1, 0, 0,"zImuonN"                       ,   10.56413,   -0.32582,    0.31294,    0.82592},
	{ 5,-1, 0, 1,"zOmuonN"                       ,   10.27557,   -0.26816,    0.30998,    0.87392} },{
	{ 5,-1, 1, 0,"zImuonP"                       ,   10.55512,   -0.33291,    0.31743,    0.83453},
	{ 5,-1, 1, 1,"zOmuonP"                       ,   10.26948,   -0.27538,    0.31207,    0.87484} },{
	{ 5,-1, 2, 0,"zImuon"                        ,   11.25263,   -0.32868,    0.31558,    0.83457},
	{ 5,-1, 2, 1,"zOmuon"                        ,   10.96573,   -0.27168,    0.31111,    0.87481} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 0, 0, 0, 0,"zIpionN+zIpionN"               ,   10.55530,    0.41430,    0.23879,    0.75320},
	{ 0, 0, 0, 1,"zOpionN+zOpionN"               ,   10.26730,    0.46644,    0.23700,    0.80071} },{
	{ 0, 0, 1, 0,"zIpionP+zIpionP"               ,   10.53489,    0.40615,    0.24111,    0.75516},
	{ 0, 0, 1, 1,"zOpionP+zOpionP"               ,   10.25038,    0.45829,    0.23818,    0.79117} },{
	{ 0, 0, 2, 0,"zIpion+zIpion"                 ,   11.23806,    0.41037,    0.23980,    0.75785},
	{ 0, 0, 2, 1,"zOpion+zOpion"                 ,   10.95192,    0.46176,    0.23734,    0.79497} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1, 0, 0, 0,"zIprotonN+zIpionN"             ,   10.38930,    1.17017,    0.22065,    0.84928},
	{ 1, 0, 0, 1,"zOprotonN+zOpionN"             ,   10.06592,    1.25105,    0.20493,    0.83134} },{
	{ 1, 0, 1, 0,"zIprotonP+zIpionP"             ,   10.38029,    1.16798,    0.21949,    0.82434},
	{ 1, 0, 1, 1,"zOprotonP+zOpionP"             ,   10.06248,    1.25067,    0.20384,    0.80985} },{
	{ 1, 0, 2, 0,"zIproton+zIpion"               ,   11.07813,    1.16954,    0.22025,    0.83618},
	{ 1, 0, 2, 1,"zOproton+zOpion"               ,   10.75692,    1.25086,    0.20371,    0.82237} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 1, 1, 0, 0,"zIprotonN+zIprotonN"           ,   10.38804,    1.60514,    0.20048,    0.89655},
	{ 1, 1, 0, 1,"zOprotonN+zOprotonN"           ,   10.06453,    1.69720,    0.18462,    0.88034} },{
	{ 1, 1, 1, 0,"zIprotonP+zIprotonP"           ,   10.37907,    1.60805,    0.20013,    0.87639},
	{ 1, 1, 1, 1,"zOprotonP+zOprotonP"           ,   10.06394,    1.69610,    0.18149,    0.81466} },{
	{ 1, 1, 2, 0,"zIproton+zIproton"             ,   11.07640,    1.60733,    0.20069,    0.89467},
	{ 1, 1, 2, 1,"zOproton+zOproton"             ,   10.75847,    1.69541,    0.18200,    0.83005} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 0, 0, 0,"zIkaonN+zIpionN"               ,   10.51246,    0.64126,    0.23062,    0.76221},
	{ 2, 0, 0, 1,"zOkaonN+zOpionN"               ,   10.18996,    0.71166,    0.22145,    0.77865} },{
	{ 2, 0, 1, 0,"zIkaonP+zIpionP"               ,   10.50350,    0.63481,    0.23185,    0.76200},
	{ 2, 0, 1, 1,"zOkaonP+zOpionP"               ,   10.18053,    0.70429,    0.22117,    0.77305} },{
	{ 2, 0, 2, 0,"zIkaon+zIpion"                 ,   11.20058,    0.63861,    0.23158,    0.76656},
	{ 2, 0, 2, 1,"zOkaon+zOpion"                 ,   10.87865,    0.70756,    0.22204,    0.78047} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 1, 0, 0,"zIkaonN+zIprotonN"             ,   10.51068,    1.28658,    0.21226,    0.83906},
	{ 2, 1, 0, 1,"zOkaonN+zOprotonN"             ,   10.18847,    1.37606,    0.19697,    0.83154} },{
	{ 2, 1, 1, 0,"zIkaonP+zIprotonP"             ,   10.50093,    1.28811,    0.21286,    0.83595},
	{ 2, 1, 1, 1,"zOkaonP+zOprotonP"             ,   10.17927,    1.37547,    0.19570,    0.81254} },{
	{ 2, 1, 2, 0,"zIkaon+zIproton"               ,   11.19840,    1.28880,    0.21346,    0.84917},
	{ 2, 1, 2, 1,"zOkaon+zOproton"               ,   10.87755,    1.37427,    0.19495,    0.80828} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 2, 2, 0, 0,"zIkaonN+zIkaonN"               ,   10.51186,    0.82781,    0.22283,    0.76878},
	{ 2, 2, 0, 1,"zOkaonN+zOkaonN"               ,   10.18907,    0.91114,    0.21095,    0.77876} },{
	{ 2, 2, 1, 0,"zIkaonP+zIkaonP"               ,   10.50305,    0.82208,    0.22242,    0.75048},
	{ 2, 2, 1, 1,"zOkaonP+zOkaonP"               ,   10.18000,    0.90563,    0.21006,    0.76841} },{
	{ 2, 2, 2, 0,"zIkaon+zIkaon"                 ,   11.20037,    0.82615,    0.22343,    0.76764},
	{ 2, 2, 2, 1,"zOkaon+zOkaon"                 ,   10.87810,    0.90786,    0.21029,    0.77264} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 0, 0, 0,"zIelectronN+zIpionN"           ,   10.59902,    0.57568,    0.23174,    0.79386},
	{ 3, 0, 0, 1,"zOelectronN+zOpionN"           ,   10.25462,    0.64409,    0.22531,    0.83707} },{
	{ 3, 0, 1, 0,"zIelectronP+zIpionP"           ,   10.59190,    0.57063,    0.23428,    0.79557},
	{ 3, 0, 1, 1,"zOelectronP+zOpionP"           ,   10.25882,    0.63566,    0.22449,    0.81746} },{
	{ 3, 0, 2, 0,"zIelectron+zIpion"             ,   11.28841,    0.57377,    0.23271,    0.79814},
	{ 3, 0, 2, 1,"zOelectron+zOpion"             ,   10.94993,    0.63998,    0.22475,    0.82313} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 1, 0, 0,"zIelectronN+zIprotonN"         ,   10.59727,    1.25037,    0.21368,    0.86405},
	{ 3, 1, 0, 1,"zOelectronN+zOprotonN"         ,   10.25354,    1.33730,    0.19682,    0.84092} },{
	{ 3, 1, 1, 0,"zIelectronP+zIprotonP"         ,   10.59027,    1.25007,    0.21334,    0.84334},
	{ 3, 1, 1, 1,"zOelectronP+zOprotonP"         ,   10.25739,    1.33763,    0.19684,    0.82885} },{
	{ 3, 1, 2, 0,"zIelectron+zIproton"           ,   11.28632,    1.25187,    0.21467,    0.86972},
	{ 3, 1, 2, 1,"zOelectron+zOproton"           ,   10.94854,    1.33728,    0.19652,    0.83387} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 2, 0, 0,"zIelectronN+zIkaonN"           ,   10.59885,    0.77369,    0.22414,    0.79716},
	{ 3, 2, 0, 1,"zOelectronN+zOkaonN"           ,   10.25392,    0.85334,    0.21243,    0.81407} },{
	{ 3, 2, 1, 0,"zIelectronP+zIkaonP"           ,   10.59100,    0.76836,    0.22420,    0.78487},
	{ 3, 2, 1, 1,"zOelectronP+zOkaonP"           ,   10.25793,    0.84852,    0.21262,    0.80313} },{
	{ 3, 2, 2, 0,"zIelectron+zIkaon"             ,   11.28830,    0.77079,    0.22470,    0.79153},
	{ 3, 2, 2, 1,"zOelectron+zOkaon"             ,   10.94905,    0.85069,    0.21211,    0.80369} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 3, 3, 0, 0,"zIelectronN+zIelectronN"       ,   10.59868,    0.71523,    0.22458,    0.82536},
	{ 3, 3, 0, 1,"zOelectronN+zOelectronN"       ,   10.25350,    0.79393,    0.21470,    0.85952} },{
	{ 3, 3, 1, 0,"zIelectronP+zIelectronP"       ,   10.59102,    0.71027,    0.22577,    0.81602},
	{ 3, 3, 1, 1,"zOelectronP+zOelectronP"       ,   10.25812,    0.78927,    0.21500,    0.84899} },{
	{ 3, 3, 2, 0,"zIelectron+zIelectron"         ,   11.28795,    0.71303,    0.22568,    0.82283},
	{ 3, 3, 2, 1,"zOelectron+zOelectron"         ,   10.94894,    0.79175,    0.21455,    0.85680} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 0, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 0, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 0, 1, 0,"zIdeuteronP+zIpionP"           ,    9.93361,    2.10873,    0.20686,    1.21184},
	{ 4, 0, 1, 1,"zOdeuteronP+zOpionP"           ,    9.75252,    2.24791,    0.20442,    1.17685} },{
	{ 4, 0, 2, 0,"zIdeuteron+zIpion"             ,    9.93408,    2.10692,    0.20549,    1.18247},
	{ 4, 0, 2, 1,"zOdeuteron+zOpion"             ,    9.75281,    2.24898,    0.20507,    1.18366} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 1, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 1, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 1, 1, 0,"zIdeuteronP+zIprotonP"         ,    9.93393,    2.30664,    0.18784,    1.14099},
	{ 4, 1, 1, 1,"zOdeuteronP+zOprotonP"         ,    9.75628,    2.44287,    0.18494,    1.12702} },{
	{ 4, 1, 2, 0,"zIdeuteron+zIproton"           ,    9.93317,    2.30602,    0.18779,    1.15696},
	{ 4, 1, 2, 1,"zOdeuteron+zOproton"           ,    9.75347,    2.44446,    0.18626,    1.16733} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 2, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 2, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 2, 1, 0,"zIdeuteronP+zIkaonP"           ,    9.93447,    2.15702,    0.20038,    1.16114},
	{ 4, 2, 1, 1,"zOdeuteronP+zOkaonP"           ,    9.75315,    2.29811,    0.19891,    1.15942} },{
	{ 4, 2, 2, 0,"zIdeuteron+zIkaon"             ,    9.93329,    2.15794,    0.20096,    1.18037},
	{ 4, 2, 2, 1,"zOdeuteron+zOkaon"             ,    9.75439,    2.29835,    0.19819,    1.15263} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 3, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 3, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 3, 1, 0,"zIdeuteronP+zIelectronP"       ,    9.93453,    2.13911,    0.20104,    1.17364},
	{ 4, 3, 1, 1,"zOdeuteronP+zOelectronP"       ,    9.75398,    2.28067,    0.19954,    1.16948} },{
	{ 4, 3, 2, 0,"zIdeuteron+zIelectron"         ,    9.93543,    2.13934,    0.20081,    1.16639},
	{ 4, 3, 2, 1,"zOdeuteron+zOelectron"         ,    9.75559,    2.27880,    0.19785,    1.12697} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 4, 4, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 4, 4, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 4, 4, 1, 0,"zIdeuteronP+zIdeuteronP"       ,    9.93403,    2.71400,    0.16892,    1.30000},
	{ 4, 4, 1, 1,"zOdeuteronP+zOdeuteronP"       ,    9.75048,    2.86851,    0.16996,    1.30000} },{
	{ 4, 4, 2, 0,"zIdeuteron+zIdeuteron"         ,    9.93382,    2.71375,    0.16870,    1.30000},
	{ 4, 4, 2, 1,"zOdeuteron+zOdeuteron"         ,    9.75094,    2.87012,    0.17111,    1.30000} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 0, 0, 0,"zImuonN+zIpionN"               ,   10.56236,    0.41903,    0.23832,    0.75538},
	{ 5, 0, 0, 1,"zOmuonN+zOpionN"               ,   10.27468,    0.46987,    0.23628,    0.80094} },{
	{ 5, 0, 1, 0,"zImuonP+zIpionP"               ,   10.55352,    0.41016,    0.24020,    0.75187},
	{ 5, 0, 1, 1,"zOmuonP+zOpionP"               ,   10.26824,    0.46343,    0.23838,    0.80201} },{
	{ 5, 0, 2, 0,"zImuon+zIpion"                 ,   11.25095,    0.41538,    0.24026,    0.76055},
	{ 5, 0, 2, 1,"zOmuon+zOpion"                 ,   10.96454,    0.46701,    0.23728,    0.80408} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 1, 0, 0,"zImuonN+zIprotonN"             ,   10.55957,    1.17104,    0.21947,    0.84487},
	{ 5, 1, 0, 1,"zOmuonN+zOprotonN"             ,   10.27257,    1.25157,    0.20307,    0.81785} },{
	{ 5, 1, 1, 0,"zImuonP+zIprotonP"             ,   10.55117,    1.17023,    0.21958,    0.82305},
	{ 5, 1, 1, 1,"zOmuonP+zOprotonP"             ,   10.26619,    1.25280,    0.20284,    0.80976} },{
	{ 5, 1, 2, 0,"zImuon+zIproton"               ,   11.24756,    1.17256,    0.22081,    0.85299},
	{ 5, 1, 2, 1,"zOmuon+zOproton"               ,   10.96200,    1.25316,    0.20361,    0.82544} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 2, 0, 0,"zImuonN+zIkaonN"               ,   10.56166,    0.64363,    0.23030,    0.76271},
	{ 5, 2, 0, 1,"zOmuonN+zOkaonN"               ,   10.27324,    0.71432,    0.22170,    0.78563} },{
	{ 5, 2, 1, 0,"zImuonP+zIkaonP"               ,   10.55256,    0.63883,    0.23231,    0.76189},
	{ 5, 2, 1, 1,"zOmuonP+zOkaonP"               ,   10.26685,    0.70870,    0.22209,    0.78140} },{
	{ 5, 2, 2, 0,"zImuon+zIkaon"                 ,   11.25019,    0.64235,    0.23178,    0.76789},
	{ 5, 2, 2, 1,"zOmuon+zOkaon"                 ,   10.96336,    0.71182,    0.22170,    0.78383} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 3, 0, 0,"zImuonN+zIelectronN"           ,   10.56191,    0.57906,    0.23154,    0.79341},
	{ 5, 3, 0, 1,"zOmuonN+zOelectronN"           ,   10.27367,    0.64585,    0.22356,    0.83021} },{
	{ 5, 3, 1, 0,"zImuonP+zIelectronP"           ,   10.55277,    0.57359,    0.23343,    0.79213},
	{ 5, 3, 1, 1,"zOmuonP+zOelectronP"           ,   10.26687,    0.64131,    0.22523,    0.83030} },{
	{ 5, 3, 2, 0,"zImuon+zIelectron"             ,   11.25012,    0.57668,    0.23286,    0.79712},
	{ 5, 3, 2, 1,"zOmuon+zOelectron"             ,   10.96356,    0.64270,    0.22361,    0.82424} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 4, 0, 0,""                              ,    0.00000,    0.00000,    0.00000,    0.00000},
	{ 5, 4, 0, 1,""                              ,    0.00000,    0.00000,    0.00000,    0.00000} },{
	{ 5, 4, 1, 0,"zImuonP+zIdeuteronP"           ,   10.54783,    2.10936,    0.20639,    1.21243},
	{ 5, 4, 1, 1,"zOmuonP+zOdeuteronP"           ,   10.24705,    2.24968,    0.20498,    1.19850} },{
	{ 5, 4, 2, 0,"zImuon+zIdeuteron"             ,   11.24602,    2.10951,    0.20620,    1.20367},
	{ 5, 4, 2, 1,"zOmuon+zOdeuteron"             ,   10.94412,    2.25042,    0.20510,    1.20284} }},
    {{ // particle, norml, mu, sigma, alpha
	{ 5, 5, 0, 0,"zImuonN+zImuonN"               ,   10.56221,    0.42347,    0.23871,    0.75891},
	{ 5, 5, 0, 1,"zOmuonN+zOmuonN"               ,   10.27450,    0.47652,    0.23713,    0.81575} },{
	{ 5, 5, 1, 0,"zImuonP+zImuonP"               ,   10.55346,    0.41397,    0.24053,    0.75588},
	{ 5, 5, 1, 1,"zOmuonP+zOmuonP"               ,   10.26834,    0.46777,    0.23770,    0.80561} },{
	{ 5, 5, 2, 0,"zImuon+zImuon"                 ,   11.25099,    0.41934,    0.24002,    0.76136},
	{ 5, 5, 2, 1,"zOmuon+zOmuon"                 ,   10.96445,    0.47349,    0.23807,    0.81444} }},
  };
#endif
#ifdef __ELOSS__  
  /*
                        bgL10min Tcut            bgL10MIP     bg  log(beta)                 dN/dx    scale = -1.13000e-01; alpha = 1.13000e-01/9.27077905202451547e+01 = 1.21888354113372510e-03
                                                                                                                    scale = 1/(1 +alpha*dNdx) - 1 

minimu log10(p/m) alpga    -0.7   40 keV          -0.850  0.141 -1.96883836551568292e+00  3.45122406159667935e+03  -0.807
                  He3      -0.7   40              -0.727  0.187 -1.69383239941652541e+00  2.13316907245180437e+03  -0.722
                  deuteron -1.0    1 keV          -0.552  0.280 -1.31070490640084025e+00  2.70689788452519849e+02  -0.248
                  proton   -1.0    1 keV          -0.251  0.561 -7.14846611368781581e-01  9.27077905202451547e+01  -0.101
                  kaon     -0.5  100 keV           0.027  1.064 -3.16517375796772749e-01  4.48350287856880172e+01
                  pion     -0.4  100 keV  bgL10 =  0.576  3.767 -3.40492423408368661e-02  2.97372112287192998e+01
                  e         2.0  100 keV           3.012  1028. -4.73133358730884409e-07  4.03895078708591129e+01

  */
  //                                    pi+           p        K+             e+          d       mu+        t      He3     alpha
  static Double_t dNdxMIP[9] = {    29.7388,    93.6498,  45.0091,       40.3891,    273.02,  29.9792, 539.102, 2156.38,  3486.43};
  const Double_t  Masses[9]  = { 0.13956995, 0.93827231, 01.493677, 0.51099907e-3, 0.1056584, 1.875613, 2.80925, 2.80923, 3.727417};
#endif /* __ELOSS__  */
  static Double_t MuShiftIO[3] = {-7.98670e-02, 6.43941e-03, 0}; // Aerage for sectors for 7p7GeV_2021
  Double_t Value = 0;
  Int_t icase = (Int_t) par[12];
  Int_t i1 = 0;
  Int_t i2 = Npart4EY - 1;
  if (icase >= 0) {i1 = i2 = icase;}
  TF1 *g = GausExp();
  static Int_t _debug = 0;
  for (Int_t i = i1; i <= i2; i++) { 
    if (frac[i] <= 0.0) continue;
#ifdef __SCALE__
    //    Double_t Mu = (mu + parMIPs[i][sign][IO].mu - parMIPs[0][sign][IO].mu)/(scale + MuShiftIO[IO]);
    Double_t Mu = (mu + parMIPs[i][sign][IO].mu - parMIPs[0][0][IO].mu)/(scale + MuShiftIO[IO]);
    Double_t Sigma =                  (parMIPs[i][sign][IO].sigma + sigma)/(scale + MuShiftIO[IO]);
#else
    Double_t Mu =    (mu    + parMIPs[i][sign][IO].mu - parMIPs[0][sign][IO].mu);
    Double_t Sigma = (sigma + parMIPs[i][sign][IO].sigma                       );
#endif
#ifdef __ELOSS__ 
    Double_t eps = dNdxMIP[i]/dNdxMIP[0] - 1;
    Double_t dMu = eps*eLoss*TMath::Exp(XX[0]);
    if (dMu > 0.1) dMu = 0.1;
    Mu += dMu;
#endif
    //    Double_t pars[4] = {0, Mu, parMIPs[i][sign][IO].sigma*(1 + sigma), parMIPs[i][sign][IO].alpha};
    Double_t pars[4] = {0, Mu, Sigma, parMIPs[i][sign][IO].alpha};
    Value += frac[i]*g->EvalPar(XX, pars);
    if (_debug) {
      cout << "i: " << i << " " << parMIPs[i][sign][IO].Name << " frac[" << i <<"] = " << frac[i] << "\t"; parMIPs[i][sign][IO].Print("",pars);
    }
  }
  if (occupancy > 0) {
    Double_t overlap = 0;
    for (Int_t i = i1; i <= i2; i++) { 
      if (frac[i] <= 0.0) continue;
      Double_t cont = 0;
      for (Int_t j = 0; j < Npart4EY; j++) {
	if (frac[j] <= 0.0) continue;
	Int_t l = (i <= j) ? i + j*(j+1)/2 : j + i*(i+1)/2;
	l += Npart4EY;
	//	Double_t Mu =    (mu    + parMIPs[l][sign][IO].mu     - parMIPs[0][sign][IO].mu)/(scale + MuShiftIO[IO]);
#ifdef __SCALE__
	Double_t Mu =    (mu    + parMIPs[l][sign][IO].mu     - parMIPs[0][0][IO].mu)/(scale + MuShiftIO[IO]);
	Double_t Sigma = (sigma + parMIPs[l][sign][IO].sigma                           )/(scale + MuShiftIO[IO]);
#else
	Double_t Mu =    (mu    + parMIPs[l][sign][IO].mu     - parMIPs[0][sign][IO].mu);
	Double_t Sigma = (sigma + parMIPs[l][sign][IO].sigma                            );
#endif
	Double_t pars[4] = {0, Mu, Sigma, parMIPs[l][sign][IO].alpha};
	cont += frac[j]*g->EvalPar(XX, pars);
	if (_debug) {
	  cout << "i:" << i << " " << parMIPs[i][sign][IO].Name << "\t+ j:" << j << " " 
	       <<  parMIPs[j][sign][IO].Name << "\t frac[" << j <<"] = " << frac[j] 
	       << "\t l:" << l << " "; parMIPs[l][sign][IO].Print("",pars);
	}
      }
      overlap += frac[i]*cont;
    }
    Value += occupancy*overlap;
  }
  return par[11]*TMath::Exp(par[0])*Value;
}
//________________________________________________________________________________
Bool_t  PreSetParametersY(TH1 *proj, TF1 *g2) { // Fit peak nearest to 0 by gaus
  if (! proj || ! g2) return kFALSE;
  static TSpectrum *fSpectrum = 0;
  if (! fSpectrum) {
    fSpectrum = new TSpectrum(6);
  }
  // Find pion peak
  Int_t nfound = fSpectrum->Search(proj);
  Int_t NN = nfound + 1;
#if  ROOT_VERSION_CODE < ROOT_VERSION(6,0,0)
  TArrayF X(NN,fSpectrum->GetPositionX());
  TArrayF Y(NN,fSpectrum->GetPositionY());
#else
  TArrayD X(NN,fSpectrum->GetPositionX());
  TArrayD Y(NN,fSpectrum->GetPositionY());
#endif
  TArrayI idxT(NN);
  Int_t nP = 0;
  //                             pi,        proton,          kaon,             e,     deuteron, 
  Double_t frac[5] = {           1.,          1e-6,          1e-6,          1e-6,          1e-6};
  Double_t post[5] = {Peaks[0].peak, Peaks[1].peak, Peaks[2].peak, Peaks[3].peak, Peaks[4].peak};
  Double_t T = 0;
  Double_t tots[5] = {0};
  Double_t xpiPd[3] = {0};
  Double_t ypiPd[3] = {0};
  Double_t ppiPd[3] = {Peaks[0].peak, Peaks[1].peak, Peaks[4].peak};
  Double_t shift = 0;
  Double_t total = 0;
  if (nfound > 0) {
    TMath::Sort(nfound,X.GetArray(),idxT.GetArray(),kFALSE);
    for (Int_t i = 0; i < nfound; i++) {
      Int_t p = idxT[i];
      Double_t xp = X[p];
      Int_t bin = proj->GetXaxis()->FindBin(xp);
      Double_t yp = proj->GetBinContent(bin);
      Double_t ep = proj->GetBinError(bin);
      if (yp-5*ep < 0) continue;
      // take only peak associated with pion, proton and deuteron
      xpiPd[nP] = xp;
      ypiPd[nP] = yp;
      total += yp;
      shift += (xp - ppiPd[nP])*yp; 
      nP++;
      if (nP == 3) break;
    }
  }   
  if (nP > 0) {
    tots[0] = ypiPd[0];
    tots[1] = ypiPd[1];
    tots[5] = ypiPd[2];
    shift = shift/total;
    Double_t xpi = ppiPd[0] + shift;
    g2->SetParameters(1, xpi);
    for (Int_t l = 1; l < 5; l++) {
      if (tots[l] > 0) {
	frac[l] = tots[l]/total;
      }
      Double_t phi = TMath::ASin(TMath::Sqrt(frac[l]));
      g2->FixParameter(l+2, phi);
    }
  } else {// nP == 0 
    g2->SetParameters(1, proj->GetMean());
  }
  g2->SetParameter(2, 0.01);
  total = proj->Integral()*proj->GetBinWidth(5);
  g2->FixParameter(11, total);
  return kTRUE;
}
//________________________________________________________________________________
TF1 *FitG4EY(TH1 *proj, Option_t *opt="RM", Int_t IO = 0, Int_t Sign = 2) {
  // fit in momentum range p = 0.526 +/- 0.05;
  if (! proj) return 0;
  TString Opt(opt);
  //  Bool_t quet = Opt.Contains("Q",TString::kIgnoreCase);
  TF1 *g2 = (TF1*) gROOT->GetFunction("G4EY");
  if (! g2) {
#if defined(__ELOSS__) || defined(__SCALE__)
    g2 = new TF1("G4EY",gf4EYFunc, -5, 5, 17);
#else
    g2 = new TF1("G4EY",gf4EYFunc, -5, 5, 16);
#endif
    g2->SetParName(0,"norm");      g2->SetParLimits(0,-0.6,0.6); // g2->FixParameter(0,0.0); // 
    g2->SetParName(1,"mu");        g2->SetParLimits(1,-1.2,0.4);				     
    g2->SetParName(2,"Sigma");     g2->FixParameter(2,0.0); // g2->SetParLimits(2,-0.1,0.1);	     
    g2->SetParName(3,"P");         g2->SetParLimits(3,0.0,1.2); // TMath::Pi()/2);		     
    g2->SetParName(4,"K");         g2->SetParLimits(4,0.0,1.0); // TMath::Pi()/2); 	     
    g2->SetParName(5,"e");         g2->SetParLimits(5,0.0,TMath::Pi()/2);			     
    g2->SetParName(6,"d");         g2->FixParameter(6,0.0); //g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(7,"muon");      g2->FixParameter(7,0.0); //g2->SetParLimits(7,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(8,"triton");    g2->FixParameter(8,0.0); //g2->SetParLimits(8,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(9,"He3");       g2->FixParameter(9,0.0); //g2->SetParLimits(9,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(10,"alpha");    g2->FixParameter(10,0.0); //g2->SetParLimits(10,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(11,"Total");
    g2->SetParName(12,"Case");
    g2->SetParName(13,"occupancy");g2->FixParameter(13,0); //g2->SetParLimits(13,0.0,0.25);
    g2->SetParName(14,"IO");       g2->FixParameter(14,IO); 
    g2->SetParName(15,"sign");     g2->FixParameter(15,Sign); 
#ifdef __ELOSS__
    g2->SetParName(16,"eLoss"); g2->FixParameter(16,0.0); g2->SetParLimits(16,-0.01,0.01);
#else
#ifdef __SCALE__
    g2->SetParName(16,"Scale"); g2->FixParameter(16,0.0); g2->SetParLimits(16,-0.1,0.1);
#endif
#endif
    //    g2->SetParName(15,"factor"); g2->SetParLimits(15,-.1,0.1);
  }
  PreSetParametersY(proj, g2);
  //  g2->ReleaseParameter(2);  g2->SetParLimits(2,-0.1,0.1);
  g2->ReleaseParameter(3);  g2->SetParLimits(3,0.0,TMath::Pi()/2);
  g2->FixParameter(4,0.01); 
  g2->FixParameter(5,0.01);
  g2->FixParameter(6,0.00);
  g2->FixParameter(7,0.00);
  g2->FixParameter(8,0.00);
  g2->FixParameter(9,0.00);
  g2->FixParameter(10,0.0);
  g2->FixParameter(12,-1);
  g2->FixParameter(13,0.);
  g2->FixParameter(14,IO); 
  g2->FixParameter(15,Sign); 
#if defined(__ELOSS__) || defined(__SCALE__)
  g2->FixParameter(16,0.0);
#endif
  //  Fit pion + proton 
  proj->Fit(g2,Opt.Data());
  //  g2->ReleaseParameter(2);
  g2->ReleaseParameter(4);     g2->SetParLimits(4,0.0,TMath::Pi()/2); 
  g2->ReleaseParameter(5);     g2->SetParLimits(5,0.0,TMath::Pi()/2);	
  if (Sign > 0) {
    g2->ReleaseParameter(6);     g2->SetParLimits(6,0.0,TMath::Pi()/2);
  }
  //  Fit pion + proton + K + e + d
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
#if defined(__ELOSS__) || defined(__SCALE__)
  g2->ReleaseParameter(16); g2->SetParLimits(16,-0.1,0.1);
  iok = proj->Fit(g2,Opt.Data());
#endif
  //  g2->ReleaseParameter(7);     g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 
  //  g2->ReleaseParameter(8);     g2->SetParLimits(8,0.0,TMath::Pi()/2);		
  //  g2->ReleaseParameter(9);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  g2->ReleaseParameter(10);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  Fit pion + proton + K + e + d + mu + triton // + He3 + alpha
  //  iok = proj->Fit(g2,Opt.Data());
  // + occupancy
  g2->ReleaseParameter(13); g2->SetParLimits(13,0.0,0.15);
  iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  Int_t fixit = 0;
  for (Int_t p = 3; p <= 10; p++) {
    Double_t par = g2->GetParameter(p);
    if (par != 0) {
      Double_t dpar = g2->GetParError(p);
      if (TMath::Abs(par) < dpar) {
	fixit++;
	g2->FixParameter(p, 0);
      }
    }
  }
  if (fixit) iok = proj->Fit(g2,Opt.Data());
  if (! Opt.Contains("q",TString::kIgnoreCase)) {
    Double_t params[20];
    g2->GetParameters(params);
    Double_t X = params[1];
    Double_t Y = TMath::Exp(params[0]);
    TPolyMarker *pm = new TPolyMarker(1, &X, &Y);
    proj->GetListOfFunctions()->Add(pm);
    pm->SetMarkerStyle(23);
    pm->SetMarkerColor(kRed);
    pm->SetMarkerSize(1.3);
    Double_t occupancy = params[13];
    for (int i = 0; i < Npart4EY; i++) {
      Double_t frac = 1.0;
      if (i > 0) frac = TMath::Power(TMath::Sin(g2->GetParameter(2+i)), 2);
      if (frac < 1e-3) continue;
      TF1 *f = new TF1(*g2);
      static const Char_t *pnames[9] = {"pion", "P", "K", "e", "d", "muon", "t", "He3", "alpha"};
      f->SetName(pnames[i]);
      f->FixParameter(12,i);
      f->SetLineColor(i+2);
      proj->GetListOfFunctions()->Add(f);
      if (occupancy > 0) {
	TF1 *f = new TF1(*g2);
	f->SetName(TString(Peaks[i].Name)+"C");
	f->FixParameter(12,i);
	f->FixParameter(13,0);
	f->SetLineColor(i+2);
	f->SetLineStyle(2);
	proj->GetListOfFunctions()->Add(f);
      }
    }
    proj->Draw();
  }
  return g2;
}

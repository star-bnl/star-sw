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

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
  // par[16] - eLoss
  Double_t mu    = par[1];
  Double_t sigma = par[2];
  Double_t occupancy = par[13];
  //#define __ELOSS__
#ifdef __ELOSS__
  Double_t eLoss = par[16];
#else 
  //#define __SCALE__
#ifdef __SCALE__
  Double_t scale = 1 + par[16];
  XX[0] *= scale;
#endif /* __SCALE__ */
#endif /* __ELOSS__ */
  Int_t IO = par[14];
  Int_t sign = par[15];
  Double_t frac[9];
  Double_t ff[9] = {0};
  for (Int_t i = 1; i < 9; i++) {
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
  if (frac[0] < 0.4 && frac[1] < 0.4) return 0;
  // /hlt/cephfs/fisyak/TpcRS_2019.Ideal/dEdx/Fit  
  //                          P  S IO
  static MIPFitParX_t parMIPs[54][3][2] = {
    {{
	// particle, norml, mu, sigma, alpha
	{ 0,-1, 0, 0,"zIpionN"           ,    8.82230,    0.40860,    0.31662,    0.87089},
	{ 0,-1, 0, 1,"zOpionN"           ,    8.31910,    0.91721,    0.29593,    0.91399} },{
	{ 0,-1, 1, 0,"zIpionP"           ,    8.77634,    0.40914,    0.32013,    0.88385},
	{ 0,-1, 1, 1,"zOpionP"           ,    8.27099,    0.91954,    0.29654,    0.91618} },{
	{ 0,-1, 2, 0,"zIpion"            ,    9.49298,    0.40883,    0.31836,    0.87680},
	{ 0,-1, 2, 1,"zOpion"            ,    8.98905,    0.91812,    0.29621,    0.91230} }},{{ // 0

	{ 1,-1, 0, 0,"zIprotonN"         ,    8.95346,    1.64948,    0.23670,    0.91720},
	{ 1,-1, 0, 1,"zOprotonN"         ,    8.43398,    2.15448,    0.22674,    0.93431} },{
	{ 1,-1, 1, 0,"zIprotonP"         ,    8.84263,    1.65351,    0.23304,    0.87960},
	{ 1,-1, 1, 1,"zOprotonP"         ,    8.33699,    2.15525,    0.22167,    0.89802} },{
	{ 1,-1, 2, 0,"zIproton"          ,    9.59286,    1.65141,    0.23505,    0.89917},
	{ 1,-1, 2, 1,"zOproton"          ,    9.08007,    2.15492,    0.22448,    0.91751} }},{{ // 1

	{ 2,-1, 0, 0,"zIkaonN"           ,    8.90829,    0.86411,    0.27819,    0.84274},
	{ 2,-1, 0, 1,"zOkaonN"           ,    8.39899,    1.36987,    0.25916,    0.91348} },{
	{ 2,-1, 1, 0,"zIkaonP"           ,    8.94351,    0.86845,    0.28004,    0.84783},
	{ 2,-1, 1, 1,"zOkaonP"           ,    8.45512,    1.37178,    0.25939,    0.91543} },{
	{ 2,-1, 2, 0,"zIkaon"            ,    9.61939,    0.86627,    0.27918,    0.84485},
	{ 2,-1, 2, 1,"zOkaon"            ,    9.12102,    1.37074,    0.25929,    0.91275} }},{{ // 2

	{ 3,-1, 0, 0,"zIelectronN"       ,    8.42747,    0.74766,    0.28159,    0.90009},
	{ 3,-1, 0, 1,"zOelectronN"       ,    7.93100,    1.25216,    0.26024,    0.95109} },{
	{ 3,-1, 1, 0,"zIelectronP"       ,    8.45032,    0.74757,    0.28753,    0.91711},
	{ 3,-1, 1, 1,"zOelectronP"       ,    7.96365,    1.25103,    0.26272,    0.96487} },{
	{ 3,-1, 2, 0,"zIelectron"        ,    9.13246,    0.74751,    0.28460,    0.90716},
	{ 3,-1, 2, 1,"zOelectron"        ,    8.64102,    1.25149,    0.26162,    0.95672} }},{{ // 3

	{ 4,-1, 1, 0,"zIdeuteronP"       ,    8.14847,    2.80197,    0.22448,    1.05826},
	{ 4,-1, 1, 1,"zOdeuteronP"       ,    8.10470,    3.33420,    0.20586,    1.31784} },{
	{ 4,-1, 1, 0,"zIdeuteronP"       ,    8.14847,    2.80197,    0.22448,    1.05826},
	{ 4,-1, 1, 1,"zOdeuteronP"       ,    8.10470,    3.33420,    0.20586,    1.31784} },{
	{ 4,-1, 2, 0,"zIdeuteron"        ,    8.14847,    2.80197,    0.22448,    1.05826},
	{ 4,-1, 2, 1,"zOdeuteron"        ,    8.10470,    3.33420,    0.20586,    1.31784} }},{{ // 4

	{ 5,-1, 0, 0,"zImuonN"           ,    8.80913,    0.41859,    0.31686,    0.88776},
	{ 5,-1, 0, 1,"zOmuonN"           ,    8.29543,    0.92490,    0.29119,    0.90324} },{
	{ 5,-1, 1, 0,"zImuonP"           ,    8.82135,    0.41859,    0.31770,    0.88569},
	{ 5,-1, 1, 1,"zOmuonP"           ,    8.32968,    0.92884,    0.29551,    0.90727} },{
	{ 5,-1, 2, 0,"zImuon"            ,    9.50865,    0.41846,    0.31729,    0.88559},
	{ 5,-1, 2, 1,"zOmuon"            ,    9.00627,    0.92666,    0.29333,    0.90275} }},{{ // 5

	{ 6,-1, 1, 0,"zItritonP"         ,    7.85307,    3.47623,    0.17079,    3.93668},
	{ 6,-1, 1, 1,"zOtritonP"         ,    7.61146,    3.86115,    0.15059,    3.69017} },{
	{ 6,-1, 1, 0,"zItritonP"         ,    7.85307,    3.47623,    0.17079,    3.93668},
	{ 6,-1, 1, 1,"zOtritonP"         ,    7.61146,    3.86115,    0.15059,    3.69017} },{
	{ 6,-1, 2, 0,"zItriton"          ,    7.85307,    3.47623,    0.17079,    3.93657},
	{ 6,-1, 2, 1,"zOtriton"          ,    7.61146,    3.86115,    0.15059,    3.69014} }},{{ // 6

	{ 7,-1, 1, 0,"zIHe3P"            ,    8.10501,    3.67301,    0.16604,    0.56829},
	{ 7,-1, 1, 1,"zOHe3P"            ,    7.51020,    4.05634,    0.16274,    0.59221} },{
	{ 7,-1, 1, 0,"zIHe3P"            ,    8.10501,    3.67301,    0.16604,    0.56829},
	{ 7,-1, 1, 1,"zOHe3P"            ,    7.51020,    4.05634,    0.16274,    0.59221} },{
	{ 7,-1, 2, 0,"zIHe3"             ,    8.10501,    3.67301,    0.16604,    0.56829},
	{ 7,-1, 2, 1,"zOHe3"             ,    7.51020,    4.05634,    0.16274,    0.59221} }},{{ // 7 

	{ 8,-1, 1, 0,"zIalphaP"          ,    7.75131,    4.04752,    0.20170,    0.78259},
	{ 8,-1, 1, 1,"zOalphaP"          ,    7.13555,    4.42423,    0.16480,    0.49419} },{
	{ 8,-1, 1, 0,"zIalphaP"          ,    7.75131,    4.04752,    0.20170,    0.78259},
	{ 8,-1, 1, 1,"zOalphaP"          ,    7.13555,    4.42423,    0.16480,    0.49419} },{
	{ 8,-1, 2, 0,"zIalpha"           ,    7.75131,    4.04752,    0.20170,    0.78259},
	{ 8,-1, 2, 1,"zOalpha"           ,    7.13555,    4.42423,    0.16480,    0.49419} }},{{ // 8 

	{ 0, 0, 0, 0,"zIpionN+zIpionN"   ,    8.82332,    1.14944,    0.23929,    0.78826},
	{ 0, 0, 0, 1,"zOpionN+zOpionN"   ,    8.31694,    1.66303,    0.22934,    0.89826} },{
	{ 0, 0, 1, 0,"zIpionP+zIpionP"   ,    8.77703,    1.15338,    0.24478,    0.81557},
	{ 0, 0, 1, 1,"zOpionP+zOpionP"   ,    8.26892,    1.66537,    0.23015,    0.90317} },{
	{ 0, 0, 2, 0,"zIpion+zIpion"     ,    9.49372,    1.15292,    0.24279,    0.80513},
	{ 0, 0, 2, 1,"zOpion+zOpion"     ,    8.98618,    1.66260,    0.22773,    0.88468} }},{{ // 9

	{ 1, 0, 0, 0,"zIprotonN+zIpionN" ,    8.95462,    1.93996,    0.20864,    0.88412},
	{ 1, 0, 0, 1,"zOprotonN+zOpionN" ,    8.43600,    2.44144,    0.19666,    0.90879} },{
	{ 1, 0, 1, 0,"zIprotonP+zIpionP" ,    8.84362,    1.94280,    0.20666,    0.86024},
	{ 1, 0, 1, 1,"zOprotonP+zOpionP" ,    8.33840,    2.44399,    0.19501,    0.88449} },{
	{ 1, 0, 2, 0,"zIproton+zIpion"   ,    9.59394,    1.94188,    0.20794,    0.87508},
	{ 1, 0, 2, 1,"zOproton+zOpion"   ,    9.08166,    2.44349,    0.19762,    0.90648} }},{{ // 10

	{ 1, 1, 0, 0,"zIprotonN+zIprotonN",    8.95616,    2.38181,    0.18810,    0.93265},
	{ 1, 1, 0, 1,"zOprotonN+zOprotonN",    8.43829,    2.87757,    0.17369,    0.88364} },{
	{ 1, 1, 1, 0,"zIprotonP+zIprotonP",    8.84535,    2.38555,    0.18456,    0.88993},
	{ 1, 1, 1, 1,"zOprotonP+zOprotonP",    8.34007,    2.88113,    0.17118,    0.85414} },{
	{ 1, 1, 2, 0,"zIproton+zIproton" ,    9.59525,    2.38377,    0.18596,    0.91016},
	{ 1, 1, 2, 1,"zOproton+zOproton" ,    9.08333,    2.87956,    0.17324,    0.88116} }},{{

	{ 2, 0, 0, 0,"zIkaonN+zIpionN"   ,    8.90979,    1.40486,    0.22857,    0.80828},
	{ 2, 0, 0, 1,"zOkaonN+zOpionN"   ,    8.39698,    1.90864,    0.21299,    0.89135} },{
	{ 2, 0, 1, 0,"zIkaonP+zIpionP"   ,    8.94504,    1.40642,    0.23070,    0.81684},
	{ 2, 0, 1, 1,"zOkaonP+zOpionP"   ,    8.45458,    1.90978,    0.21412,    0.90003} },{
	{ 2, 0, 2, 0,"zIkaon+zIpion"     ,    9.62069,    1.40501,    0.22952,    0.81192},
	{ 2, 0, 2, 1,"zOkaon+zOpion"     ,    9.11987,    1.91202,    0.21506,    0.91888} }},{{

	{ 2, 1, 0, 0,"zIkaonN+zIprotonN" ,    8.90768,    2.06487,    0.20051,    0.88450},
	{ 2, 1, 0, 1,"zOkaonN+zOprotonN" ,    8.39662,    2.56501,    0.18775,    0.90208} },{
	{ 2, 1, 1, 0,"zIkaonP+zIprotonP" ,    8.94254,    2.06816,    0.19736,    0.84033},
	{ 2, 1, 1, 1,"zOkaonP+zOprotonP" ,    8.45536,    2.56674,    0.18466,    0.87727} },{
	{ 2, 1, 2, 0,"zIkaon+zIproton"   ,    9.61851,    2.06657,    0.19985,    0.87046},
	{ 2, 1, 2, 1,"zOkaon+zOproton"   ,    9.12073,    2.56525,    0.18642,    0.89802} }},{{

	{ 2, 2, 0, 0,"zIkaonN+zIkaonN"   ,    8.90871,    1.60669,    0.21807,    0.81590},
	{ 2, 2, 0, 1,"zOkaonN+zOkaonN"   ,    8.39813,    2.10886,    0.20210,    0.91649} },{
	{ 2, 2, 1, 0,"zIkaonP+zIkaonP"   ,    8.94474,    1.60968,    0.21776,    0.80325},
	{ 2, 2, 1, 1,"zOkaonP+zOkaonP"   ,    8.45406,    2.11298,    0.20361,    0.92267} },{
	{ 2, 2, 2, 0,"zIkaon+zIkaon"     ,    9.62031,    1.61002,    0.21884,    0.82087},
	{ 2, 2, 2, 1,"zOkaon+zOkaon"     ,    9.11947,    2.11027,    0.20296,    0.91549} }},{{

	{ 3, 0, 0, 0,"zIelectronN+zIpionN",    8.42890,    1.33375,    0.22874,    0.83202},
	{ 3, 0, 0, 1,"zOelectronN+zOpionN",    7.92964,    1.84223,    0.21605,    0.93743} },{
	{ 3, 0, 1, 0,"zIelectronP+zIpionP",    8.45133,    1.33492,    0.23364,    0.86809},
	{ 3, 0, 1, 1,"zOelectronP+zOpionP",    7.96452,    1.84181,    0.21699,    0.93985} },{
	{ 3, 0, 2, 0,"zIelectron+zIpion" ,    9.13387,    1.33467,    0.23217,    0.85499},
	{ 3, 0, 2, 1,"zOelectron+zOpion" ,    8.64085,    1.84050,    0.21647,    0.94581} }},{{

	{ 3, 1, 0, 0,"zIelectronN+zIprotonN",    8.42667,    2.02890,    0.20303,    0.92604},
	{ 3, 1, 0, 1,"zOelectronN+zOprotonN",    7.92999,    2.52873,    0.18940,    0.92182} },{
	{ 3, 1, 1, 0,"zIelectronP+zIprotonP",    8.44852,    2.03186,    0.20269,    0.89901},
	{ 3, 1, 1, 1,"zOelectronP+zOprotonP",    7.96377,    2.52884,    0.18666,    0.89282} },{
	{ 3, 1, 2, 0,"zIelectron+zIproton",    9.13114,    2.02926,    0.20164,    0.90153},
	{ 3, 1, 2, 1,"zOelectron+zOproton",    8.64052,    2.52881,    0.18772,    0.91454} }},{{

	{ 3, 2, 0, 0,"zIelectronN+zIkaonN",    8.42928,    1.55001,    0.21995,    0.85663},
	{ 3, 2, 0, 1,"zOelectronN+zOkaonN",    7.93135,    2.05261,    0.20450,    0.95691} },{
	{ 3, 2, 1, 0,"zIelectronP+zIkaonP",    8.45071,    1.54974,    0.22141,    0.84550},
	{ 3, 2, 1, 1,"zOelectronP+zOkaonP",    7.96263,    2.05258,    0.20154,    0.91593} },{
	{ 3, 2, 2, 0,"zIelectron+zIkaon" ,    9.13363,    1.55037,    0.22162,    0.85151},
	{ 3, 2, 2, 1,"zOelectron+zOkaon" ,    8.64027,    2.05354,    0.20431,    0.94475} }},{{

	{ 3, 3, 0, 0,"zIelectronN+zIelectronN",    8.42782,    1.48993,    0.22246,    0.89431},
	{ 3, 3, 0, 1,"zOelectronN+zOelectronN",    7.92805,    1.99485,    0.20487,    0.98795} },{
	{ 3, 3, 1, 0,"zIelectronP+zIelectronP",    8.45032,    1.48926,    0.22373,    0.89899},
	{ 3, 3, 1, 1,"zOelectronP+zOelectronP",    7.96273,    1.99093,    0.20594,    0.99147} },{
	{ 3, 3, 2, 0,"zIelectron+zIelectron",    9.13265,    1.48888,    0.22162,    0.88330},
	{ 3, 3, 2, 1,"zOelectron+zOelectron",    8.64009,    1.99262,    0.20599,    0.98485} }},{{

	{ 4, 0, 1, 0,"zIdeuteronP+zIpionP",    8.14809,    2.90598,    0.21023,    1.07111},
	{ 4, 0, 1, 1,"zOdeuteronP+zOpionP",    8.10928,    3.43426,    0.19139,    1.23429} },{
	{ 4, 0, 1, 0,"zIdeuteronP+zIpionP",    8.14809,    2.90598,    0.21023,    1.07111},
	{ 4, 0, 1, 1,"zOdeuteronP+zOpionP",    8.10928,    3.43426,    0.19139,    1.23429} },{
	{ 4, 0, 2, 0,"zIdeuteron+zIpion" ,    8.14634,    2.91111,    0.21248,    1.10196},
	{ 4, 0, 2, 1,"zOdeuteron+zOpion" ,    8.10783,    3.43474,    0.19170,    1.26404} }},{{

	{ 4, 1, 1, 0,"zIdeuteronP+zIprotonP",    8.14985,    3.10759,    0.18969,    1.06963},
	{ 4, 1, 1, 1,"zOdeuteronP+zOprotonP",    8.10963,    3.62554,    0.17356,    1.19750} },{
	{ 4, 1, 1, 0,"zIdeuteronP+zIprotonP",    8.14985,    3.10759,    0.18969,    1.06963},
	{ 4, 1, 1, 1,"zOdeuteronP+zOprotonP",    8.10963,    3.62554,    0.17356,    1.19750} },{
	{ 4, 1, 2, 0,"zIdeuteron+zIproton",    8.15057,    3.10666,    0.19095,    1.06154},
	{ 4, 1, 2, 1,"zOdeuteron+zOproton",    8.10959,    3.62566,    0.17443,    1.22044} }},{{

	{ 4, 2, 1, 0,"zIdeuteronP+zIkaonP",    8.14967,    2.95927,    0.20362,    1.03153},
	{ 4, 2, 1, 1,"zOdeuteronP+zOkaonP",    8.11011,    3.48273,    0.18500,    1.21216} },{
	{ 4, 2, 1, 0,"zIdeuteronP+zIkaonP",    8.14967,    2.95927,    0.20362,    1.03153},
	{ 4, 2, 1, 1,"zOdeuteronP+zOkaonP",    8.11011,    3.48273,    0.18500,    1.21216} },{
	{ 4, 2, 2, 0,"zIdeuteron+zIkaon" ,    8.15108,    2.95935,    0.20551,    1.04816},
	{ 4, 2, 2, 1,"zOdeuteron+zOkaon" ,    8.10588,    3.48444,    0.18533,    1.27366} }},{{

	{ 4, 3, 1, 0,"zIdeuteronP+zIelectronP",    8.14988,    2.94011,    0.20503,    1.05259},
	{ 4, 3, 1, 1,"zOdeuteronP+zOelectronP",    8.10560,    3.46555,    0.18765,    1.28975} },{
	{ 4, 3, 1, 0,"zIdeuteronP+zIelectronP",    8.14988,    2.94011,    0.20503,    1.05259},
	{ 4, 3, 1, 1,"zOdeuteronP+zOelectronP",    8.10560,    3.46555,    0.18765,    1.28975} },{
	{ 4, 3, 2, 0,"zIdeuteron+zIelectron",    8.15130,    2.93962,    0.20241,    1.01392},
	{ 4, 3, 2, 1,"zOdeuteron+zOelectron",    8.10694,    3.46672,    0.18601,    1.26609} }},{{

	{ 4, 4, 1, 0,"zIdeuteronP+zIdeuteronP",    8.14039,    3.54822,    0.19636,    1.57550},
	{ 4, 4, 1, 1,"zOdeuteronP+zOdeuteronP",    8.10809,    4.05298,    0.16269,    1.59118} },{
	{ 4, 4, 1, 0,"zIdeuteronP+zIdeuteronP",    8.14039,    3.54822,    0.19636,    1.57550},
	{ 4, 4, 1, 1,"zOdeuteronP+zOdeuteronP",    8.10809,    4.05298,    0.16269,    1.59118} },{
	{ 4, 4, 2, 0,"zIdeuteron+zIdeuteron",    8.14687,    3.53863,    0.18984,    1.31438},
	{ 4, 4, 2, 1,"zOdeuteron+zOdeuteron",    8.10922,    4.05291,    0.16140,    1.54936} }},{{

	{ 5, 0, 0, 0,"zImuonN+zIpionN"   ,    8.80962,    1.15529,    0.23975,    0.80405},
	{ 5, 0, 0, 1,"zOmuonN+zOpionN"   ,    8.29295,    1.66513,    0.22733,    0.87297} },{
	{ 5, 0, 1, 0,"zImuonP+zIpionP"   ,    8.82244,    1.15769,    0.24442,    0.82124},
	{ 5, 0, 1, 1,"zOmuonP+zOpionP"   ,    8.32705,    1.66778,    0.22619,    0.87429} },{
	{ 5, 0, 2, 0,"zImuon+zIpion"     ,    9.50907,    1.15748,    0.24267,    0.81167},
	{ 5, 0, 2, 1,"zOmuon+zOpion"     ,    9.00331,    1.66489,    0.22610,    0.87181} }},{{

	{ 5, 1, 0, 0,"zImuonN+zIprotonN" ,    8.80493,    1.94315,    0.20941,    0.89699},
	{ 5, 1, 0, 1,"zOmuonN+zOprotonN" ,    8.29016,    2.44378,    0.19793,    0.91342} },{
	{ 5, 1, 1, 0,"zImuonP+zIprotonP" ,    8.81876,    1.94642,    0.20786,    0.87165},
	{ 5, 1, 1, 1,"zOmuonP+zOprotonP" ,    8.32560,    2.44651,    0.19604,    0.88706} },{
	{ 5, 1, 2, 0,"zImuon+zIproton"   ,    9.50527,    1.94373,    0.20800,    0.88433},
	{ 5, 1, 2, 1,"zOmuon+zOproton"   ,    9.00070,    2.44609,    0.19619,    0.90395} }},{{

	{ 5, 2, 0, 0,"zImuonN+zIkaonN"   ,    8.80851,    1.40924,    0.23082,    0.83251},
	{ 5, 2, 0, 1,"zOmuonN+zOkaonN"   ,    8.29182,    1.91142,    0.21298,    0.90115} },{
	{ 5, 2, 1, 0,"zImuonP+zIkaonP"   ,    8.82123,    1.41161,    0.23046,    0.82108},
	{ 5, 2, 1, 1,"zOmuonP+zOkaonP"   ,    8.28391,    1.96136,    0.24556,    4.97319} },{
	{ 5, 2, 2, 0,"zImuon+zIkaon"     ,    9.50831,    1.40962,    0.22932,    0.82062},
	{ 5, 2, 2, 1,"zOmuon+zOkaon"     ,    9.00151,    1.91136,    0.21223,    0.88346} }},{{

	{ 5, 3, 0, 0,"zImuonN+zIelectronN",    8.80828,    1.33854,    0.22921,    0.85626},
	{ 5, 3, 0, 1,"zOmuonN+zOelectronN",    8.29062,    1.84608,    0.21505,    0.95403} },{
	{ 5, 3, 1, 0,"zImuonP+zIelectronP",    8.82082,    1.33837,    0.23266,    0.86871},
	{ 5, 3, 1, 1,"zOmuonP+zOelectronP",    8.32566,    1.84493,    0.21502,    0.93147} },{
	{ 5, 3, 2, 0,"zImuon+zIelectron" ,    9.50823,    1.33915,    0.23145,    0.85785},
	{ 5, 3, 2, 1,"zOmuon+zOelectron" ,    9.00135,    1.84409,    0.21453,    0.93462} }},{{

	{ 5, 4, 1, 0,"zImuonP+zIdeuteronP",    8.79988,    2.90876,    0.20998,    1.07284},
	{ 5, 4, 1, 1,"zOmuonP+zOdeuteronP",    8.31941,    3.43367,    0.19146,    1.22857} },{
	{ 5, 4, 1, 0,"zImuonP+zIdeuteronP",    8.79988,    2.90876,    0.20998,    1.07284},
	{ 5, 4, 1, 1,"zOmuonP+zOdeuteronP",    8.31941,    3.43367,    0.19146,    1.22857} },{
	{ 5, 4, 2, 0,"zImuon+zIdeuteron" ,    9.48104,    2.91424,    0.21414,    1.16961},
	{ 5, 4, 2, 1,"zOmuon+zOdeuteron" ,    8.99469,    3.43551,    0.19257,    1.27943} }},{{

	{ 5, 5, 0, 0,"zImuonN+zImuonN"   ,    8.80950,    1.16106,    0.24167,    0.82116},
	{ 5, 5, 0, 1,"zOmuonN+zOmuonN"   ,    8.29263,    1.66993,    0.22331,    0.87467} },{
	{ 5, 5, 1, 0,"zImuonP+zImuonP"   ,    8.82208,    1.16521,    0.24407,    0.83042},
	{ 5, 5, 1, 1,"zOmuonP+zOmuonP"   ,    8.32683,    1.67085,    0.22523,    0.86422} },{
	{ 5, 5, 2, 0,"zImuon+zImuon"     ,    9.50939,    1.16143,    0.24162,    0.81826},
	{ 5, 5, 2, 1,"zOmuon+zOmuon"     ,    9.00354,    1.67179,    0.22553,    0.87376} }},{{

	{ 6, 0, 1, 0,"zItritonP+zIpionP" ,    7.85467,    3.53203,    0.16713,    3.24286},
	{ 6, 0, 1, 1,"zOtritonP+zOpionP" ,    7.61438,    3.92388,    0.14655,    2.69059} },{
	{ 6, 0, 1, 0,"zItritonP+zIpionP" ,    7.85467,    3.53203,    0.16713,    3.24286},
	{ 6, 0, 1, 1,"zOtritonP+zOpionP" ,    7.61438,    3.92388,    0.14655,    2.69059} },{
	{ 6, 0, 2, 0,"zItriton+zIpion"   ,    7.85487,    3.53231,    0.16707,    2.94602},
	{ 6, 0, 2, 1,"zOtriton+zOpion"   ,    7.61455,    3.92112,    0.14701,    2.65630} }},{{

	{ 6, 1, 1, 0,"zItritonP+zIprotonP",    7.85555,    3.64405,    0.15798,    2.26726},
	{ 6, 1, 1, 1,"zOtritonP+zOprotonP",    7.61496,    4.04497,    0.13823,    1.85156} },{
	{ 6, 1, 1, 0,"zItritonP+zIprotonP",    7.85555,    3.64405,    0.15798,    2.26726},
	{ 6, 1, 1, 1,"zOtritonP+zOprotonP",    7.61496,    4.04497,    0.13823,    1.85156} },{
	{ 6, 1, 2, 0,"zItriton+zIproton" ,    7.85590,    3.64310,    0.15776,    2.26428},
	{ 6, 1, 2, 1,"zOtriton+zOproton" ,    7.61560,    4.04395,    0.13915,    1.96592} }},{{

	{ 6, 2, 1, 0,"zItritonP+zIkaonP" ,    7.85543,    3.56007,    0.16422,    2.55484},
	{ 6, 2, 1, 1,"zOtritonP+zOkaonP" ,    7.61377,    3.95323,    0.14281,    2.50878} },{
	{ 6, 2, 1, 0,"zItritonP+zIkaonP" ,    7.85543,    3.56007,    0.16422,    2.55484},
	{ 6, 2, 1, 1,"zOtritonP+zOkaonP" ,    7.61377,    3.95323,    0.14281,    2.50878} },{
	{ 6, 2, 2, 0,"zItriton+zIkaon"   ,    7.85514,    3.56121,    0.16373,    2.55002},
	{ 6, 2, 2, 1,"zOtriton+zOkaon"   ,    7.61522,    3.95264,    0.14504,    2.48570} }},{{

	{ 6, 3, 1, 0,"zItritonP+zIelectronP",    7.85382,    3.55052,    0.16421,    3.19791},
	{ 6, 3, 1, 1,"zOtritonP+zOelectronP",    7.61500,    3.94261,    0.14428,    2.76834} },{
	{ 6, 3, 1, 0,"zItritonP+zIelectronP",    7.85382,    3.55052,    0.16421,    3.19791},
	{ 6, 3, 1, 1,"zOtritonP+zOelectronP",    7.61500,    3.94261,    0.14428,    2.76834} },{
	{ 6, 3, 2, 0,"zItriton+zIelectron",    7.85484,    3.55207,    0.16415,    3.31139},
	{ 6, 3, 2, 1,"zOtriton+zOelectron",    7.61456,    3.94131,    0.14435,    2.80470} }},{{

	{ 6, 4, 1, 0,"zItritonP+zIdeuteronP",    7.85534,    3.91330,    0.15295,    1.79867},
	{ 6, 4, 1, 1,"zOtritonP+zOdeuteronP",    7.61414,    4.34027,    0.13116,    1.91632} },{
	{ 6, 4, 1, 0,"zItritonP+zIdeuteronP",    7.85534,    3.91330,    0.15295,    1.79867},
	{ 6, 4, 1, 1,"zOtritonP+zOdeuteronP",    7.61414,    4.34027,    0.13116,    1.91632} },{
	{ 6, 4, 2, 0,"zItriton+zIdeuteron",    7.85297,    3.91283,    0.15369,    1.92693},
	{ 6, 4, 2, 1,"zOtriton+zOdeuteron",    7.61506,    4.34039,    0.13127,    2.01008} }},{{

	{ 6, 5, 1, 0,"zItritonP+zImuonP" ,    7.85459,    3.53431,    0.16656,    3.65253},
	{ 6, 5, 1, 1,"zOtritonP+zOmuonP" ,    7.61459,    3.92373,    0.14676,    3.27848} },{
	{ 6, 5, 1, 0,"zItritonP+zImuonP" ,    7.85459,    3.53431,    0.16656,    3.65253},
	{ 6, 5, 1, 1,"zOtritonP+zOmuonP" ,    7.61459,    3.92373,    0.14676,    3.27848} },{
	{ 6, 5, 2, 0,"zItriton+zImuon"   ,    7.85434,    3.53314,    0.16667,    3.60853},
	{ 6, 5, 2, 1,"zOtriton+zOmuon"   ,    7.61433,    3.92416,    0.14770,    3.25604} }},{{

	{ 6, 6, 1, 0,"zItritonP+zItritonP",    7.85353,    4.17549,    0.12347,    4.06358},
	{ 6, 6, 1, 1,"zOtritonP+zOtritonP",    7.61081,    4.55850,    0.10825,    3.85105} },{
	{ 6, 6, 1, 0,"zItritonP+zItritonP",    7.85353,    4.17549,    0.12347,    4.06358},
	{ 6, 6, 1, 1,"zOtritonP+zOtritonP",    7.61081,    4.55850,    0.10825,    3.85105} },{
	{ 6, 6, 2, 0,"zItriton+zItriton" ,    7.85217,    4.17534,    0.12292,    4.02091},
	{ 6, 6, 2, 1,"zOtriton+zOtriton" ,    7.61158,    4.55916,    0.10885,    3.82421} }},{{

	{ 7, 0, 1, 0,"zIHe3P+zIpionP"    ,    8.10516,    3.71948,    0.16156,    0.57063},
	{ 7, 0, 1, 1,"zOHe3P+zOpionP"    ,    7.52076,    4.10623,    0.15762,    0.59735} },{
	{ 7, 0, 1, 0,"zIHe3P+zIpionP"    ,    8.10516,    3.71948,    0.16156,    0.57063},
	{ 7, 0, 1, 1,"zOHe3P+zOpionP"    ,    7.52076,    4.10623,    0.15762,    0.59735} },{
	{ 7, 0, 2, 0,"zIHe3+zIpion"      ,    8.10599,    3.71933,    0.16194,    0.57093},
	{ 7, 0, 2, 1,"zOHe3+zOpion"      ,    7.52207,    4.10780,    0.15817,    0.59120} }},{{

	{ 7, 1, 1, 0,"zIHe3P+zIprotonP"  ,    8.10942,    3.81400,    0.15437,    0.57026},
	{ 7, 1, 1, 1,"zOHe3P+zOprotonP"  ,    7.52260,    4.21209,    0.15042,    0.60595} },{
	{ 7, 1, 1, 0,"zIHe3P+zIprotonP"  ,    8.10942,    3.81400,    0.15437,    0.57026},
	{ 7, 1, 1, 1,"zOHe3P+zOprotonP"  ,    7.52260,    4.21209,    0.15042,    0.60595} },{
	{ 7, 1, 2, 0,"zIHe3+zIproton"    ,    8.10851,    3.81531,    0.15527,    0.58243},
	{ 7, 1, 2, 1,"zOHe3+zOproton"    ,    7.52164,    4.21181,    0.14865,    0.60549} }},{{

	{ 7, 2, 1, 0,"zIHe3P+zIkaonP"    ,    8.10684,    3.74482,    0.16122,    0.58312},
	{ 7, 2, 1, 1,"zOHe3P+zOkaonP"    ,    7.52155,    4.12911,    0.15479,    0.59277} },{
	{ 7, 2, 1, 0,"zIHe3P+zIkaonP"    ,    8.10684,    3.74482,    0.16122,    0.58312},
	{ 7, 2, 1, 1,"zOHe3P+zOkaonP"    ,    7.52155,    4.12911,    0.15479,    0.59277} },{
	{ 7, 2, 2, 0,"zIHe3+zIkaon"      ,    8.10837,    3.74219,    0.15885,    0.56858},
	{ 7, 2, 2, 1,"zOHe3+zOkaon"      ,    7.52076,    4.12901,    0.15237,    0.58589} }},{{

	{ 7, 3, 1, 0,"zIHe3P+zIelectronP",    8.10698,    3.73500,    0.15979,    0.56560},
	{ 7, 3, 1, 1,"zOHe3P+zOelectronP",    7.52009,    4.12389,    0.15599,    0.60091} },{
	{ 7, 3, 1, 0,"zIHe3P+zIelectronP",    8.10698,    3.73500,    0.15979,    0.56560},
	{ 7, 3, 1, 1,"zOHe3P+zOelectronP",    7.52009,    4.12389,    0.15599,    0.60091} },{
	{ 7, 3, 2, 0,"zIHe3+zIelectron"  ,    8.10783,    3.73223,    0.15799,    0.56249},
	{ 7, 3, 2, 1,"zOHe3+zOelectron"  ,    7.52153,    4.12244,    0.15469,    0.58718} }},{{

	{ 7, 4, 1, 0,"zIHe3P+zIdeuteronP",    8.11090,    4.05751,    0.15311,    0.64889},
	{ 7, 4, 1, 1,"zOHe3P+zOdeuteronP",    7.52720,    4.47686,    0.14054,    0.67256} },{
	{ 7, 4, 1, 0,"zIHe3P+zIdeuteronP",    8.11090,    4.05751,    0.15311,    0.64889},
	{ 7, 4, 1, 1,"zOHe3P+zOdeuteronP",    7.52720,    4.47686,    0.14054,    0.67256} },{
	{ 7, 4, 2, 0,"zIHe3+zIdeuteron"  ,    8.11146,    4.05742,    0.15283,    0.64992},
	{ 7, 4, 2, 1,"zOHe3+zOdeuteron"  ,    7.53040,    4.47633,    0.14089,    0.66297} }},{{

	{ 7, 5, 1, 0,"zIHe3P+zImuonP"    ,    8.10745,    3.71839,    0.16094,    0.56424},
	{ 7, 5, 1, 1,"zOHe3P+zOmuonP"    ,    7.52243,    4.10725,    0.15815,    0.59023} },{
	{ 7, 5, 1, 0,"zIHe3P+zImuonP"    ,    8.10745,    3.71839,    0.16094,    0.56424},
	{ 7, 5, 1, 1,"zOHe3P+zOmuonP"    ,    7.52243,    4.10725,    0.15815,    0.59023} },{
	{ 7, 5, 2, 0,"zIHe3+zImuon"      ,    8.10524,    3.71831,    0.16099,    0.56517},
	{ 7, 5, 2, 1,"zOHe3+zOmuon"      ,    7.52125,    4.10933,    0.15878,    0.60737} }},{{

	{ 7, 6, 1, 0,"zIHe3P+zItritonP"  ,    8.11217,    4.29615,    0.13254,    0.71427},
	{ 7, 6, 1, 1,"zOHe3P+zOtritonP"  ,    7.52130,    4.67445,    0.12397,    0.74305} },{
	{ 7, 6, 1, 0,"zIHe3P+zItritonP"  ,    8.11217,    4.29615,    0.13254,    0.71427},
	{ 7, 6, 1, 1,"zOHe3P+zOtritonP"  ,    7.52130,    4.67445,    0.12397,    0.74305} },{
	{ 7, 6, 2, 0,"zIHe3+zItriton"    ,    8.11212,    4.29690,    0.13353,    0.72461},
	{ 7, 6, 2, 1,"zOHe3+zOtriton"    ,    7.52409,    4.67441,    0.12267,    0.72101} }},{{

	{ 7, 7, 1, 0,"zIHe3P+zIHe3P"     ,    8.15874,    4.41230,    0.14149,    0.48919},
	{ 7, 7, 1, 1,"zOHe3P+zOHe3P"     ,    7.77743,    4.77727,    0.13051,    0.32596} },{
	{ 7, 7, 1, 0,"zIHe3P+zIHe3P"     ,    8.15874,    4.41230,    0.14149,    0.48919},
	{ 7, 7, 1, 1,"zOHe3P+zOHe3P"     ,    7.77743,    4.77727,    0.13051,    0.32596} },{
	{ 7, 7, 2, 0,"zIHe3+zIHe3"       ,    8.15950,    4.41305,    0.14001,    0.48624},
	{ 7, 7, 2, 1,"zOHe3+zOHe3"       ,    7.69479,    4.79111,    0.13981,    0.42359} }},{{

	{ 8, 0, 1, 0,"zIalphaP+zIpionP"  ,    7.75834,    4.07343,    0.19270,    0.73008},
	{ 8, 0, 1, 1,"zOalphaP+zOpionP"  ,    7.17470,    4.45906,    0.16219,    0.48185} },{
	{ 8, 0, 1, 0,"zIalphaP+zIpionP"  ,    7.75834,    4.07343,    0.19270,    0.73008},
	{ 8, 0, 1, 1,"zOalphaP+zOpionP"  ,    7.17470,    4.45906,    0.16219,    0.48185} },{
	{ 8, 0, 2, 0,"zIalpha+zIpion"    ,    7.75517,    4.08414,    0.20034,    0.79492},
	{ 8, 0, 2, 1,"zOalpha+zOpion"    ,    7.18891,    4.45503,    0.15816,    0.44799} }},{{

	{ 8, 1, 1, 0,"zIalphaP+zIprotonP",    7.76328,    4.14080,    0.18314,    0.72478},
	{ 8, 1, 1, 1,"zOalphaP+zOprotonP",    7.19974,    4.52896,    0.15056,    0.44389} },{
	{ 8, 1, 1, 0,"zIalphaP+zIprotonP",    7.76328,    4.14080,    0.18314,    0.72478},
	{ 8, 1, 1, 1,"zOalphaP+zOprotonP",    7.19974,    4.52896,    0.15056,    0.44389} },{
	{ 8, 1, 2, 0,"zIalpha+zIproton"  ,    7.76148,    4.14446,    0.18673,    0.76163},
	{ 8, 1, 2, 1,"zOalpha+zOproton"  ,    7.19549,    4.53149,    0.15264,    0.45746} }},{{

	{ 8, 2, 1, 0,"zIalphaP+zIkaonP"  ,    7.75698,    4.09229,    0.19211,    0.74507},
	{ 8, 2, 1, 1,"zOalphaP+zOkaonP"  ,    7.17754,    4.46768,    0.15276,    0.45506} },{
	{ 8, 2, 1, 0,"zIalphaP+zIkaonP"  ,    7.75698,    4.09229,    0.19211,    0.74507},
	{ 8, 2, 1, 1,"zOalphaP+zOkaonP"  ,    7.17754,    4.46768,    0.15276,    0.45506} },{
	{ 8, 2, 2, 0,"zIalpha+zIkaon"    ,    7.75848,    4.09540,    0.19343,    0.75974},
	{ 8, 2, 2, 1,"zOalpha+zOkaon"    ,    7.18990,    4.47246,    0.15595,    0.45243} }},{{

	{ 8, 3, 1, 0,"zIalphaP+zIelectronP",    7.75841,    4.08747,    0.19278,    0.74558},
	{ 8, 3, 1, 1,"zOalphaP+zOelectronP",    7.17636,    4.46694,    0.15668,    0.46595} },{
	{ 8, 3, 1, 0,"zIalphaP+zIelectronP",    7.75841,    4.08747,    0.19278,    0.74558},
	{ 8, 3, 1, 1,"zOalphaP+zOelectronP",    7.17636,    4.46694,    0.15668,    0.46595} },{
	{ 8, 3, 2, 0,"zIalpha+zIelectron",    7.75749,    4.08563,    0.19403,    0.75284},
	{ 8, 3, 2, 1,"zOalpha+zOelectron",    7.16327,    4.47297,    0.15688,    0.49761} }},{{

	{ 8, 4, 1, 0,"zIalphaP+zIdeuteronP",    7.77772,    4.32617,    0.17509,    0.75677},
	{ 8, 4, 1, 1,"zOalphaP+zOdeuteronP",    7.32571,    4.72242,    0.13712,    0.35275} },{
	{ 8, 4, 1, 0,"zIalphaP+zIdeuteronP",    7.77772,    4.32617,    0.17509,    0.75677},
	{ 8, 4, 1, 1,"zOalphaP+zOdeuteronP",    7.32571,    4.72242,    0.13712,    0.35275} },{
	{ 8, 4, 2, 0,"zIalpha+zIdeuteron",    7.77573,    4.32061,    0.17243,    0.73081},
	{ 8, 4, 2, 1,"zOalpha+zOdeuteron",    7.31628,    4.71999,    0.13578,    0.35751} }},{{

	{ 8, 5, 1, 0,"zIalphaP+zImuonP"  ,    7.75403,    4.08441,    0.20200,    0.81058},
	{ 8, 5, 1, 1,"zOalphaP+zOmuonP"  ,    7.16864,    4.45689,    0.15838,    0.48401} },{
	{ 8, 5, 1, 0,"zIalphaP+zImuonP"  ,    7.75403,    4.08441,    0.20200,    0.81058},
	{ 8, 5, 1, 1,"zOalphaP+zOmuonP"  ,    7.16864,    4.45689,    0.15838,    0.48401} },{
	{ 8, 5, 2, 0,"zIalpha+zImuon"    ,    7.75489,    4.07616,    0.19440,    0.75432},
	{ 8, 5, 2, 1,"zOalpha+zOmuon"    ,    7.17075,    4.46289,    0.16292,    0.49611} }},{{

	{ 8, 6, 1, 0,"zIalphaP+zItritonP",    7.79806,    4.49920,    0.14487,    0.66823},
	{ 8, 6, 1, 1,"zOalphaP+zOtritonP",    6.93008,    4.89055,    0.12816,    1.25241} },{
	{ 8, 6, 1, 0,"zIalphaP+zItritonP",    7.79806,    4.49920,    0.14487,    0.66823},
	{ 8, 6, 1, 1,"zOalphaP+zOtritonP",    6.93008,    4.89055,    0.12816,    1.25241} },{
	{ 8, 6, 2, 0,"zIalpha+zItriton"  ,    7.79414,    4.49767,    0.14535,    0.67789},
	{ 8, 6, 2, 1,"zOalpha+zOtriton"  ,    7.61729,    4.87803,    0.12327,    0.24091} }},{{

	{ 8, 7, 1, 0,"zIalphaP+zIHe3P"   ,    7.89681,    4.60586,    0.15341,    0.45950},
	{ 8, 7, 1, 1,"zOalphaP+zOHe3P"   ,    6.85323,    4.99347,    0.14046,    2.22744} },{
	{ 8, 7, 1, 0,"zIalphaP+zIHe3P"   ,    7.89681,    4.60586,    0.15341,    0.45950},
	{ 8, 7, 1, 1,"zOalphaP+zOHe3P"   ,    6.85323,    4.99347,    0.14046,    2.22744} },{
	{ 8, 7, 2, 0,"zIalpha+zIHe3"     ,    7.88360,    4.61005,    0.15585,    0.48843},
	{ 8, 7, 2, 1,"zOalpha+zOHe3"     ,    6.84206,    4.98576,    0.13924,    1.23076} }},{{

	{ 8, 8, 1, 0,"zIalphaP+zIalphaP" ,    8.11071,    4.76307,    0.15390,    0.32526},
	{ 8, 8, 1, 1,"zOalphaP+zOalphaP" ,    5.77686,    5.00000,    0.08936,    0.48475} },{
	{ 8, 8, 1, 0,"zIalphaP+zIalphaP" ,    8.11071,    4.76307,    0.15390,    0.32526},
	{ 8, 8, 1, 1,"zOalphaP+zOalphaP" ,    5.77686,    5.00000,    0.08936,    0.48475} },{
	{ 8, 8, 2, 0,"zIalpha+zIalpha"   ,    8.12819,    4.77121,    0.15965,    0.33141},
	{ 8, 8, 2, 1,"zOalpha+zOalpha"   ,    5.45762,    5.00000,    0.08705,    2.31971} }}
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
  Double_t Value = 0;
  Int_t icase = (Int_t) par[12];
  Int_t i1 = 0;
  Int_t i2 = 8;
  if (icase >= 0) {i1 = i2 = icase;}
  TF1 *g = GausExp();
  static Int_t _debug = 0;
  for (Int_t i = i1; i <= i2; i++) { 
    Double_t Mu = mu + parMIPs[i][sign][IO].mu - parMIPs[0][sign][IO].mu;
#ifdef __ELOSS__ 
    Double_t eps = dNdxMIP[i]/dNdxMIP[0] - 1;
    Double_t dMu = eps*eLoss*TMath::Exp(XX[0]);
    if (dMu > 0.1) dMu = 0.1;
    Mu += dMu;
#endif
    //    Double_t pars[4] = {0, Mu, parMIPs[i][sign][IO].sigma*(1 + sigma), parMIPs[i][sign][IO].alpha};
    Double_t pars[4] = {0, Mu, parMIPs[i][sign][IO].sigma + sigma, parMIPs[i][sign][IO].alpha};
    Value += frac[i]*g->EvalPar(XX, pars);
    if (_debug) {
      cout << "i: " << i << " " << parMIPs[i][sign][IO].Name << " frac[" << i <<"] = " << frac[i] << "\t"; parMIPs[i][sign][IO].Print();
    }
  }
  if (occupancy > 0) {
    Double_t overlap = 0;
    for (Int_t i = i1; i <= i2; i++) { 
      Double_t cont = 0;
      for (Int_t j = 0; j < 9; j++) {
	Int_t l = (i <= j) ? i + j*(j+1)/2 : j + i*(i+1)/2;
	l += 9;
	Double_t Mu = mu + parMIPs[l][sign][IO].mu - parMIPs[0][sign][IO].mu;
	Double_t pars[4] = {0, Mu, parMIPs[l][sign][IO].sigma + sigma, parMIPs[l][sign][IO].alpha};
	cont += frac[j]*g->EvalPar(XX, pars);
	if (_debug) {
	  cout << "i:" << i << " " << parMIPs[i][sign][IO].Name << "\t+ j:" << j << " " <<  parMIPs[j][sign][IO].Name << "\t frac[" << j <<"] = " << frac[j] << "\t l:" << l << " "; parMIPs[l][sign][IO].Print();
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
#ifdef __ELOSS__
    g2 = new TF1("G4EY",gf4EYFunc, -5, 5, 17);
#else
    g2 = new TF1("G4EY",gf4EYFunc, -5, 5, 16);
#endif
    g2->SetParName(0,"norm");      g2->SetParLimits(0,-0.2,0.2); // g2->FixParameter(0,0.0); // 
    g2->SetParName(1,"mu");        g2->SetParLimits(1,-0.2,0.4);				     
    g2->SetParName(2,"Sigma");     g2->FixParameter(2,0.0); g2->SetParLimits(2,0.0,0.1);	     
    g2->SetParName(3,"P");         g2->SetParLimits(3,0.0,1.2); // TMath::Pi()/2);		     
    g2->SetParName(4,"K");         g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 	     
    g2->SetParName(5,"e");         g2->SetParLimits(5,0.0,TMath::Pi()/2);			     
    g2->SetParName(6,"d");         g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(7,"muon");      g2->SetParLimits(7,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(8,"triton");    g2->SetParLimits(8,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(9,"He3");       g2->SetParLimits(9,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(10,"alpha");    g2->SetParLimits(10,0.0, 1.0); // TMath::Pi()/2);             
    g2->SetParName(11,"Total");
    g2->SetParName(12,"Case");
    g2->SetParName(13,"occupancy"); g2->FixParameter(13,0); g2->SetParLimits(13,0.0,0.25);
    g2->SetParName(14,"IO");       g2->FixParameter(14,IO); 
    g2->SetParName(15,"sign");     g2->FixParameter(15,Sign); 
#ifdef __ELOSS__
    g2->SetParName(16,"eLoss"); {g2->FixParameter(16,0.0); g2->SetParLimits(16,-0.01,0.01);}
#endif
    //    g2->SetParName(15,"factor"); g2->SetParLimits(15,-.1,0.1);
  }
  PreSetParametersY(proj, g2);
  g2->ReleaseParameter(2);
  g2->ReleaseParameter(3);
  g2->FixParameter(4,0.01); 
  g2->FixParameter(5,0.01);
  g2->FixParameter(6,0.01);
  g2->FixParameter(7,0.01);
  g2->FixParameter(8,0.01);
  g2->FixParameter(9,0.0);
  g2->FixParameter(10,0.0);
  g2->FixParameter(12,-1);
  g2->FixParameter(13,0.);
  g2->FixParameter(14,IO); 
  g2->FixParameter(15,Sign); 
#ifdef __ELOSS__
  g2->FixParameter(16,0.0);
#endif
  //  Fit pion + proton 
  proj->Fit(g2,Opt.Data());
  //  g2->ReleaseParameter(2);
  g2->ReleaseParameter(4);     g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 
  g2->ReleaseParameter(5);     g2->SetParLimits(5,0.0,TMath::Pi()/2);		
  g2->ReleaseParameter(6);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  Fit pion + proton + K + e + d
  Int_t iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
#ifdef __ELOSS__
  g2->ReleaseParameter(16);
  iok = proj->Fit(g2,Opt.Data());
#endif
  //  g2->ReleaseParameter(7);     g2->SetParLimits(4,0.0,0.6); // TMath::Pi()/2); 
  g2->ReleaseParameter(8);     g2->SetParLimits(5,0.0,TMath::Pi()/2);		
  //  g2->ReleaseParameter(9);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  g2->ReleaseParameter(10);     g2->SetParLimits(6,0.0, 1.0); // TMath::Pi()/2);
  //  Fit pion + proton + K + e + d + mu + triton // + He3 + alpha
  iok = proj->Fit(g2,Opt.Data());
  // + occupancy
  g2->ReleaseParameter(13);
  iok = proj->Fit(g2,Opt.Data());
  if ( iok < 0) {
    cout << g2->GetName() << " fit has failed with " << iok << " for " 
	 << proj->GetName() << "/" << proj->GetTitle() << " Try one again" << endl; 
    proj->Fit(g2,Opt.Data());
  }
  Opt += "m";
  iok = proj->Fit(g2,Opt.Data());
  if (iok < 0 ) return 0;
  
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
    for (int i = 0; i < 9; i++) {
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

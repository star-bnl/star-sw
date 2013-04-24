TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  Int_t PolyType = 0; // kMonomials
  Int_t NVariables = 2;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;// TPoints70BUGPRunXII24pp510P13ia_dEdxA
  row.idx        =     1;// 
  row.PolyType =        0;
  row.NVariables =      2;
  row.NCoefficients =   20;
  row.XMin[ 0] =     2.3514;  row.XMin[ 1] =      0.225;
  row.XMax[ 0] =     4.8481;  row.XMax[ 1] =      2.675;
  row.Power[ 0] =  1;  row.Power[ 1] =  1;
  row.Power[ 2] =  1;  row.Power[ 3] =  3;
  row.Power[ 4] =  2;  row.Power[ 5] =  2;
  row.Power[ 6] =  3;  row.Power[ 7] =  1;
  row.Power[ 8] =  4;  row.Power[ 9] =  1;
  row.Power[10] =  6;  row.Power[11] =  1;
  row.Power[12] =  2;  row.Power[13] =  1;
  row.Power[14] =  2;  row.Power[15] =  3;
  row.Power[16] =  3;  row.Power[17] =  2;
  row.Power[18] =  1;  row.Power[19] =  5;
  row.Power[20] =  1;  row.Power[21] =  6;
  row.Power[22] =  1;  row.Power[23] =  2;
  row.Power[24] =  5;  row.Power[25] =  1;
  row.Power[26] =  4;  row.Power[27] =  3;
  row.Power[28] =  5;  row.Power[29] =  4;
  row.Power[30] =  4;  row.Power[31] =  6;
  row.Power[32] =  6;  row.Power[33] =  5;
  row.Power[34] =  3;  row.Power[35] =  3;
  row.Power[36] =  4;  row.Power[37] =  2;
  row.Power[38] =  2;  row.Power[39] =  4;
  row.DMean =   0.03714;
  row.Coefficients[ 0]    =        0.002768;  row.Coefficients[ 1]    =        -0.01273;
  row.Coefficients[ 2]    =        0.019598;  row.Coefficients[ 3]    =       -0.046844;
  row.Coefficients[ 4]    =       0.0026655;  row.Coefficients[ 5]    =       -0.013952;
  row.Coefficients[ 6]    =        0.018287;  row.Coefficients[ 7]    =         0.01301;
  row.Coefficients[ 8]    =        0.066897;  row.Coefficients[ 9]    =        0.064461;
  row.Coefficients[10]    =      -0.0048809;  row.Coefficients[11]    =      -0.0093044;
  row.Coefficients[12]    =        0.023832;  row.Coefficients[13]    =       -0.055015;
  row.Coefficients[14]    =        -0.19536;  row.Coefficients[15]    =       -0.039438;
  row.Coefficients[16]    =         0.13844;  row.Coefficients[17]    =        0.056563;
  row.Coefficients[18]    =       -0.057522;  row.Coefficients[19]    =       -0.040336;
  row.CoefficientsRMS[ 0] =      6.0223e-05;  row.CoefficientsRMS[ 1] =      0.00085499;
  row.CoefficientsRMS[ 2] =      0.00072344;  row.CoefficientsRMS[ 3] =      0.00048174;
  row.CoefficientsRMS[ 4] =       0.0010784;  row.CoefficientsRMS[ 5] =       0.0010873;
  row.CoefficientsRMS[ 6] =      0.00025609;  row.CoefficientsRMS[ 7] =       0.0017639;
  row.CoefficientsRMS[ 8] =        0.001034;  row.CoefficientsRMS[ 9] =       0.0024855;
  row.CoefficientsRMS[10] =       0.0022281;  row.CoefficientsRMS[11] =      0.00024087;
  row.CoefficientsRMS[12] =      0.00062294;  row.CoefficientsRMS[13] =       0.0034424;
  row.CoefficientsRMS[14] =       0.0066153;  row.CoefficientsRMS[15] =       0.0064972;
  row.CoefficientsRMS[16] =       0.0069454;  row.CoefficientsRMS[17] =       0.0016235;
  row.CoefficientsRMS[18] =       0.0014911;  row.CoefficientsRMS[19] =       0.0022922;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;// TPoints70BUGPRunXII24pp510P13ia_dEdxA
  row.idx        =     2;// 
  row.PolyType =        0;
  row.NVariables =      2;
  row.NCoefficients =   20;
  row.XMin[ 0] =     2.3514;  row.XMin[ 1] =      0.225;
  row.XMax[ 0] =     4.8481;  row.XMax[ 1] =      2.675;
  row.Power[ 0] =  1;  row.Power[ 1] =  1;
  row.Power[ 2] =  2;  row.Power[ 3] =  1;
  row.Power[ 4] =  1;  row.Power[ 5] =  2;
  row.Power[ 6] =  1;  row.Power[ 7] =  3;
  row.Power[ 8] =  2;  row.Power[ 9] =  2;
  row.Power[10] =  3;  row.Power[11] =  1;
  row.Power[12] =  1;  row.Power[13] =  4;
  row.Power[14] =  3;  row.Power[15] =  2;
  row.Power[16] =  1;  row.Power[17] =  5;
  row.Power[18] =  4;  row.Power[19] =  1;
  row.Power[20] =  3;  row.Power[21] =  3;
  row.Power[22] =  1;  row.Power[23] =  6;
  row.Power[24] =  2;  row.Power[25] =  5;
  row.Power[26] =  3;  row.Power[27] =  4;
  row.Power[28] =  2;  row.Power[29] =  3;
  row.Power[30] =  2;  row.Power[31] =  4;
  row.Power[32] =  5;  row.Power[33] =  1;
  row.Power[34] =  4;  row.Power[35] =  6;
  row.Power[36] =  5;  row.Power[37] =  5;
  row.Power[38] =  4;  row.Power[39] =  2;
  row.DMean =   0.1115;
  row.Coefficients[ 0]    =       0.0063527;  row.Coefficients[ 1]    =       -0.063783;
  row.Coefficients[ 2]    =        0.048165;  row.Coefficients[ 3]    =        -0.10036;
  row.Coefficients[ 4]    =        -0.03153;  row.Coefficients[ 5]    =       -0.014879;
  row.Coefficients[ 6]    =       -0.098543;  row.Coefficients[ 7]    =       -0.023203;
  row.Coefficients[ 8]    =         0.24261;  row.Coefficients[ 9]    =      -0.0018766;
  row.Coefficients[10]    =         0.20239;  row.Coefficients[11]    =         0.15653;
  row.Coefficients[12]    =         0.16125;  row.Coefficients[13]    =         0.28882;
  row.Coefficients[14]    =       -0.036195;  row.Coefficients[15]    =        0.048446;
  row.Coefficients[16]    =        0.031315;  row.Coefficients[17]    =         0.19134;
  row.Coefficients[18]    =       -0.097455;  row.Coefficients[19]    =        -0.02349;
  row.CoefficientsRMS[ 0] =      7.1041e-05;  row.CoefficientsRMS[ 1] =      0.00024002;
  row.CoefficientsRMS[ 2] =      0.00032986;  row.CoefficientsRMS[ 3] =       0.0011069;
  row.CoefficientsRMS[ 4] =      0.00084466;  row.CoefficientsRMS[ 5] =      0.00052364;
  row.CoefficientsRMS[ 6] =       0.0023979;  row.CoefficientsRMS[ 7] =       0.0011827;
  row.CoefficientsRMS[ 8] =       0.0030096;  row.CoefficientsRMS[ 9] =      0.00042759;
  row.CoefficientsRMS[10] =        0.003164;  row.CoefficientsRMS[11] =       0.0048616;
  row.CoefficientsRMS[12] =       0.0052888;  row.CoefficientsRMS[13] =       0.0049655;
  row.CoefficientsRMS[14] =       0.0021367;  row.CoefficientsRMS[15] =       0.0032283;
  row.CoefficientsRMS[16] =      0.00068409;  row.CoefficientsRMS[17] =        0.011726;
  row.CoefficientsRMS[18] =       0.0097255;  row.CoefficientsRMS[19] =       0.0013594;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;// TPointsBUGPRunXII24pp510P13ia_dEdxA
  row.idx        =     5;// 
  row.PolyType =        1;
  row.NVariables =      2;
  row.NCoefficients =   20;
  row.XMin[ 0] =     2.3514;  row.XMin[ 1] =      0.225;
  row.XMax[ 0] =     4.8481;  row.XMax[ 1] =      2.425;
  row.Power[ 0] =  1;  row.Power[ 1] =  1;
  row.Power[ 2] =  3;  row.Power[ 3] =  1;
  row.Power[ 4] =  2;  row.Power[ 5] =  3;
  row.Power[ 6] =  1;  row.Power[ 7] =  2;
  row.Power[ 8] =  1;  row.Power[ 9] =  3;
  row.Power[10] =  5;  row.Power[11] =  2;
  row.Power[12] =  4;  row.Power[13] =  1;
  row.Power[14] =  3;  row.Power[15] =  3;
  row.Power[16] =  1;  row.Power[17] =  6;
  row.Power[18] =  4;  row.Power[19] =  3;
  row.Power[20] =  5;  row.Power[21] =  3;
  row.Power[22] =  3;  row.Power[23] =  5;
  row.Power[24] =  5;  row.Power[25] =  4;
  row.Power[26] =  4;  row.Power[27] =  6;
  row.Power[28] =  1;  row.Power[29] =  4;
  row.Power[30] =  1;  row.Power[31] =  5;
  row.Power[32] =  2;  row.Power[33] =  5;
  row.Power[34] =  3;  row.Power[35] =  6;
  row.Power[36] =  6;  row.Power[37] =  3;
  row.Power[38] =  4;  row.Power[39] =  2;
  row.DMean =   0.0342;
  row.Coefficients[ 0]    =       0.0048044;  row.Coefficients[ 1]    =       0.0072747;
  row.Coefficients[ 2]    =      -0.0054532;  row.Coefficients[ 3]    =        0.031675;
  row.Coefficients[ 4]    =        0.022136;  row.Coefficients[ 5]    =      -0.0072574;
  row.Coefficients[ 6]    =       0.0076138;  row.Coefficients[ 7]    =        0.032095;
  row.Coefficients[ 8]    =       0.0070446;  row.Coefficients[ 9]    =        0.021778;
  row.Coefficients[10]    =       -0.004983;  row.Coefficients[11]    =         0.01818;
  row.Coefficients[12]    =      0.00039561;  row.Coefficients[13]    =       0.0032994;
  row.Coefficients[14]    =        0.015007;  row.Coefficients[15]    =       0.0079627;
  row.Coefficients[16]    =      -0.0069372;  row.Coefficients[17]    =       0.0050123;
  row.Coefficients[18]    =       0.0030901;  row.Coefficients[19]    =      -0.0013987;
  row.CoefficientsRMS[ 0] =      0.00041099;  row.CoefficientsRMS[ 1] =       0.0004235;
  row.CoefficientsRMS[ 2] =       0.0003971;  row.CoefficientsRMS[ 3] =      0.00091662;
  row.CoefficientsRMS[ 4] =      0.00056428;  row.CoefficientsRMS[ 5] =      0.00041751;
  row.CoefficientsRMS[ 6] =      0.00027708;  row.CoefficientsRMS[ 7] =      0.00055188;
  row.CoefficientsRMS[ 8] =      0.00022153;  row.CoefficientsRMS[ 9] =       0.0004695;
  row.CoefficientsRMS[10] =      0.00012837;  row.CoefficientsRMS[11] =      0.00029486;
  row.CoefficientsRMS[12] =      0.00025982;  row.CoefficientsRMS[13] =      8.0299e-05;
  row.CoefficientsRMS[14] =      0.00057517;  row.CoefficientsRMS[15] =       0.0002728;
  row.CoefficientsRMS[16] =      0.00024044;  row.CoefficientsRMS[17] =      0.00019193;
  row.CoefficientsRMS[18] =      0.00010668;  row.CoefficientsRMS[19] =      0.00024714;
  tableSet->AddAt(&row);// 4 -> I 
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;// TPointsBUGPRunXII24pp510P13ia_dEdxA
  row.idx        =     6;// 
  row.PolyType =        1;
  row.NVariables =      2;
  row.NCoefficients =   20;
  row.XMin[ 0] =     2.3514;  row.XMin[ 1] =      0.225;
  row.XMax[ 0] =     4.8481;  row.XMax[ 1] =      2.425;
  row.Power[ 0] =  1;  row.Power[ 1] =  1;
  row.Power[ 2] =  2;  row.Power[ 3] =  1;
  row.Power[ 4] =  1;  row.Power[ 5] =  2;
  row.Power[ 6] =  3;  row.Power[ 7] =  3;
  row.Power[ 8] =  2;  row.Power[ 9] =  2;
  row.Power[10] =  3;  row.Power[11] =  5;
  row.Power[12] =  1;  row.Power[13] =  3;
  row.Power[14] =  3;  row.Power[15] =  1;
  row.Power[16] =  2;  row.Power[17] =  3;
  row.Power[18] =  3;  row.Power[19] =  2;
  row.Power[20] =  1;  row.Power[21] =  5;
  row.Power[22] =  4;  row.Power[23] =  3;
  row.Power[24] =  2;  row.Power[25] =  5;
  row.Power[26] =  5;  row.Power[27] =  3;
  row.Power[28] =  6;  row.Power[29] =  2;
  row.Power[30] =  4;  row.Power[31] =  6;
  row.Power[32] =  5;  row.Power[33] =  5;
  row.Power[34] =  6;  row.Power[35] =  5;
  row.Power[36] =  1;  row.Power[37] =  4;
  row.Power[38] =  4;  row.Power[39] =  1;
  row.DMean =   0.09933;
  row.Coefficients[ 0]    =         0.10625;  row.Coefficients[ 1]    =       -0.095554;
  row.Coefficients[ 2]    =         0.12167;  row.Coefficients[ 3]    =        0.054882;
  row.Coefficients[ 4]    =        -0.11842;  row.Coefficients[ 5]    =        0.015463;
  row.Coefficients[ 6]    =        0.087877;  row.Coefficients[ 7]    =        0.082979;
  row.Coefficients[ 8]    =        0.040907;  row.Coefficients[ 9]    =        0.087418;
  row.Coefficients[10]    =        0.020843;  row.Coefficients[11]    =        0.021547;
  row.Coefficients[12]    =        0.029785;  row.Coefficients[13]    =      -0.0062272;
  row.Coefficients[14]    =        0.019592;  row.Coefficients[15]    =      -0.0053715;
  row.Coefficients[16]    =       0.0090725;  row.Coefficients[17]    =      -0.0074894;
  row.Coefficients[18]    =        0.006297;  row.Coefficients[19]    =      -0.0060557;
  row.CoefficientsRMS[ 0] =      0.00072963;  row.CoefficientsRMS[ 1] =       0.0012317;
  row.CoefficientsRMS[ 2] =      0.00086198;  row.CoefficientsRMS[ 3] =       0.0010154;
  row.CoefficientsRMS[ 4] =      0.00083589;  row.CoefficientsRMS[ 5] =      0.00037878;
  row.CoefficientsRMS[ 6] =      0.00084544;  row.CoefficientsRMS[ 7] =      0.00085501;
  row.CoefficientsRMS[ 8] =       0.0015259;  row.CoefficientsRMS[ 9] =       0.0007295;
  row.CoefficientsRMS[10] =      0.00027337;  row.CoefficientsRMS[11] =      0.00045127;
  row.CoefficientsRMS[12] =      0.00039703;  row.CoefficientsRMS[13] =      0.00013594;
  row.CoefficientsRMS[14] =      0.00018549;  row.CoefficientsRMS[15] =      7.4031e-05;
  row.CoefficientsRMS[16] =      0.00013021;  row.CoefficientsRMS[17] =      8.9994e-05;
  row.CoefficientsRMS[18] =      0.00018647;  row.CoefficientsRMS[19] =      0.00031457;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

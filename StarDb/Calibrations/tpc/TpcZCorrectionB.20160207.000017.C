TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 45;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   1;
  row.nrows = 45;
  row.min =  95.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =     -30.36331;
  row.a[1]       =      1.012177;
  row.a[2]       =    -0.0129439;
  row.a[3]       =  7.970724e-05;
  row.a[4]       = -2.374937e-07;
  row.a[5]       =  2.753214e-10;
  tableSet->AddAt(&row); // row  1
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   2;
  row.nrows = 45;
  row.min =  90.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =     -18.46946;
  row.a[1]       =     0.6406083;
  row.a[2]       =  -0.008439085;
  row.a[3]       =  5.307679e-05;
  row.a[4]       = -1.603827e-07;
  row.a[5]       =   1.87479e-10;
  tableSet->AddAt(&row); // row  2
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   3;
  row.nrows = 45;
  row.min =  77.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =     -9.939486;
  row.a[1]       =      0.369138;
  row.a[2]       =  -0.005097903;
  row.a[3]       =  3.309232e-05;
  row.a[4]       = -1.020187e-07;
  row.a[5]       =  1.206294e-10;
  tableSet->AddAt(&row); // row  3
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   4;
  row.nrows = 45;
  row.min =  65.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =     -4.271703;
  row.a[1]       =     0.1761097;
  row.a[2]       =  -0.002569727;
  row.a[3]       =  1.704539e-05;
  row.a[4]       = -5.232884e-08;
  row.a[5]       =  6.024694e-11;
  tableSet->AddAt(&row); // row  4
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   5;
  row.nrows = 45;
  row.min =  45.00;
  row.max =  210.00;
  row.npar       =           107;
  row.a[0]       =      2.763577;
  row.a[1]       =    -0.1374754;
  row.a[2]       =   0.003072659;
  row.a[3]       = -3.601885e-05;
  row.a[4]       =  2.244555e-07;
  row.a[5]       = -7.009158e-10;
  row.a[6]       =  8.627039e-13;
  tableSet->AddAt(&row); // row  5
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   6;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =    -0.2802378;
  row.a[1]       =    0.03245767;
  row.a[2]       = -0.0006330869;
  row.a[3]       =  4.608543e-06;
  row.a[4]       = -1.380926e-08;
  row.a[5]       =  1.378466e-11;
  tableSet->AddAt(&row); // row  6
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   7;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           105;
  row.a[0]       =     0.2819612;
  row.a[1]       =   0.007416024;
  row.a[2]       = -0.0002373825;
  row.a[3]       =  1.703736e-06;
  row.a[4]       = -3.645288e-09;
  tableSet->AddAt(&row); // row  7
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   8;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           105;
  row.a[0]       =     0.4143043;
  row.a[1]       = -0.0002667951;
  row.a[2]       = -0.0001281756;
  row.a[3]       =   1.12343e-06;
  row.a[4]       = -2.596166e-09;
  tableSet->AddAt(&row); // row  8
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =   9;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           105;
  row.a[0]       =     0.3429899;
  row.a[1]       = -0.0005491711;
  row.a[2]       = -0.0001043067;
  row.a[3]       =  9.412598e-07;
  row.a[4]       = -2.206771e-09;
  tableSet->AddAt(&row); // row  9
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  10;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           107;
  row.a[0]       =      1.322084;
  row.a[1]       =   -0.06320873;
  row.a[2]       =   0.001362141;
  row.a[3]       = -1.601653e-05;
  row.a[4]       =   1.02481e-07;
  row.a[5]       = -3.310562e-10;
  row.a[6]       =  4.216918e-13;
  tableSet->AddAt(&row); // row 10
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  11;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           107;
  row.a[0]       =      1.498748;
  row.a[1]       =   -0.07553289;
  row.a[2]       =   0.001653442;
  row.a[3]       = -1.946205e-05;
  row.a[4]       =  1.247187e-07;
  row.a[5]       = -4.054507e-10;
  row.a[6]       =  5.221089e-13;
  tableSet->AddAt(&row); // row 11
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  12;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           106;
  row.a[0]       =  -0.007932146;
  row.a[1]       =   0.007992557;
  row.a[2]       = -0.0002154969;
  row.a[3]       =  1.895889e-06;
  row.a[4]       = -6.585776e-09;
  row.a[5]       =   7.67303e-12;
  tableSet->AddAt(&row); // row 12
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  13;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           107;
  row.a[0]       =      1.075086;
  row.a[1]       =   -0.06174152;
  row.a[2]       =   0.001481035;
  row.a[3]       = -1.866937e-05;
  row.a[4]       =   1.26338e-07;
  row.a[5]       = -4.302849e-10;
  row.a[6]       =  5.779737e-13;
  tableSet->AddAt(&row); // row 13
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  14;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01291899;
  row.a[1]       = -9.832645e-05;
  row.a[2]       = -1.417046e-07;
  tableSet->AddAt(&row); // row 14
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  15;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.007089074;
  row.a[1]       =  1.068129e-05;
  row.a[2]       = -5.813204e-07;
  tableSet->AddAt(&row); // row 15
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  16;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.006330322;
  row.a[1]       =   1.94709e-06;
  row.a[2]       = -4.908754e-07;
  tableSet->AddAt(&row); // row 16
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  17;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.006797041;
  row.a[1]       =  1.386368e-05;
  row.a[2]       = -6.268815e-07;
  tableSet->AddAt(&row); // row 17
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  18;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.004629064;
  row.a[1]       =  2.058369e-05;
  row.a[2]       = -5.749814e-07;
  tableSet->AddAt(&row); // row 18
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  19;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.002866506;
  row.a[1]       =  6.845483e-05;
  row.a[2]       = -8.117419e-07;
  tableSet->AddAt(&row); // row 19
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  20;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  0.0007093924;
  row.a[1]       =  6.581688e-05;
  row.a[2]       = -6.902292e-07;
  tableSet->AddAt(&row); // row 20
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  21;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.002457839;
  row.a[1]       =  0.0001018163;
  row.a[2]       = -7.628707e-07;
  tableSet->AddAt(&row); // row 21
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  22;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01534602;
  row.a[1]       =  0.0002707578;
  row.a[2]       = -1.204123e-06;
  tableSet->AddAt(&row); // row 22
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  23;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01423169;
  row.a[1]       =  0.0002913642;
  row.a[2]       = -1.405844e-06;
  tableSet->AddAt(&row); // row 23
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  24;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01270795;
  row.a[1]       =  0.0002549167;
  row.a[2]       = -1.241972e-06;
  tableSet->AddAt(&row); // row 24
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  25;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01411409;
  row.a[1]       =  0.0002999168;
  row.a[2]       = -1.476913e-06;
  tableSet->AddAt(&row); // row 25
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  26;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01342932;
  row.a[1]       =  0.0002800295;
  row.a[2]       =  -1.38202e-06;
  tableSet->AddAt(&row); // row 26
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  27;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    -0.0130469;
  row.a[1]       =  0.0002838197;
  row.a[2]       = -1.446192e-06;
  tableSet->AddAt(&row); // row 27
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  28;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01093066;
  row.a[1]       =  0.0002351895;
  row.a[2]       = -1.224473e-06;
  tableSet->AddAt(&row); // row 28
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  29;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.001598131;
  row.a[1]       =   2.68678e-05;
  row.a[2]       = -5.075714e-07;
  tableSet->AddAt(&row); // row 29
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  30;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   -0.01112015;
  row.a[1]       =  0.0002404452;
  row.a[2]       = -1.269456e-06;
  tableSet->AddAt(&row); // row 30
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  31;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.009371558;
  row.a[1]       =   0.000225264;
  row.a[2]       = -1.271451e-06;
  tableSet->AddAt(&row); // row 31
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  32;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.009018911;
  row.a[1]       =  0.0002064571;
  row.a[2]       = -1.152003e-06;
  tableSet->AddAt(&row); // row 32
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  33;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.007692437;
  row.a[1]       =  0.0002047472;
  row.a[2]       = -1.209439e-06;
  tableSet->AddAt(&row); // row 33
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  34;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.006921873;
  row.a[1]       =  0.0001886746;
  row.a[2]       = -1.153384e-06;
  tableSet->AddAt(&row); // row 34
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  35;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.006671792;
  row.a[1]       =  0.0001823434;
  row.a[2]       = -1.117326e-06;
  tableSet->AddAt(&row); // row 35
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  36;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =  -0.004804825;
  row.a[1]       =  0.0001491855;
  row.a[2]       = -9.925764e-07;
  tableSet->AddAt(&row); // row 36
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  37;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.008754847;
  row.a[1]       = -8.139987e-05;
  row.a[2]       = -1.983205e-07;
  tableSet->AddAt(&row); // row 37
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  38;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01564739;
  row.a[1]       = -0.0001519586;
  row.a[2]       = -1.271078e-07;
  tableSet->AddAt(&row); // row 38
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  39;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01158321;
  row.a[1]       =   2.86478e-05;
  row.a[2]       = -1.069889e-06;
  tableSet->AddAt(&row); // row 39
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  40;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.007646167;
  row.a[1]       = -5.121022e-05;
  row.a[2]       = -3.273282e-07;
  tableSet->AddAt(&row); // row 40
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  41;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01015626;
  row.a[1]       = -6.884662e-05;
  row.a[2]       = -3.524503e-07;
  tableSet->AddAt(&row); // row 41
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  42;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =   0.008885202;
  row.a[1]       = -6.885647e-05;
  row.a[2]       = -2.700398e-07;
  tableSet->AddAt(&row); // row 42
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  43;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =     0.0111323;
  row.a[1]       = -9.790573e-05;
  row.a[2]       = -1.961408e-07;
  tableSet->AddAt(&row); // row 43
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  44;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01210972;
  row.a[1]       = -0.0001654945;
  row.a[2]       =  2.216431e-07;
  tableSet->AddAt(&row); // row 44
  memset(&row,0,tableSet->GetRowSize()); // Z3CGFRunXVIAuAu200p17.root
  row.idx   =  45;
  row.nrows = 45;
  row.min =  20.00;
  row.max =  210.00;
  row.npar       =           103;
  row.a[0]       =    0.01238175;
  row.a[1]       = -0.0002147658;
  row.a[2]       =  5.382927e-07;
  tableSet->AddAt(&row); // row 45
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 

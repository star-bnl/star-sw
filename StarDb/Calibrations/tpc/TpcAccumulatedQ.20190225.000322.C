TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrect192")) return 0;
  tpcCorrect192_st row;
  Int_t nrows = 192;
  St_tpcCorrect192 *tableSet = new St_tpcCorrect192("TpcAccumulatedQ",nrows);
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   1;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0408687;
  row.a[1]       =  -0.001374599;
  tableSet->AddAt(&row); // row   1
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   2;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06460624;
  row.a[1]       =  -0.001204914;
  tableSet->AddAt(&row); // row   2
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   3;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05779252;
  row.a[1]       =   -0.00162564;
  tableSet->AddAt(&row); // row   3
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   4;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0214875;
  row.a[1]       =  -0.001659013;
  tableSet->AddAt(&row); // row   4
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   5;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01233157;
  row.a[1]       = -0.0008254849;
  tableSet->AddAt(&row); // row   5
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   6;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02398822;
  row.a[1]       =   -0.00070427;
  tableSet->AddAt(&row); // row   6
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   7;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02275312;
  row.a[1]       = -0.0007488466;
  tableSet->AddAt(&row); // row   7
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   8;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00903796;
  row.a[1]       =  -0.001118422;
  tableSet->AddAt(&row); // row   8
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =   9;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.1151769;
  row.a[1]       =  -0.001285486;
  tableSet->AddAt(&row); // row   9
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  10;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.07916791;
  row.a[1]       =  -0.001070533;
  tableSet->AddAt(&row); // row  10
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  11;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0865277;
  row.a[1]       =  -0.001423599;
  tableSet->AddAt(&row); // row  11
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  12;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05160461;
  row.a[1]       =  -0.001583877;
  tableSet->AddAt(&row); // row  12
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  13;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =      0.012906;
  row.a[1]       = -0.0005865358;
  tableSet->AddAt(&row); // row  13
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  14;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02926783;
  row.a[1]       = -0.0008195958;
  tableSet->AddAt(&row); // row  14
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  15;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008556842;
  row.a[1]       = -0.0008677414;
  tableSet->AddAt(&row); // row  15
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  16;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02848884;
  row.a[1]       =  -0.001174636;
  tableSet->AddAt(&row); // row  16
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  17;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.07749695;
  row.a[1]       =  -0.001217078;
  tableSet->AddAt(&row); // row  17
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  18;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05304796;
  row.a[1]       =  -0.001416282;
  tableSet->AddAt(&row); // row  18
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  19;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06730102;
  row.a[1]       =  -0.001415468;
  tableSet->AddAt(&row); // row  19
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  20;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05101186;
  row.a[1]       =   -0.00175211;
  tableSet->AddAt(&row); // row  20
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  21;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02340534;
  row.a[1]       = -0.0007161299;
  tableSet->AddAt(&row); // row  21
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  22;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02138315;
  row.a[1]       = -0.0007321205;
  tableSet->AddAt(&row); // row  22
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  23;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01640929;
  row.a[1]       = -0.0008961365;
  tableSet->AddAt(&row); // row  23
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  24;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.004657279;
  row.a[1]       = -0.0007364852;
  tableSet->AddAt(&row); // row  24
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  25;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03898456;
  row.a[1]       =  -0.001436847;
  tableSet->AddAt(&row); // row  25
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  26;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01671165;
  row.a[1]       =  -0.001085311;
  tableSet->AddAt(&row); // row  26
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  27;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0261446;
  row.a[1]       =  -0.001439651;
  tableSet->AddAt(&row); // row  27
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  28;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01722657;
  row.a[1]       =  -0.001331322;
  tableSet->AddAt(&row); // row  28
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  29;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0116347;
  row.a[1]       =  -0.000885683;
  tableSet->AddAt(&row); // row  29
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  30;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01117408;
  row.a[1]       =  -0.001034642;
  tableSet->AddAt(&row); // row  30
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  31;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.009237915;
  row.a[1]       =  -0.001341628;
  tableSet->AddAt(&row); // row  31
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  32;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007551343;
  row.a[1]       =  -0.001824904;
  tableSet->AddAt(&row); // row  32
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  33;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03236767;
  row.a[1]       = -0.0009491037;
  tableSet->AddAt(&row); // row  33
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  34;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0256547;
  row.a[1]       =  -0.001131539;
  tableSet->AddAt(&row); // row  34
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  35;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02054343;
  row.a[1]       =  -0.001137157;
  tableSet->AddAt(&row); // row  35
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  36;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01779223;
  row.a[1]       =  -0.001241998;
  tableSet->AddAt(&row); // row  36
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  37;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.009065066;
  row.a[1]       = -0.0007637555;
  tableSet->AddAt(&row); // row  37
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  38;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.009615791;
  row.a[1]       =  -0.001102519;
  tableSet->AddAt(&row); // row  38
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  39;
  row.nrows = 192;
  tableSet->AddAt(&row); // row  39
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  40;
  row.nrows = 192;
  tableSet->AddAt(&row); // row  40
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  41;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03327548;
  row.a[1]       =  -0.001079841;
  tableSet->AddAt(&row); // row  41
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  42;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02557151;
  row.a[1]       =  -0.001146378;
  tableSet->AddAt(&row); // row  42
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  43;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02107052;
  row.a[1]       =  -0.001186917;
  tableSet->AddAt(&row); // row  43
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  44;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0148292;
  row.a[1]       =  -0.001045672;
  tableSet->AddAt(&row); // row  44
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  45;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01403158;
  row.a[1]       =  -0.001284456;
  tableSet->AddAt(&row); // row  45
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  46;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01422078;
  row.a[1]       =  -0.001741137;
  tableSet->AddAt(&row); // row  46
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  47;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01322541;
  row.a[1]       =  -0.001664206;
  tableSet->AddAt(&row); // row  47
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  48;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01000544;
  row.a[1]       =  -0.002467592;
  tableSet->AddAt(&row); // row  48
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  49;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03446506;
  row.a[1]       =  -0.001594957;
  tableSet->AddAt(&row); // row  49
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  50;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02695339;
  row.a[1]       =  -0.001869916;
  tableSet->AddAt(&row); // row  50
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  51;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02695339;
  row.a[1]       =  -0.001869916;
  tableSet->AddAt(&row); // row  51
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  52;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01335623;
  row.a[1]       =  -0.001204779;
  tableSet->AddAt(&row); // row  52
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  53;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =      0.009982;
  row.a[1]       = -0.0008909645;
  tableSet->AddAt(&row); // row  53
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  54;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01297865;
  row.a[1]       =  -0.001832766;
  tableSet->AddAt(&row); // row  54
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  55;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01297865;
  row.a[1]       =  -0.001832766;
  tableSet->AddAt(&row); // row  55
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  56;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01297865;
  row.a[1]       =  -0.001832766;
  tableSet->AddAt(&row); // row  56
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  57;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04109695;
  row.a[1]       =  -0.005122776;
  tableSet->AddAt(&row); // row  57
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  58;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02393893;
  row.a[1]       = -0.0009235803;
  tableSet->AddAt(&row); // row  58
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  59;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02393893;
  row.a[1]       = -0.0009235803;
  tableSet->AddAt(&row); // row  59
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  60;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01962173;
  row.a[1]       = -0.0009361643;
  tableSet->AddAt(&row); // row  60
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  61;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01962173;
  row.a[1]       = -0.0009361643;
  tableSet->AddAt(&row); // row  61
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  62;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008242004;
  row.a[1]       =  -0.001008975;
  tableSet->AddAt(&row); // row  62
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  63;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008242004;
  row.a[1]       =  -0.001008975;
  tableSet->AddAt(&row); // row  63
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  64;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00511649;
  row.a[1]       =  -0.001207647;
  tableSet->AddAt(&row); // row  64
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  65;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04118767;
  row.a[1]       =  -0.001292903;
  tableSet->AddAt(&row); // row  65
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  66;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.1094096;
  row.a[1]       =  -0.003307345;
  tableSet->AddAt(&row); // row  66
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  67;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02448301;
  row.a[1]       =  -0.001127905;
  tableSet->AddAt(&row); // row  67
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  68;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01746666;
  row.a[1]       =    -0.0023377;
  tableSet->AddAt(&row); // row  68
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  69;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01746666;
  row.a[1]       =    -0.0023377;
  tableSet->AddAt(&row); // row  69
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  70;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01746666;
  row.a[1]       =    -0.0023377;
  tableSet->AddAt(&row); // row  70
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  71;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008033041;
  row.a[1]       =  -0.001187213;
  tableSet->AddAt(&row); // row  71
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  72;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008033041;
  row.a[1]       =  -0.001187213;
  tableSet->AddAt(&row); // row  72
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  73;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03680239;
  row.a[1]       =  -0.001517567;
  tableSet->AddAt(&row); // row  73
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  74;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02629655;
  row.a[1]       =  -0.001149774;
  tableSet->AddAt(&row); // row  74
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  75;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02239257;
  row.a[1]       =  -0.001037916;
  tableSet->AddAt(&row); // row  75
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  76;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01727761;
  row.a[1]       =  -0.001352309;
  tableSet->AddAt(&row); // row  76
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  77;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.009504555;
  row.a[1]       = -0.0007455809;
  tableSet->AddAt(&row); // row  77
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  78;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006718579;
  row.a[1]       = -0.0006755946;
  tableSet->AddAt(&row); // row  78
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  79;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.004574547;
  row.a[1]       = -0.0007781021;
  tableSet->AddAt(&row); // row  79
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  80;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.004229974;
  row.a[1]       =  -0.001112066;
  tableSet->AddAt(&row); // row  80
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  81;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03094976;
  row.a[1]       = -0.0009790426;
  tableSet->AddAt(&row); // row  81
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  82;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02368155;
  row.a[1]       =  -0.001050793;
  tableSet->AddAt(&row); // row  82
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  83;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01956756;
  row.a[1]       =   -0.00114344;
  tableSet->AddAt(&row); // row  83
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  84;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01676861;
  row.a[1]       =  -0.001185884;
  tableSet->AddAt(&row); // row  84
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  85;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00766443;
  row.a[1]       = -0.0006723569;
  tableSet->AddAt(&row); // row  85
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  86;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007730516;
  row.a[1]       = -0.0006778775;
  tableSet->AddAt(&row); // row  86
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  87;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006327592;
  row.a[1]       = -0.0008373177;
  tableSet->AddAt(&row); // row  87
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  88;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00286136;
  row.a[1]       = -0.0008627038;
  tableSet->AddAt(&row); // row  88
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  89;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04014405;
  row.a[1]       =  -0.001497574;
  tableSet->AddAt(&row); // row  89
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  90;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03091574;
  row.a[1]       =   -0.00142111;
  tableSet->AddAt(&row); // row  90
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  91;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02629144;
  row.a[1]       =  -0.001659067;
  tableSet->AddAt(&row); // row  91
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  92;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02025645;
  row.a[1]       =  -0.001543921;
  tableSet->AddAt(&row); // row  92
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  93;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008191903;
  row.a[1]       = -0.0007119103;
  tableSet->AddAt(&row); // row  93
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  94;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.005916666;
  row.a[1]       = -0.0008201658;
  tableSet->AddAt(&row); // row  94
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  95;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.004135776;
  row.a[1]       = -0.0006947361;
  tableSet->AddAt(&row); // row  95
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  96;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.001473121;
  row.a[1]       = -0.0006758316;
  tableSet->AddAt(&row); // row  96
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  97;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03834645;
  row.a[1]       =   -0.00147871;
  tableSet->AddAt(&row); // row  97
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  98;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03041856;
  row.a[1]       =  -0.001436422;
  tableSet->AddAt(&row); // row  98
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   =  99;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02476463;
  row.a[1]       =  -0.001891057;
  tableSet->AddAt(&row); // row  99
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 100;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01861835;
  row.a[1]       =  -0.001327021;
  tableSet->AddAt(&row); // row 100
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 101;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01316793;
  row.a[1]       = -0.0006124298;
  tableSet->AddAt(&row); // row 101
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 102;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01146592;
  row.a[1]       = -0.0005550039;
  tableSet->AddAt(&row); // row 102
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 103;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01008822;
  row.a[1]       =  -0.001278916;
  tableSet->AddAt(&row); // row 103
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 104;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00526657;
  row.a[1]       = -0.0009490662;
  tableSet->AddAt(&row); // row 104
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 105;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03463065;
  row.a[1]       = -0.0008716662;
  tableSet->AddAt(&row); // row 105
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 106;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02555206;
  row.a[1]       = -0.0009831825;
  tableSet->AddAt(&row); // row 106
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 107;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02144247;
  row.a[1]       =  -0.001041748;
  tableSet->AddAt(&row); // row 107
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 108;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01944112;
  row.a[1]       =  -0.001111204;
  tableSet->AddAt(&row); // row 108
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 109;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01170489;
  row.a[1]       = -0.0007361103;
  tableSet->AddAt(&row); // row 109
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 110;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01199407;
  row.a[1]       = -0.0009409552;
  tableSet->AddAt(&row); // row 110
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 111;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007956943;
  row.a[1]       =  -0.000680708;
  tableSet->AddAt(&row); // row 111
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 112;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.004729194;
  row.a[1]       = -0.0007207849;
  tableSet->AddAt(&row); // row 112
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 113;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0378296;
  row.a[1]       =  -0.001098449;
  tableSet->AddAt(&row); // row 113
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 114;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02889265;
  row.a[1]       =   -0.00127119;
  tableSet->AddAt(&row); // row 114
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 115;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02322534;
  row.a[1]       =  -0.001135733;
  tableSet->AddAt(&row); // row 115
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 116;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02087966;
  row.a[1]       =  -0.001121031;
  tableSet->AddAt(&row); // row 116
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 117;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01078701;
  row.a[1]       = -0.0008206264;
  tableSet->AddAt(&row); // row 117
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 118;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01067062;
  row.a[1]       = -0.0008237676;
  tableSet->AddAt(&row); // row 118
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 119;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00757199;
  row.a[1]       =  -0.000763828;
  tableSet->AddAt(&row); // row 119
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 120;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003326811;
  row.a[1]       = -0.0006655148;
  tableSet->AddAt(&row); // row 120
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 121;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003326811;
  row.a[1]       = -0.0006655148;
  tableSet->AddAt(&row); // row 121
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 122;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003326811;
  row.a[1]       = -0.0006655148;
  tableSet->AddAt(&row); // row 122
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 123;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003326811;
  row.a[1]       = -0.0006655148;
  tableSet->AddAt(&row); // row 123
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 124;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 124
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 125;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 125
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 126;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 126
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 127;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 127
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 128;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 128
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 129;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 129
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 130;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01688862;
  row.a[1]       =  -0.001249979;
  tableSet->AddAt(&row); // row 130
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 131;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02388617;
  row.a[1]       =  -0.003108368;
  tableSet->AddAt(&row); // row 131
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 132;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03536438;
  row.a[1]       = -0.0003866426;
  tableSet->AddAt(&row); // row 132
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 133;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03536438;
  row.a[1]       = -0.0003866426;
  tableSet->AddAt(&row); // row 133
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 134;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03536438;
  row.a[1]       = -0.0003866426;
  tableSet->AddAt(&row); // row 134
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 135;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03536438;
  row.a[1]       = -0.0003866426;
  tableSet->AddAt(&row); // row 135
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 136;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.002044709;
  row.a[1]       =  -0.001237965;
  tableSet->AddAt(&row); // row 136
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 137;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02176876;
  row.a[1]       = -0.0008272522;
  tableSet->AddAt(&row); // row 137
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 138;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02137449;
  row.a[1]       = -0.0009919569;
  tableSet->AddAt(&row); // row 138
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 139;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01807517;
  row.a[1]       = -0.0009172297;
  tableSet->AddAt(&row); // row 139
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 140;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01525831;
  row.a[1]       =  -0.002371552;
  tableSet->AddAt(&row); // row 140
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 141;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01525831;
  row.a[1]       =  -0.002371552;
  tableSet->AddAt(&row); // row 141
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 142;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006906142;
  row.a[1]       = -0.0008545803;
  tableSet->AddAt(&row); // row 142
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 143;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006906142;
  row.a[1]       = -0.0008545803;
  tableSet->AddAt(&row); // row 143
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 144;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.001500574;
  row.a[1]       =  -0.001215194;
  tableSet->AddAt(&row); // row 144
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 145;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.8226102;
  row.a[1]       =    -0.0121958;
  tableSet->AddAt(&row); // row 145
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 146;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =       0.27006;
  row.a[1]       =  -0.004244237;
  tableSet->AddAt(&row); // row 146
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 147;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05854003;
  row.a[1]       =  -0.002041283;
  tableSet->AddAt(&row); // row 147
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 148;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05854003;
  row.a[1]       =  -0.002041283;
  tableSet->AddAt(&row); // row 148
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 149;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05854003;
  row.a[1]       =  -0.002041283;
  tableSet->AddAt(&row); // row 149
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 150;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007299068;
  row.a[1]       =  -0.000739992;
  tableSet->AddAt(&row); // row 150
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 151;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007299068;
  row.a[1]       =  -0.000739992;
  tableSet->AddAt(&row); // row 151
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 152;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003571095;
  row.a[1]       =  -0.001193774;
  tableSet->AddAt(&row); // row 152
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 153;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0356762;
  row.a[1]       =   -0.00126609;
  tableSet->AddAt(&row); // row 153
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 154;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03386329;
  row.a[1]       =   -0.00141713;
  tableSet->AddAt(&row); // row 154
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 155;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02873205;
  row.a[1]       =  -0.001318314;
  tableSet->AddAt(&row); // row 155
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 156;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02265016;
  row.a[1]       =  -0.003544887;
  tableSet->AddAt(&row); // row 156
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 157;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02047379;
  row.a[1]       =  -0.001635159;
  tableSet->AddAt(&row); // row 157
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 158;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.005764116;
  row.a[1]       = -0.0006900629;
  tableSet->AddAt(&row); // row 158
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 159;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.005764116;
  row.a[1]       = -0.0006900629;
  tableSet->AddAt(&row); // row 159
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 160;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.005764116;
  row.a[1]       = -0.0006900629;
  tableSet->AddAt(&row); // row 160
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 161;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03574473;
  row.a[1]       =  -0.001283498;
  tableSet->AddAt(&row); // row 161
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 162;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02847204;
  row.a[1]       =  -0.001250249;
  tableSet->AddAt(&row); // row 162
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 163;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02510758;
  row.a[1]       =  -0.002177273;
  tableSet->AddAt(&row); // row 163
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 164;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05863476;
  row.a[1]       =  -0.002806828;
  tableSet->AddAt(&row); // row 164
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 165;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05863476;
  row.a[1]       =  -0.002806828;
  tableSet->AddAt(&row); // row 165
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 166;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05863476;
  row.a[1]       =  -0.002806828;
  tableSet->AddAt(&row); // row 166
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 167;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.002461099;
  row.a[1]       = -0.0005899275;
  tableSet->AddAt(&row); // row 167
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 168;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       = -0.0004035172;
  row.a[1]       = -0.0006523775;
  tableSet->AddAt(&row); // row 168
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 169;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03287915;
  row.a[1]       =  -0.001250217;
  tableSet->AddAt(&row); // row 169
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 170;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02947805;
  row.a[1]       =  -0.001193344;
  tableSet->AddAt(&row); // row 170
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 171;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02396977;
  row.a[1]       =  -0.001225428;
  tableSet->AddAt(&row); // row 171
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 172;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02134484;
  row.a[1]       =  -0.001097246;
  tableSet->AddAt(&row); // row 172
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 173;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01003273;
  row.a[1]       = -0.0006290887;
  tableSet->AddAt(&row); // row 173
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 174;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00958618;
  row.a[1]       = -0.0008015565;
  tableSet->AddAt(&row); // row 174
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 175;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.008536918;
  row.a[1]       = -0.0008207045;
  tableSet->AddAt(&row); // row 175
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 176;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.003803425;
  row.a[1]       = -0.0005576593;
  tableSet->AddAt(&row); // row 176
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 177;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03828292;
  row.a[1]       =  -0.001104003;
  tableSet->AddAt(&row); // row 177
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 178;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03307568;
  row.a[1]       =  -0.001341171;
  tableSet->AddAt(&row); // row 178
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 179;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02502633;
  row.a[1]       =  -0.001139049;
  tableSet->AddAt(&row); // row 179
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 180;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =      0.020045;
  row.a[1]       =  -0.001124227;
  tableSet->AddAt(&row); // row 180
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 181;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01181486;
  row.a[1]       = -0.0007710794;
  tableSet->AddAt(&row); // row 181
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 182;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01060837;
  row.a[1]       = -0.0007024912;
  tableSet->AddAt(&row); // row 182
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 183;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006546149;
  row.a[1]       =  -0.000699566;
  tableSet->AddAt(&row); // row 183
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 184;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.00720555;
  row.a[1]       =  -0.001249639;
  tableSet->AddAt(&row); // row 184
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 185;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03768369;
  row.a[1]       =  -0.001091689;
  tableSet->AddAt(&row); // row 185
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 186;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02602344;
  row.a[1]       = -0.0009820769;
  tableSet->AddAt(&row); // row 186
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 187;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02251851;
  row.a[1]       =  -0.001103016;
  tableSet->AddAt(&row); // row 187
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 188;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01693787;
  row.a[1]       =  -0.001098569;
  tableSet->AddAt(&row); // row 188
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 189;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01241124;
  row.a[1]       = -0.0009816854;
  tableSet->AddAt(&row); // row 189
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 190;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.007364681;
  row.a[1]       = -0.0007499346;
  tableSet->AddAt(&row); // row 190
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 191;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.006553479;
  row.a[1]       = -0.0007966609;
  tableSet->AddAt(&row); // row 191
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.idx   = 192;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =   0.005875663;
  row.a[1]       = -0.0008541282;
  tableSet->AddAt(&row); // row 192
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

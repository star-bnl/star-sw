TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 192;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrectionX",nrows);
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   1;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06691644;
  row.a[1]       =    -0.1779473;
  tableSet->AddAt(&row); // row   1
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   2;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04176993;
  row.a[1]       =    -0.1234859;
  tableSet->AddAt(&row); // row   2
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   3;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02803196;
  row.a[1]       =   -0.09656797;
  tableSet->AddAt(&row); // row   3
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   4;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02604869;
  row.a[1]       =   -0.07317418;
  tableSet->AddAt(&row); // row   4
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   5;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0321285;
  row.a[1]       =    -0.1204445;
  tableSet->AddAt(&row); // row   5
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   6;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02369913;
  row.a[1]       =    -0.1116576;
  tableSet->AddAt(&row); // row   6
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   7;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01766867;
  row.a[1]       =    -0.0943832;
  tableSet->AddAt(&row); // row   7
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   8;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02136003;
  row.a[1]       =    -0.1134481;
  tableSet->AddAt(&row); // row   8
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =   9;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05488472;
  row.a[1]       =      -0.15811;
  tableSet->AddAt(&row); // row   9
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  10;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04296195;
  row.a[1]       =    -0.1258134;
  tableSet->AddAt(&row); // row  10
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  11;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04313551;
  row.a[1]       =    -0.1243882;
  tableSet->AddAt(&row); // row  11
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  12;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02207389;
  row.a[1]       =   -0.06036242;
  tableSet->AddAt(&row); // row  12
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  13;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02233408;
  row.a[1]       =   -0.08646502;
  tableSet->AddAt(&row); // row  13
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  14;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01701574;
  row.a[1]       =   -0.08429215;
  tableSet->AddAt(&row); // row  14
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  15;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02248401;
  row.a[1]       =    -0.1112125;
  tableSet->AddAt(&row); // row  15
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  16;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01730757;
  row.a[1]       =   -0.09250926;
  tableSet->AddAt(&row); // row  16
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  17;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05135983;
  row.a[1]       =    -0.1656379;
  tableSet->AddAt(&row); // row  17
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  18;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03573726;
  row.a[1]       =    -0.1137703;
  tableSet->AddAt(&row); // row  18
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  19;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02099486;
  row.a[1]       =   -0.06645768;
  tableSet->AddAt(&row); // row  19
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  20;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02210021;
  row.a[1]       =   -0.06260153;
  tableSet->AddAt(&row); // row  20
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  21;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02426455;
  row.a[1]       =   -0.07869292;
  tableSet->AddAt(&row); // row  21
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  22;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02001446;
  row.a[1]       =   -0.08511682;
  tableSet->AddAt(&row); // row  22
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  23;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02198203;
  row.a[1]       =   -0.09377448;
  tableSet->AddAt(&row); // row  23
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  24;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01474716;
  row.a[1]       =   -0.08814642;
  tableSet->AddAt(&row); // row  24
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  25;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06950208;
  row.a[1]       =    -0.1812491;
  tableSet->AddAt(&row); // row  25
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  26;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04010954;
  row.a[1]       =    -0.1339634;
  tableSet->AddAt(&row); // row  26
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  27;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02639837;
  row.a[1]       =   -0.09668085;
  tableSet->AddAt(&row); // row  27
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  28;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02483468;
  row.a[1]       =   -0.07238418;
  tableSet->AddAt(&row); // row  28
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  29;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02505278;
  row.a[1]       =   -0.09259927;
  tableSet->AddAt(&row); // row  29
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  30;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02194604;
  row.a[1]       =   -0.09393393;
  tableSet->AddAt(&row); // row  30
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  31;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03160427;
  row.a[1]       =     -0.145425;
  tableSet->AddAt(&row); // row  31
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  32;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03061179;
  row.a[1]       =     -0.149576;
  tableSet->AddAt(&row); // row  32
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  33;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06301924;
  row.a[1]       =    -0.1684257;
  tableSet->AddAt(&row); // row  33
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  34;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04302694;
  row.a[1]       =    -0.1280875;
  tableSet->AddAt(&row); // row  34
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  35;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03189883;
  row.a[1]       =    -0.1046367;
  tableSet->AddAt(&row); // row  35
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  36;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0273195;
  row.a[1]       =   -0.08110179;
  tableSet->AddAt(&row); // row  36
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  37;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02573161;
  row.a[1]       =   -0.09541417;
  tableSet->AddAt(&row); // row  37
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  38;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01475827;
  row.a[1]       =    -0.0848503;
  tableSet->AddAt(&row); // row  38
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  39;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03116445;
  row.a[1]       =    -0.1839042;
  tableSet->AddAt(&row); // row  39
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  40;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03053587;
  row.a[1]       =    -0.1938592;
  tableSet->AddAt(&row); // row  40
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  41;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04695443;
  row.a[1]       =    -0.1429099;
  tableSet->AddAt(&row); // row  41
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  42;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04145156;
  row.a[1]       =    -0.1248686;
  tableSet->AddAt(&row); // row  42
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  43;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02748704;
  row.a[1]       =   -0.08604885;
  tableSet->AddAt(&row); // row  43
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  44;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01720746;
  row.a[1]       =    -0.0477419;
  tableSet->AddAt(&row); // row  44
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  45;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02305877;
  row.a[1]       =   -0.08619019;
  tableSet->AddAt(&row); // row  45
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  46;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0290181;
  row.a[1]       =    -0.1525636;
  tableSet->AddAt(&row); // row  46
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  47;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0415166;
  row.a[1]       =    -0.2194435;
  tableSet->AddAt(&row); // row  47
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  48;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04014461;
  row.a[1]       =    -0.2326205;
  tableSet->AddAt(&row); // row  48
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  49;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06327232;
  row.a[1]       =    -0.1811209;
  tableSet->AddAt(&row); // row  49
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  50;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03904498;
  row.a[1]       =    -0.1100876;
  tableSet->AddAt(&row); // row  50
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  51;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03329573;
  row.a[1]       =    -0.1043311;
  tableSet->AddAt(&row); // row  51
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  52;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02242785;
  row.a[1]       =    -0.0594125;
  tableSet->AddAt(&row); // row  52
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  53;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02530684;
  row.a[1]       =   -0.09001853;
  tableSet->AddAt(&row); // row  53
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  54;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0284152;
  row.a[1]       =    -0.2425836;
  tableSet->AddAt(&row); // row  54
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  55;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01889048;
  row.a[1]       =   -0.09049447;
  tableSet->AddAt(&row); // row  55
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  56;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03020105;
  row.a[1]       =    -0.1662987;
  tableSet->AddAt(&row); // row  56
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  57;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06360003;
  row.a[1]       =    -0.1722153;
  tableSet->AddAt(&row); // row  57
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  58;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04193693;
  row.a[1]       =     -0.123056;
  tableSet->AddAt(&row); // row  58
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  59;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0308779;
  row.a[1]       =   -0.09510856;
  tableSet->AddAt(&row); // row  59
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  60;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01824715;
  row.a[1]       =   -0.06967629;
  tableSet->AddAt(&row); // row  60
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  61;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02091739;
  row.a[1]       =   -0.07804868;
  tableSet->AddAt(&row); // row  61
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  62;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03212769;
  row.a[1]       =    -0.1447872;
  tableSet->AddAt(&row); // row  62
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  63;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02936948;
  row.a[1]       =    -0.1630941;
  tableSet->AddAt(&row); // row  63
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  64;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02363254;
  row.a[1]       =    -0.1339476;
  tableSet->AddAt(&row); // row  64
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  65;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05534861;
  row.a[1]       =    -0.1560739;
  tableSet->AddAt(&row); // row  65
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  66;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04499696;
  row.a[1]       =    -0.1494852;
  tableSet->AddAt(&row); // row  66
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  67;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03361979;
  row.a[1]       =    -0.1091307;
  tableSet->AddAt(&row); // row  67
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  68;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02783262;
  row.a[1]       =   -0.07858615;
  tableSet->AddAt(&row); // row  68
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  69;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0240176;
  row.a[1]       =   -0.08834261;
  tableSet->AddAt(&row); // row  69
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  70;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02309788;
  row.a[1]       =    -0.1084689;
  tableSet->AddAt(&row); // row  70
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  71;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02401478;
  row.a[1]       =    -0.1208208;
  tableSet->AddAt(&row); // row  71
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  72;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03153506;
  row.a[1]       =    -0.1665086;
  tableSet->AddAt(&row); // row  72
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  73;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06409879;
  row.a[1]       =    -0.1739823;
  tableSet->AddAt(&row); // row  73
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  74;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04060883;
  row.a[1]       =    -0.1274406;
  tableSet->AddAt(&row); // row  74
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  75;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03107577;
  row.a[1]       =    -0.1061862;
  tableSet->AddAt(&row); // row  75
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  76;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0248978;
  row.a[1]       =   -0.06615821;
  tableSet->AddAt(&row); // row  76
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  77;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02238417;
  row.a[1]       =   -0.08142872;
  tableSet->AddAt(&row); // row  77
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  78;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01689224;
  row.a[1]       =   -0.08083404;
  tableSet->AddAt(&row); // row  78
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  79;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02165948;
  row.a[1]       =    -0.1000007;
  tableSet->AddAt(&row); // row  79
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  80;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02110268;
  row.a[1]       =    -0.1075876;
  tableSet->AddAt(&row); // row  80
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  81;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05134212;
  row.a[1]       =    -0.1540157;
  tableSet->AddAt(&row); // row  81
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  82;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0362302;
  row.a[1]       =    -0.1066182;
  tableSet->AddAt(&row); // row  82
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  83;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02428871;
  row.a[1]       =   -0.08078132;
  tableSet->AddAt(&row); // row  83
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  84;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01566183;
  row.a[1]       =   -0.05037662;
  tableSet->AddAt(&row); // row  84
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  85;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01738201;
  row.a[1]       =   -0.06663077;
  tableSet->AddAt(&row); // row  85
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  86;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01927902;
  row.a[1]       =   -0.09144855;
  tableSet->AddAt(&row); // row  86
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  87;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02035303;
  row.a[1]       =    -0.1005654;
  tableSet->AddAt(&row); // row  87
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  88;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02421223;
  row.a[1]       =    -0.1063177;
  tableSet->AddAt(&row); // row  88
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  89;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05935962;
  row.a[1]       =    -0.1654081;
  tableSet->AddAt(&row); // row  89
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  90;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03862989;
  row.a[1]       =    -0.1136954;
  tableSet->AddAt(&row); // row  90
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  91;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0336408;
  row.a[1]       =    -0.1045493;
  tableSet->AddAt(&row); // row  91
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  92;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03086595;
  row.a[1]       =   -0.08401121;
  tableSet->AddAt(&row); // row  92
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  93;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01866603;
  row.a[1]       =   -0.06581679;
  tableSet->AddAt(&row); // row  93
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  94;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01764127;
  row.a[1]       =   -0.08012437;
  tableSet->AddAt(&row); // row  94
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  95;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02322541;
  row.a[1]       =    -0.1162328;
  tableSet->AddAt(&row); // row  95
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  96;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01636291;
  row.a[1]       =   -0.08508001;
  tableSet->AddAt(&row); // row  96
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  97;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.08015586;
  row.a[1]       =    -0.1641202;
  tableSet->AddAt(&row); // row  97
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  98;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0616129;
  row.a[1]       =    -0.1234969;
  tableSet->AddAt(&row); // row  98
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   =  99;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03384329;
  row.a[1]       =    -0.0788045;
  tableSet->AddAt(&row); // row  99
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 100;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03116801;
  row.a[1]       =   -0.06016841;
  tableSet->AddAt(&row); // row 100
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 101;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02320591;
  row.a[1]       =   -0.06710047;
  tableSet->AddAt(&row); // row 101
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 102;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02393878;
  row.a[1]       =   -0.07631934;
  tableSet->AddAt(&row); // row 102
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 103;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02619966;
  row.a[1]       =    -0.1636358;
  tableSet->AddAt(&row); // row 103
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 104;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01802427;
  row.a[1]       =   -0.06911335;
  tableSet->AddAt(&row); // row 104
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 105;
  row.nrows = 192;
  tableSet->AddAt(&row); // row 105
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 106;
  row.nrows = 192;
  tableSet->AddAt(&row); // row 106
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 107;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06032875;
  row.a[1]       =    -0.1181864;
  tableSet->AddAt(&row); // row 107
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 108;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02814457;
  row.a[1]       =   -0.05352046;
  tableSet->AddAt(&row); // row 108
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 109;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02696645;
  row.a[1]       =   -0.07022296;
  tableSet->AddAt(&row); // row 109
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 110;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02927657;
  row.a[1]       =   -0.09837736;
  tableSet->AddAt(&row); // row 110
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 111;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02106393;
  row.a[1]       =   -0.07430119;
  tableSet->AddAt(&row); // row 111
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 112;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02766131;
  row.a[1]       =    -0.1059418;
  tableSet->AddAt(&row); // row 112
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 113;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.08562106;
  row.a[1]       =    -0.1633663;
  tableSet->AddAt(&row); // row 113
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 114;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06818716;
  row.a[1]       =     -0.127554;
  tableSet->AddAt(&row); // row 114
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 115;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04137104;
  row.a[1]       =   -0.08578087;
  tableSet->AddAt(&row); // row 115
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 116;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02983104;
  row.a[1]       =   -0.05736021;
  tableSet->AddAt(&row); // row 116
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 117;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02470343;
  row.a[1]       =   -0.06860503;
  tableSet->AddAt(&row); // row 117
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 118;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02120529;
  row.a[1]       =   -0.07018704;
  tableSet->AddAt(&row); // row 118
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 119;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02713381;
  row.a[1]       =   -0.09203599;
  tableSet->AddAt(&row); // row 119
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 120;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01961274;
  row.a[1]       =   -0.08206464;
  tableSet->AddAt(&row); // row 120
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 121;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.08219395;
  row.a[1]       =    -0.1607085;
  tableSet->AddAt(&row); // row 121
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 122;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05700955;
  row.a[1]       =     -0.115926;
  tableSet->AddAt(&row); // row 122
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 123;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04052554;
  row.a[1]       =   -0.08921486;
  tableSet->AddAt(&row); // row 123
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 124;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02657954;
  row.a[1]       =    -0.0526318;
  tableSet->AddAt(&row); // row 124
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 125;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02814598;
  row.a[1]       =   -0.07345997;
  tableSet->AddAt(&row); // row 125
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 126;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01974475;
  row.a[1]       =   -0.07180895;
  tableSet->AddAt(&row); // row 126
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 127;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02742928;
  row.a[1]       =    -0.1045222;
  tableSet->AddAt(&row); // row 127
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 128;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02198531;
  row.a[1]       =   -0.07450246;
  tableSet->AddAt(&row); // row 128
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 129;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.09325475;
  row.a[1]       =    -0.1750781;
  tableSet->AddAt(&row); // row 129
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 130;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05444528;
  row.a[1]       =    -0.1136409;
  tableSet->AddAt(&row); // row 130
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 131;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05363624;
  row.a[1]       =    -0.1154614;
  tableSet->AddAt(&row); // row 131
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 132;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03192978;
  row.a[1]       =   -0.06667139;
  tableSet->AddAt(&row); // row 132
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 133;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02883718;
  row.a[1]       =   -0.08203288;
  tableSet->AddAt(&row); // row 133
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 134;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02323434;
  row.a[1]       =    -0.0831144;
  tableSet->AddAt(&row); // row 134
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 135;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02819328;
  row.a[1]       =    -0.1099425;
  tableSet->AddAt(&row); // row 135
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 136;
  row.nrows = 192;
  tableSet->AddAt(&row); // row 136
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 137;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.09103891;
  row.a[1]       =    -0.1710265;
  tableSet->AddAt(&row); // row 137
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 138;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05787078;
  row.a[1]       =     -0.115294;
  tableSet->AddAt(&row); // row 138
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 139;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04027946;
  row.a[1]       =   -0.09110699;
  tableSet->AddAt(&row); // row 139
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 140;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03321175;
  row.a[1]       =   -0.06617272;
  tableSet->AddAt(&row); // row 140
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 141;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03045662;
  row.a[1]       =   -0.07315454;
  tableSet->AddAt(&row); // row 141
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 142;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.01891986;
  row.a[1]       =   -0.06623166;
  tableSet->AddAt(&row); // row 142
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 143;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02291199;
  row.a[1]       =   -0.08552025;
  tableSet->AddAt(&row); // row 143
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 144;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02701901;
  row.a[1]       =    -0.1030158;
  tableSet->AddAt(&row); // row 144
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 145;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0932892;
  row.a[1]       =    -0.1717198;
  tableSet->AddAt(&row); // row 145
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 146;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05490632;
  row.a[1]       =    -0.1127453;
  tableSet->AddAt(&row); // row 146
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 147;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04038152;
  row.a[1]       =   -0.09104704;
  tableSet->AddAt(&row); // row 147
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 148;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03077407;
  row.a[1]       =   -0.05939453;
  tableSet->AddAt(&row); // row 148
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 149;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0289538;
  row.a[1]       =   -0.07124053;
  tableSet->AddAt(&row); // row 149
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 150;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02811246;
  row.a[1]       =   -0.09272677;
  tableSet->AddAt(&row); // row 150
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 151;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02326304;
  row.a[1]       =   -0.09136086;
  tableSet->AddAt(&row); // row 151
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 152;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02492002;
  row.a[1]       =   -0.09026901;
  tableSet->AddAt(&row); // row 152
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 153;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06618629;
  row.a[1]       =    -0.1486675;
  tableSet->AddAt(&row); // row 153
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 154;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04625431;
  row.a[1]       =   -0.09672502;
  tableSet->AddAt(&row); // row 154
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 155;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04383533;
  row.a[1]       =   -0.09189499;
  tableSet->AddAt(&row); // row 155
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 156;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02493729;
  row.a[1]       =   -0.05451764;
  tableSet->AddAt(&row); // row 156
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 157;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02175204;
  row.a[1]       =   -0.08208009;
  tableSet->AddAt(&row); // row 157
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 158;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02229746;
  row.a[1]       =   -0.07555089;
  tableSet->AddAt(&row); // row 158
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 159;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02733009;
  row.a[1]       =    -0.1081994;
  tableSet->AddAt(&row); // row 159
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 160;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02490766;
  row.a[1]       =   -0.09368234;
  tableSet->AddAt(&row); // row 160
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 161;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.08755573;
  row.a[1]       =    -0.1675407;
  tableSet->AddAt(&row); // row 161
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 162;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.06035121;
  row.a[1]       =    -0.1176386;
  tableSet->AddAt(&row); // row 162
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 163;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04850322;
  row.a[1]       =    -0.1072683;
  tableSet->AddAt(&row); // row 163
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 164;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03471289;
  row.a[1]       =   -0.06673372;
  tableSet->AddAt(&row); // row 164
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 165;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02066979;
  row.a[1]       =   -0.05510799;
  tableSet->AddAt(&row); // row 165
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 166;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02121684;
  row.a[1]       =   -0.07180098;
  tableSet->AddAt(&row); // row 166
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 167;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02754845;
  row.a[1]       =   -0.09665172;
  tableSet->AddAt(&row); // row 167
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 168;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02391503;
  row.a[1]       =   -0.09448932;
  tableSet->AddAt(&row); // row 168
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 169;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0936404;
  row.a[1]       =    -0.1707302;
  tableSet->AddAt(&row); // row 169
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 170;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05536126;
  row.a[1]       =    -0.1151567;
  tableSet->AddAt(&row); // row 170
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 171;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04046954;
  row.a[1]       =   -0.08659393;
  tableSet->AddAt(&row); // row 171
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 172;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03336591;
  row.a[1]       =   -0.06663975;
  tableSet->AddAt(&row); // row 172
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 173;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02327549;
  row.a[1]       =   -0.06168928;
  tableSet->AddAt(&row); // row 173
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 174;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02052753;
  row.a[1]       =   -0.06913428;
  tableSet->AddAt(&row); // row 174
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 175;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02318683;
  row.a[1]       =   -0.09180073;
  tableSet->AddAt(&row); // row 175
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 176;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02230884;
  row.a[1]       =   -0.08562475;
  tableSet->AddAt(&row); // row 176
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 177;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0926003;
  row.a[1]       =     -0.178259;
  tableSet->AddAt(&row); // row 177
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 178;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05668091;
  row.a[1]       =     -0.115966;
  tableSet->AddAt(&row); // row 178
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 179;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03116641;
  row.a[1]       =   -0.08406382;
  tableSet->AddAt(&row); // row 179
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 180;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02473744;
  row.a[1]       =   -0.06597449;
  tableSet->AddAt(&row); // row 180
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 181;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02413328;
  row.a[1]       =   -0.06398916;
  tableSet->AddAt(&row); // row 181
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 182;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =     0.0251631;
  row.a[1]       =   -0.08625035;
  tableSet->AddAt(&row); // row 182
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 183;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02484735;
  row.a[1]       =   -0.09323832;
  tableSet->AddAt(&row); // row 183
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 184;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02822394;
  row.a[1]       =    -0.1051015;
  tableSet->AddAt(&row); // row 184
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 185;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.07834178;
  row.a[1]       =    -0.1548014;
  tableSet->AddAt(&row); // row 185
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 186;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.05835064;
  row.a[1]       =    -0.1134553;
  tableSet->AddAt(&row); // row 186
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 187;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.04147112;
  row.a[1]       =   -0.08568215;
  tableSet->AddAt(&row); // row 187
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 188;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.03207968;
  row.a[1]       =   -0.06359711;
  tableSet->AddAt(&row); // row 188
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 189;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02925305;
  row.a[1]       =   -0.07134705;
  tableSet->AddAt(&row); // row 189
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 190;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02612099;
  row.a[1]       =   -0.08623618;
  tableSet->AddAt(&row); // row 190
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 191;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02696469;
  row.a[1]       =   -0.09802922;
  tableSet->AddAt(&row); // row 191
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVIdAu200p203.root
  row.idx   = 192;
  row.nrows = 192;
  row.npar       =             2;
  row.a[0]       =    0.02248317;
  row.a[1]       =   -0.08264418;
  tableSet->AddAt(&row); // row 192
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

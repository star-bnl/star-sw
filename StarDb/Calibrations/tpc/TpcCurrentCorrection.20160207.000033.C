TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 192;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   1;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01383526;
  row.a[1]       =    0.05834656;
  row.a[2]       =   -0.05839385;
  row.a[3]       =   -0.04731573;
  tableSet->AddAt(&row); // row   1
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   2;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03603714;
  row.a[1]       =    -0.1085532;
  row.a[2]       =     0.3391257;
  row.a[3]       =    -0.3245876;
  tableSet->AddAt(&row); // row   2
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   3;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1062128;
  row.a[1]       =    -0.5240045;
  row.a[2]       =       1.17107;
  row.a[3]       =    -0.8969009;
  tableSet->AddAt(&row); // row   3
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   4;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01972083;
  row.a[1]       =    0.06186347;
  row.a[2]       =    -0.1751421;
  row.a[3]       =    0.08670607;
  tableSet->AddAt(&row); // row   4
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   5;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  0.0008122461;
  row.a[1]       =   0.006864741;
  row.a[2]       =    -0.3063111;
  row.a[3]       =     0.3485127;
  tableSet->AddAt(&row); // row   5
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   6;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03124276;
  row.a[1]       =   0.002093727;
  row.a[2]       =    -0.2615002;
  row.a[3]       =     0.3592252;
  tableSet->AddAt(&row); // row   6
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   7;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01435947;
  row.a[1]       =    0.02512075;
  row.a[2]       =    -0.6076387;
  row.a[3]       =      1.063224;
  tableSet->AddAt(&row); // row   7
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   8;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04973053;
  row.a[1]       =     0.0650193;
  row.a[2]       =    -0.7806414;
  row.a[3]       =      1.292688;
  tableSet->AddAt(&row); // row   8
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =   9;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01968569;
  row.a[1]       =     0.1130978;
  row.a[2]       =    -0.3071606;
  row.a[3]       =      0.158017;
  tableSet->AddAt(&row); // row   9
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  10;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.002604327;
  row.a[1]       =     0.1717069;
  row.a[2]       =    -0.3690537;
  row.a[3]       =     0.1738717;
  tableSet->AddAt(&row); // row  10
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  11;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02278957;
  row.a[1]       =     0.3770219;
  row.a[2]       =    -0.8254558;
  row.a[3]       =      0.507997;
  tableSet->AddAt(&row); // row  11
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  12;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02669145;
  row.a[1]       =    -0.0625156;
  row.a[2]       =    0.03448725;
  row.a[3]       =   0.001634402;
  tableSet->AddAt(&row); // row  12
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  13;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  -0.005107049;
  row.a[1]       =    0.09377155;
  row.a[2]       =    -0.4686474;
  row.a[3]       =     0.4548712;
  tableSet->AddAt(&row); // row  13
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  14;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  -0.008528939;
  row.a[1]       =     0.1354645;
  row.a[2]       =    -0.8338954;
  row.a[3]       =      1.120083;
  tableSet->AddAt(&row); // row  14
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  15;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.004390729;
  row.a[1]       =    0.05186822;
  row.a[2]       =    -0.6747439;
  row.a[3]       =      1.122011;
  tableSet->AddAt(&row); // row  15
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  16;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01810071;
  row.a[1]       =     0.1082573;
  row.a[2]       =     -0.816085;
  row.a[3]       =      1.213251;
  tableSet->AddAt(&row); // row  16
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  17;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.007331466;
  row.a[1]       =      0.142005;
  row.a[2]       =    -0.3084547;
  row.a[3]       =     0.1050535;
  tableSet->AddAt(&row); // row  17
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  18;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02229339;
  row.a[1]       =   -0.03246976;
  row.a[2]       =    0.02131549;
  row.a[3]       =   -0.07432641;
  tableSet->AddAt(&row); // row  18
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  19;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.06073471;
  row.a[1]       =    -0.4070672;
  row.a[2]       =      1.095421;
  row.a[3]       =    -0.9596128;
  tableSet->AddAt(&row); // row  19
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  20;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.000855038;
  row.a[1]       =    0.04662402;
  row.a[2]       =   -0.07831523;
  row.a[3]       =   0.001919285;
  tableSet->AddAt(&row); // row  20
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  21;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.005285602;
  row.a[1]       =     0.2026844;
  row.a[2]       =    -0.7182032;
  row.a[3]       =     0.6180758;
  tableSet->AddAt(&row); // row  21
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  22;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04418191;
  row.a[1]       =   0.005247122;
  row.a[2]       =    -0.3937521;
  row.a[3]       =      0.539505;
  tableSet->AddAt(&row); // row  22
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  23;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.06253711;
  row.a[1]       =   -0.03432853;
  row.a[2]       =    -0.3384204;
  row.a[3]       =     0.5571951;
  tableSet->AddAt(&row); // row  23
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  24;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05090072;
  row.a[1]       =    -0.1022152;
  row.a[2]       =    -0.1518902;
  row.a[3]       =     0.5256356;
  tableSet->AddAt(&row); // row  24
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  25;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02266721;
  row.a[1]       =   -0.04364704;
  row.a[2]       =     -0.028469;
  row.a[3]       =  -0.005851549;
  tableSet->AddAt(&row); // row  25
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  26;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03065368;
  row.a[1]       =    -0.3118859;
  row.a[2]       =     0.5910981;
  row.a[3]       =    -0.4197639;
  tableSet->AddAt(&row); // row  26
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  27;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    -0.0109329;
  row.a[1]       =   -0.07221731;
  row.a[2]       =     0.1389219;
  row.a[3]       =    -0.1472724;
  tableSet->AddAt(&row); // row  27
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  28;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.01607002;
  row.a[1]       =   -0.05372144;
  row.a[2]       =    0.02025138;
  row.a[3]       =   0.002029746;
  tableSet->AddAt(&row); // row  28
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  29;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03435083;
  row.a[1]       =       0.01158;
  row.a[2]       =    -0.2688094;
  row.a[3]       =     0.2894272;
  tableSet->AddAt(&row); // row  29
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  30;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01539423;
  row.a[1]       =     0.1534326;
  row.a[2]       =    -0.9017994;
  row.a[3]       =      1.096634;
  tableSet->AddAt(&row); // row  30
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  31;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0130503;
  row.a[1]       =    0.05199014;
  row.a[2]       =    -0.6983507;
  row.a[3]       =      1.082628;
  tableSet->AddAt(&row); // row  31
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  32;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05215738;
  row.a[1]       =    -0.4210133;
  row.a[2]       =     0.8895025;
  row.a[3]       =    -0.6700164;
  tableSet->AddAt(&row); // row  32
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  33;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0350369;
  row.a[1]       =  -0.004735337;
  row.a[2]       =   -0.06525169;
  row.a[3]       =   0.002949688;
  tableSet->AddAt(&row); // row  33
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  34;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.08674294;
  row.a[1]       =    -0.4120464;
  row.a[2]       =     0.8664948;
  row.a[3]       =    -0.6451465;
  tableSet->AddAt(&row); // row  34
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  35;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01549502;
  row.a[1]       =   0.001151143;
  row.a[2]       =   0.006421452;
  row.a[3]       =    -0.1015823;
  tableSet->AddAt(&row); // row  35
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  36;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.03639165;
  row.a[1]       =     0.2417479;
  row.a[2]       =    -0.4720647;
  row.a[3]       =     0.2678496;
  tableSet->AddAt(&row); // row  36
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  37;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02197667;
  row.a[1]       =     0.2722171;
  row.a[2]       =    -0.9528541;
  row.a[3]       =     0.8526876;
  tableSet->AddAt(&row); // row  37
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  38;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.03322098;
  row.a[1]       =    0.05789789;
  row.a[2]       =    -0.5483821;
  row.a[3]       =     0.7512849;
  tableSet->AddAt(&row); // row  38
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  39;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.07558335;
  row.a[1]       =   -0.01500251;
  row.a[2]       =    -0.5953644;
  row.a[3]       =      1.094488;
  tableSet->AddAt(&row); // row  39
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  40;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    -0.2069242;
  row.a[1]       =     0.3786985;
  row.a[2]       =     -2.087359;
  row.a[3]       =       3.12644;
  tableSet->AddAt(&row); // row  40
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  41;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02200073;
  row.a[1]       =    0.05364861;
  row.a[2]       =    -0.1417842;
  row.a[3]       =    0.03523734;
  tableSet->AddAt(&row); // row  41
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  42;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0128938;
  row.a[1]       =    0.05234765;
  row.a[2]       =    -0.1731074;
  row.a[3]       =    0.07648745;
  tableSet->AddAt(&row); // row  42
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  43;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.002214918;
  row.a[1]       =    0.03078268;
  row.a[2]       =    -0.1848645;
  row.a[3]       =     0.1282777;
  tableSet->AddAt(&row); // row  43
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  44;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.009386006;
  row.a[1]       =    -0.1564168;
  row.a[2]       =     0.4042572;
  row.a[3]       =    -0.3398916;
  tableSet->AddAt(&row); // row  44
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  45;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02464038;
  row.a[1]       =     0.0707346;
  row.a[2]       =    -0.4616029;
  row.a[3]       =     0.5023463;
  tableSet->AddAt(&row); // row  45
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  46;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.03242802;
  row.a[1]       =    -0.1373469;
  row.a[2]       =   -0.02261197;
  row.a[3]       =     0.2510726;
  tableSet->AddAt(&row); // row  46
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  47;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.03423213;
  row.a[1]       =    0.04214841;
  row.a[2]       =    -0.9019501;
  row.a[3]       =      1.567017;
  tableSet->AddAt(&row); // row  47
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  48;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  -0.005236489;
  row.a[1]       =     -0.202841;
  row.a[2]       =    0.07228932;
  row.a[3]       =     0.3745625;
  tableSet->AddAt(&row); // row  48
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  49;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.008501622;
  row.a[1]       =     0.1180492;
  row.a[2]       =    -0.3227121;
  row.a[3]       =     0.1466404;
  tableSet->AddAt(&row); // row  49
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  50;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01431678;
  row.a[1]       =    0.09497715;
  row.a[2]       =    -0.2799892;
  row.a[3]       =     0.1390889;
  tableSet->AddAt(&row); // row  50
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  51;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04079338;
  row.a[1]       =   -0.09312378;
  row.a[2]       =     0.2625728;
  row.a[3]       =    -0.3124489;
  tableSet->AddAt(&row); // row  51
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  52;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01623583;
  row.a[1]       =   0.002262148;
  row.a[2]       =   -0.08280252;
  row.a[3]       =    0.05680367;
  tableSet->AddAt(&row); // row  52
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  53;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01516385;
  row.a[1]       =    0.04815715;
  row.a[2]       =    -0.3653051;
  row.a[3]       =     0.3824786;
  tableSet->AddAt(&row); // row  53
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  54;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.09700439;
  row.a[1]       =    -0.5377115;
  row.a[2]       =      4.437929;
  row.a[3]       =     -13.33014;
  tableSet->AddAt(&row); // row  54
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  55;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07325876;
  row.a[1]       =    -0.3485241;
  row.a[2]       =     0.8266455;
  row.a[3]       =    -0.7097493;
  tableSet->AddAt(&row); // row  55
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  56;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05732452;
  row.a[1]       =    -0.3562438;
  row.a[2]       =     0.7156551;
  row.a[3]       =    -0.4844454;
  tableSet->AddAt(&row); // row  56
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  57;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1363073;
  row.a[1]       =    -0.4404643;
  row.a[2]       =     0.6269327;
  row.a[3]       =    -0.3509397;
  tableSet->AddAt(&row); // row  57
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  58;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01947962;
  row.a[1]       =      0.011583;
  row.a[2]       =   -0.07414393;
  row.a[3]       =  -0.002894927;
  tableSet->AddAt(&row); // row  58
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  59;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02698284;
  row.a[1]       =     -0.098072;
  row.a[2]       =     0.1455604;
  row.a[3]       =    -0.1093799;
  tableSet->AddAt(&row); // row  59
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  60;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.07467706;
  row.a[1]       =     0.1058227;
  row.a[2]       =    -0.3878107;
  row.a[3]       =     0.3421399;
  tableSet->AddAt(&row); // row  60
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  61;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01617743;
  row.a[1]       =     0.0558755;
  row.a[2]       =    -0.3238211;
  row.a[3]       =     0.3036443;
  tableSet->AddAt(&row); // row  61
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  62;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02027539;
  row.a[1]       =   -0.02371605;
  row.a[2]       =    -0.3077398;
  row.a[3]       =     0.5131539;
  tableSet->AddAt(&row); // row  62
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  63;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.01486184;
  row.a[1]       =   -0.02736209;
  row.a[2]       =    -0.3925487;
  row.a[3]       =      0.724795;
  tableSet->AddAt(&row); // row  63
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  64;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.008294235;
  row.a[1]       =    0.00306028;
  row.a[2]       =    -0.6308169;
  row.a[3]       =       1.05435;
  tableSet->AddAt(&row); // row  64
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  65;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03889306;
  row.a[1]       =   -0.05363452;
  row.a[2]       =    0.05905804;
  row.a[3]       =   -0.08790388;
  tableSet->AddAt(&row); // row  65
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  66;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01360294;
  row.a[1]       =    0.08045867;
  row.a[2]       =    -0.1908236;
  row.a[3]       =    0.04995714;
  tableSet->AddAt(&row); // row  66
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  67;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.002188675;
  row.a[1]       =    0.07196544;
  row.a[2]       =    -0.2019464;
  row.a[3]       =    0.08851279;
  tableSet->AddAt(&row); // row  67
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  68;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.05223144;
  row.a[1]       =     0.2163316;
  row.a[2]       =     -0.529862;
  row.a[3]       =     0.3735175;
  tableSet->AddAt(&row); // row  68
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  69;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.004398449;
  row.a[1]       =     0.1197233;
  row.a[2]       =    -0.5250457;
  row.a[3]       =     0.4921471;
  tableSet->AddAt(&row); // row  69
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  70;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.009775038;
  row.a[1]       =    0.08381219;
  row.a[2]       =    -0.7034752;
  row.a[3]       =      0.942989;
  tableSet->AddAt(&row); // row  70
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  71;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.01103629;
  row.a[1]       =   -0.09390636;
  row.a[2]       =    -0.1701382;
  row.a[3]       =     0.4978708;
  tableSet->AddAt(&row); // row  71
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  72;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.04097164;
  row.a[1]       =    -0.2388233;
  row.a[2]       =     0.2404421;
  row.a[3]       =    0.09120144;
  tableSet->AddAt(&row); // row  72
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  73;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.00908894;
  row.a[1]       =      0.163295;
  row.a[2]       =      -0.41697;
  row.a[3]       =     0.2240723;
  tableSet->AddAt(&row); // row  73
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  74;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1079957;
  row.a[1]       =    -0.6489136;
  row.a[2]       =      1.046189;
  row.a[3]       =    -0.5580672;
  tableSet->AddAt(&row); // row  74
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  75;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.01355139;
  row.a[1]       =    0.09996659;
  row.a[2]       =    -0.2868368;
  row.a[3]       =     0.1635999;
  tableSet->AddAt(&row); // row  75
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  76;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01813166;
  row.a[1]       =   -0.01682413;
  row.a[2]       =   0.002664062;
  row.a[3]       =   -0.03119429;
  tableSet->AddAt(&row); // row  76
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  77;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02624416;
  row.a[1]       =  -0.006325463;
  row.a[2]       =    -0.2154564;
  row.a[3]       =     0.2424612;
  tableSet->AddAt(&row); // row  77
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  78;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01553133;
  row.a[1]       =    0.02292903;
  row.a[2]       =    -0.3179734;
  row.a[3]       =     0.4154131;
  tableSet->AddAt(&row); // row  78
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  79;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.009393405;
  row.a[1]       =     0.1330173;
  row.a[2]       =    -0.8714317;
  row.a[3]       =      1.179064;
  tableSet->AddAt(&row); // row  79
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  80;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0309616;
  row.a[1]       =    0.01651103;
  row.a[2]       =    -0.4422463;
  row.a[3]       =     0.7790444;
  tableSet->AddAt(&row); // row  80
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  81;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03845663;
  row.a[1]       =   -0.04173779;
  row.a[2]       =   -0.02087548;
  row.a[3]       =   -0.02206596;
  tableSet->AddAt(&row); // row  81
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  82;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04401718;
  row.a[1]       =    -0.2427031;
  row.a[2]       =     0.4635518;
  row.a[3]       =    -0.3646785;
  tableSet->AddAt(&row); // row  82
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  83;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0244418;
  row.a[1]       =    0.03410647;
  row.a[2]       =   -0.08574953;
  row.a[3]       =    -0.0141196;
  tableSet->AddAt(&row); // row  83
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  84;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02840448;
  row.a[1]       =   -0.03147045;
  row.a[2]       =     0.1107517;
  row.a[3]       =    -0.1436538;
  tableSet->AddAt(&row); // row  84
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  85;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04494215;
  row.a[1]       =     0.0387717;
  row.a[2]       =    -0.3722489;
  row.a[3]       =     0.3800595;
  tableSet->AddAt(&row); // row  85
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  86;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.06088889;
  row.a[1]       =     0.1306346;
  row.a[2]       =    -0.6765745;
  row.a[3]       =     0.7313547;
  tableSet->AddAt(&row); // row  86
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  87;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07235611;
  row.a[1]       =    0.07186054;
  row.a[2]       =    -0.5110758;
  row.a[3]       =     0.6295239;
  tableSet->AddAt(&row); // row  87
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  88;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.09018652;
  row.a[1]       =     0.1350073;
  row.a[2]       =    -0.7442091;
  row.a[3]       =     0.8605765;
  tableSet->AddAt(&row); // row  88
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  89;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07033714;
  row.a[1]       =    -0.1454655;
  row.a[2]       =     0.2063874;
  row.a[3]       =    -0.1658331;
  tableSet->AddAt(&row); // row  89
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  90;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05466507;
  row.a[1]       =   -0.07988029;
  row.a[2]       =    0.06281208;
  row.a[3]       =    -0.0563871;
  tableSet->AddAt(&row); // row  90
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  91;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02892896;
  row.a[1]       =    0.06763188;
  row.a[2]       =    -0.1999789;
  row.a[3]       =    0.09913333;
  tableSet->AddAt(&row); // row  91
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  92;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05577511;
  row.a[1]       =   -0.08033349;
  row.a[2]       =     0.1076153;
  row.a[3]       =   -0.06994065;
  tableSet->AddAt(&row); // row  92
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  93;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02430972;
  row.a[1]       =      0.106981;
  row.a[2]       =      -0.53166;
  row.a[3]       =     0.5250138;
  tableSet->AddAt(&row); // row  93
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  94;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03972706;
  row.a[1]       =     0.1692332;
  row.a[2]       =    -0.9543203;
  row.a[3]       =      1.169831;
  tableSet->AddAt(&row); // row  94
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  95;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03753996;
  row.a[1]       =    0.08054448;
  row.a[2]       =    -0.6365991;
  row.a[3]       =     0.8866954;
  tableSet->AddAt(&row); // row  95
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  96;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03747848;
  row.a[1]       =   -0.05363286;
  row.a[2]       =   -0.09235354;
  row.a[3]       =     0.2880621;
  tableSet->AddAt(&row); // row  96
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  97;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03500366;
  row.a[1]       =   -0.05017576;
  row.a[2]       =     0.1139833;
  row.a[3]       =    -0.1295789;
  tableSet->AddAt(&row); // row  97
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  98;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.09158012;
  row.a[1]       =    -0.2971537;
  row.a[2]       =     0.5274503;
  row.a[3]       =    -0.3475883;
  tableSet->AddAt(&row); // row  98
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   =  99;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05044026;
  row.a[1]       =    -0.1004694;
  row.a[2]       =     0.2063578;
  row.a[3]       =     -0.190912;
  tableSet->AddAt(&row); // row  99
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 100;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04208479;
  row.a[1]       =    0.02008661;
  row.a[2]       =      -0.16523;
  row.a[3]       =     0.1443547;
  tableSet->AddAt(&row); // row 100
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 101;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03617202;
  row.a[1]       =   -0.01208714;
  row.a[2]       =    -0.1733039;
  row.a[3]       =     0.1742978;
  tableSet->AddAt(&row); // row 101
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 102;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04030955;
  row.a[1]       =    0.03442056;
  row.a[2]       =    -0.3774726;
  row.a[3]       =     0.4197227;
  tableSet->AddAt(&row); // row 102
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 103;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1314943;
  row.a[1]       =    -0.7968787;
  row.a[2]       =      7.801099;
  row.a[3]       =     -23.11101;
  tableSet->AddAt(&row); // row 103
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 104;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04795434;
  row.a[1]       =    0.01764313;
  row.a[2]       =    -0.4793557;
  row.a[3]       =     0.6989153;
  tableSet->AddAt(&row); // row 104
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 105;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1372511;
  row.a[1]       =    -0.5707671;
  row.a[2]       =      1.124385;
  row.a[3]       =    -0.7601494;
  tableSet->AddAt(&row); // row 105
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 106;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1826526;
  row.a[1]       =    -0.8785278;
  row.a[2]       =      1.793883;
  row.a[3]       =     -1.220595;
  tableSet->AddAt(&row); // row 106
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 107;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07866488;
  row.a[1]       =    -0.1695498;
  row.a[2]       =      0.251509;
  row.a[3]       =    -0.1746508;
  tableSet->AddAt(&row); // row 107
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 108;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05060902;
  row.a[1]       =   -0.04716175;
  row.a[2]       =    0.06248633;
  row.a[3]       =     -0.048737;
  tableSet->AddAt(&row); // row 108
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 109;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04362513;
  row.a[1]       =    0.01471427;
  row.a[2]       =    -0.2421683;
  row.a[3]       =      0.239402;
  tableSet->AddAt(&row); // row 109
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 110;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01019472;
  row.a[1]       =    0.06167553;
  row.a[2]       =    -0.4586971;
  row.a[3]       =      0.512902;
  tableSet->AddAt(&row); // row 110
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 111;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =      0.058475;
  row.a[1]       =    0.01374099;
  row.a[2]       =    -0.4466087;
  row.a[3]       =     0.6425993;
  tableSet->AddAt(&row); // row 111
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 112;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04034352;
  row.a[1]       =   0.008260837;
  row.a[2]       =    -0.3665514;
  row.a[3]       =     0.5487042;
  tableSet->AddAt(&row); // row 112
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 113;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05720428;
  row.a[1]       =  -0.007405102;
  row.a[2]       =   -0.01424177;
  row.a[3]       =   -0.03612108;
  tableSet->AddAt(&row); // row 113
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 114;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0505676;
  row.a[1]       =    -0.0480061;
  row.a[2]       =    0.07035954;
  row.a[3]       =    -0.0819644;
  tableSet->AddAt(&row); // row 114
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 115;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05230762;
  row.a[1]       =    -0.1157975;
  row.a[2]       =     0.2446171;
  row.a[3]       =    -0.2091084;
  tableSet->AddAt(&row); // row 115
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 116;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03195098;
  row.a[1]       =   -0.03923601;
  row.a[2]       =    0.04964072;
  row.a[3]       =   -0.05075883;
  tableSet->AddAt(&row); // row 116
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 117;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01047028;
  row.a[1]       =    0.07113869;
  row.a[2]       =     -0.348371;
  row.a[3]       =     0.2929278;
  tableSet->AddAt(&row); // row 117
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 118;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01923148;
  row.a[1]       =    0.07512427;
  row.a[2]       =    -0.4470906;
  row.a[3]       =     0.4634864;
  tableSet->AddAt(&row); // row 118
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 119;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01384755;
  row.a[1]       =    0.07046531;
  row.a[2]       =    -0.3921882;
  row.a[3]       =      0.394639;
  tableSet->AddAt(&row); // row 119
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 120;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05183457;
  row.a[1]       =    -0.1032292;
  row.a[2]       =   -0.02408339;
  row.a[3]       =     0.1645019;
  tableSet->AddAt(&row); // row 120
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 121;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.09082526;
  row.a[1]       =    -0.2903054;
  row.a[2]       =     0.4794464;
  row.a[3]       =    -0.3015746;
  tableSet->AddAt(&row); // row 121
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 122;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0473479;
  row.a[1]       =     -0.161747;
  row.a[2]       =     0.3347688;
  row.a[3]       =    -0.2662294;
  tableSet->AddAt(&row); // row 122
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 123;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03075752;
  row.a[1]       =    -0.1455893;
  row.a[2]       =     0.3723245;
  row.a[3]       =    -0.3474518;
  tableSet->AddAt(&row); // row 123
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 124;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03052572;
  row.a[1]       =    -0.0833355;
  row.a[2]       =     0.1428943;
  row.a[3]       =    -0.1073001;
  tableSet->AddAt(&row); // row 124
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 125;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0564248;
  row.a[1]       =    0.05556935;
  row.a[2]       =    -0.2839847;
  row.a[3]       =     0.2177236;
  tableSet->AddAt(&row); // row 125
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 126;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0545375;
  row.a[1]       =     0.1203385;
  row.a[2]       =    -0.6774143;
  row.a[3]       =     0.7306636;
  tableSet->AddAt(&row); // row 126
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 127;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.06493573;
  row.a[1]       =     0.1184901;
  row.a[2]       =    -0.6951357;
  row.a[3]       =     0.8050957;
  tableSet->AddAt(&row); // row 127
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 128;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07866677;
  row.a[1]       =    0.04472735;
  row.a[2]       =    -0.5636345;
  row.a[3]       =     0.7670419;
  tableSet->AddAt(&row); // row 128
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 129;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.009520538;
  row.a[1]       =     0.1040915;
  row.a[2]       =    -0.2180237;
  row.a[3]       =    0.07233138;
  tableSet->AddAt(&row); // row 129
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 130;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.05804645;
  row.a[1]       =     0.3686606;
  row.a[2]       =    -0.6253336;
  row.a[3]       =     0.2708636;
  tableSet->AddAt(&row); // row 130
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 131;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03640904;
  row.a[1]       =    -0.1353555;
  row.a[2]       =     0.2722917;
  row.a[3]       =    -0.2380534;
  tableSet->AddAt(&row); // row 131
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 132;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.002331362;
  row.a[1]       =   -0.07775419;
  row.a[2]       =      0.143829;
  row.a[3]       =    -0.1192607;
  tableSet->AddAt(&row); // row 132
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 133;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.01367643;
  row.a[1]       =     0.1678589;
  row.a[2]       =    -0.6706774;
  row.a[3]       =     0.5750818;
  tableSet->AddAt(&row); // row 133
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 134;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    -0.0127255;
  row.a[1]       =     0.2015714;
  row.a[2]       =     -0.961673;
  row.a[3]       =      1.002726;
  tableSet->AddAt(&row); // row 134
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 135;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  -0.007503597;
  row.a[1]       =      0.157919;
  row.a[2]       =     -1.146944;
  row.a[3]       =      1.531755;
  tableSet->AddAt(&row); // row 135
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 136;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    -0.1358891;
  row.a[1]       =      1.865646;
  row.a[2]       =     -6.820006;
  row.a[3]       =      7.808009;
  tableSet->AddAt(&row); // row 136
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 137;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01396175;
  row.a[1]       =  -0.007817785;
  row.a[2]       =    0.03989675;
  row.a[3]       =   -0.09456935;
  tableSet->AddAt(&row); // row 137
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 138;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02300725;
  row.a[1]       =   -0.03141246;
  row.a[2]       =    0.08808366;
  row.a[3]       =     -0.131086;
  tableSet->AddAt(&row); // row 138
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 139;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0770439;
  row.a[1]       =    -0.4226559;
  row.a[2]       =     0.8589434;
  row.a[3]       =    -0.6098521;
  tableSet->AddAt(&row); // row 139
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 140;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02134121;
  row.a[1]       =    -0.0481946;
  row.a[2]       =    0.06155603;
  row.a[3]       =   -0.05747831;
  tableSet->AddAt(&row); // row 140
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 141;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01988878;
  row.a[1]       =     -0.237932;
  row.a[2]       =     0.4071225;
  row.a[3]       =    -0.2797944;
  tableSet->AddAt(&row); // row 141
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 142;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02693734;
  row.a[1]       =    0.02956127;
  row.a[2]       =    -0.2044368;
  row.a[3]       =     0.1546754;
  tableSet->AddAt(&row); // row 142
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 143;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   -0.02477551;
  row.a[1]       =  -0.006900894;
  row.a[2]       =   -0.09194869;
  row.a[3]       =    0.07029085;
  tableSet->AddAt(&row); // row 143
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 144;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  0.0008197104;
  row.a[1]       =   0.005592615;
  row.a[2]       =    -0.3691154;
  row.a[3]       =     0.5454285;
  tableSet->AddAt(&row); // row 144
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 145;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04588552;
  row.a[1]       =    -0.1085563;
  row.a[2]       =     0.1996798;
  row.a[3]       =     -0.179187;
  tableSet->AddAt(&row); // row 145
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 146;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0711416;
  row.a[1]       =     -0.343102;
  row.a[2]       =     0.6255978;
  row.a[3]       =    -0.4210005;
  tableSet->AddAt(&row); // row 146
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 147;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02539447;
  row.a[1]       =   0.006387749;
  row.a[2]       =    0.03440716;
  row.a[3]       =    -0.1129961;
  tableSet->AddAt(&row); // row 147
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 148;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02139694;
  row.a[1]       =    0.02736365;
  row.a[2]       =   -0.05278643;
  row.a[3]       =   0.005287374;
  tableSet->AddAt(&row); // row 148
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 149;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05334403;
  row.a[1]       =    -0.1107694;
  row.a[2]       =    0.05716379;
  row.a[3]       =    0.00857693;
  tableSet->AddAt(&row); // row 149
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 150;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02361252;
  row.a[1]       =    0.05489666;
  row.a[2]       =    -0.4141373;
  row.a[3]       =     0.4521253;
  tableSet->AddAt(&row); // row 150
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 151;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01643405;
  row.a[1]       =      0.168821;
  row.a[2]       =     -0.829377;
  row.a[3]       =     0.9269253;
  tableSet->AddAt(&row); // row 151
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 152;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07939794;
  row.a[1]       =    -0.1633733;
  row.a[2]       =     0.1074786;
  row.a[3]       =    0.03690921;
  tableSet->AddAt(&row); // row 152
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 153;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    -0.1029046;
  row.a[1]       =     0.7082525;
  row.a[2]       =     -1.234254;
  row.a[3]       =     0.6071129;
  tableSet->AddAt(&row); // row 153
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 154;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02938071;
  row.a[1]       =   -0.01556083;
  row.a[2]       =      0.034895;
  row.a[3]       =   -0.09430639;
  tableSet->AddAt(&row); // row 154
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 155;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04801618;
  row.a[1]       =    -0.0557504;
  row.a[2]       =    0.08283416;
  row.a[3]       =    -0.1156763;
  tableSet->AddAt(&row); // row 155
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 156;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =  -0.002295947;
  row.a[1]       =     0.1718067;
  row.a[2]       =    -0.4440709;
  row.a[3]       =     0.2678294;
  tableSet->AddAt(&row); // row 156
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 157;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1814506;
  row.a[1]       =    -0.7503077;
  row.a[2]       =      2.369961;
  row.a[3]       =     -3.424301;
  tableSet->AddAt(&row); // row 157
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 158;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.07854055;
  row.a[1]       =    -0.3235441;
  row.a[2]       =     0.5858533;
  row.a[3]       =    -0.4257627;
  tableSet->AddAt(&row); // row 158
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 159;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03841052;
  row.a[1]       =      0.203136;
  row.a[2]       =     -1.220593;
  row.a[3]       =      1.509725;
  tableSet->AddAt(&row); // row 159
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 160;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0841106;
  row.a[1]       =   -0.05747304;
  row.a[2]       =     -0.319718;
  row.a[3]       =     0.5480049;
  tableSet->AddAt(&row); // row 160
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 161;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05153736;
  row.a[1]       =    0.02177058;
  row.a[2]       =   -0.07605393;
  row.a[3]       =  -0.000719321;
  tableSet->AddAt(&row); // row 161
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 162;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04803587;
  row.a[1]       =   -0.05385681;
  row.a[2]       =    0.07154059;
  row.a[3]       =   -0.08460927;
  tableSet->AddAt(&row); // row 162
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 163;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03131174;
  row.a[1]       =   -0.04273533;
  row.a[2]       =     0.1200069;
  row.a[3]       =    -0.1406882;
  tableSet->AddAt(&row); // row 163
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 164;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.08649498;
  row.a[1]       =    -0.2991889;
  row.a[2]       =     0.5054588;
  row.a[3]       =    -0.3013683;
  tableSet->AddAt(&row); // row 164
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 165;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01212531;
  row.a[1]       =    0.05564496;
  row.a[2]       =    -0.2902188;
  row.a[3]       =      0.240007;
  tableSet->AddAt(&row); // row 165
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 166;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03208331;
  row.a[1]       =    0.03469086;
  row.a[2]       =    -0.3726338;
  row.a[3]       =     0.4265633;
  tableSet->AddAt(&row); // row 166
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 167;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =   0.009718893;
  row.a[1]       =     0.1239171;
  row.a[2]       =    -0.7045836;
  row.a[3]       =     0.8253473;
  tableSet->AddAt(&row); // row 167
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 168;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0346534;
  row.a[1]       =    -0.1148551;
  row.a[2]       =     0.2013923;
  row.a[3]       =    -0.2279677;
  tableSet->AddAt(&row); // row 168
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 169;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03800885;
  row.a[1]       =   -0.05179488;
  row.a[2]       =     0.1317659;
  row.a[3]       =    -0.1473018;
  tableSet->AddAt(&row); // row 169
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 170;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.09294239;
  row.a[1]       =    -0.3670678;
  row.a[2]       =     0.6918986;
  row.a[3]       =    -0.4668068;
  tableSet->AddAt(&row); // row 170
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 171;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.1168673;
  row.a[1]       =    -0.4737734;
  row.a[2]       =     0.9122321;
  row.a[3]       =    -0.6103147;
  tableSet->AddAt(&row); // row 171
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 172;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05057454;
  row.a[1]       =    0.01987163;
  row.a[2]       =   -0.04959877;
  row.a[3]       =  -0.003261145;
  tableSet->AddAt(&row); // row 172
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 173;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02055176;
  row.a[1]       =    0.04058901;
  row.a[2]       =      -0.25055;
  row.a[3]       =     0.2058836;
  tableSet->AddAt(&row); // row 173
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 174;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01585743;
  row.a[1]       =    0.03719396;
  row.a[2]       =    -0.3890206;
  row.a[3]       =     0.4459991;
  tableSet->AddAt(&row); // row 174
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 175;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.01645472;
  row.a[1]       =    0.06551379;
  row.a[2]       =    -0.5708709;
  row.a[3]       =     0.7811294;
  tableSet->AddAt(&row); // row 175
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 176;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03879381;
  row.a[1]       =   -0.05649751;
  row.a[2]       =   -0.02810611;
  row.a[3]       =     0.1183657;
  tableSet->AddAt(&row); // row 176
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 177;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02570721;
  row.a[1]       =   -0.02092841;
  row.a[2]       =     0.1068361;
  row.a[3]       =    -0.1503589;
  tableSet->AddAt(&row); // row 177
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 178;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.08796688;
  row.a[1]       =    -0.4005599;
  row.a[2]       =     0.7684359;
  row.a[3]       =    -0.5141031;
  tableSet->AddAt(&row); // row 178
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 179;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05348094;
  row.a[1]       =    -0.1896664;
  row.a[2]       =     0.3748145;
  row.a[3]       =    -0.2925762;
  tableSet->AddAt(&row); // row 179
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 180;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03390625;
  row.a[1]       =   -0.05616314;
  row.a[2]       =     0.1133602;
  row.a[3]       =    -0.1052427;
  tableSet->AddAt(&row); // row 180
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 181;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0329405;
  row.a[1]       =     0.1255161;
  row.a[2]       =      -0.44218;
  row.a[3]       =     0.3343204;
  tableSet->AddAt(&row); // row 181
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 182;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =     0.0147554;
  row.a[1]       =    0.02864635;
  row.a[2]       =    -0.2274989;
  row.a[3]       =     0.2224774;
  tableSet->AddAt(&row); // row 182
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 183;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04086715;
  row.a[1]       =     0.1311387;
  row.a[2]       =    -0.7856524;
  row.a[3]       =      0.967783;
  tableSet->AddAt(&row); // row 183
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 184;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.05752248;
  row.a[1]       =    -0.2041172;
  row.a[2]       =     0.3344597;
  row.a[3]       =    -0.1833438;
  tableSet->AddAt(&row); // row 184
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 185;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03390386;
  row.a[1]       =   -0.07769608;
  row.a[2]       =      0.221449;
  row.a[3]       =    -0.2063944;
  tableSet->AddAt(&row); // row 185
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 186;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02890919;
  row.a[1]       =   -0.08809933;
  row.a[2]       =     0.1890367;
  row.a[3]       =    -0.1791047;
  tableSet->AddAt(&row); // row 186
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 187;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02462374;
  row.a[1]       =    -0.1109586;
  row.a[2]       =      0.258386;
  row.a[3]       =    -0.2397447;
  tableSet->AddAt(&row); // row 187
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 188;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.02395068;
  row.a[1]       =   -0.07507377;
  row.a[2]       =     0.1621283;
  row.a[3]       =    -0.1439394;
  tableSet->AddAt(&row); // row 188
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 189;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       = -0.0008708955;
  row.a[1]       =     0.1598172;
  row.a[2]       =    -0.4965726;
  row.a[3]       =     0.3665519;
  tableSet->AddAt(&row); // row 189
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 190;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.03344356;
  row.a[1]       =  -0.002313663;
  row.a[2]       =    -0.2307217;
  row.a[3]       =     0.2773086;
  tableSet->AddAt(&row); // row 190
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 191;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.04416726;
  row.a[1]       =     0.0808816;
  row.a[2]       =    -0.5513825;
  row.a[3]       =     0.6362411;
  tableSet->AddAt(&row); // row 191
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  row.idx   = 192;
  row.nrows = 192;
  row.npar       =             4;
  row.a[0]       =    0.06383262;
  row.a[1]       =    0.06222492;
  row.a[2]       =    -0.4245437;
  row.a[3]       =     0.4809561;
  tableSet->AddAt(&row); // row 192
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

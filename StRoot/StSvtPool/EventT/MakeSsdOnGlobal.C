struct data_t {
  Int_t day;
  Double_t dx, ddx, dy, ddy, dz, ddz, alpha, dalpha, beta, dbeta, gamma, dgamma;
  Int_t  run, date, time;
  Double_t zdc; 
  Double_t field;
  Char_t *comment;
};
data_t Data[] = {
#if 0
#if 0
  { 20, 163.93,14.68, 186.40, 1.24,-325.13,12.82,  -0.43, 0.01,  -0.01, 0.00,  -0.39, 0.06, 6020062, 20050120,101458, 9965.6,-1, "CuCu200RF"},
  { 21,  36.89, 3.34, 223.35, 2.26,-331.49,23.54,  -0.22, 0.00,  -0.04, 0.01,  -0.38, 0.10, 6021012, 20050121,103145,  714.4,-1, "CuCu200RF"},
  { 25, 317.98, 5.08, 443.98, 5.18,-323.08, 4.45,  -0.47, 0.01,  -0.08, 0.01,  -0.35, 0.02, 6025041, 20050125, 91811, 4633.9,-1, "CuCu200RF"},
  { 32, 125.45,13.71, 335.51,14.03,-322.33,11.92,  -0.27, 0.01,  -0.05, 0.00,  -0.18, 0.05, 6032011, 20050201, 75822,23801.3,-1, "CuCu200RF"},
  { 34, 162.66, 8.97, 295.76, 9.20,-345.65, 7.83,  -0.48, 0.01,  -0.06, 0.00,  -0.28, 0.03, 6034014, 20050202, 54736, 6222.1,-1, "CuCu200RF"},
  { 44,  13.42, 6.80, 116.51, 0.57,-279.59, 5.88,  -0.49, 0.02,  -0.11, 0.01,  -0.31, 0.03, 6044001, 20050213, 53235, 7831.2,-1, "CuCu200RF"},
  { 45, 219.62,14.98, 533.72,15.17,-276.52,12.96,  -0.29, 0.00,  -0.12, 0.01,  -0.49, 0.06, 6045082, 20050213,152450, 8504.7,-1, "CuCu200RF"},
  { 48,-709.95, 5.70,-785.77, 5.78,  12.55, 1.63,  -0.07, 0.02,   0.11, 0.01,   0.29, 0.02, 6048024, 20050217,  5609, 7511.3, 1, "CuCu200FF"},
  { 49,-117.92, 0.93,-202.97, 9.92,   6.43, 2.13,  -0.01, 0.00,   0.02, 0.00,  -0.01, 0.03, 6049129, 20050218, 75627, 6319.5, 1, "CuCu200FF"},
  { 51,-464.59,12.05,-533.79,11.81,  24.72, 3.22,  -0.04, 0.00,  -0.00, 0.01,   0.19, 0.04, 6051055, 20050220,193744,10248.1, 1, "CuCu200FF"},
  { 57,-244.74, 4.62,-315.28, 4.47,  14.36, 1.04,  -0.04, 0.02,  -0.02, 0.00,   0.05, 0.02, 6057048, 20050221,052710, 4840.1, 1, "CuCu200FF"},
  { 61, -32.41, 1.14,-108.70,13.55,   2.86, 2.94,   0.02, 0.00,   0.04, 0.00,  -0.15, 0.05, 6061046, 20050302,122743,22698.7, 1, "CuCu200FF"},
  { 63,-125.85, 1.78,-226.20,18.69,  19.64, 5.02,   0.00, 0.00,   0.03, 0.00,  -0.15, 0.07, 6063052, 20050304,194755,17777.7, 1, "CuCu200FF"},
  { 64,-142.76, 1.48,-276.29,15.63,  13.01, 3.47,  -0.02, 0.00,   0.01, 0.00,   0.16, 0.05, 6064073, 20050305,181946, 9271.3, 1, "CuCu200FF"},
  { 65, -38.17, 6.41,-212.22, 6.17,  53.43, 5.36,  -0.02, 0.01,   0.03, 0.01,   6.29, 0.02, 6065045, 20050306,194324,20629.7, 0, "NoField"  },
  { 69,  69.23, 1.08,  -5.25, 0.81, -10.78, 2.11,   0.02, 0.00,   0.00, 0.00,  -0.02, 0.00, 6069100, 20050309, 61908, 1028.9, 1, "CuCu62FF" },
  { 73,  41.02, 1.19,  -5.13, 0.69,  -5.51, 2.05,   0.03, 0.00,  -0.00, 0.00,  -0.01, 0.00, 6073023, 20050314,123550, 1270.1, 1, "CuCu62FF" },
  { 81,  36.86, 0.96,  -0.99, 0.52,  26.09, 1.66,  -0.01, 0.01,   0.02, 0.00,  -0.06, 0.03, 6081041, 20050316,195612, 1791.2, 1, "CuCu62FF" }
#else
  //6065045B   
  { 65, 184.57, 6.24,  69.27, 0.81,-114.15, 5.50,  -0.13, 0.02,  -0.16, 0.01,  -0.32, 0.02, 6065045, 20050306,194324,20629.7, 0, "NoFieldB"  } 
  //  { 65,1207.03, 6.42, 717.66, 6.35,1118.89, 5.85,  -0.12, 0.01,  -0.18, 0.02,  -0.18, 0.03, 6065045, 20050306,194324,20629.7, 0, "NoFieldC" }//6065045C
#endif
  { 20, 52.67, 3.94 ,113.41, 3.46,-110.53, 3.80,-0.12, 0.01, 0.42, 0.02,-0.59, 0.02, 6020062, 20050120, 0, 9965.6,-1, "CuCu200RF"},
  { 48,-86.76, 3.45,-105.29, 3.23,-103.13, 3.35, 0.13, 0.01, 0.42, 0.01,-0.23, 0.01, 6048024, 20050217, 0, 7511.3, 1, "CuCu200FF"}
#else
  /*  AuAu Pass213 FF      | -11.07+- 0.55 |  -7.50+- 0.52 |  13.40+- 0.88 |   0.00+- 0.00 |  -0.05+- 0.00 |   0.10+- 0.02 | Average for All Ssd */
  /*  {120, -11.07, 0.55,  -7.50, 0.52,  13.40, 0.88, 0.00, 0.00,-0.05, 0.00, 0.10, 0.02, 8120052, 20070321,31,    0.0, 1, "AuAu200FF"} */
  /* AuAu Pass213 RF       |-170.25+- 0.69 |  29.44+- 0.75 |  76.39+- 0.99 |  -0.44+- 0.00 |  -0.19+- 0.02 |  -0.03+- 0.00 | Average for All Ssd */
  {159,-170.25, 0.69,  29.44, 0.75 , 76.39, 0.99,-0.44, 0.00,-0.19, 0.02,-0.03, 0.00 ,8159044, 20070524, 31, 0.,-1,"AuAu200RF"}
#endif
};
const Int_t N = sizeof(Data)/sizeof(data_t);

//________________________________________________________________________________
void MakeSsdOnGlobal(){
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,tpcDb,MagF,nodefault");
  StMaker *db = chain->Maker("db");
  if (! db) return;
  db->SetDebug(1);
  for (Int_t i = 0; i < N; i++) {
    if (! StarMagField::Instance()) new StarMagField;
    StarMagField::Instance()->SetFactor(Data[i].field);
    StEvtHddr *header = chain->GetEvtHddr();
    header->SetRunNumber(i+1);
    header->SetDateTime(20050101,i+1);
    chain->MakeEvent();
    db->SetDateTime(Data[i].date,Data[i].time); 
    St_Survey *SsdOnGlobal = (St_Survey *) chain->GetDataBase("Geometry/ssd/SsdOnGlobal");
    if (! SsdOnGlobal)  {cout << "SsdOnGlobal has not been found"  << endl; return 0;}
    const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();  cout << "Tpc2Global\t"; Tpc2Global.Print();
    TGeoHMatrix GL;
    Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole 
    GL.SetRotation(&OnGlobal->r00);
    GL.SetTranslation(&OnGlobal->t0); cout << "GL\t"; GL.Print();
    TGeoHMatrix TPCGL = Tpc2Global * GL;  cout << "TPCGL\t"; TPCGL.Print();
    TGeoHMatrix TPC2Inv = Tpc2Global.Inverse();  cout << "TPC2Inv\t"; TPC2Inv.Print();
    TGeoHMatrix dR;
    dR.RotateX(180./TMath::Pi()*Data[i].alpha*1e-3);
    dR.RotateY(180./TMath::Pi()*Data[i].beta*1e-3); 
    dR.RotateZ(180./TMath::Pi()*Data[i].gamma*1e-3);
    Double_t xyz[3], dxyz[3], drot[3];
    xyz[0] = 1e-4*Data[i].dx;
    xyz[1] = 1e-4*Data[i].dy;
    xyz[2] = 1e-4*Data[i].dz;
    dxyz[0] = 1e-4*Data[i].ddx;
    dxyz[1] = 1e-4*Data[i].ddy;
    dxyz[2] = 1e-4*Data[i].ddz;
    drot[0] = Data[i].dalpha*1e-3;
    drot[1] = Data[i].dbeta*1e-3; 
    drot[2] = Data[i].dgamma*1e-3;
    dR.SetTranslation(xyz);
    cout << "Additional rotation for Ssd\t"; dR.Print();
    TGeoHMatrix GLnew = TPC2Inv * dR * TPCGL;  cout << "GLnew\t"; GLnew.Print();
    Double_t *R = GLnew.GetRotationMatrix();
    Survey_st row;
    memcpy(&row.r00, R, 9*sizeof(Double_t));
    Double_t *tr = GLnew.GetTranslation();
    memcpy(&row.t0, tr, 3*sizeof(Double_t));
    memcpy(&row.sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row.sigmaTrX, dxyz, 3*sizeof(Double_t));
    TString fOut =  Form("SsdOnGlobal.%8i.%06i.C", Data[i].date, Data[i].time);
    ofstream out;
    cout << "Create " << fOut << endl;
    out.open(fOut.Data());
    out << "TDataSet *CreateTable() {" << endl;
    out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
    out << "  Survey_st row = " << endl; 
    out << "\t{0,"; out << endl; out << "\t"; 
    Double_t *r = &(row.r00);
    for (Int_t j = 0; j < 9; j++) out << Form("%f,",r[j]);
    out << endl;
    out << "\t";
    for (Int_t j = 9; j < 12; j++) out << Form("%f,",r[j]); 
    out << endl;
    out << "\t";
    for (Int_t j = 12; j < 18; j++) out << Form("%f,",r[j]);
    out << endl;
    out << "\t";
    out << "\"Run" << Data[i].run << " " << Data[i].comment << "\"};" << endl;
    out << "  St_Survey *tableSet = new St_Survey(\"SsdOnGlobal\",1);" << endl; 
    out << "  tableSet->AddAt(&row.Id, 0);" << endl;
    out << "  return (TDataSet *)tableSet;" << endl;
    out << "}" << endl;
    out.close(); 
  } 
}

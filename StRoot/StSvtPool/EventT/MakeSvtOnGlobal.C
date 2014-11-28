struct data_t {
  Int_t day;
  Double_t dx, ddx, dy, ddy, dz, ddz, alpha, dalpha, beta, dbeta, gamma, dgamma;
  Int_t  run, date, time;
  Double_t zdc; 
  Double_t field;
  Char_t *comment;
};
data_t Data[] = {
  /*  // Pass121
  { 20,  18.80, 2.23, 51.73, 2.22,-16.06, 2.35,  0.01, 0.02,  0.08, 0.02,  0.13, 0.02, 6020062, 20050120, 0, 9965.6,-1, "CuCu200RF"},  
  { 48, -55.12, 2.11,-80.74, 2.06,  5.15, 2.19,  0.04, 0.02,  0.07, 0.02, -0.25, 0.02, 6048024, 20050217, 0, 7511.3, 1, "CuCu200FF"} 
  Pass 213 |  12.63+- 2.22 | -13.28+- 2.14 | -19.54+- 1.87 |   0.09+- 0.02 |  -0.01+- 0.00 |   0.25+- 0.02 | Average for All Svt 
  {120,  12.63, 2.22,-13.28, 2.14,-19.54, 1.87,  0.09, 0.02, -0.01, 0.00,  0.25, 0.02, 8120052, 20070321,31,    0.0, 0, "AuAu200FF"} 
  Pass 213 RF | -45.65+- 2.73 |  13.73+- 2.64 |  -9.93+- 0.37 |  -0.03+- 0.02 |  -0.06+- 0.00 |   0.06+- 0.02 | Average for All Svt
  {159,-170.25, 0.69,  29.44, 0.75 , 76.39, 0.99,-0.44, 0.00,-0.19, 0.02,-0.03, 0.00 ,8159044, 20070524, 31, 0.,-1,"AuAu200RF"} */
  {159, -45.65, 2.73,  13.73, 2.64,  -9.93, 0.37,-0.03, 0.02,-0.06, 0.00, 0.06, 0.02,8159044, 20070524, 31, 0.,-1,"AuAu200RF"}
};
const Int_t N = sizeof(Data)/sizeof(data_t);

//________________________________________________________________________________
void MakeSvtOnGlobal(){
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
    St_Survey *SvtOnGlobal = (St_Survey *) chain->GetDataBase("Geometry/svt/SvtOnGlobal");
    if (! SvtOnGlobal)  {cout << "SvtOnGlobal has not been found"  << endl; return 0;}
    const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();  cout << "Tpc2Global\t"; Tpc2Global.Print();
    TGeoHMatrix GL;
    Survey_st *OnGlobal         = SvtOnGlobal->GetTable();        // SVT and SVT as whole 
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
    cout << "Additional rotation for Svt\t"; dR.Print();
    TGeoHMatrix GLnew = TPC2Inv * dR * TPCGL;  cout << "GLnew\t"; GLnew.Print();
    Double_t *R = GLnew.GetRotationMatrix();
    Survey_st row;
    memcpy(&row.r00, R, 9*sizeof(Double_t));
    Double_t *tr = GLnew.GetTranslation();
    memcpy(&row.t0, tr, 3*sizeof(Double_t));
    memcpy(&row.sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row.sigmaTrX, dxyz, 3*sizeof(Double_t));
    TString fOut =  Form("SvtOnGlobal.%8i.%06i.C", Data[i].date, Data[i].time);
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
    out << "  St_Survey *tableSet = new St_Survey(\"SvtOnGlobal\",1);" << endl; 
    out << "  tableSet->AddAt(&row.Id, 0);" << endl;
    out << "  return (TDataSet *)tableSet;" << endl;
    out << "}" << endl;
    out.close(); 
  } 
}

struct data_t {
  Int_t day;
  Double_t dx, ddx, dy, ddy, dz, ddz, alpha, dalpha, beta, dbeta, gamma, dgamma;
  Int_t  run, date, time;
  Double_t zdc; 
  Double_t field;
  Int_t    sector; // 0-1 SVT Clam shell, 2-5 Ssd Sector + 1
  Char_t *comment;
};
Int_t N = 6;
#if 0
  /*
|   1.59+- 5.15 |   3.04+- 6.39 |  92.85+- 5.86 |   0.21+- 0.01 |   0.19+- 0.01 |   0.27+- 0.06 | Average for SVT ClamShell 0
|   3.20+- 1.60 |  20.68+- 8.68 |  45.31+- 7.86 |  -0.42+- 0.01 |   0.01+- 0.02 |   0.01+- 0.08 | Average for SVT ClamShell 1
| 400.17+- 6.87 | -47.25+-20.45 | -71.67+-12.00 |   0.01+- 0.00 |   0.41+- 0.01 |  -0.10+- 0.33 | Average for SSD Sector 1
|  78.60+-12.05 |  62.20+- 2.28 | -45.57+- 6.35 |  -0.00+- 0.00 |   0.15+- 0.02 |   0.17+- 0.07 | Average for SSD Sector 2
| -46.41+- 3.83 | -69.14+-19.39 |   4.24+- 9.72 |   0.00+- 0.00 |   0.14+- 0.03 |  -1.30+- 0.07 | Average for SSD Sector 3
|  82.93+-10.04 |  25.05+- 2.26 | -88.17+- 5.69 |   0.05+- 0.04 |   0.04+- 0.02 |   0.08+- 0.08 | Average for SSD Sector 4
  */
data_t Data[6] = {
  { 65,  1.59, 5.15,   3.04, 6.39,  92.85, 5.86,   0.21, 0.01,   0.19, 0.01,   0.27, 0.06, 6065045, 20050306,194324,20629.7, 0, 0, "NoField 065D SVT 0 (+x South) "},
  { 65,  3.20, 1.60,  20.68, 8.68,  45.31, 7.86,  -0.42, 0.01,   0.01, 0.02,   0.01, 0.08, 6065045, 20050306,194324,20629.7, 0, 1, "NoField 065D SVT 1 (-x North)"},
  { 65,400.17, 6.87, -47.25,20.45, -71.67,12.00,   0.01, 0.00,   0.41, 0.01,  -0.10, 0.33, 6065045, 20050306,194324,20629.7, 0, 2, "NoField 065D SSD 1 top sector"},
  { 65, 78.60,12.05,  62.20, 2.28, -45.57, 6.35,  -0.00, 0.00,   0.15, 0.02,   0.17, 0.07, 6065045, 20050306,194324,20629.7, 0, 3, "NoField 065D SSD 2 right sector"},
  { 65,-46.41, 3.83, -69.14,19.39,   4.24, 9.72,   0.00, 0.00,   0.14, 0.03,  -1.30, 0.07, 6065045, 20050306,194324,20629.7, 0, 4, "NoField 065D SSD 3 bottom sector"},
  { 65, 82.93,10.04,  25.05, 2.26, -88.17, 5.69,   0.05, 0.04,   0.04, 0.02,   0.08, 0.08, 6065045, 20050306,194324,20629.7, 0, 5, "NoField 065D SSD 4 left sector"}
};
#endif
/*
|dX mkm         |dY mkm         |dZ mkm         |alpha mrad     |beta mrad      |gamma mrad     |Comment
______________________________________________________________________________________________ ../../D/Pass110_TpcOnly_065_6065045EPlotsdEdxNFP25rCut0.5cm.root
|  15.98+- 1.00 |   2.58+- 6.39 |  51.57+- 5.85 |   0.04+- 0.01 |   0.17+- 0.02 |   0.12+- 0.06 | Average for SVT ClamShell 0
|  -4.37+- 1.62 |  38.35+- 1.11 |  40.56+- 2.39 |  -0.16+- 0.02 |   0.02+- 0.00 |   0.06+- 0.08 | Average for SVT ClamShell 1
| -27.06+- 6.91 |   1.89+- 0.10 |  -6.57+-11.95 |   0.00+- 0.00 |   0.01+- 0.01 |  -0.09+- 0.10 | Average for SSD Sector 1
|   4.10+- 1.75 |  12.58+- 2.27 |  24.59+- 6.31 |  -0.01+- 0.00 |   0.22+- 0.01 |   0.12+- 0.08 | Average for SSD Sector 2
| 302.66+- 5.41 |  -3.52+- 1.53 |  -1.93+- 8.43 |   0.00+- 0.00 |   0.15+- 0.01 |   0.08+- 0.29 | Average for SSD Sector 3
|  32.15+-10.55 |  27.21+- 2.26 |  -5.99+- 5.67 |   0.01+- 0.04 |   0.10+- 0.02 |   0.03+- 0.08 | Average for SSD Sector 4 */
data_t Data[6] = {
  { 65,  15.98, 1.00,   2.58, 6.39,  51.57, 5.85,   0.04, 0.01,   0.17, 0.02,   0.12, 0.06, 6065045, 20050306,194324,20629.7, 0, 0, "NoField 065E SVT 0 (+x South) "},
  { 65,  -4.37, 1.62,  38.35, 1.11,  40.56, 2.39,  -0.16, 0.02,   0.02, 0.00,   0.06, 0.08, 6065045, 20050306,194324,20629.7, 0, 1, "NoField 065E SVT 1 (-x North)"},
  { 65, -27.06, 6.91,   1.89, 0.10,  -6.57,11.95,   0.00, 0.00,   0.01, 0.01,  -0.09, 0.10, 6065045, 20050306,194324,20629.7, 0, 2, "NoField 065E SSD 1 top sector"},
  { 65,   4.10, 1.75,  12.58, 2.27,  24.59, 6.31,  -0.01, 0.00,   0.22, 0.01,   0.12, 0.08, 6065045, 20050306,194324,20629.7, 0, 3, "NoField 065E SSD 2 right sector"},
  { 65, 302.66, 5.41,  -3.52, 1.53,  -1.93, 8.43,   0.00, 0.00,   0.15, 0.01,   0.08, 0.29, 6065045, 20050306,194324,20629.7, 0, 4, "NoField 065E SSD 3 bottom sector"},
  { 65,  32.15,10.55,  27.21, 2.26,  -5.99, 5.67,   0.01, 0.04,   0.10, 0.02,   0.03, 0.08, 6065045, 20050306,194324,20629.7, 0, 5, "NoField 065E SSD 4 left sector"}
};

//________________________________________________________________________________
void MakeShellSectorOnGlobal(){ 
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,tpcDb,MagF,nodefault");
  StMaker *dbMk = chain->Maker("db");
  if (! dbMk) return;
  dbMk->SetDebug(1);
  if (! StarMagField::Instance()) new StarMagField(1,Data[0].field);
  //  StarMagField::Instance()->SetFactor(Data[0].field);
  StEvtHddr *header = chain->GetEvtHddr();
  header->SetRunNumber(1);
  header->SetDateTime(Data[0].date,Data[0].time);
  dbMk->SetDateTime(Data[0].date,Data[0].time); 
  chain->MakeEvent();
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();  cout << "Tpc2Global\t"; Tpc2Global.Print();
  TDataSet *setSsd = dbMk->GetDataBase("Geometry/ssd");
  if (! setSsd) {cout << "No Geometry/ssd" << endl; return;}
  TDataSet *setSvt = dbMk->GetDataBase("Geometry/svt");
  if (! setSvt) {cout << "No Geometry/svt" << endl; return;}
  St_Survey *SsdOnGlobal = (St_Survey *) setSsd->Find("SsdOnGlobal");
  if (! SsdOnGlobal)  {cout << "SsdOnGlobal has not been found"  << endl; return;}
  St_Survey *SsdSectorsOnGlobalOld = (St_Survey *) setSsd->Find("SsdSectorsOnGlobal");  // sectors in the SSD barrel coordinate system
  St_Survey *SvtShellOnGlobalOld = (St_Survey *) setSvt->Find("ShellOnGlobal");  // sectors in the SSD barrel coordinate system
  if (! (SsdSectorsOnGlobalOld)) return;
  Survey_st *SectorsOnGlobal = SsdSectorsOnGlobalOld->GetTable();  // sectors in the SSD barrel coordinate system
  Survey_st *ShellOnGlobal = SvtShellOnGlobalOld->GetTable();  // sectors in the SSD barrel coordinate system
  Int_t NoSectors = SsdSectorsOnGlobalOld->GetNRows();
  Int_t NoShells = SvtShellOnGlobalOld->GetNRows();
  St_Survey *SsdSectorsOnGlobal = new St_Survey("SsdSectorsOnGlobal",NoSectors);
  St_Survey *SvtShellOnGlobal   = new St_Survey("ShellOnGlobal",NoShells);
  TGeoHMatrix GL, GSSD, GLI, SG,SGold;
  Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole 
  GSSD.SetRotation(&OnGlobal->r00);
  GSSD.SetTranslation(&OnGlobal->t0); //cout << "WL\t"; WL.Print();
  GL  = Tpc2Global * GSSD;
  GLI = GL.Inverse();
//________________________________________________________________________________
  Double_t xyz[3], dxyz[3], drot[3];
  for (Int_t s = 0; s < NoSectors; s++, SectorsOnGlobal++) {
    Int_t sector = SectorsOnGlobal->Id;
    Int_t i = sector + 1;
    TGeoHMatrix dR, dr;
    dR.RotateX(180./TMath::Pi()*Data[i].alpha*1e-3);
    dR.RotateY(180./TMath::Pi()*Data[i].beta*1e-3); 
    dR.RotateZ(180./TMath::Pi()*Data[i].gamma*1e-3);
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
    cout << "Additional rotation for Sector\t" << sector; dR.Print();
    SGold.SetRotation(&SectorsOnGlobal->r00);
    SGold.SetTranslation(&SectorsOnGlobal->t0); //cout << "Sector\t" << Sector << "\tSGold\t"; SGold.Print();
    cout << "Original SG\t"; SGold.Print();
    //________________________________________________________________________________
    Double_t norm;
    TVector3 d(&SectorsOnGlobal->r00); norm = 1./d.Mag(); d *= norm;
    TVector3 t(&SectorsOnGlobal->r10); norm = 1./t.Mag(); t *= norm;
    TVector3 n(&SectorsOnGlobal->r20);
    TVector3 c = d.Cross(t);
    if (c.Dot(n) < 0) c *= -1;
    Double_t rot[9] = {
      d.x(),d.y(),d.z(),
      t.x(),t.y(),t.z(),
      c.x(),c.y(),c.z(),
    };
    SGold.SetRotation(rot);
    cout << "Original SG after normalization\t"; SGold.Print();
    //________________________________________________________________________________
    dr = GLI * dR * GL;
    cout << " GLI\t"; GLI.Print();
    cout << " GL \t"; GL.Print();
    cout << " dR \t"; dR.Print(); 
    cout << " dr \t"; dr.Print(); 
    SG = dr * SGold; //cout << "SG\t"; SG.Print();
    cout << "new SG \t"; SG.Print();
    Survey_st row = *SectorsOnGlobal;
    Double_t *r = SG.GetRotationMatrix();
    memcpy(&row.r00, r, 9*sizeof(Double_t));
    Double_t *tr = SG.GetTranslation();
    memcpy(&row.t0, tr, 3*sizeof(Double_t));
    memcpy(&row.sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row.sigmaTrX, dxyz, 3*sizeof(Double_t));
    SsdSectorsOnGlobal->AddAt(&row);
  }
  for (Int_t s = 0; s < NoShells; s++, ShellOnGlobal++) {
    Int_t shell = ShellOnGlobal->Id;
    Int_t i = shell;
    TGeoHMatrix dR, dr;
    dR.RotateX(180./TMath::Pi()*Data[i].alpha*1e-3);
    dR.RotateY(180./TMath::Pi()*Data[i].beta*1e-3); 
    dR.RotateZ(180./TMath::Pi()*Data[i].gamma*1e-3);
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
    cout << "Additional rotation for Shell\t" << shell; dR.Print();
    SGold.SetRotation(&ShellOnGlobal->r00);
    SGold.SetTranslation(&ShellOnGlobal->t0); //cout << "Shell\t" << Shell << "\tSGold\t"; SGold.Print();
    cout << "Original SG\t"; SGold.Print();
    //________________________________________________________________________________
    Double_t norm;
    TVector3 d(&ShellOnGlobal->r00); norm = 1./d.Mag(); d *= norm;
    TVector3 t(&ShellOnGlobal->r10); norm = 1./t.Mag(); t *= norm;
    TVector3 n(&ShellOnGlobal->r20);
    TVector3 c = d.Cross(t);
    if (c.Dot(n) < 0) c *= -1;
    Double_t rot[9] = {
      d.x(),d.y(),d.z(),
      t.x(),t.y(),t.z(),
      c.x(),c.y(),c.z(),
    };
    SGold.SetRotation(rot);
    cout << "Original SG after normalization\t"; SGold.Print();
    //________________________________________________________________________________
    dr = GLI * dR * GL;
    cout << " GLI\t"; GLI.Print();
    cout << " GL \t"; GL.Print();
    cout << " dR \t"; dR.Print(); 
    cout << " dr \t"; dr.Print(); 
    SG = dr * SGold; //cout << "SG\t"; SG.Print();
    cout << "new SG \t"; SG.Print();
    Survey_st row = *ShellOnGlobal;
    Double_t *r = SG.GetRotationMatrix();
    memcpy(&row.r00, r, 9*sizeof(Double_t));
    Double_t *tr = SG.GetTranslation();
    memcpy(&row.t0, tr, 3*sizeof(Double_t));
    memcpy(&row.sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row.sigmaTrX, dxyz, 3*sizeof(Double_t));
    SvtShellOnGlobal->AddAt(&row);
  }
  TString fOut =  Form("%s.%8i.%06i.C",SsdSectorsOnGlobal->GetName(),Data[0].date,Data[0].time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[" << NoSectors << "] = {" << endl; 
  Survey_st *SectorOnGlobal = SsdSectorsOnGlobal->GetTable(); 
  for (Int_t k = 1; k < NoSectors+1; k++, SectorOnGlobal++) { 
    out << "    {" << Form("%1i",SectorOnGlobal->Id); 
    Double_t *r = &(SectorOnGlobal->r00);
    for (Int_t j = 0; j < 9; j++) out << Form(",%7.5f",r[j]);
    for (Int_t j = 9; j < 12; j++) out << Form(",%7.4f",r[j]);
    for (Int_t j = 12; j < 18; j++) out << Form(",%7.5f",r[j]);
    out << ",\"" << Data[i].comment << "\"}";
    if (i != NoSectors - 1) out << ",";
    out << endl;
  } 
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"" << SsdSectorsOnGlobal->GetName() << "\"," << NoSectors << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NoSectors << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
  //________________________________________________________________________________
  TString fOut =  Form("%s.%8i.%06i.C",SvtShellOnGlobal->GetName(),Data[0].date,Data[0].time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[" << NoShells << "] = {" << endl; 
  Survey_st *ShellOnGlobal = SvtShellOnGlobal->GetTable(); 
  for (Int_t i = 0; i < NoShells; i++, ShellOnGlobal++) { 
    out << "    {" << Form("%1i",ShellOnGlobal->Id); 
    Double_t *r = &(ShellOnGlobal->r00);
    for (Int_t j = 0; j < 9; j++) out << Form(",%7.5f",r[j]);
    for (Int_t j = 9; j < 12; j++) out << Form(",%7.4f",r[j]);
    for (Int_t j = 12; j < 18; j++) out << Form(",%7.5f",r[j]);
    out << ",\"" << Data[i].comment << "\"}";
    if (i != NoShells - 1) out << ",";
    out << endl;
  } 
  out << "  };" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"" << SvtShellOnGlobal->GetName() << "\"," << NoShells << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NoShells << "; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
 }

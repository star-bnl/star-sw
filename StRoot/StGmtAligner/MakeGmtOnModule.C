/*
  ---------------------------------------------------------------------------------
  The MakeGmtOnModule.C macros reads the last version of the GMT survery table
  in StarDb (in .DEV2 the standard place is: $STAR/StarDb/Geometry/gmt/GmtOnModule.C),
  reads the last file there, for instance, GmtOnModule.20140101.000015.C, and adds
  corrections that are listed in the Data[] for each module.
  The final rotation matrix is defined as: Tfinal = Tinitial * dT^-1,
  where dT is the correction matrix.

  As an output the macros creates the GmtOnModule.date.time.C file
  with the new alignment table. This file should be added into the $STAR/StarDb/Geometry/gmt/.
  
  To run the macro one should go to the directory where the StarDb folder is placed
  and call:
  root pathtomacro/MakeGmtOnModule.C
  The produced file should be added to the database.
  ---------------------------------------------------------------------------------
  gnigmat: 11 Dec 2015
  ---------------------------------------------------------------------------------
 */

struct data_t {
  Int_t id , emtpy;
  Double_t dx, ddx, dy, ddy, dz, ddz, alpha, dalpha, beta, dbeta, gamma, dgamma;
  Int_t  date, time;  //data = 20140101, time = version number = 2
  Char_t *comment;
};
data_t Data[] = {
  /*  This values were obtained after db1 has been done*/
  /* 
     db2
  {0, -1,  -72.69,   12.17, -1442.02,   38.53, -1960.28,   51.86,  -7.41,   1.02,    4.35,    1.01,   -1.56,    0.41, 20140101, 2, "Average"},
  {1, -1, -284.73,   18.59,  -786.51,   18.31,  -141.35,   35.45, -15.91,   1.44,   -3.39,    1.49,   -3.13,    0.35, 20140101, 2, "Average"},
  {2, -1,  255.65,   10.39, -1418.44,   27.07, -1734.94,   42.74,  -2.90,   1.00,    4.69,    0.98,    0.85,    0.32, 20140101, 2, "Average"},
  {3, -1,  185.35,   11.47,  -178.22,   12.35,  -264.18,   32.45,  -8.31,   1.26,   -3.32,    1.21,   -0.69,    0.29, 20140101, 2, "Average"},
  {4, -1,  448.79,   14.24,  -898.16,   39.97, -2250.31,   63.16, -11.89,   1.17,  -11.53,    1.13,   -3.23,    0.44, 20140101, 2, "Average"},
  {5, -1,  521.53,   12.37,   119.61,   17.79, -3008.27,   50.63, -13.72,   1.34,  -12.24,    1.35,   -1.02,    0.33, 20140101, 2, "Average"},
  {6, -1,       0,       0,        0,       0,        0,       0,      0,      0,       0,       0,       0,       0, 20140101, 2, "Average"},
  {7, -1, -224.38,   10.93,  -144.95,   19.36, -4544.15,   43.17, -14.02,   1.60,   11.94,    1.58,    0.76,    0.37, 20140101, 2, "Average"}
  
     db3
  {0, -1, -146.73,   11.66,  -983.01,   35.00,  643.91,    51.58,   0.47,   0.95,    1.25,    0.95,   -1.24,    0.36, 20140101, 3, "Average"},
  {1, -1,   25.96,   19.15,    29.42,   18.90,  516.64,    35.07,   3.04,   1.27,   -2.14,    1.41,    1.21,    0.35, 20140101, 3, "Average"},
  {2, -1,   62.13,   10.44,  -658.62,   25.77,  434.08,    43.41,   1.97,   0.90,   -2.34,    0.89,   -1.03,    0.33, 20140101, 3, "Average"},
  {3, -1,  -43.43,   11.48,    67.14,   12.23,  172.13,    32.33,  -2.77,   1.23,   -0.03,    1.23,   -0.55,    0.28, 20140101, 3, "Average"},
  {4, -1, -162.22,   12.51, -1298.29,   36.07,  452.63,    57.82,   5.28,   1.10,    6.63,    1.08,    3.36,    0.42, 20140101, 3, "Average"},
  {5, -1,  -90.98,   11.89,  -350.61,   21.00, 1120.19,    42.03,   1.43,   1.42,    1.24,    1.37,    1.25,    0.33, 20140101, 3, "Average"},
  {6, -1,       0,   -9.99,        0,   -9.99,       0,    -9.99,      0,  -9.99,       0,   -9.99,       0,   -9.99, 20140101, 3, "Average"},
  {7, -1,  131.01,   10.72,   102.83,   18.63, 1986.94,    43.13,  -2.55,   1.57,    0.76,    1.52,   -1.23,    0.30, 20140101, 3, "Average"}

     db4
  {0, -1,  -39.94,   11.43,   311.87,   34.86,  -49.07,    51.35,   1.90,   0.97,    1.94,    0.91,   -0.75,    0.36, 20140101, 4, "Average"},
  {1, -1,    2.31,   20.83,    59.55,   18.36, -179.60,    37.14,  -1.94,   1.28,   -1.28,    1.38,   -0.32,    0.34, 20140101, 4, "Average"},
  {2, -1,  -60.00,   10.44,   224.18,   25.90, -123.82,    44.14,   3.16,   0.93,    2.63,    0.86,    0.93,    0.32, 20140101, 4, "Average"},
  {3, -1,  -37.45,   11.78,    18.46,   12.19,  220.48,    32.55,   1.05,   1.25,   -2.29,    1.21,   -0.22,    0.28, 20140101, 4, "Average"},
  {4, -1,  140.66,   13.48,   270.97,   34.58, -221.36,    59.71,   2.99,   1.04,   -1.36,    1.04,   -1.78,    0.44, 20140101, 4, "Average"},
  {5, -1,   78.30,   11.97,    97.38,   19.99, -396.87,    44.08,  -4.23,   1.34,   -1.02,    1.36,   -0.30,    0.31, 20140101, 4, "Average"},
  {6, -1,       0,   -9.99,        0,   -9.99,       0,    -9.99,      0,  -9.99,       0,   -9.99,       0,   -9.99, 20140101, 4, "Average"},
  {7, -1,  -74.77,   10.86,  -138.33,   19.62,  302.53,    40.52,   2.74,   1.44,   -2.15,    1.56,   -0.01,    0.31, 20140101, 4, "Average"}
  */
  //db5
  {0, -1,  -31.89,   11.25,    15.39,   35.94,   29.46,    51.16,    0.93,  1.04,    1.48,    0.91,   -0.28,    0.36, 20140101, 5, "Average"},
  {1, -1,  -43.12,   20.77,   -21.38,   18.32,   90.54,    36.84,   -0.57,  1.32,    0.10,    1.42,    0.09,    0.35, 20140101, 5, "Average"},
  {2, -1,   37.73,   10.00,    -1.78,   26.38,  -19.08,    43.34,   -2.03,  0.90,   -1.76,    0.86,   -0.26,    0.33, 20140101, 5, "Average"},
  {3, -1,  -14.22,   12.26,     0.41,   12.79, -163.72,    33.66,    0.23,  1.25,    0.88,    1.22,   -0.12,    0.28, 20140101, 5, "Average"},
  {4, -1,  -80.96,   12.70,  -172.64,   35.50, -378.67,    57.72,    0.62,  1.15,    1.20,    1.05,    0.16,    0.42, 20140101, 5, "Average"},
  {5, -1,  -26.19,   11.11,   203.25,   16.53, -470.85,    45.33,    3.60,  1.40,    1.32,    1.40,    0.41,    0.31, 20140101, 5, "Average"},
  {6, -1,       0,   -9.99,        0,   -9.99,       0,    -9.99,       0, -9.99,       0,   -9.99,       0,   -9.99, 20140101, 5, "Average"},
  {7, -1,  -22.49,   10.33,   -37.55,   18.99, -330.35,    39.46,    1.25,  1.50,    1.60,    1.50,   -0.35,    0.30, 20140101, 5, "Average"}
};
const Int_t N = sizeof(Data)/sizeof(data_t);

//________________________________________________________________________________
void MakeGmtOnModule(){
  gROOT->LoadMacro("bfc.C");
  bfc(0,"mysql,tpcDb,MagF,nodefault");
  StMaker *db = chain->Maker("db");
  db->SetDateTime(Data[0].date,Data[0].time);
  if (! db) return;
  db->SetDebug(1);
  Survey_st row[8];
  //Get last Survey
  St_Survey *GmtOnModule = (St_Survey *) chain->GetDataBase("Geometry/gmt/GmtOnModule");
  if (! GmtOnModule)  {cout << "GmtOnModule has not been found"  << endl; return 0;}
  GmtOnModule->Print(0,8);
  Survey_st *OnModuleOld         = GmtOnModule->GetTable();
  //TGeoHMatrix T; //(u,v,w)->(w,u,v)
  //Double_t tArr[9] = { 0, 0, 1, 1, 0, 0, 0, 1, 0};
  //Double_t tArr[9] = { 0, 1, 0, 0, 0, 1, 1, 0, 0};
  //T.SetRotation(tArr);
  //T.Print();
  for (Int_t i = 0; i < N; i++) {
    //Get old matrix
    TGeoHMatrix ModuleMatrixOld;
    ModuleMatrixOld.SetRotation(&OnModuleOld[i].r00);
    ModuleMatrixOld.SetTranslation(&OnModuleOld[i].t0);
    ModuleMatrixOld.SetName(Form("Old matrix :%i",i));
    cout << "ModuleMatrixOld\t"; ModuleMatrixOld.Print();
    //Create rotation matrix obtained from fits
    TGeoHMatrix dR;
    //Important note:
    //All (x,y,z) coordinates correspond to (w,u,v) 
    //and not to (u,v,w)!
    //This is the reason why all indexes are swapped

    //Rotate rotation matrix with conversion from mrad to degrees
    //X - correspond to w, Y to u, Z to v
    dR.RotateX(180./TMath::Pi()*Data[i].gamma*1e-3);
    dR.RotateY(180./TMath::Pi()*Data[i].alpha*1e-3); 
    dR.RotateZ(180./TMath::Pi()*Data[i].beta*1e-3);
    //Convert coordonates mum to cm. 
    Double_t xyz[3], dxyz[3], drot[3];
    xyz[1] = 1e-4*Data[i].dx;
    xyz[2] = 1e-4*Data[i].dy;
    xyz[0] = 1e-4*Data[i].dz;
    dxyz[1] = 1e-4*Data[i].ddx;
    dxyz[2] = 1e-4*Data[i].ddy;
    dxyz[0] = 1e-4*Data[i].ddz;
    //Errors of the angles in rad
    drot[1] = Data[i].dalpha*1e-3;
    drot[2] = Data[i].dbeta*1e-3; 
    drot[0] = Data[i].dgamma*1e-3;
    //Set corrected rotation matrix
    dR.SetTranslation(xyz);
    cout << "Additional rotation for Gmt\t"; dR.Print();
    //Create new matrix and set it as R*dR^-1
    //TGeoHMatrix dRInv = dR.Inverse();
    //Inversion removed due to the TDraw.C gives inverted dR
    TGeoHMatrix dRInv = dR;
    TGeoHMatrix ModuleMatrixNew = ModuleMatrixOld * dRInv; 
    cout << "i: " << i << " ModuleMatrixNew\t"; ModuleMatrixNew.Print();
    Double_t *R = ModuleMatrixNew.GetRotationMatrix();
    
    memcpy(&row[i].r00, R, 9*sizeof(Double_t));
    Double_t *tr = ModuleMatrixNew.GetTranslation();
    memcpy(&row[i].t0, tr, 3*sizeof(Double_t));
    memcpy(&row[i].sigmaRotX, drot, 3*sizeof(Double_t));
    memcpy(&row[i].sigmaTrX, dxyz, 3*sizeof(Double_t));
  }
   
  //Create new GmtOnModule.date.time.C file
  TString fOut =  Form("GmtOnModule.%8i.%06i.C", Data[0].date, Data[0].time);
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "/////////////////////////////////////////////////////////" << endl;
  out << "// This file was generated by MakeGmtOnModule.C macros //" << endl;
  out << "/////////////////////////////////////////////////////////" << endl << endl;
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_Survey\")) return 0;" << endl;
  out << "  Survey_st row[8] = {" << endl; 
 
  for(Int_t i=0; i<8; i++) {
    out << "    {" << Form("%1i,",OnModuleOld[i].Id); 
    Double_t *r = &(row[i].r00);
    for (Int_t j = 0; j < 9; j++) out << Form("%6.4f,",r[j]);
    for (Int_t j = 9; j < 12; j++) out << Form("%6.4f,",r[j]); 
    for (Int_t j = 12; j < 18; j++) out << Form("%6.4f,",r[j]);
    out << "\"" << "\"}";
    if (i != 8 - 1) out << ",";
    out << endl;
  }
  out << "  };" << endl;
  out << "  Int_t noModules = 8;" << endl;
  out << "  St_Survey *tableSet = new St_Survey(\"GmtOnModule\",noModules);" << endl; 
  out << "  for (Int_t i = 0; i < noModules; i++) tableSet->AddAt(&row[i].Id, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}

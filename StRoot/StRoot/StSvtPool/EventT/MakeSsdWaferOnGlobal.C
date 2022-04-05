class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    //    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("libmysqlclient");
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  dbMk = new St_db_Maker("db","$STAR/StarDb","$PWD/StarDb");
  //  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
  //  dbMk->SetFlavor("simu","ssdWafersPosition"); 
  dbMk->Init();
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
// void DbS(const Char_t *tabNam  = 
// 	"Survey/ssd/LadderOnSurvey",Int_t date = 20051101, Int_t time = 0 
void MakeSsdWaferOnGlobal(Int_t date = 20050316, Int_t time = 195612){ 
  if (dbMk == 0) Load();
  dbMk->SetDebug(2);
  dbMk->SetDateTime(date,time); 
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20040520,0);
    // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase("Geometry/ssd");
  if (! set) return;                                                              // Positioning of the SSD: 
  St_Survey *SsdOnGlobal = (St_Survey *) set->Find("SsdOnGlobal");
  if (! SsdOnGlobal)  {cout << "SsdOnGlobal has not been found"  << endl; return;}
  St_Survey *SsdSectorsOnGlobal = (St_Survey *) set->Find("SsdSectorsOnGlobal");  // sectors in the SSD barrel coordinate system
  if (! SsdSectorsOnGlobal)  {cout << "SsdSectorsOnGlobal has not been found"  << endl; return;}
  St_Survey *SsdLaddersOnSectors = (St_Survey *) set->Find("SsdLaddersOnSectors");// ladders in the SSD sector coordinate systems
  if (! SsdLaddersOnSectors) {cout << "SsdLaddersOnSectors has not been found" << endl; return;}
  St_Survey *SsdWafersOnLadders = (St_Survey *) set->Find("SsdWafersOnLadders");  // wafers in the SSD ladder coordinate systems
  //  if (! SsdLadderAdjustment)  {cout << "SsdLadderAdjustment has not been found"  << endl; return;}
  if (! SsdWafersOnLadders)  {cout << "SsdWafersOnLadders has not been found"  << endl; return;}
  St_ssdWafersPosition *ssdWafersPosition = (St_ssdWafersPosition *) set->Find("ssdWafersPosition");
  if (! ssdWafersPosition)  {cout << "ssdWafersPosition has not been found"    << endl; return;}
  Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole 
  Survey_st *SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();  // sectors in the SSD barrel coordinate system
  Survey_st *LaddersOnSectors = SsdLaddersOnSectors->GetTable();// ladders in the SSD sector coordinate systems
  Survey_st *WafersOnLadders = SsdWafersOnLadders->GetTable();  // wafers in the SSD ladder coordinate systems
  ssdWafersPosition_st *WafersPosition = ssdWafersPosition->GetTable(); 
  Int_t NoSectors = SsdSectorsOnGlobal->GetNRows();
  Int_t NoLadders = SsdLaddersOnSectors->GetNRows();
  Int_t NoWafers  = ssdWafersPosition->GetNRows();
  St_ssdWafersPosition *ssdwafer = new St_ssdWafersPosition("ssdWafersPosition",NoWafers);
  TGeoHMatrix GL, WL,LS,SG,LA,WG;
  GL.SetRotation(&OnGlobal->r00);
  GL.SetTranslation(&OnGlobal->t0); //cout << "WL\t"; WL.Print();
  
  Int_t num = 0;
  for (Int_t w  = 0; w < NoWafers; w++, WafersPosition++) {
    ssdWafersPosition_st row = *WafersPosition;
    WafersOnLadders = SsdWafersOnLadders->GetTable();
    Int_t Id = 0;
    for (Int_t i = 0; i < NoWafers; i++,WafersOnLadders++) {
      if (WafersOnLadders->Id != row.id) continue;
      Id = row.id;
      break;
    }
    if (! Id ) {cout << "Wafer Id\t" << Id << " has not been found" << endl;}
    Int_t layer  = Id/1000;
    if (layer > 7) layer = 7;
    Int_t wafer  = (Id - 1000*layer)/100;
    Int_t ladder  = Id%100;
    //cout << "Id\t "<< Id << " " <<  100*wafer + ladder + 1000*layer <<endl;
    WL.SetRotation(&WafersOnLadders->r00);
    WL.SetTranslation(&WafersOnLadders->t0); //cout << "WL\t"; WL.Print();
    LaddersOnSectors = SsdLaddersOnSectors->GetTable();
    Int_t Ladder = 0;
    Int_t Sector = 0;
    for (Int_t l = 0; l < NoLadders; l++, LaddersOnSectors++) {
      //cout << "LaddersOnSectors Id\t" << LaddersOnSectors->Id << endl;
      Ladder = LaddersOnSectors->Id%100;
      if (Ladder == ladder) {
	Sector = LaddersOnSectors->Id/100;
	LS.SetRotation(&LaddersOnSectors->r00);
	LS.SetTranslation(&LaddersOnSectors->t0);
	//cout << "LS\t"; LS.Print();
	break;
      }
    }
    if (Sector <= 0 || Sector > 4) {cout << "Sector has not been defined" << endl; continue;}
    SectorsOnGlobal = SsdSectorsOnGlobal->GetTable();
    Int_t sector = 0;
    for (Int_t s = 0; s <NoSectors; s++, SectorsOnGlobal++) {
      //cout << "SectorsOnGlobal Id\t" << SectorsOnGlobal->Id << endl;
      if (SectorsOnGlobal->Id != Sector) continue;
      sector = Sector;
      SG.SetRotation(&SectorsOnGlobal->r00);
      SG.SetTranslation(&SectorsOnGlobal->t0); //cout << "Sector\t" << Sector << "\tSG\t"; SG.Print();
      break;
    }
    if (! sector) {cout << "Sector\t" << Sector << " has not been found" << endl; continue;}
    //    WG = SG * LS * WL * LA; //cout << "WG\t"; WG.Print();
    //    WG = SG * LS * WL * LA = SG * ( LS * WL * LA * WL**-1 ) *WL
    WG = GL * SG * LS * WL; //cout << "WG\t"; WG.Print();
    ssdWafersPosition_st row;
    row.id = Id;
    row.id_shape  = 2;
    row.ladder = ladder;
    row.layer  = layer;
    num++;
    row.num_chip  = (num-1)%16 + 1;
    //    TGeoHMatrix WGInv = WG.Inverse();
    //    Double_t *wgrot = WGInv.GetRotationMatrix();
    Double_t *r = WG.GetRotationMatrix();
    row.driftDirection[0] = r[0]; row.normalDirection[0] = r[1]; row.transverseDirection[0] = r[2];
    row.driftDirection[1] = r[3]; row.normalDirection[1] = r[4]; row.transverseDirection[1] = r[5];
    row.driftDirection[2] = r[6]; row.normalDirection[2] = r[7]; row.transverseDirection[2] = r[8];
    Double_t norm;
    TVector3 d(row.driftDirection); norm = 1/d.Mag(); d *= norm;
    TVector3 t(row.transverseDirection); norm = 1/t.Mag(); t *= norm;
    TVector3 n(row.normalDirection);
    TVector3 c = d.Cross(t);
    if (c.Dot(n) < 0) c *= -1;
    d.GetXYZ(row.driftDirection);
    t.GetXYZ(row.transverseDirection);
    c.GetXYZ(row.normalDirection);
    
    Double_t *wgtr = WG.GetTranslation();
    //    memcpy(row.driftDirection,wgrot, 9*sizeof(Double_t));
    memcpy(row.centerPosition,wgtr, 3*sizeof(Double_t));
    ssdwafer->AddAt(&row);
  }
  cout << "Create " << Form("ssdWafersPosition.%8i.%06i.C",date,time) << endl;
  ofstream out;
  out.open(Form("ssdWafersPosition.%8i.%06i.C",date,time));
  ssdwafer->SavePrimitive(out,""); 
  out.close();
  
}

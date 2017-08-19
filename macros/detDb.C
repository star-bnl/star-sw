/*
cout <<  StTpcPosition::instance()->GetMatrix().Determinant() << endl;
cout <<   StTpcHalfPosition::instance()->GetMatrix4Id(0).Determinant() << endl
cout <<   StTpcHalfPosition::instance()->GetMatrix4Id(1).Determinant() << endl
StidsOnTpc::instance()->GetMatrix(0)
StPxlpstOnIds::instance()->GetMatrix()
for (Int_t half = 0; half < 2; half++) {
StpxlHalfOnPxl::instance()->GetMatrix4Id(half+1)
StpxlSectorOnHalf::instance()->GetMatrix4Id(sector)
	Id = 4*(sector-1) + ladder;
 StpxlLadderOnSector::instance()->GetMatrix4Id(Id)
	Id = sensor + 10*(ladder+4*(sector-1) - 1);
StpxlSensorOnLadder::instance()->GetMatrix4Id(Id)
StpstOnIds::instance()->GetMatrix(0)
StistOnPst::instance()->GetMatrix(0)
StLadderOnIst::instance()->GetMatrix4Id(ladder)
Id = 1000 + sensor + 6*(ladder - 1);
 StistSensorOnLadder::instance()->GetMatrix4Id(Id)
 StsstOnOsc::instance()->GetMatrix(0)
Id = 100 + ladder
StsstLadderOnSst::instance()->GetMatrix4Id(Id);
ID     = 7000 + ladder + 100*sensor;
StsstSensorOnLadder::instance()->GetMatrix4Id(ID)

 */
class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    gROOT->LoadMacro("bfc.C");
    //    bfc(-1,"tpcDb,detDb,CorrX,nodefault");
    bfc(-1,"tpcDb,detDb,mysql,nodefault,CorrX,detDb"); // ,dbV20151120");
    dbMk = (St_db_Maker *) chain->Maker("db");
  }    
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void detDb(Int_t date = -1, Int_t time = 0,
	   Int_t debugL = 1) {
  if (dbMk == 0) Load();
  dbMk->SetDebug(debugL);
  Int_t D = date;
  Int_t T = time;
  if (D <= 0) {
    TDatime dt;
    Int_t i = dt.Convert(kTRUE); // to GMT
    dt.Set(i);
    D = dt.GetDate();
    T = dt.GetTime();
    cout << "Set GMT Date " << D << " Time " << T << endl;
  }
  dbMk->SetDateTime(D,T); 
  dbMk->Init();
}

/*
  root.exe lDb.C beamInfo.C
 */
class St_beamInfoC;
St_beamInfoC *beamInfo(Int_t D = 20190708, Int_t T = 35000) {
  St_db_Maker *dbMk = (St_db_Maker *) chain->Maker("db");
  dbMk->SetDateTime(D,T);
  chain->Init();
  chain->MakeEvent();
  if (St_beamInfoC::instance()->IsFixedTarget()) {
    cout << "D/T = " << D << "/" << T << " is Fixed Target" << endl;
  }
  return St_beamInfoC::instance();
}

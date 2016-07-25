class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void DbSVT(Int_t date = 20051101, Int_t time = 0, const Char_t *flavor = "ofl+sim"){ 
  gSystem->Load("libTable");
  gSystem->Load("St_base"); 
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so"); 
  gSystem->Load("St_db_Maker.so");
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
  //  dbMk->SetFlavor("simu","svtWafersPosition"); 
  dbMk->SetFlavor(flavor,"svtWafersPosition"); 
  dbMk->Init();

  dbMk->SetDebug(2);
  dbMk->SetDateTime(date,time); 
  Char_t *Names[2] = {"svt","ssd"};
  TDatime t[2];
  THashList *fRotList = new THashList(100,0);
  fRotList->SetOwner();
  for (Int_t i = 0; i<1; i++) {
    TDataSet *set = dbMk->GetDataBase(Form("Geometry/%s",Names[i]));
    if (! set) continue;
    TDataSetIter next(set,10);
    TDataSet *dat = 0;
    while ((dat = next())) {
      TString Name(dat->GetName());
      if (! Name.EndsWith("WafersPosition")) continue;
      if (Name == "svtWafersPosition") {
	St_svtWafersPosition *table = (St_svtWafersPosition *) dat;
	dbMk->GetValidity(table,t);
	svtWafersPosition_st *wafer = table->GetTable();
	Double_t rot[9] = {
	  wafer->driftDirection[0], wafer->transverseDirection[0], wafer->normalDirection[0],
	  wafer->driftDirection[1], wafer->transverseDirection[1], wafer->normalDirection[1],
	  wafer->driftDirection[2], wafer->transverseDirection[2], wafer->normalDirection[2]};
	Double_t tr[3] = {wafer->centerPosition[0],
			  wafer->centerPosition[1],
			  wafer->centerPosition[2]};
	TGeoHMatrix *comb = new TGeoHMatrix(Form("M%04i",wafer->ID)); 
	comb->SetRotation(rot);
	comb->SetTranslation(tr);
	fRotList->Add(comb);
      }
      if (Name == "ssdWafersPosition") {
	St_ssdWafersPosition *table = (St_ssdWafersPosition *) dat;
	ssdWafersPosition_st *wafer = table->GetTable();
	Int_t N = table->GetNRows();
	for (Int_t k = 0; k < N; k++, wafer++) {
	  Double_t rot[9] = {
	    wafer->driftDirection[0], wafer->transverseDirection[0], wafer->normalDirection[0],
	    wafer->driftDirection[1], wafer->transverseDirection[1], wafer->normalDirection[1],
	    wafer->driftDirection[2], wafer->transverseDirection[2], wafer->normalDirection[2]};
	  Double_t tr[3] = {wafer->centerPosition[0],
			    wafer->centerPosition[1],
			    wafer->centerPosition[2]};
	  TGeoHMatrix *comb = new TGeoHMatrix(Form("M%04i",wafer->id)); 
	  comb->SetRotation(rot);
	  comb->SetTranslation(tr);
	  fRotList->Add(comb);
	}
      }
    }
  }
  TString fName = Form("%i",t[0].GetDate()); fName += "."; fName += t[0].GetTime(); fName += "."; fName += flavor; fName += ".root";
  TFile *f = new TFile(fName,"recreate");
  fRotList->Write();
  delete f;
}








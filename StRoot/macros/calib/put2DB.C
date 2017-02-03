/*
  root.exe lDb.C 'put2DB.C("StarDb/Geometry/tpc/TpcHalfPosition.r2016.C")'
 */
//#include "SvtIndexMap.h"
class TTable;
TTable *table = 0;
class StDbTable;
StDbTable* dbTable = 0;
class StDbManager;
StDbManager* mgr = 0;
Int_t DT = 0;
class TDirIter;
//________________________________________________________________________________
void put2DB(const char* files=
	    "$STAR/StarDb/Geometry/svt/svtWafersPosition.20050101.000200.C"
	    /* 
	      unsetenv DB_SERVER_LOCAL_CONFIG
	      setenv DB_ACCESS_MODE write # from Michael 02/26/07
              update beginTime if necessary 
              and
	      UPDATE svtDriftCorrection set entryTime=entryTime,beginTime=''2005-01-01 00:00:00' where beginTime like '2005-01-01%'; 
	    */
	    ){
  gSystem->Setenv("DB_ACCESS_MODE","write");
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TFile *f = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    if (f) {delete f; f = 0;}
    NFiles++;
    TString bName(gSystem->BaseName(file));// cout << bName << endl;
    Int_t indx = bName.Index(".");
    TString TName(bName.Data(),indx);  cout << "Table name " << TName << endl;
    TString Time(bName.Data()+indx+1); 
    Time.ReplaceAll("C","");
    Time.ReplaceAll("root","");//  cout << "Time " << Time << endl;
    //  1996-12-01 23:59:59
    //  Int_t d=19960101;
    Int_t d=20000101;
    Int_t t =      0;
    //  sscanf(Time.Data(),"%d",&d);
    Int_t n = sscanf(Time.Data(),"%d.%d",&d,&t);
    if (n != 2) {
      Char_t tag[10];
      n = sscanf(Time.Data(),"%s",&tag);
      if (n == 1) {
	d = StMaker::AliasDate(tag);
	t = StMaker::AliasTime(tag);
	cout << "n = " << n << " tag: " << tag << " d = " << d << " t = " << t << endl;
      }
    }
    TDatime Date(d,t); cout << " Date " << Date.GetDate() << "\tTime " << Date.GetTime() << endl;
    TString dName(gSystem->DirName(file));
    TString DB("");
    Int_t indx = dName.Index("StarDb/");
    if (indx >= 0) {
      DB = dName.Data()+indx+7;
      DB.ReplaceAll("/","_");
    } else {
      DB = dName;
    }
    TString fName(file); cout << "Load " << fName << " to DB: " << DB << endl;
    if (DB == "") return;
    if (gClassTable->GetID("StDbManager") < 0) Load();
    if (fName.EndsWith(".root")) { 
      f = new TFile(file);
      if (! f) return;
      table = (TTable *) f->Get(TName.Data());
    }
    else {
      //      gROOT->LoadMacro(file);
      //      table = (TTable *) CreateTable();
      TString command;
      command = ".L "; command += file;
      TInterpreter::EErrorCode ee;
      gInterpreter->ProcessLine(command,&ee);
      if (ee) return;
      table = (TTable *) gInterpreter->Calc("CreateTable()",&ee);
      if (ee) return;
      command.ReplaceAll(".L ",".U ");
      gInterpreter->ProcessLine(command,&ee);
      if (ee) return;
    }
    if (! table) {
      cout << "Table has not been found" << endl;
      return;
    }
    TString TType("St_");
    TType += table->GetTitle(); cout << "\t" << TType << endl;
    if (TType.EndsWith("_st")) {
      TType = TString(TType,TType.Length()-3);
      cout << "\t" << TType << endl;
    }
    cout << "name = " << table->GetName() << "\ttype = " << TType << endl;
    cout << "ProcessLine(\"" << Form("%s *myTable = (%s *) table",TType.Data(),TType.Data()) << "\");" << endl;
    gInterpreter->ProcessLine(Form("%s *myTable = (%s *) table",TType.Data(),TType.Data()));
    if (! myTable) return;
    cout<< " --- Save to DB "<<endl;
    Int_t  rowSize  = myTable -> GetRowSize();   // Get the size (in bytes) of each row - fixes gap/padding problem
    StDbTableDescriptor* TD = new StDbTableDescriptor();
    TD->storeRowSize(rowSize); 
    mgr = StDbManager::Instance();
    mgr->setVerbose(kTRUE);
    //  mgr->setUser("yuri","c@lib");
    //  mgr->setUser("","");
    StDbConfigNode* node=mgr->initConfig(DB.Data());
    dbTable=node->addDbTable(TName.Data());
    cout << " --- Store table " << TName << endl;
    dbTable->checkDescriptor();
    Int_t N = myTable->GetNRows();
    Int_t NN = N;
    if (NN > 10) NN = 10;
    //  myTable->Print(0,NN);
    Int_t Nmax = N;
    if ( myTable->IsA()->InheritsFrom( "St_tpcCorrection" ) ) {
      // enlarge table up to 50 rows
      //      const Int_t Nmax = 192; 
      Nmax = 50; 
      if (TName == "TpcCurrentCorrectionX") Nmax = 192;
      if (N > Nmax) {cout << "Table has " << N << " more than " << Nmax << " rows. Possible BUG " << endl; return;}
      myTable->ReAllocate(Nmax);
      tpcCorrection_st row;
      memset(&row, 0, sizeof(tpcCorrection_st));
      for (Int_t i = N; i < Nmax; i++) myTable->AddAt(&row);
      tpcCorrection_st *r = myTable->GetTable();
      for (Int_t i = 0; i < N; i++, r++) {
	r->idx = i+1;
	r->nrows = N;
      }
      myTable->Print(0,N+1);
      N = Nmax;
    }
    Int_t offset = 1; 
    if (N == 1) offset = 0;
    if (TName.Contains("tpcDriftVelocity") ||TName.Contains("ssdConfiguration") || TName.Contains("trgTimeOffset")) offset = 0;
    if (TName.Contains("svtWafersPosition")) {cout << "Un comment SvtIndexMap include" << endl; return;}
    Int_t *rowIDs = new Int_t[N];
    for(Int_t ti=0;ti<N;ti++) rowIDs[ti]=ti + offset;
    Char_t* gstr = (Char_t*) myTable->GetTable();
    dbTable->SetTable(gstr,N,rowIDs);
#if 1
    Int_t status = 0;
    Int_t maxIter = 1; // 5
    for (Int_t i = 0; i < maxIter; i++) {
      TString TimeStamp(Form("%8d.%06d",d,t+i)); cout << "TimeStamp " << TimeStamp << endl;
      //    TDatime dt(d,t+i);
      TUnixTime dt(d,t+i,1);
      DT = dt();
      //    continue;
      mgr->setStoreTime(DT);
      status = mgr->storeDbTable(dbTable);
      if(!status) {
	if (i+1 < maxIter)  cout<<" ------> error storing in DB status = " << status << " --> try to change TimeStamp by 1 sec"<<endl;
      } else {break;}
    }
    delete [] rowIDs;
    if (! status) {cout << file << " ------> Failed" << endl; return;}
    cout << file << " ------> Done" << endl;
    TString cmd(Form("mv %s %s.HOLD",file, file)); 
    if (gSystem->Exec(cmd)) return;
#endif
  }
}
/*
  select * from tpcDriftVelocity where beginTime > "2014" and beginTime < "2014-03-14 19:37:58" and deactive=1423867472 order by beginTime ;
  select * from tpcDriftVelocity where beginTime > "2014-06-16 10:06:06" and beginTime < "2014-07-30 19:38:58" and deactive=1423867472 order by beginTime ;

 update tpcDriftVelocity set deactive=0, entryTime = entryTime  where deactive = 1423867472 and ( beginTime > "2014" and beginTime < "2014-03-14 19:37:58");
 update tpcDriftVelocity set deactive=0, entryTime = entryTime  where deactive = 1423867472 and ( beginTime >"2014-06-16 10:06:06" and beginTime < "2014-07-30 19:38:58");

*/

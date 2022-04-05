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
#if 0
//________________________________________________________________________________
void Load() {
  //  gSystem->Load("libTable");
  gSystem->Load("libStDb_Tables");
  Char_t *mysql = "libmysqlclient";
  Char_t *libs[]  = {"", "/usr/mysql/lib/", "/usr/lib/mysql/","/usr/lib/", 0}; // "$ROOTSYS/mysql-4.1.20/lib/",
  //Char_t *libs[]  = {"/usr/lib/", 0};
  Int_t i = 0;
  while ((libs[i])) {
    TString lib(libs[i]);
    lib += mysql;
    lib = gSystem->ExpandPathName(lib.Data());
    if (gSystem->DynamicPathName(lib,kTRUE)) {
      gSystem->Load(lib.Data()); cout << " + " << lib.Data() << endl;
      break;
    }
    i++;
  }
  gSystem->Load("liblog4cxx.so");
  gSystem->Load("libSt_base");                                        //  TMemStat::PrintMem("load St_base");
  gSystem->Load("libStStarLogger.so");
  gROOT->ProcessLine("StLoggerManager::StarLoggerInit();");      //  TMemStat::PrintMem("load StStarLogger");
  gSystem->Load("StDbLib");
  
}
#endif
//________________________________________________________________________________
void put2DB(const char* file=
	    "$STAR/StarDb/Geometry/svt/svtWafersPosition.20050101.000200.C"
	    /* 
	      setenv DB_ACCESS_MODE write # from Michael 02/26/07
              update beginTime if necessary 
              and
	      UPDATE svtDriftCorrection set entryTime=entryTime,beginTime=''2005-01-01 00:00:00' where beginTime like '2005-01-01%'; 
	    */
	    ){
  gSystem->Setenv("DB_ACCESS_MODE","write");
#if 0
  Load();
#endif
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
    TFile *f = new TFile(file);
    table = (TTable *) f->Get(TName.Data());
  }
  else {
    gROOT->LoadMacro(file);
    table = (TTable *) CreateTable();
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
  if ( myTable->IsA()->InheritsFrom( "St_tpcCorrection" ) ) {
    // enlarge table up to 50 rows
    const Int_t Nmax = 50; 
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
  if ( myTable->IsA()->InheritsFrom( "St_svtHybridDriftVelocity" ) ) {
    // enlarge table up to 432 rows
    const Int_t Nmax = 432; 
    if (N > Nmax) {cout << "Table has " << N << " more than " << Nmax << " rows. Possible BUG " << endl; return;}
    myTable->ReAllocate(Nmax);
    svtHybridDriftVelocity_st row;
    memset(&row, 0, sizeof(svtHybridDriftVelocity_st));
    for (Int_t i = N; i < Nmax; i++) myTable->AddAt(&row);
    svtHybridDriftVelocity_st *r = myTable->GetTable();
    for (Int_t i = 0; i < N; i++, r++) {
      r->idx = i+1;
      r->nrows = N;
    }
    myTable->Print(0,N+1);
    N = Nmax;
  }
  Int_t *rowIDs = new Int_t[N];
  Int_t offset = 1; 
  if (N == 1) offset = 0;
  if (TName.Contains("tpcDriftVelocity") ||TName.Contains("ssdConfiguration") || TName.Contains("trgTimeOffset")) offset = 0;
#ifndef _SvtIndexMap_h
  if (TName.Contains("svtWafersPosition")) {cout << "Un comment SvtIndexMap include" << endl; return;}
  for(Int_t ti=0;ti<N;ti++) rowIDs[ti]=ti + offset;
#else
  if (TName.Contains("svtWafersPosition")) {
    offset = 0; // Mike found that for svt offset should be 0
    St_svtWafersPosition *S = (St_svtWafersPosition *) table;
    svtWafersPosition_st *s = S->GetTable();
    for(Int_t ti=0;ti<N;ti++, s++) {
      Int_t elementID = -1;
      for (Int_t j = 0; j < N; j++) {
	Int_t layer = 2*SvtMap[j].Barrel - 1 + SvtMap[j].Ladder%2; 
	Int_t Id = 1000*layer + 100*SvtMap[j].Wafer + SvtMap[j].Ladder;
	if (Id != SvtMap[j].Id) {
	  cout << "Mismatch  for ID " << s->ID 
	       << " in SvtMap " 
	       << SvtMap[j].name << "\t" 
	       << SvtMap[j].Id << "\t" 
	       << SvtMap[j].Index << "\t" 
	       << SvtMap[j].elementID << "\t" 
	       << SvtMap[j].Barrel << "\t" 
	       << SvtMap[j].Ladder << "\t" 
	       << SvtMap[j].Wafer << endl;
	  return;
	}
	if (Id == s->ID) {
	  cout << "Found match for ID " << s->ID 
	       << " in SvtMap " 
	       << SvtMap[j].name << "\t" 
	       << SvtMap[j].Id << "\t" 
	       << SvtMap[j].Index << "\t" 
	       << SvtMap[j].elementID << "\t" 
	       << SvtMap[j].Barrel << "\t" 
	       << SvtMap[j].Ladder << "\t" 
	       << SvtMap[j].Wafer << endl;

	  elementID = SvtMap[j].elementID;
	  break;
	}
      }
      if (elementID < 0) {
	cout << "Don't find match for ID " << s->ID << endl;
	return;
      }
      rowIDs[ti]= elementID;
    }
  } else {
    for(Int_t ti=0;ti<N;ti++) rowIDs[ti]=ti + offset;
  }
#endif 
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
      if (i+1 < maxIter)  cout<<" ------> error storing in DB --> try to change TimeStamp by 1 sec"<<endl;
    } else {break;}
  }
  if (status) cout << " ------> Done" << endl;
  else        cout << " ------> Failed" << endl;
  delete [] rowIDs;
#endif
}
/*
  select * from tpcDriftVelocity where beginTime > "2014" and beginTime < "2014-03-14 19:37:58" and deactive=1423867472 order by beginTime ;
  select * from tpcDriftVelocity where beginTime > "2014-06-16 10:06:06" and beginTime < "2014-07-30 19:38:58" and deactive=1423867472 order by beginTime ;

 update tpcDriftVelocity set deactive=0, entryTime = entryTime  where deactive = 1423867472 and ( beginTime > "2014" and beginTime < "2014-03-14 19:37:58");
 update tpcDriftVelocity set deactive=0, entryTime = entryTime  where deactive = 1423867472 and ( beginTime >"2014-06-16 10:06:06" and beginTime < "2014-07-30 19:38:58");

*/

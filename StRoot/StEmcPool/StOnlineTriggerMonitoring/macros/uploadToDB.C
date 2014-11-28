#include <iostream>
#include <fstream>

// Macro to upload BEMC trigger DB tables after completion of data taking.
// The user must specify :
// 1) a list of DB files to upload
//   eg. ls /ldaphome/onlmon/bemctrgdb2011/tables.emconline_trg/StarDb/Calibrations/emc/trigger/bemcTriggerPed* > bemcTriggerPed.lis
// 2) path to the files (needed to convert timestamp from filename)
// 3) table type to be uploaded

void uploadToDB(const char* filelist="bemcTriggerPed.lis", TString tableType = "bemcTriggerPed", TString path = "/ldaphome/onlmon/bemctrgdb2011/tables.emconline_trg/StarDb/Calibrations/emc/trigger/") {

  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDaqLib");

  ifstream list(filelist);
  char file[200];

  // loop over list of files to upload
  while(1){
    list>>file;
    if(!list.good())break;
    //cout<<file<<endl;
    TFile *dbfile = new TFile(file,"READ");

    // convert timestamp from filename
    TString time;
    time += file;
    time.ReplaceAll(path,"");
    time.ReplaceAll(tableType+".","");
    time.ReplaceAll(".root","");
    time.ReplaceAll("."," ");
    time.Insert(4,"-");
    time.Insert(7,"-");
    time.Insert(13,":");
    time.Insert(16,":");     
    cout<<time<<endl;

    // upload tables to DB
    StBemcTablesWriter w;
    w.loadTables(time.Data());
    w.loadTableFromFile(dbfile);
    w.writeToDb(tableType.Data(), time.Data());
  }

  return;
}

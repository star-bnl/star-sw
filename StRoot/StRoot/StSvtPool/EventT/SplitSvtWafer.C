void SplitSvtWafer(const Char_t *name = "StarDb/Geometry/svt/svtWafersPosition.20050101.000100.C") {
  TString Top(name);
  Top.ReplaceAll("$STAR/","");
  Top = gSystem->DirName(Top.Data());
  static const Char_t *Barrels[4] = {"InnerBarrel","MiddleBarrel","OuterBarrel","Ssd"};
  static const Char_t *Ladder = "Ladder_";
  static const Char_t *Wafer  = "Wafer_";
  TString Name(name);
  gSystem->ExpandPathName(Name);
  gSystem->Load("libStDb_Tables.so");
  TString cmd(".L ");
  cmd += Name;
  gInterpreter->ProcessLine(cmd);
  St_svtWafersPosition *table = CreateTable();
  cmd.ReplaceAll(".L ",".U ");
  gInterpreter->ProcessLine(cmd);
  table->Print(0,10);
  TString tname = gSystem->BaseName(Name);
  Int_t N = table->GetNRows();
  svtWafersPosition_st *row = table->GetTable();
  TString separator("/");
  for (Int_t i = 0; i < N; i++, row++) {
    //  Int_t Id = ladder + 100*(wafer + 10*layer);
    Int_t Id = row->ID;
    Int_t layer = Id/1000; if (layer > 7) layer = 7; 
    Id = Id - 1000*layer;
    Int_t wafer = Id/100;
    Int_t ladder = Id%100;
    Int_t barrel = (layer-1)/2;
    TString dir(Top);
    dir += Form("/%s/%s%02i/%s%02i",Barrels[barrel],Ladder,ladder,Wafer,wafer);
    if (gSystem->AccessPathName(dir)) {
      Int_t iok = gSystem->mkdir(dir,kTRUE);
      if (iok > -1) cout << "Make directory " << dir << " done " << endl;
      else         {cout << "Make directory " << dir << " failed with " << iok << endl;}
    }
    TString ts(dir);
    ts += "/";
    ts += tname;
    ofstream out;
    St_svtWafersPosition *newtable = new St_svtWafersPosition(table->GetName(),1);
    newtable->AddAt(row);
    out.open(ts.Data());       cout << "Create " << ts << endl;
    newtable->SavePrimitive(out,""); 
    out.close();
    delete newtable;
  }
}

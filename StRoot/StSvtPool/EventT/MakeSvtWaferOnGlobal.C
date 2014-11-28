class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  dbMk = new St_db_Maker("db","$STAR/StarDb","$PWD/StarDb");
  //  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
  dbMk->SetFlavor("simu","svtWafersPosition"); 
  dbMk->Init();
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
// void DbS(const Char_t *tabNam  = 
// 	"Survey/svt/LadderOnSurvey",Int_t date = 20051101, Int_t time = 0 
void MakeSvtWaferOnGlobal(Int_t date = 20050101, Int_t time = 65 ){ 
  TGeoHMatrix GL, WL,LSU,LSH,SHG,WG;
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
  Survey_st *OnGlobal         = SsdOnGlobal->GetTable();        // SSD and SVT as whole 
  GL.SetRotation(&OnGlobal->r00);
  GL.SetTranslation(&OnGlobal->t0); //cout << "WL\t"; WL.Print();
  set = dbMk->GetDataBase("Geometry/svt");
  St_Survey *WaferOnLadder = (St_Survey *) set->Find("WaferOnLadder");
  St_Survey *LadderOnSurvey = (St_Survey *) set->Find("LadderOnSurvey");
  St_Survey *LadderOnShell = (St_Survey *) set->Find("LadderOnShell");
  St_Survey *ShellOnGlobal = (St_Survey *) set->Find("ShellOnGlobal");
  Int_t NW = WaferOnLadder->GetNRows();
  Int_t NL = LadderOnSurvey->GetNRows();
  Survey_st *waferOnLadder = WaferOnLadder->GetTable();
  Survey_st *ladderOnSurvey = LadderOnSurvey->GetTable();
  Survey_st *ladderOnShell = LadderOnShell->GetTable();
  Survey_st *shellOnGlobal0 = ShellOnGlobal->GetTable(0);
  Survey_st *shellOnGlobal1 = ShellOnGlobal->GetTable(1);
  St_svtWafersPosition *svtwafer = new St_svtWafersPosition("svtWafersPosition",216);
  svtWafersPosition_st row;
  for (Int_t i = 0; i < NW; i++, waferOnLadder++)
    {
      Int_t Idw = waferOnLadder->Id;
      WL.SetRotation(&waferOnLadder->r00);
      WL.SetTranslation(&waferOnLadder->t0);
      //	    if (i==0) WL.Print();
      Int_t wshell  = 0;
      Int_t wbarrel  = Idw/1000;
      Int_t wwafer  = (Idw - 1000*wbarrel)/100;
      Int_t wladder = Idw%100;
      Int_t wlayer = 2*wbarrel + wladder%2 - 1;
      //	    cout << waferOnLadder->Id << "  "<< Idw<< " " <<  100*wwafer + wladder + 1000*wlayer <<endl;
      for ( Int_t j = 0; j < NL; j++, ladderOnSurvey++, ladderOnShell++)
	{
	  Int_t Idl =  ladderOnSurvey->Id;
	  Int_t lbarrel  = Idl/1000;
	  Int_t lladder = Idl%100;
	  if( wladder ==  lladder )
	    {
	      LSU.SetRotation(&ladderOnSurvey->r00);
	      LSU.SetTranslation(&ladderOnSurvey->t0);
	      LSH.SetRotation(&ladderOnShell->r00);
	      LSH.SetTranslation(&ladderOnShell->t0);
	      if( (wbarrel == 1 && wladder <= 4) || (wbarrel == 2 && wladder <= 6) ||  (wbarrel == 3 && wladder <= 8) )
		{
		  SHG.SetRotation(&shellOnGlobal0->r00);
		  SHG.SetTranslation(&shellOnGlobal0->t0);
		}else
		{
		  SHG.SetRotation(&shellOnGlobal1->r00);
		  SHG.SetTranslation(&shellOnGlobal1->t0);
		}		    
	      //   SsdOnGlobal * ShellOnGlobal * LadderOnShell * LadderOnSurvey * WaferOnLadder 
	      WG = GL * SHG * LSH * LSU * WL; //  WG.Print();
	      //			    TGeoHMatrix WGInv = WG.Inverse();
	      Double_t *r = WG.GetRotationMatrix();
	      Int_t fail = 0;
	      for (int l = 0; l < 9; l++) {
		if (TMath::Abs(r[l]) >=  1.000001) fail++;
	      }
	      if (fail) {
		cout << "===============" << waferOnLadder->Id << "  "<< Idw << " " <<  100*wwafer + wladder + 1000*wlayer <<endl;
		cout << "WG\t"; WG.Print();
		//			      cout << "SHG\t"; SHG.Print();
		//			      cout << "LSH\t"; LSH.Print();
		//			      cout << "LSU\t"; LSU.Print();
		//			      cout << "WL\t"; WL.Print();
	      }
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
	      
	      row.ID = 100*wwafer + wladder + 1000*wlayer;
	      Double_t *wgtr = WG.GetTranslation();
	      memcpy(row.centerPosition,wgtr, 3*sizeof(Double_t));
	      svtwafer->AddAt(&row);
	      break;
	      
	    }
	}
    }
  ofstream out;
  out.open(Form("svtWafersPosition.%8i.%06i.C",date,time));
  svtwafer->SavePrimitive(out,""); 
  out.close();
  
}

//     //    table = (TTable *) set->FindByName(gSystem->BaseName(tabNam));
//     if (table) {
// 	TDatime t[2];
// 	dbMk->GetValidity(table,t);
//     cout << "==============================================" << endl;
//     Int_t Nrows = table->GetNRows();
//     cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
//     cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
// 	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
//     if (Nrows > 10) Nrows = 10;
//     table->Print(0,Nrows);
//     cout << "==============================================" << endl;
// //     TString name(gSystem->BaseName(tabNam));
// //     name += Form(".%06i.%06i.old.root",t[0].GetDate(),t[0].GetTime());
// //     TFile *f = new TFile(name.Data(),"RECREATE");
// //     table->Write();
// //     delete f;
//   }
//   else cout << "Table:" << tabNam << " has not been found" << endl;

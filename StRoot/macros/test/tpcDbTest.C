class St_db_Maker;
class StTpcDbMaker;
void tpcDbTest() {
  St_db_Maker *dbMk = 0;
  StTpcDbMaker *tpcDbMk = 0;
  if ( gClassTable->GetID("StTpcCoordinateTransform") < 0) { 
    gROOT->LoadMacro("bfc.C");
    bfc(-1,"tpcDb,sdt20140201,CorrX,nodefault");
  } 
  if (! dbMk) {
    dbMk = (St_db_Maker *) chain->Maker("db");
    tpcDbMk = (StTpcDbMaker *) chain->Maker("tpcDB");
  }
  dbMk->SetDebug();
  tpcDbMk->SetDebug();
  chain->SetDebug();
  chain->Init();
  new StarMagField;
  chain->Make();
  StTpcCoordinateTransform tran;
  Double_t z = 0;
  Double_t x = 122;
  StTpcLocalCoordinate locP(0,x,z,12,13); cout << "locP\t" << locP << endl;
  StTpcPadCoordinate padP;
  tran(locP,padP);                      cout << "padP\t" << padP << endl;
#if 0
  Double_t R[24];
  for (Int_t s = 1; s <= 24; s++) {
    padP->setSector(s);  cout << "padP\t" << padP << endl;
    tran(padP,locP);     cout << "locP\t" << locP << endl;
    R[s-1] = locP.position().y();
  }
  for (Int_t s = 1; s <= 24; s++) {
    cout <<  " " << Form("%8.3f",R[s-1]) << ",";
  }
  cout << endl;
#else 
  const Double_t zGG = 208.707;
  const Double_t zI  = zGG + 1.0;
  const Double_t zO  = zGG + 1.4;
  StTpcLocalSectorCoordinate lSec, lSecR;
  tran(locP,lSec);                      cout << "lSec\t" << lSec << endl;
  StGlobalCoordinate glob; 
  StTpcPadCoordinate padP;
  tran(locP,padP);                      cout << "padP\t" << padP << endl;
#if 0
  StTpcDb::instance()->Tpc2GlobalMatrix().Print();
  StTpcDb::instance()->Flip().Print();
  StTpcDb::instance()->TpcHalf(0).Print();
  StTpcDb::instance()->TpcHalf(1).Print();
  StTpcDb::instance()->Half2Tpc( 1).Print();
  StTpcDb::instance()->Half2Tpc(13).Print();
  StTpcDb::instance()->Half2Glob( 1).Print();
  StTpcDb::instance()->Half2Glob(13).Print();
#endif
  for (Int_t iz = 0; iz < 3; iz++) {
    for (Int_t row = 13; row <= 14; row++) {
      for (Int_t sector = 3; sector <= 24; sector += 12) {
	cout << "=============== sec " << sector << " ====== row " << row << " ===============" << endl;
#if 0
	StTpcDb::instance()->SupS2Half(sector).Print();
#endif
        z = 0;
	if      (iz == 1) z = 100;
	else if (iz == 2) z = zGG;
	Double_t x = tran.yFromRow(row);
	if (sector > 12) x = -x;
	if (sector > 12) z = -z;
#if 0
	StTpcDb::instance()->SubS2SupS(sector,row).Print();
	StTpcDb::instance()->SupS2Half(sector).Print();
#endif
	StTpcLocalCoordinate lTpc(x,0,z,sector,row);  cout << "lTpc \t" << lTpc  << endl;
	
	cout << "begin =============== sec " << sector << " ====== row " << row << " ===============" << endl;
	tran(lTpc,lSec);                             cout << "lTpc  => lSec \t" << lSec  << endl;
	tran(lSec,padP);                             cout << "lSec => padP\t" << padP << endl;
	tran(lSec,lTpc);                             cout << "lSec => lTpc\t" << lTpc << endl;
	tran(padP,lTpc);                             cout << "padP => lTpc\t" << lTpc << endl;
#if 0
	StThreeVectorD temp;
	StTpcDb::instance()->Half2Tpc(sector).MasterToLocal(lTpcR.position().xyz(),temp.xyz());                cout << "lTpcR => half\t"  << temp << endl;
	StTpcDb::instance()->Pad2Half(sector,row).MasterToLocal(temp.xyz(),lSecR.position().xyz());            cout << "half  => lSecR\t" << lSecR << endl;
#endif
	tran(lSec,lTpc);                                                                                       cout << "lSec  => lTpc \t" << lTpc  << endl;
#if 0
	StTpcDb::instance()->Pad2Half(sector,row).LocalToMaster(lSecR.position().xyz(),temp.xyz());            cout << "lSecR => half\t"  << temp << endl;
	StTpcDb::instance()->Half2Tpc(sector).LocalToMaster(temp.xyz(),lTpcR.position().xyz());                cout << "half  => lTpcR\t" << lTpcR << endl;
#endif
#if 1
	tran(lTpc,glob);                             cout << "lTpc => glob\t" << glob << endl;
	tran(lSec,padP);                             cout << "lSec => padP\t" << padP  << endl;
	tran(padP,lSec);                             cout << "padP => lSec\t" << lSec << endl;
	tran(lSec,lTpc);                             cout << "lSec => lTpc\t" << lTpc << endl;
	tran(lTpc,glob);                             cout << "lTpc => glob\t" << glob << endl;
	//	tran(glob,lTpc,sector,row);                  cout << "glob => lTpc\t" << lTpc  << endl;
#endif
	cout << "end   =============== sec " << sector << " ====== row " << row << " ===============" << endl;
      }
      //      break;
    }
    //    break;
  }
  /*
    StTpcCoordinateTransform tran;
    Int_t row = 1;
    Int_t sector = 3;
    StTpcPadCoordinate padP(sector,row,1,0);  cout << "padP\t" << padP << endl;
    StTpcLocalCoordinate lTpc;
    tran(padP,lTpc,kFALSE,kFALSE);            cout << "lTpc\t" << lTpc << endl;
    tran(lTpc,padP,kFALSE,kFALSE);            cout << "padP\t"  << padP  << endl;
    

    StTpcLocalCoordinate lTpc(119.628, 27.2423, 213.373 , 3,13 ); cout << "lTpc\t" << lTpc << endl;
    
  */
#endif
}


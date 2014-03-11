class St_db_Maker;
class StTpcDbMaker;
void tpcDbTest() {
  St_db_Maker *dbMk = 0;
  StTpcDbMaker *tpcDbMk = 0;
  if ( gClassTable->GetID("StTpcCoordinateTransform") < 0) { 
    gROOT->LoadMacro("bfc.C");
    bfc(-1,"tpcDb,sdt20140201,nodefault");
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
  StTpcCoordinateTransform tran(gStTpcDb);
  const Double_t zGG = 208.707;
  const Double_t zI  = zGG + 1.0;
  const Double_t zO  = zGG + 1.4;
  Double_t z = 0;
  Double_t x = 122;
  StTpcLocalCoordinate locP(x,0,z,1,1); cout << "locP\t" << locP << endl;
  StTpcLocalSectorCoordinate lSec;
  tran(locP,lSec);
  StGlobalCoordinate glob;
  StTpcPadCoordinate padP;
  tran(locP,padP);
  for (Int_t sector = 3; sector <= 24; sector += 12) {
    for (Int_t row = 13; row <= 14; row++) {
      cout << "=============== sec " << sector << " ====== row " << row << " ===============" << endl;
#if 0
      Double_t x = tran.yFromRow(row);
      if (sector > 12) x = -x;
#endif
      for (Int_t iz = 0; iz < 3; iz++) {
#if 0
	if (iz == 0) {z = 0;}
	else {
	  z = zI;
	  if (row > 13) z = zO;
	  if (sector > 12) z = -z;
	  if (iz == 2) z *= 0.5; 
	}
	cout << "============ z " << z << " ===============" << endl;
	StTpcLocalCoordinate lTpc(x,0,z,sector,row); cout << "lTpc\t" << lTpc << endl;
#else
	Double_t tb = 0;
	if (iz == 1) tb = 100;
	else if (iz == 2) tb = 350;
	cout << "============ tb " << tb << " ===============" << endl;
	Double_t pad = 10;
	padP = StTpcPadCoordinate(sector,row,pad,tb);cout << "padP        \t" << padP << endl;
	tran(padP,lSec);                             cout << "padP => lSec\t" << lSec << endl;
	tran(lSec,padP);                             cout << "lSec => padP\t" << padP << endl;
	StTpcLocalCoordinate lTpc;
	tran(lSec,lTpc);                             cout << "lSec => lTpc\t" << lTpc << endl;
#endif
	tran(lTpc,lSec);                             cout << "lTpc => lSec\t" << lSec << endl;
	tran(lTpc,glob);                             cout << "lTpc => glob\t" << glob << endl;
	//      tran(glob,lTpc,sector,row);                    
	tran(lTpc,lSec);                             cout << "lTpc => lSec\t" << lSec << endl;
	tran(lSec,padP);                             cout << "lSec => padP\t" << padP  << endl;
	tran(padP,lSec);                             cout << "padP => lSec\t" << lSec << endl;
	tran(lSec,lTpc);                             cout << "lSec => lTpc\t" << lTpc << endl;
	tran(lTpc,glob);                             cout << "lTpc => glob\t" << glob << endl;
	tran(glob,lTpc,sector,row);                  cout << "glob => lTpc\t" << lTpc  << endl;
	tran(lTpc,lSec);                             cout << "lTpc => lSec\t" << lSec  << endl;
	tran(glob,lSec,sector,row);                  cout << "glob => lSec\t" << lSec  << endl;
	tran(lSec,padP);                             cout << "lSec => padP\t" << padP << endl;
	//	tran(glob,padP,sector,row);                  cout << "glob => padP\t" << padP  << endl;
      }
      //      break;
    }
    //    break;
  }
  /*
    StTpcCoordinateTransform tran(gStTpcDb);
    Int_t row = 1;
    Int_t sector = 3;
    StTpcPadCoordinate padP(sector,row,1,0);  cout << "padP\t" << padP << endl;
    StTpcLocalCoordinate lTpc;
    tran(padP,lTpc,kFALSE,kFALSE);            cout << "lTpc\t" << lTpc << endl;
    tran(lTpc,padP,kFALSE,kFALSE);            cout << "padP\t"  << padP  << endl;
   */
}


void moveTpcHit(StTpcLocalCoordinate  &coorL,StGlobalCoordinate &coorG) {
  StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalSectorCoordinate  coorLS;
  transform(coorL,coorLS);   // to sector 12
  static StTpcLocalSectorAlignedCoordinate  coorLSA;
  transform(coorLS,coorLSA); // alignment
  static StTpcLocalCoordinate  coorLT;
  transform(coorLSA,coorLT); //
  static StTpcLocalCoordinate  coorLTD;
  coorLTD = coorLT;          // distortions
#if 0
  // ExB corrections
  Float_t pos[3] = {coorLTD.position().x(),coorLTD.position().y(),coorLTD.position().z()};
  if ( mExB ) {
    Float_t posMoved[3];
    mExB->UndoDistortion(pos,posMoved);   // input pos[], returns posMoved[]
    StThreeVector<double> newPos(posMoved[0],posMoved[1],posMoved[2]);
    coorLTD.setPosition(newPos);
  }
#endif
  transform(coorLTD,coorG);
}
//________________________________________________________________________________
void test() {
  StTpcCoordinateTransform tran(gStTpcDb);
  Int_t row = 45;
  StTpcLocalCoordinate locP;
  StGlobalCoordinate glob;
  for (Int_t sector = 1; sector <= 45; sector++) {
    StTpcPadCoordinate padP(sector,row,1,0);  cout << "padP\t" << padP << endl;
    tran(padP,locP,kFALSE,kFALSE);            cout << "locP\t" << locP << endl;
    moveTpcHit(locP,glob);                    cout << "glob\t" << glob << endl;
  }
}


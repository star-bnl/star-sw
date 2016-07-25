/* 
   root.exe 'Load.C("St_base,StChain,libStDb_Tables,StDetectorDbMaker,StEvent,StTpcDb,StSvtDbMaker,StDbUtilities")' Claude.C+
   root.exe 'bfc.C(-1,"svtDb,tpcDb,StEvent,nodefault,sdt20110101")' Claude.C+
*/
#include "Rtypes.h"
#include "Stiostream.h"
#include "StMessMgr.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
#include "StMatrixD.hh"
using namespace std;
int Debug = 1;
extern "C" void Claude() {
  int numberOfSectors   = gStTpcDb->Dimensions()->numberOfSectors();
  int NumberOfRows      = gStTpcDb->PadPlaneGeometry()->numberOfRows();
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  Local2GlobalRotation[24][45]; 
  StThreeVectorD RowPosition[24][45];
  for (int sector = 1; sector<= numberOfSectors; sector++) {
    for (int row = 1; row <= NumberOfRows; row++) {
      if (Debug>1) cout << "========= sector/row ========" << sector << "/" << row << endl;
      StTpcLocalSectorDirection  dirLS[3];
      dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,sector,row);
      dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,sector,row);
      dirLS[2] = StTpcLocalSectorDirection(0.,1.,1.,sector,row);
      for (int i = 0; i < 3; i++) {
	if (Debug>1) cout << "dirLS\t" << dirLS[i] << endl;
	StTpcLocalDirection        dirL;      
	StTpcLocalSectorAlignedDirection  dirLSA;
	transform(dirLS[i],dirLSA);   if (Debug>1) cout << "dirLSA\t" << dirLSA << endl;
	transform(dirLSA,dirL);       if (Debug>1) cout << "dirL\t" << dirL << endl;
	StGlobalDirection          dirG;
	transform(dirL,dirG);      if (Debug>1) cout << "dirG\t" << dirG << endl;
	Local2GlobalRotation[sector-1][row-1](i+1,1) = dirG.position().x();
	Local2GlobalRotation[sector-1][row-1](i+1,2) = dirG.position().y();
	Local2GlobalRotation[sector-1][row-1](i+1,3) = dirG.position().z();
      }
      if (Debug>1) cout << "Local2GlobalRotation[" << sector-1 << "][" << row-1 << "] = " 
			<< Local2GlobalRotation[sector-1][row-1] << endl;
      double y  = transform.yFromRow(row);
      double tb = transform.tBFromZ(0.,sector,row);
      StTpcLocalSectorCoordinate  lsCoord(0., y, tb,sector,row); if (Debug>1) cout << lsCoord << endl;
      StGlobalCoordinate  gCoord; 
      StTpcLocalSectorAlignedCoordinate lsCoordA;  
      transform(lsCoord,lsCoordA);                       if (Debug>1) cout << lsCoordA << endl;                   
      transform(lsCoordA, gCoord);                       if (Debug>1) cout << gCoord << endl;                   
      RowPosition[sector-1][row-1] =   
	StThreeVectorD(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
      if (Debug>1) cout << "RowPosition[" << sector-1 << "][" << row-1 << "] = " 
			<< RowPosition[sector-1][row-1] << endl;
    }
  }
}

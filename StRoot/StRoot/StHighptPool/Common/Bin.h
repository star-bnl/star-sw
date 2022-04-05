/*
  standard histogram bins and ranges
*/

#ifndef Bin_H
#define Bin_H

#include "TObject.h"
#include "TArrayD.h"
#include <cmath>
#include <cstdlib>

namespace Bin
{
  //
  // event stuff
  //

  const float vertexZEvtMin = -100;
  const float vertexZEvtMax = 100;
  const int    nVertexZEvtBin = 
    (int) ceil((vertexZEvtMax-vertexZEvtMin)/5);
  const int    nVertexZEvtThinBin = (int)ceil((vertexZEvtMax-vertexZEvtMin)/1);

   const float vertexXMin = -1;
   const float vertexXMax = 1;
   const int nVertexXBin = (int)ceil((vertexXMax-vertexXMin)/.05);

   const float vertexREvtMin = 0;
   const float vertexREvtMax = 5;
   const int nVertexREvtBin = (int)ceil((vertexREvtMax-vertexREvtMin)/.1);

  // 0-9 9 is overflow(most central)
  const float flowCentMin = -0.5;
  const float flowCentMax = 9.5;
  const int    nFlowCentBin = 10;
  
  // 0-9 0 is most central (kFive)
   const float zdcCentMin = -0.5; 
   const float zdcCentMax = 9.5;
   const int    nZdcCentBin = 10;

   const float ctbMin = -0.5;
   const float ctbMax = 50000.5;
   const int nCtbBin=(int)ceil((ctbMax-ctbMin)/100);

   const float zdcMin=-0.5;
   const float zdcMax=200.5;
   const int nZdcBin=(int)ceil((zdcMax-zdcMin));
   
   const float hMinusMin=-0.5;
   const float hMinusMax=400.5;
   const int nHMinusBin=(int)ceil((hMinusMax-hMinusMin));

   const float nChMin=-0.5;
   const float nChMax=800.5;
   const int nNchBin=(int)ceil((nChMax-nChMin));

   const float nGGMin=-0.5;
   const float nGGMax=2000.5;
   const int nNGGBin=(int)ceil((nGGMax-nGGMin));

  //
  //****** track stuff
  //
  void initPtAry(TArrayD* a, int type);

  const float lowPtMin = 0;
  const float lowPtMax = 3;
  const int nLowPtBin = (int)ceil((lowPtMax-lowPtMin)/.1);

  const float lowPtThinMin = 0;
  const float lowPtThinMax = 2;
  const int nLowPtThinBin = (int)ceil((lowPtThinMax-lowPtThinMin)/.05);

  const float mtMin = 0;
  const float mtMax = 6;
  const int   nMtBin = (int)ceil((mtMax-mtMin)/.2);

  const float ptMin = 0.5;
  const float ptMax = 12.0;
  const int    nPtBin = (int)ceil((ptMax-ptMin)/.5);
  
   const float ptThinMin = 1.0;
   const float ptThinMax = 12.0;
   const int    nPtThinBin = (int)ceil((ptThinMax-ptThinMin)/.1);

   const float ptTinyMin = 0;
   const float ptTinyMax = 12;
   const int nPtTinyBin = (int)ceil((ptTinyMax-ptTinyMin)/.1);

   // will rebin with these
   const double ptRebinMin = 1;
   const double ptRebinMax = 12;
   const int nPtRebinBin = (int)ceil((ptRebinMax-ptRebinMin)/.05);
   

   const float resPtMin = -1.5;
   const float resPtMax = 1.5;
   const int nResPtBin = (int)ceil((resPtMax-resPtMin)/.005);

   const float resPtWideMin = -5;
   const float resPtWideMax = 1.0;
   const int nResPtWideBin = (int)ceil((resPtMax-resPtMin)/.01);

   const float resEtaMin = -.5;
   const float resEtaMax = .5;
   const int nResEtaBin = (int)ceil((resEtaMax-resEtaMin)/.02);

   const float resCurvMin = -2;
   const float resCurvMax = 1;
   const int nResCurvBin = (int)ceil((resCurvMax-resCurvMin)/.02);
			
   const float etaWideMin=-2;
   const float etaWideMax=2;
   const int    nEtaWideBin = (int)ceil((etaWideMax-etaWideMin)/.1);
  
   const float etaMin = -1.0;
   const float etaMax = 1.0;
   const int nEtaBin = (int)ceil((etaMax-etaMin)/.1);

   const float etaBigMin = -1.0;
   const float etaBigMax = 1.0;
   const int nEtaBigBin = (int)ceil((etaBigMax-etaBigMin)/.25);

   const float etaThinMin = -1.;
   const float etaThinMax = 1;
   const int    nEtaThinBin = (int)ceil((etaThinMax-etaThinMin)/.025);

   const float etaSmallMin = -1.0;
   const float etaSmallMax = 1.0;
   const int nEtaSmallBin = (int)ceil((etaSmallMax-etaSmallMin)/.05); 

   const float sDcaMin = -3;
   const float sDcaMax = 3;
   const int nSDcaBin = (int)ceil((sDcaMax-sDcaMin)/.1);

   const float dcaXYGlMin = -3;
   const float dcaXYGlMax = 3;
   const int nDcaXYGlBin =(int)ceil((dcaXYGlMax-dcaXYGlMin)/.05);

   const float dcaXYGlWideMin = -3;
   const float dcaXYGlWideMax = 3;
   const int nDcaXYGlWideBin =(int)ceil((dcaXYGlMax-dcaXYGlMin)/.1);
  
   const float dcaMin = 0;
   const float dcaMax = 3;
   const int nDcaBin = (int)ceil((dcaMax-dcaMin)/.05);

   const float dcaThinMin = 0;
   const float dcaThinMax = 3;
   const int nDcaThinBin = (int)ceil((dcaThinMax-dcaThinMin)/.05);

   const float commonFracMin = 0;
   const float commonFracMax = 1;
   const int nCommonFracBin = (int)ceil((commonFracMax-commonFracMin)/.02);

  const float fitPtsMin = -0.5;
  const float fitPtsMax = 50.5;
  const int nFitPtsBin = 51;

  const float fitPtsWideMin=25.5; // 26-29,30-33,34-37,38-41,42-45
  const float fitPtsWideMax=45.5;
  const int nFitPtsWideBin=5;

  const float fitPtsCentMin = -0.5;
  const float fitPtsCentMax = 50.5;
  const int nFitPtsCentBin = 51;

   const float fitHitMin = 0.5;
   const float fitHitMax = 45.5;
   const int nFitHitBin = 1;

   const float fracHitMin = 0;
   const float fracHitMax = 1;
   const int nFracHitBin = (int)ceil((fracHitMax-fracHitMin)/.02);

   const float fitHitThinMin = 0.5;
   const float fitHitThinMax = 45.5;
   const int nFitHitThinBin = 45;
  
   const float vertexZMin = -100;
   const float vertexZMax = 100;
   const int nVertexZBin = (int)ceil((vertexZMax-vertexZMin)/10);
   const int nEnterZBin  = (int)ceil((vertexZMax-vertexZMin)/10);

  const float vertexZWideMin = -100;
  const float vertexZWideMax = 100;
  const int nVertexZWideBin = (int)ceil((vertexZWideMax-vertexZWideMin)/25);

  const float vertexZBigMin = -100;
  const float vertexZBigMax = 100;
  const int nVertexZBigBin = (int) ceil((vertexZBigMax-vertexZBigMin)/50);
    
   const float midZBigMin = -100;
   const float midZBigMax = 100;
   const int nMidZBigBin = (int) ceil((midZBigMax-midZBigMin)/50);

   const float absZBigMin = 0;
   const float absZBigMax = 100;
   const int nAbsZBigBin = 4;
  
   const float sectorWestMin = 0.5;
   const float sectorWestMax = 12.5;

   const float sectorEastMin = 12.5;
   const float sectorEastMax = 24.5;

   const float sectorMin = 0.5;
   const float sectorMax = 24.5;
  
   const int nSectorEWBin = 12;
   const int nSectorBin = 24;


   const float dPhiMin = -.02;
   const float dPhiMax = .02;
   const int nDPhiBin = (int)ceil((dPhiMax-dPhiMin)/.001);

   const float dEtaMin = -.1;
   const float dEtaMax = .1;
   const int nDEtaBin = (int)ceil((dEtaMax-dEtaMin)/.001);

  //   const float dPtMin = -6;
  //  const float dPtMax = 6;
  //  const int nDPtBin = (int)ceil((dPtMax-dPtMin)/.2);

   const float phiMin = -3.14159;
   const float phiMax = 3.14159;
   const float phiDegMin = -165;
   const float phiDegMax = 195;
   const int nPhiBin = 12;
   const int nPhiSmallBin = 24;
   const int nPhiThinBin = 24*5;

   const float dipMin = -.5;
   const float dipMax = .5;
   const int nDipBin = (int)ceil((dipMax-dipMin)/.02);

   const float dipWideMin = -.5;
   const float dipWideMax = .5;
   const int nDipWideBin = (int)ceil((dipMax-dipMin)/.1);

   const float pzMin = 5;
   const float pzMax = -5;
   const int nPzBin = (int)ceil((pzMax-pzMin)/1);

   const float dcaZGlMin = -.5;
   const float dcaZGlMax = .5;
   const int nDcaZGlBin = (int)ceil((dcaZGlMax-dcaZGlMin)/.02);

   const float dcaZGlWideMin = -.5;
   const float dcaZGlWideMax = .5;
   const int nDcaZGlWideBin = (int)ceil((dcaZGlWideMax-dcaZGlWideMin)/.1);

   const float dDipMin = -.003;
   const float dDipMax = .003;
   const int nDDipBin = (int)ceil((dDipMax-dDipMin)/.0001);

   const float dcaXYPrMin = -.2;
   const float dcaXYPrMax = .2;
   const int    nDcaXYPrBin = (int)ceil((dcaXYPrMax-dcaXYPrMin)/.01);

   const float dedxMin = 0;
   const float dedxMax = 1e-5;
   const int nDedxBin = (int)ceil((dedxMax-dedxMin)/.2e-6);

}






#endif

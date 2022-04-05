#include <stdio.h>
#include "iomanip.h"

void genOffset()
{
  gROOT->Reset();

  const Int_t mNTrays = 120;

   ifstream inData;
   double x0[mNTrays], phi0[mNTrays], z0[mNTrays];
   double x1[mNTrays], phi1[mNTrays], z1[mNTrays];
   double tmp;
   inData.open("/star/u/dongx/lbl/tof/Run10/GeomAlign/dat/yOffset.dat");
   for(int i=0;i<mNTrays;i++) {
     inData >> phi0[i] >> tmp;
   }
   inData.close();
   inData.open("/star/u/dongx/lbl/tof/Run10/GeomAlign/dat/yOffset1.dat");
   for(int i=0;i<mNTrays;i++) {
     inData >> phi1[i] >> tmp;
     phi0[i] += phi1[i];
   }
   inData.close();
   inData.open("/star/u/dongx/lbl/tof/Run10/GeomAlign/dat/xOffset.dat");
   for(int i=0;i<mNTrays;i++) {
     inData >> z0[i] >> tmp >> x0[i] >> tmp;
   }
   inData.close();
   inData.open("/star/u/dongx/lbl/tof/Run10/GeomAlign/dat/xOffset1.dat");
   for(int i=0;i<mNTrays;i++) {
     inData >> z1[i] >> tmp >> x1[i] >> tmp;
     z0[i] += z1[i];
     x0[i] += x1[i];
   }
   inData.close();

   ofstream outData;
   outData.open("geomAlign.txt");
   for(int i=0;i<mNTrays;i++) {
     outData << setw(15) << phi0[i] << setw(15) << -z0[i] << setw(15) << x0[i] << endl;
   }
   outData.close();

}

#include "VecBosEventInfo.h"


ClassImp(DetEventBemc)


void DetEventBemc::clear()
{
   memset(adcTile,   0, sizeof(adcTile));
   memset(eneTile,   0, sizeof(eneTile));
   memset(statTile, -1, sizeof(statTile)); // default all dead
   memset(tileIn,    0, sizeof(tileIn));   // detector was On/Off
   memset(adcBsmd,   0, sizeof(adcBsmd));
   memset(statBsmd, -1, sizeof(statBsmd)); // default all dead
   maxAdc   = 0;
   maxHtDsm = -1;
}


void DetEventBemc::print(int flag)
{
   printf(" BTOW tower ADC>500 list: ");
   for (int i = 0; i < mxBtow; i++) {
      if (adcTile[kBTow][i] < 500) continue;
      int id = i + 1;
      printf("id=%d adc=%.1f ene=%.1f;  ", id, adcTile[kBTow][i], eneTile[kBTow][i]);
   }    printf("\n");

   printf(" BSMDE tower ADC>200 list: ");

   for (int i = 0; i < mxBStrips; i++) {
      if (adcBsmd[ kBSE][i] < 200) continue;
      int id = i + 1;
      int module = 1 + i / 150;
      printf("id=%d mod=%d adc=%.1f ;  ", id, module, adcBsmd[ kBSE][i]);
   }

   printf("\n");
   printf(" BTOW maxAdc=%.1f  maxHtDsm=%d\n", maxAdc, maxHtDsm);

   if (flag & 1) {
      for (int i = 0; i < 120; i++) {
         int id = 7 + i * 40;
         if (i % 10 == 0) printf("\n  softID=%4d adc format  BTOW:BPRS=  ", id);
         printf("%.0f : %.0f, ", adcTile[kBTow][id - 1], adcTile[kBPrs][id - 1]);
      }
      printf("\n");
   }
}

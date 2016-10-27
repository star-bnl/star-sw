
#include "CalTower.h"


void CalTower::Clear(const Option_t* opt)
{
   id    = 0;
   R     = TVector3(0, 0, 0);
   Rglob = TVector3(0, 0, 0);
   iEta  = iPhi = 9999;
}


void CalTower::Print(const Option_t* opt) const
{
   printf("\n");
   Info("print(int flag)", "");
   printf("pointed tower ID=%d; L2index: iEta=%d iPhi=%d; XYZ=(%.0f,%.0f,%.0f)cm\n",
          id, iEta, iPhi, R.x(), R.y(), R.z());
}

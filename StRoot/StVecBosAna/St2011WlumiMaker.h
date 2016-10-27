#ifndef St2011WlumiMaker_h
#define St2011WlumiMaker_h

#include "StMaker.h"

class StMuDstMaker;
class StVecBosMaker;


/**
 * (Deprecated) This class has been inherited from the Run 9 analysis of the longitudinal asymmetry
 * and it is not used much.
 */
class St2011WlumiMaker : public StMaker
{
public:

   St2011WlumiMaker(const char *name = "2011Wlumi");
   virtual Int_t Init();
   virtual Int_t Make();
   void setHList(TObjArray *x) {HList = x;}

   void AttachWalgoMaker(StVecBosMaker *mk) { wMK = mk;}
   void AttachMuMaker(StMuDstMaker *mk) { muMK = mk;}

   virtual Int_t InitRun(int runumber);   //< Overload empty StMaker::InitRun
   virtual Int_t FinishRun(int runumber); //< Overload empty StMaker::FinishRun

   float effective_lumi;                  ///< pb^-1, effective integrated luminosity, given not all the detector is necessarily working.
   float total_lumi;                      ///< pb^-1, total integrated luminosity if the whole detector were working.

private:

   int nActiveTowers;       ///< number of towers in the run that have good status
   bool towerInfoIsCurrent; ///< whether we've computed the active fraction for this run
   int nBHT3_hardware_L0;   ///< number of L2W random accepts that pass the hardware L0 requirement.
   int nBHT3_software_L0;   ///< number of L2W random accepts that pass the software-imposed L0 requirement, hence the number of BHT3 triggers in general (prescaled)
   int nBHT3[16];           ///< number of L2W random accepts, hence the number of BHT3 triggers in general (prescaled)
   int nBx[16][120];        ///< number of randoms, broken up by bxing.

   float par_highET;        ///< cut off for W 2x2 cluster ET

   StVecBosMaker *wMK;      ///< W-algo maker with all data
   StMuDstMaker *muMK;

   TObjArray *HList;
   enum {mxHA = 120};
   TH1 *hA[mxHA];

   void initHistos();
   void sortTrigger();
   void getActiveTowers();
   void getAbortGapCounts(int angle, int *n1, int *n2);

   ClassDef(St2011WlumiMaker, 0)
};

#endif

#ifndef St2011pubWanaMaker_h
#define St2011pubWanaMaker_h


class StVecBosMaker;
class StEmcDecoder;


/**
 * Deprecated. Gathers all results from  W-analysis
 */
class St2011pubWanaMaker : public StMaker
{
private:

   float  par_highET; // cut of for W 2x2 cluster ET

   StVecBosMaker *wMK; // W-algo maker with all data
   StEmcDecoder  *mMappB;

   // histograms
   TObjArray *HList;
   enum {mxHA = 50};
   TH1  *hA[mxHA];

   void  initHistos();
   void  evalWeleTrackSign();
   void  scanCrateRate();
   void  varyCuts4backgStudy();

public:

   St2011pubWanaMaker(const char *name = "2011pubWana");
   virtual       ~St2011pubWanaMaker() {};
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t InitRun  (int runumber);
   virtual Int_t FinishRun(int runumber) {return 0;}; // Overload empty StMaker::FinishRun

   void setHList(TObjArray *x) {HList = x;}
   void AttachWalgoMaker(StVecBosMaker *mk) { wMK = mk;}

   ClassDef(St2011pubWanaMaker, 0)
};

#endif

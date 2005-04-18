// code 'as is' from Murad, used to generate AuAu200 pedestals from regualr muDst

#ifndef StAdcPedHistoMaker_HH
#define StAdcPedHistoMaker_HH

#include "StMaker.h"

#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

class TTree;
class TFile;
class TNtuple;
class StMuDstMaker;
class StEEmcDbMaker;
class EEmcGeomSimple;
class StMuTrack;
class StBemcTables;
class TH2F;
/*!
  \class StAdcPedHistoMaker
  \author M.L. Miller (MIT Software)
  StAdcPedHistoMaker is a utility maker that reads the jet tree from file.  Currently it only supports
  single file reading, but additions to multiple files and chain reading are planned.  Also,
  we are still debugging the InitTree() method, which would allow for the jet tree to be
  a "friend" of the StMuDst, and would therefore make the StMuDstMaker responsible for the reading
  from file (a great savings in effort and bookkeeping!).
  An example block of analysis code is given in the TrackAna() method.
 */
class StAdcPedHistoMaker : public StMaker
{
  TObjArray  *HList; /// output histo access point

 public:

  ///A useful typedef for the StTracks map
  //typedef map<string, StTracks*, less<string> > TrackBranchesMap;
  
  ///The constructor requires a valid instance of StMuDstMaker
  StAdcPedHistoMaker(const char* name, StMuDstMaker* uDstMaker);
  virtual ~StAdcPedHistoMaker();
    
  virtual Int_t Init();
  virtual Int_t InitRun(int runNo);
  virtual Int_t Finish();
  virtual Int_t Make();
  void  Clear( Option_t *opts = "" );
  void initHisto();
    

  //Histogram
  void SetHList(TObjArray * x){HList=x;}

  StBemcTables *myTable;

 private:
  //protected:


  void HistoAna();

  StEEmcDbMaker* mEeDb;
  StMuDstMaker* mDstMaker;
  EEmcGeomSimple* mEeGeom;

  TH1F*  hPix[EEindexMax];
  

    
  ClassDef(StAdcPedHistoMaker,1)
    };


#endif

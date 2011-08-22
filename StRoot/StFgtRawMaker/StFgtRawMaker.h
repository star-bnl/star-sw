//
//  $Id
//  $Log
// \author Anselm Vossen (avossen@indiana.edu)
//

class StFgtRawMaker : public StRTSBaseMaker
{

 protected:
  StEvent*  mEvent;
  StFgtDB*  mDb;

 public: 
  StFgtRawMaker(const char* name="FgtRaw");
  virtual ~StFgtRawMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  virtual Int_t Make();
  virtual Int_t Finish();

}

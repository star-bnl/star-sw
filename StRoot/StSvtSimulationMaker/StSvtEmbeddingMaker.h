/***************************************************************************
 *
 * $Id: StSvtEmbeddingMaker.h,v 1.9 2007/07/12 20:18:18 fisyak Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Embedding Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtEmbeddingMaker.h,v $
 * Revision 1.9  2007/07/12 20:18:18  fisyak
 * read Db by deman
 *
 * Revision 1.8  2004/07/09 00:17:45  caines
 * Code no longer kill code is things go wrong, also  by default dont do anthing if SVT not there
 *
 * Revision 1.5  2004/02/24 15:53:21  caines
 * Read all params from database
 *
 * Revision 1.4  2004/01/22 16:30:47  caines
 * Getting closer to a final simulation
 *
 * Revision 1.3  2003/11/30 20:51:48  caines
 * New version of embedding maker and make OnlSeqAdj a stand alone maker
 *
 * Revision 1.2  2003/09/10 19:47:37  perev
 * ansi corrs
 *
 * Revision 1.1  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 **************************************************************************/
#ifndef STAR_StSvtEmbeddingMaker
#define STAR_StSvtEmbeddingMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StSvtHybridData;
class StSvtData;
class StSvtHybridCollection;
class StSvtHybridPixelsD;
/*!
 *
 * \class  StSvtEmbeddingMaker
 * \author Chaloupka
 * \date   2004/07/29
 * \brief  
 *         This maker is resposible for embedding SVT response simulation into the SVT real data
 *         and/or creation of a background.
 *         !!!IMPORTANT!!
 *         This maker can run in two modes:
 *         a)TRUE EMBEDDING
 *           It takes real data and embbeds the simulation into them.
 *           It checks if SVT is present in the real data using StDAQReader->SVTPresent().
 *           If SVT is not present it deletes the simulated data and returns empty event.
 *           This is to obtain propper efficiencies.
 *           This is the DEFAULT behaviour.
 *           This behavior CAN BE OVERLOADED by setting setPlainSimEvenIfNoSVT(kTRUE). In such a case, 
 *           instead of returning empty event the simulation will proceed in the mode b)PLAIN SIMULATION.
 *         b) PLAIN SIMULATION
 *           In this mode EmbeddingMaker creates simulated background and mixes it with the data from response simulation.
 *           This is not default behaviuor - it has to be set via setDoEmbedding(kFALSE);
 *         In both modes the embedding maker adds propper amount of background noise, depending on the mode in which it runs.
 *         In plain simulation mode it adds noise to the whole detector in embedding only under simulated hits to make them more realistic.
 *         In both modes the background creation can be turned off by setBackGround(kFALSE,"any number");       
 */
class StSvtEmbeddingMaker : public StMaker
{

public:
    StSvtEmbeddingMaker(const char* name = "SvtEmbedding");
  virtual ~StSvtEmbeddingMaker();
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual Int_t InitRun(int runumber);
    
  void setBackGround(Bool_t backgr,double backgSigma);      //default is TRUE and 1.8
  void setDoEmbedding(Bool_t doIt);                       //this allows(when set to FALSE) to force the embedding maker to ignore raw data 
  void SetPedRmsPreferences(Bool_t usePixelRMS, Bool_t useHybridRMS);                    // allows to disable reading RMS from database
  void setPlainSimEvenIfNoSVT(Bool_t doIt);                    							  //and create simple background -just a plain simulation; default is TRUE
  
private:
  Int_t GetSvtData();
  void ClearMask();
  void AddRawData();
  void CreateBackground();
  void ClearOutputData();
  Int_t NoSvt();
  double  MakeGaussDev(double sigma);

  StSvtData*               mSimPixelColl;   //!
  StSvtData*               mRealDataColl;       //!
  StSvtHybridCollection*   mPedColl;            //!
  StSvtHybridCollection*   mPedRMSColl;         //!

  ///parameterss - read from database 
  double mBackGSigma;  ///default value if individiual RMS are not available 
  Bool_t mBackGrOption; //shell I generate background
  Bool_t mDoEmbedding;  ///shell I try to run embedding if posssible 
  Bool_t mPlainSimIfNoSVT; /// if true it will run plain simulation insted of embedding if there's no SVT in real data
  Bool_t mUsePixelRMS;  ///try to read individual pixel RMS from database, default TRUE
  Bool_t mUseHybridRMS; ///try to read individual hybrid RMS from database, default TRUE
	    
  Bool_t mRunningEmbedding;   /// can I realy run embedding - ie. missing DAQ maker?
  Bool_t mMask[128*240];
  
  //global variables for temporary store in the loop
  StSvtHybridPixelsD  *mCurrentPixelData;
  int mCurrentIndex;
  
  ClassDef(StSvtEmbeddingMaker,1)

};

#endif



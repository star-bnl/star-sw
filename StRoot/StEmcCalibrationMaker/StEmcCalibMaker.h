#ifndef STAR_StEmcCalibMaker
#define STAR_StEmcCalibMaker
#include "StMaker.h"
#include "TH2.h"
#include "TH1.h"
#include "StEmcCalibrationMaker.h"
#include "TString.h"
#include "TF1.h"

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000
#define MAXTOWERCHANNEL 18000

class StEmcGeom;

class StEmcCalibMaker : public StMaker 
{
  protected: 		
    StEmcCalibrationMaker       *mCalib;
    
    TH2F                        *mSpec;
    TH1F                        *mAccept;
    
    int                         mNMinTracks;
    int                         mNMaxTracks;   
    int                         mNEvents;    
    int                         mDate;
    int                         mTime;
		long                        mZDCMin;
		long                        mZDCMax;
		long                        mCTBMin;
		long                        mCTBMax;
		
    
    int                         mDetector;
    int                         mNChannel;
    float                       mRange;
        
    TString                     mFileName;        
		
		bool                        mAutoSaveDB;          
		bool                        mDebug;          
                            
  public:
     
                                StEmcCalibMaker(const char *name="EmcCalib");
   virtual                      ~StEmcCalibMaker();
   virtual    Int_t             Init();
   virtual    Int_t             Make();
   virtual    Int_t             Finish();
   virtual    void              Clear(Option_t *option=""); 
   
              bool              accept();
              void              reset() { if(mSpec) mSpec->Reset(); mNEvents = 0; mDate = 20330101;}
              void              fill(int id, float value) { if(mSpec) mSpec->Fill(id,value);}
							
							void              calcVoltages(TH1F*, char*, char*, char*);
							
         StEmcCalibrationMaker* getCalib() { return mCalib;}
         
              TH2F*             getSpec() { return mSpec;}
              TH1D*             getSpec(int id,char* name="id") { if(mSpec) return mSpec->ProjectionY(name,id,id); else return NULL;}
              
                     
              void              saveHist(char*);     
              void              loadHist(char*);
              void              addHist(char*);
              int               getNChannel() { return mNChannel;}
              int               getNEvents()  { return mNEvents;}
              int               getDate()     { return mDate;}
              int               getTime()     { return mTime;}
              int               getDetector() { return mDetector;}
							void              getMeanAndRms(TH1D*,float,float,float*,float*);
							void              getLogMeanAndRms(TH1D*,float,float,float*,float*);
							float             getTimeInterval(int,int);
							StEmcGeom*        getGeom();
							
							bool              isAutoSaveDB() { return mAutoSaveDB;}
							bool              isDebug() { return mDebug;}
              
              void              setFile(char* f)        { mFileName = f;}   
              void              setDetector(int det)    { mDetector = det; if(mDetector<3) mNChannel = 4800; else mNChannel=18000;}
              void              setMinTracks(int t)     { mNMinTracks = t;}              
              void              setMaxTracks(int t)     { mNMaxTracks = t;} 
              void              setCTBMin(int t)        { mCTBMin = t;} 
              void              setCTBMax(int t)        { mCTBMax = t;} 
              void              setZDCMin(int t)        { mZDCMin = t;} 
              void              setZDCMax(int t)        { mZDCMax = t;} 
              void              setRange(float range)   { mRange = range;}       
							
							void              setAutoSaveDB(bool a)   { mAutoSaveDB = a;}      
							void              setDebug(bool a)        { mDebug = a;}      
   ClassDef(StEmcCalibMaker, 1)  
};

#endif

#ifndef STAR_StEmcCalibMaker
#define STAR_StEmcCalibMaker

#include <TString.h>
#include <TF1.h>
#include <TH2.h>
#include <TH1.h>

#include <StMaker.h>

#include "StEmcCalibrationMaker.h"

#define MAXTRACK 10000
#define MAXBEMC 4
#define MAXCHANNEL 18000
#define MAXTOWERCHANNEL 18000

class StEmcGeom;

class StEmcCalibMaker : public StMaker {
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
    TString                     mSpecName;        
    TString                     mAcceptName;        
		
    bool                        mAutoSaveDB;          
    bool                        mDebug;          
                            
public:
     
                                StEmcCalibMaker(const Char_t *name="EmcCalib");
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
         
              TH2F*             getSpec() const { return mSpec;}
              TH1D*             getSpec(int id,const Char_t *name = "id") const {return mSpec ? mSpec->ProjectionY(name, id, id) : NULL;}
              
                     
              void              saveHist(const Char_t *filename);
              void              loadHist(const Char_t *filename);
              void              addHist(const Char_t *filename);
              int               getNChannel() const { return mNChannel;}
              int               getNEvents() const  { return mNEvents;}
              int               getDate() const     { return mDate;}
              int               getTime() const     { return mTime;}
              int               getDetector() const { return mDetector;}
	      void              getMeanAndRms(TH1D*,float,float,float*,float*);
	      void              getLogMeanAndRms(TH1D*,float,float,float*,float*);
	      float             getTimeInterval(int,int);
	      StEmcGeom*        getGeom();
							
	      bool              isAutoSaveDB() const { return mAutoSaveDB;}
	      bool              isDebug() const { return mDebug;}
              
              void              setFile(const Char_t *f)        { mFileName = f; }
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

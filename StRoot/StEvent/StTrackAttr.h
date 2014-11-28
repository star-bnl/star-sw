#ifndef __StTrackAttr_h__
#define __StTrackAttr_h__
class StTrackAttr {
 public:
  StTrackAttr(UInt_t fExtension = 0) : mFlagExtension(fExtension) {}
  virtual ~StTrackAttr() {}
  UInt_t       flagExtension()           const {return mFlagExtension; }	  
  Bool_t       isCtbMatched()    	 const {return testBit(kCtbMatched);}     
  Bool_t       isToFMatched()  	 	 const {return testBit(kToFMatched);}     
  Bool_t       isBToFMatched()   	 const {return testBit(kToFMatched);}     
  Bool_t       isBemcMatched() 	 	 const {return testBit(kBemcMatched);}    
  Bool_t       isEemcMatched() 	 	 const {return testBit(kEemcMatched);}    
  					                                          
  Bool_t       isCtbNotMatched()  	 const {return testBit(kCtbNotMatched);}  
  Bool_t       isToFNotMatched()  	 const {return testBit(kToFNotMatched);}  
  Bool_t       isBToFNotMatched() 	 const {return testBit(kToFNotMatched);}  
  Bool_t       isBemcNotMatched() 	 const {return testBit(kBemcNotMatched);} 
  Bool_t       isEemcNotMatched() 	 const {return testBit(kEemcNotMatched);} 
  
  Bool_t       isDecayTrack()  	         const {return testBit(kDecayTrack);}   
  Bool_t       isPromptTrack() 	         const {return testBit(kPromptTrack);}       
  Bool_t       isPostXTrack()            const {return testBit(kPostXTrack);} 
  Bool_t       isMembraneCrossingTrack() const {return testBit(kXMembrane);} 
  Bool_t       isShortTrack2EMC()        const {return testBit(kShortTrack2EMC);}
  Bool_t       isRejected()              const {return testBit(kRejectedTrack);}
  Bool_t       isWestTpcOnly()           const {return testBit(kWestTpcOnlyTrack);}
  Bool_t       isEastTpcOnly()           const {return testBit(kEastTpcOnlyTrack);}
  
  virtual void setCtbMatched()           {setBit(kCtbMatched);}   
  virtual void setToFMatched()  	 {setBit(kToFMatched);}   
  virtual void setBToFMatched()  	 {setBit(kToFMatched);}   
  virtual void setBemcMatched() 	 {setBit(kBemcMatched);}  
  virtual void setEemcMatched() 	 {setBit(kEemcMatched);}  
  
  virtual void setCtbNotMatched()        {setBit(kCtbNotMatched);}   
  virtual void setToFNotMatched()  	 {setBit(kToFNotMatched);}   
  virtual void setBToFNotMatched()  	 {setBit(kToFNotMatched);}   
  virtual void setBemcNotMatched() 	 {setBit(kBemcNotMatched);}  
  virtual void setEemcNotMatched() 	 {setBit(kEemcNotMatched);}  
  virtual void setDecayTrack()  	 {setBit(kDecayTrack);}   
  virtual void setPromptTrack() 	 {setBit(kPromptTrack);}       
  virtual void setPostCrossingTrack()    {setBit(kPostXTrack);} 
  virtual void setMembraneCrossingTrack(){setBit(kXMembrane);} 
  virtual void setShortTrack2EMC()       {reSetBit(kRejectedTrack); setBit(kShortTrack2EMC);}
  virtual void setRejected()             {setBit(kRejectedTrack);}
  virtual void setWestTpcOnly()          {setBit(kWestTpcOnlyTrack);}
  virtual void setEastTpcOnly()          {setBit(kEastTpcOnlyTrack);}
  virtual void setFlagExtension(UInt_t i){mFlagExtension = i;}
  //----- bit manipulation
  void         setBit(UInt_t f, Bool_t set) {(set) ? setBit(f) : reSetBit(f);}
  void         setBit(UInt_t f)             {mFlagExtension |= f; }
  void         reSetBit(UInt_t f)           {mFlagExtension &= ~(f); }
  Bool_t       testBit(UInt_t f) const      {return (Bool_t) ((mFlagExtension & f) != 0); }
  Int_t        testBits(UInt_t f) const     {return (Int_t) (mFlagExtension & f); }
  void         invertBit(UInt_t f)          {mFlagExtension ^= f; }
 protected:
  UInt_t                  mFlagExtension; // bit wise fast detector matching status
  ClassDef(StTrackAttr,1)
};
#endif /*  __StTrackAttr_h__ */

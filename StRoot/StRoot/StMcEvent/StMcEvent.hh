/***************************************************************************
 *
 * $Id: StMcEvent.hh,v 2.35 2018/03/15 22:00:35 smirnovd Exp $
 * $Log: StMcEvent.hh,v $
 * Revision 2.35  2018/03/15 22:00:35  smirnovd
 * Fix linker error by removing declared but undefined functions
 *
 * Revision 2.34  2013/03/25 23:27:13  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.33  2012/03/22 00:37:28  perev
 * Etr added
 *
 * Revision 2.32  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.31  2011/10/11 01:12:18  perev
 * Mtd added
 *
 * Revision 2.30  2011/07/20 17:35:35  perev
 * Fsc added
 *
 * Revision 2.29  2011/04/01 19:56:32  perev
 * Redundant Browse removed
 *
 * Revision 2.28  2011/01/26 19:46:24  perev
 * FPD ==> STAR Soft
 *
 * Revision 2.27  2010/06/23 20:18:30  jwebb
 * Fixed typo.  Bug had eprsCollection returning btow hits if called using
 * the const version.
 *
 * Revision 2.26  2009/07/27 18:22:40  perev
 * Supress I/O for serice words
 *
 * Revision 2.25  2009/07/24 20:26:20  perev
 * Increase version in ClassDef
 *
 * Revision 2.24  2009/07/24 19:08:06  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.23  2007/10/16 19:49:13  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.22  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.21  2005/10/03 14:08:02  fisyak
 * fix memory leak with EmcHitCollections
 *
 * Revision 2.20  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.19  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 * Revision 2.18  2005/05/27 23:37:25  calderon
 * Update for EEMC, add eprs, esmdu esdmv hits to StMcEvent.
 *
 * Revision 2.17  2005/04/18 20:11:32  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.16  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.15  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.14  2004/01/13 21:03:34  fisyak
 * Replace iostream by Stiostream.h (for icc)
 *
 * Revision 2.13  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.12  2003/10/08 20:17:55  calderon
 * -using <iostream>, std::cout, std::ostream.
 * -changes in FTPC volume Id.
 *   o Causes changes in decoding of plane().
 *   o sector() is added.
 *   o print volumeId and sector() in the operator<<.
 *
 * Revision 2.11  2003/09/02 17:58:41  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.10  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.9  2003/05/15 18:28:47  calderon
 * Added data members from modified g2t_event table:
 * Event Generator Final State Tracks, N Binary Collisions,
 * N Wounded Nucleons East and West, N Jets.
 *
 * Revision 2.8  2003/03/18 22:37:43  calderon
 * Added member mSubProcessId which is used for Pythia events.
 * Only is set from constructor from g2t_event table.
 *
 * Revision 2.7  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.6  2000/06/06 02:58:40  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.5  2000/04/17 23:01:15  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.4  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.3  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.2  1999/12/03 00:51:52  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:15  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/24 01:23:15  fisyak
 * Reduced Include Path
 *
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.2  1999/07/28 20:27:32  calderon
 * Version with SL99f libraries
 *
 * 
 **************************************************************************/
#ifndef StMcEvent_hh
#define StMcEvent_hh

#include "Stiostream.h"
#include "StMcContainers.hh" 
#include "TString.h"
#include "TDataSet.h"
#include "TBrowser.h"
class StObject;
class StMcTpcHitCollection;
class StMcFtpcHitCollection;
class StMcRichHitCollection;
class StMcCtbHitCollection;
class StMcSvtHitCollection;
class StMcSsdHitCollection;
#include "StMcEmcHitCollection.hh"
class StMcEtrHitCollection;
class StMcTofHitCollection;
class StMcBTofHitCollection;
class StMcMtdHitCollection;
class StMcPxlHitCollection;
class StMcIstHitCollection;
class StMcFgtHitCollection;
class StMcVertex;
class g2t_event_st;

class StMcEvent : public TDataSet {
    
public:
  StMcEvent();  
  StMcEvent(g2t_event_st*);
  virtual ~StMcEvent();
  virtual void Browse(TBrowser *b); 
  virtual bool IsFolder() const {return true;}; 
  int operator==(const StMcEvent&) const;
  int operator!=(const StMcEvent&) const;
protected:  
  void initToZero();
  void makeColls();
public:
  
  
  //"Get" Methods
  
  // The following stuff will be read directly from g2t_event table
  static const TString&                    cvsTag()       {return mCvsTag; }
  unsigned long          eventGeneratorEventLabel() const {return mEventGeneratorEventLabel; }		
  unsigned long                       eventNumber() const {return mEventNumber; }					
  unsigned long                         runNumber() const {return mRunNumber;}
  unsigned long                              type() const {return mType;}
  unsigned long                             zWest() const {return mZWest;}
  unsigned long                             nWest() const {return mNWest;}
  unsigned long                             zEast() const {return mZEast;}
  unsigned long                             nEast() const {return mNEast;}
  unsigned long    eventGeneratorFinalStateTracks() const {return mEvGenFSTracks;}		
  unsigned long             numberOfPrimaryTracks() const {return mPrimaryTracks;}				
  unsigned long                      subProcessId() const {return mSubProcessId;}					
  float                           impactParameter() const {return mImpactParameter; }					
  float                          phiReactionPlane() const {return mPhiReactionPlane; }					
  float                         triggerTimeOffset() const {return mTriggerTimeOffset;}					
  unsigned long                           nBinary() const {return mNBinary;}						
  unsigned long                      nWoundedEast() const {return mNWoundedEast;}					
  unsigned long                      nWoundedWest() const {return mNWoundedWest;}
  unsigned long                             nJets() const {return mNJets;}  
  StMcVertex*                       primaryVertex()       {return mPrimaryVertex;}
  const StMcVertex*                 primaryVertex() const {return mPrimaryVertex;}					
  StSPtrVecMcVertex&                     vertices()       {return mVertices;}
  const StSPtrVecMcVertex&               vertices() const {return mVertices;}					
  StSPtrVecMcTrack&                        tracks()       {return mTracks;}
  const StSPtrVecMcTrack&                  tracks() const {return mTracks;}						
  StMcTpcHitCollection*          tpcHitCollection()       {return mTpcHits;}					
  const StMcTpcHitCollection*    tpcHitCollection() const {return mTpcHits;}				
  StMcSvtHitCollection*          svtHitCollection()       {return mSvtHits;}					
  const StMcSvtHitCollection*    svtHitCollection() const {return mSvtHits;}				
  StMcSsdHitCollection*          ssdHitCollection()       {return mSsdHits;}					
  const StMcSsdHitCollection*    ssdHitCollection() const {return mSsdHits;}				
  StMcFtpcHitCollection*        ftpcHitCollection()       {return mFtpcHits;}					
  const StMcFtpcHitCollection*  ftpcHitCollection() const {return mFtpcHits;}			
  StMcRichHitCollection*        richHitCollection()       {return mRichHits;}					
  const StMcRichHitCollection*  richHitCollection() const {return mRichHits;}			
  StMcCtbHitCollection*          ctbHitCollection()       {return mCtbHits;}					
  const StMcCtbHitCollection*    ctbHitCollection() const {return mCtbHits;}				
  			        
  StMcEmcHitCollection*          emcHitCollection(const Char_t *name) {
    StMcEmcHitCollection *emcHitColl = (StMcEmcHitCollection*) Find(name);				   	
    if (! emcHitColl) {emcHitColl = new StMcEmcHitCollection(); emcHitColl->SetName(name); Add(emcHitColl);}		   
    return emcHitColl;											   	
  }													   
  const StMcEmcHitCollection*    emcHitCollection(const Char_t *name) const {
    return (const StMcEmcHitCollection*) this->Find(name);
  }
  StMcEmcHitCollection*         bemcHitCollection()       {return emcHitCollection("BemcHits");}		        
  const StMcEmcHitCollection*   bemcHitCollection() const {return emcHitCollection("BemcHits");}	   
  StMcEmcHitCollection*         bprsHitCollection()       {return emcHitCollection("BprsHits");}
  const StMcEmcHitCollection*   bprsHitCollection() const {return emcHitCollection("BprsHits");}
  StMcEmcHitCollection*        bsmdeHitCollection()       {return emcHitCollection("BsmdeHits");}
  const StMcEmcHitCollection*  bsmdeHitCollection() const {return emcHitCollection("BsmdeHits");}
  StMcEmcHitCollection*        bsmdpHitCollection()       {return emcHitCollection("BsmdpHits");}
  const StMcEmcHitCollection*  bsmdpHitCollection() const {return emcHitCollection("BsmdpHits");}
  			      
  StMcTofHitCollection*          tofHitCollection()       {return mTofHits;}		
  const StMcTofHitCollection*    tofHitCollection() const {return mTofHits;}
  StMcBTofHitCollection*         btofHitCollection()       {return mBTofHits;}		
  const StMcBTofHitCollection*   btofHitCollection() const {return mBTofHits;}
  StMcMtdHitCollection*          mtdHitCollection()        {return mMtdHits;}           
  const StMcMtdHitCollection*    mtdHitCollection() const  {return mMtdHits;}
 			      
  StMcEmcHitCollection*         eemcHitCollection()       {return emcHitCollection("EemcHits");}
  const StMcEmcHitCollection*   eemcHitCollection() const {return emcHitCollection("EemcHits");}
  StMcEmcHitCollection*         eprsHitCollection()       {return emcHitCollection("EprsHits");}
  const StMcEmcHitCollection*   eprsHitCollection() const {return emcHitCollection("EprsHits");}
  StMcEmcHitCollection*        esmduHitCollection()       {return emcHitCollection("EsmduHits");}
  const StMcEmcHitCollection*  esmduHitCollection() const {return emcHitCollection("EsmduHits");}
  StMcEmcHitCollection*        esmdvHitCollection()       {return emcHitCollection("EsmdvHits");}
  const StMcEmcHitCollection*  esmdvHitCollection() const {return emcHitCollection("EsmdvHits");}

  StMcEtrHitCollection*          etrHitCollection()       { return mEtrHits; }
  const StMcEtrHitCollection*    etrHitCollection() const { return mEtrHits; }

  StMcEmcHitCollection*          fpdHitCollection()       { return emcHitCollection("FpdHits"); }
  const StMcEmcHitCollection*    fpdHitCollection() const { return emcHitCollection("FpdHits"); }

  StMcEmcHitCollection*          fscHitCollection()       { return emcHitCollection("FscHits"); }
  const StMcEmcHitCollection*    fscHitCollection() const { return emcHitCollection("FscHits"); }
  
  StMcPxlHitCollection*      pxlHitCollection()       {return mPxlHits;}				
  const StMcPxlHitCollection*pxlHitCollection() const {return mPxlHits;}		
  StMcIstHitCollection*          istHitCollection()       {return mIstHits;}				
  const StMcIstHitCollection*    istHitCollection() const {return mIstHits;}	  
  StMcFgtHitCollection*          fgtHitCollection()       {return mFgtHits;}		         
  const StMcFgtHitCollection*    fgtHitCollection() const {return mFgtHits;}
  
  // "Set" Methods
  
  void setEventGeneratorEventLabel(unsigned long);
  void setEventNumber(unsigned long);
  void setRunNumber(unsigned long);
  void setType(unsigned long);
  void setZWest(unsigned long);
  void setNWest(unsigned long);
  void setZEast(unsigned long);
  void setNEast(unsigned long);
  void setEventGeneratorFinalStateTracks(unsigned long);
  void setNumberOfPrimaryTracks(unsigned long);
  void setImpactParameter(float);
  void setPhiReactionPlane(float);
  void setTriggerTimeOffset(float);
  void setNBinary(unsigned long);
  void setNWoundedEast(unsigned long);
  void setNWoundedWest(unsigned long);
  void setNJets(unsigned long);
  void setPrimaryVertex(StMcVertex*);  
  void setTpcHitCollection(StMcTpcHitCollection*);               
  void setSvtHitCollection(StMcSvtHitCollection*);               
  void setSsdHitCollection(StMcSsdHitCollection*);               
  void setFtpcHitCollection(StMcFtpcHitCollection*);              
  void setRichHitCollection(StMcRichHitCollection*);
  void setTofHitCollection(StMcTofHitCollection*);
  void setBTofHitCollection(StMcBTofHitCollection*);
  void setMtdHitCollection(StMcMtdHitCollection*);
  void setPxlHitCollection(StMcPxlHitCollection*);       
  void setIstHitCollection(StMcIstHitCollection*); 
  void setFgtHitCollection(StMcFgtHitCollection*);       
  virtual void Print(Option_t *option="") const; // *MENU* 
 protected:
  char mBeg[1];				//!
  unsigned long                  mEventGeneratorEventLabel;
  unsigned long                  mEventNumber;
  unsigned long                  mRunNumber;
  unsigned long                  mType;  
  unsigned long                  mZWest;
  unsigned long                  mNWest;
  unsigned long                  mZEast;
  unsigned long                  mNEast;
  unsigned long                  mEvGenFSTracks; // Number of final state event generator tracks
  unsigned long                  mPrimaryTracks;
  unsigned long                  mSubProcessId; // Pythia subprocess Id
  float                          mImpactParameter;
  float                          mPhiReactionPlane;
  float                          mTriggerTimeOffset; // time offset wrt trigger event
  unsigned long                  mNBinary;           // Number of Binary Collisions
  unsigned long                  mNWoundedEast;      // Number of Wounded Nucleons East
  unsigned long                  mNWoundedWest;      // Number of Wounded Nucleons West
  unsigned long                  mNJets;             // Number of Jets
  StMcVertex*                    mPrimaryVertex;
  StObject*                      mBegColl[1];		//!
  StMcTpcHitCollection*          mTpcHits;
  StMcSvtHitCollection*          mSvtHits;
  StMcSsdHitCollection*          mSsdHits;
  StMcFtpcHitCollection*         mFtpcHits;
  StMcRichHitCollection*         mRichHits;
  StMcCtbHitCollection*          mCtbHits;
  StMcTofHitCollection*          mTofHits;
  StMcBTofHitCollection*         mBTofHits;
  StMcMtdHitCollection*          mMtdHits;
  StMcPxlHitCollection*          mPxlHits;
  StMcIstHitCollection*          mIstHits;
  StMcFgtHitCollection*          mFgtHits;
  StMcEtrHitCollection*          mEtrHits;
  StObject*                      mEndColl[1];	//!
  char                           mEnd[1];	//!
  StSPtrVecMcVertex              mVertices;
  StSPtrVecMcTrack               mTracks;
  static TString                 mCvsTag;
 protected:
  const StMcEvent& operator=(const StMcEvent&);
  StMcEvent(const StMcEvent&);
  ClassDef(StMcEvent,3)
};

ostream&  operator<<(ostream& os, const StMcEvent&);

// Definition of "Get" methods

#endif


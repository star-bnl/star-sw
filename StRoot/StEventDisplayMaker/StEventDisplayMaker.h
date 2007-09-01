//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StEventDisplayMaker.h,v 1.44 2007/09/01 01:42:23 fine Exp $
// $Log: StEventDisplayMaker.h,v $
// Revision 1.44  2007/09/01 01:42:23  fine
// Quick fix to allow selection one event by event id
//
// Revision 1.43  2007/03/19 00:40:36  fine
// Complete the direct rendering and Emc Towers
//
// Revision 1.42  2007/03/15 16:27:30  fine
//  Allow user to select the arbitrary StEvent object to be drawn with StEventDisplay
//
// Revision 1.41  2007/02/22 03:51:06  fine
// Rescan event if empty
//
// Revision 1.40  2007/02/21 19:16:14  fine
// Add an extra control to choose between G3 and Sti detectors geometries
//
// Revision 1.39  2007/02/01 22:40:08  fine
// Add TROOT header file
//
// Revision 1.38  2006/10/09 20:33:46  fine
// Fix to make it work under ROOT 4.04 and ROOT 5.12
//
// Revision 1.37  2006/08/24 19:03:43  fine
// Add the fake the Emc tower dataprovider to test
//
// Revision 1.36  2004/11/16 04:33:05  perev
// Bug fix. Check for same value of StEvent pointer removed
//
// Revision 1.35  2004/10/07 19:41:23  perev
// Tuning
//
// Revision 1.34  2004/09/28 03:55:23  perev
// Global filter introduced
//
// Revision 1.33  2003/09/02 17:58:08  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.32  2003/01/26 23:54:36  fine
// Add RStiostream.h header lost due removing the redundant headers files from StEvent package. Redundant iostream was removed
//
// Revision 1.31  2003/01/26 17:23:03  jeromel
// Missing iostream
//
// Revision 1.30  2003/01/18 01:35:06  fine
// add EMC
//
// Revision 1.29  2003/01/17 01:36:16  fine
// working version of Qt-based StEventDisplayMaker class
//
// Revision 1.28  2002/12/13 00:47:41  fine
// first version with Qt interface
//
// Revision 1.27  2001/09/26 23:26:56  perev
// Sorting by color added
//
// Revision 1.26  2001/09/01 19:51:24  perev
// StEvent added
//
// Revision 1.24  2001/02/14 16:52:09  perev
// include file simplyfied
//
// Revision 1.23  2000/09/25 01:29:53  fine
// new StFtpcTrackFilter for Janet has been introdcued
//
// Revision 1.22  2000/08/29 19:26:00  fine
// New method to add/remove volumes and tables
//
// Revision 1.21  2000/08/29 04:39:25  fine
// RemoveName method introduced
//
// Revision 1.20  2000/08/27 16:55:13  fine
// Title with Run event number etc
//
// Revision 1.19  2000/08/26 03:14:48  fine
// New default filter from M.Panebratcev has been introduced
//
// Revision 1.18  2000/04/05 03:58:21  fine
// Adjusted for ROOT 2.24
//
//
#ifndef STAR_StEventDisplayMaker
#define STAR_StEventDisplayMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDisplayMaker base class                                         //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TSeqCollection.h"
#include "TObjString.h"
#if 0
#include "TRootHelpDialog.h"
#endif

#include "StMaker.h"
#include "StDefaultFilter.h"
#include "TROOT.h"
class    TVolume;
class    TVolumeView;
class    TTable;

class TH2F;
class TCanvas;
class StVirtualEventFilter;
class StTrackChair;
class TVirtualPad;
class TPaveLabel;
class StEventHelper;
class StFilterABC;
class StGlobalFilterABC;
class StPoints3DABC;
class StEventDisplayInfo;
class TQtRootViewer3D;
class TCoinEmcTowers;
class TDataProvider;

class StEventDisplayMaker : public StMaker {
 private:
// static char    m_VersionCVS = "$Id: StEventDisplayMaker.h,v 1.44 2007/09/01 01:42:23 fine Exp $";

 private: 
 enum {kCOLORS=20};

static Int_t fgEventLoop;
static StEventDisplayInfo *fgInfo;

    TList         *m_HitCollector;     	//!
    TList         *m_TrackCollector;   	//!
    TList         *m_TableCollector;   	//!

 protected:
    Int_t         mRedraw;
    TVolume      *m_Hall;         	//!
    TVolumeView  *m_FullView;     	//!
    TVolumeView  *m_ShortView;    	//!
    TVolumeView  *m_Sensible;     	//!
    TVolume      *m_EventsNode;   	//!
    TVolumeView  *m_EventsView;   	//!
    TList        *m_ListDataSetNames; 	// The list of the names to be drawn
    TList        *m_VolumeList;   	// The list of the names of TVolume object
    TTable       *m_Table;        	//! The table to be drawn if any
    TObjArray    *m_FilterArray;  	// Array of the "event" user supplied filters
    StEventHelper *mEventHelper;	//!
    TCanvas      *m_PadBrowserCanvas; 	//!
    TPaveLabel   *mRunNumberLabel;    	//!
    TPaveLabel   *mEventNumberLabel;  	//!
    TPaveLabel   *mDateTimeLabel;     	//!

    TList*        mFilterList;		//! list of filters for StEvent

    TVolume *fColCash[kCOLORS];
    TQtRootViewer3D *f3DViewer; //! external 3D viewer;
    Bool_t        fNeedsClear3DView;
    Int_t         fGeomType;  // Flag to define where the geom comes from G3/ STI
    Bool_t        fEventNeedRescan; // Should we rescan the event
    Bool_t        fDrawMarkedObject; // Should we render the marked StEvent items only
    Bool_t        fCoin3DReady; // Coin3d-based viewer was loaded
    TCoinEmcTowers *fEmcTowers;  // Emc barrel geometry
    TDataProvider  *fColorProvider;
    TDataProvider  *fSizeProvider;
    Int_t         fEventIdToRender; // == 0 menas, show all events
    void          DrawObject(TObject *,Option_t *option="",Bool_t first=kFALSE);
    Int_t         MakeTable(const char   **positions);
    Int_t         MakeTableHits(const TTable *points,StVirtualEventFilter *filter,const char   *keyColumn,const char   *keyPositions[]);
    void          MakeEmcTowers();
    static Int_t  ParseName(char   *inName, char   *position[]);

 public: 
                  StEventDisplayMaker(const char *name="EventDisplay");
   virtual       ~StEventDisplayMaker();
   virtual void   AddName(const char *name,Bool_t refresh=kTRUE);   // *MENU*
   virtual TList *GetNameList()   { return m_ListDataSetNames;}
           void   AddVolume(const char *name); // *MENU*
   virtual const  TList *GetVolumeNameList()   { return m_VolumeList;}
   virtual void   AddFilter(StFilterABC*       filt); 
   virtual Int_t  BuildGeometry();
   virtual void   ClearGeometry();
   virtual Int_t  Init();
   virtual Int_t  Make();
           Int_t  Redraw();
   virtual Int_t  MakeTableTracks(const StTrackChair *points,StVirtualEventFilter *filter);
   virtual Int_t  MakeEvent(const TObject *event,const char** positions);
           void   DrawIt(StPoints3DABC *pnt,const char *opt,Color_t col,Style_t sty,Size_t siz);
   void Set3DViewer( TQtRootViewer3D *viewer) {  f3DViewer = viewer;}
   TQtRootViewer3D  *Get3DViewer() const { return f3DViewer; }

   virtual void   Clear(Option_t *option="");
   virtual void   ClearCanvas(); // *MENU*
   virtual void   ClearEvents();
   virtual TVirtualPad *CreateCanvas();
           TVirtualPad *GetEventPad();
   virtual Int_t  CreateTrackNodes();
   virtual TVolume *GetHall()           const { return m_Hall; }
   virtual TVolumeView *GetFullView()   const { return m_FullView;  }
   virtual TVolumeView *GetShortView()  const { return m_ShortView; }
   virtual TVolumeView *GetSensible()   const { return m_Sensible;  }
   virtual TVolume     *GetEventsNode() const { return m_EventsNode;}
   virtual Int_t        GeomType()      const  { return fGeomType;}
   virtual Color_t      GetColorAttribute(Int_t adc);
           Bool_t       IsMarkedDrawn()  const;
   virtual void         PrintFilterStatus(); // *MENU*
   virtual void         PrintNames();   // *MENU*
   virtual void         PrintVolumes(); // *MENU*
   virtual void         SetMode       (Int_t   m = 0){StMaker::SetMode(m);} // *MENU*
   virtual Int_t        ReDraw(){ClearCanvas(); return Make();} // *MENU*
   virtual void         RemoveName(const char *name); // *MENU*
   virtual void         RemoveVolume(const char *name); // *MENU*
           void         SetGeomType(Int_t type=0) { fGeomType = type;} // *MENU*
           void         SetDrawMarked(Bool_t draw=kTRUE); // *MENU* 
   virtual void         TurnOn() { SetMode(); }  // *MENU*
   virtual void         TurnOff(){ SetMode(1); } // *MENU*
   static  Int_t        MakeLoop(Int_t flag);
   static  void         MakeInfo(const char*info);
     // --   Filters  --
     void SetEventIdToRender(Int_t eventid) { fEventIdToRender=eventid ; }
     Int_t EventIdToRender() const  { return fEventIdToRender; }
     enum EDisplayEvents 
          {
            kPrimaryVertex  ,kTpcHit      ,kSvtHit      ,kFtpcHit      ,kEmcTowerHit,
            kEmcPreShowerHit,kSmdPhiHit   ,kSmdEtaHit   ,kVertices     ,kGlobalTracks ,
            kTrack          ,kTrackTpcHits,kTrackSvtHits,kTrackFtpcHits, kTable     , 
            kTptTrack       ,
            kEndOfEventList
          } ;

 
     // --   Filters  --
 
     Int_t SetFlag(Int_t flag, EDisplayEvents filterIndex);
     StVirtualEventFilter *SetFilter(StVirtualEventFilter *filter, EDisplayEvents filterIndex);

     //  -- Table filter ---
     Int_t SetTableFlag(Int_t flag=1); // *MENU*
     StVirtualEventFilter *SetTable(StVirtualEventFilter *filter);

    //  -- TPT track table filter ---
     Int_t SetTptTrackFlag(Int_t flag=1); // *MENU*
     StVirtualEventFilter *SetTptTrack(StVirtualEventFilter *filter);
   // --  end of filter list --

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventDisplayMaker.h,v 1.44 2007/09/01 01:42:23 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StEventDisplayMaker, 0)   //
 private:
   static StDefaultFilter m_DefaultFilters[kEndOfEventList];
};

//______________________________________________________________________________
inline TVirtualPad *StEventDisplayMaker::GetEventPad()
{
  // Protect this class from the crash if user deleted canvas accidently
  if (m_PadBrowserCanvas) {
    TSeqCollection   *cList = gROOT->GetListOfCanvases();
    if ( !cList || !cList->FindObject((TObject *)m_PadBrowserCanvas)) 
      m_PadBrowserCanvas = 0;
  }
  return (TVirtualPad *)m_PadBrowserCanvas;
};

//______________________________________________________________________________
inline void  StEventDisplayMaker::SetDrawMarked(Bool_t draw) 
{  fDrawMarkedObject = draw ;}

//______________________________________________________________________________
inline Bool_t  StEventDisplayMaker::IsMarkedDrawn()  const
{ return  fDrawMarkedObject ;}









#endif

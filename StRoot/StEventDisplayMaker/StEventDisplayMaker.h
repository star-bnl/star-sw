//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
//   
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
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif


class    St_Node;
class    St_NodeView;

class TH2F;
class TCanvas;
class StEvent;
class StVecPtrTpcHit;
class StGlobalTrack;

class StEventDisplayMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StEventDisplayMaker.h,v 1.1 1999/07/14 01:46:16 fine Exp $";
 private: 
    TList         *m_HitCollector;     //!
    TList         *m_TrackCollector;   //!

 protected:
    St_Node      *m_Hall;         //!
    St_NodeView  *m_FullView;     //!
    St_NodeView  *m_ShortView;    //!
    St_NodeView  *m_Sensible;     //!
    St_Node      *m_EventsNode;   //!
    St_NodeView  *m_EventsView;   //!
    StEvent      *m_Event;        //!
    Int_t         m_TrackFilterFlag; // Flag to control whether tracks should be drawn
    Int_t         m_HitFilterFlag;   // Flag to control whether hits should be drawn

    TCanvas      *m_PadBrowserCanvas; //!

 public: 
                  StEventDisplayMaker(const char *name="EventDisplay");
   virtual       ~StEventDisplayMaker();
   virtual Int_t  BuildGeometry();
   virtual Int_t  Init();
   virtual Int_t  Make();

   virtual void   Clear(Option_t *option);
   virtual void   ClearEvents();
   virtual Int_t  CreateCanvas();
   virtual Int_t  CreateTrackNodes();
   virtual St_Node *GetHall()          { return m_Hall; }
   virtual St_NodeView *GetFullView()  { return m_FullView;  }
   virtual St_NodeView *GetShortView() { return m_ShortView; }
   virtual St_NodeView *GetSensible()  { return m_Sensible;  }
   virtual St_Node     *GetEventsNode(){ return m_EventsNode;}
   virtual Color_t      GetColorAttribute(Int_t adc);
   virtual void         SetMode       (Int_t   m = 0){StMaker::SetMode(m);} // *MENU*
   virtual Int_t        GetTrackFilterFlag(){ return m_TrackFilterFlag;}
   virtual Int_t        GetHitFilterFlag(){ return m_HitFilterFlag;}
   virtual Int_t        SetTrackFilterFlag(Int_t flag=1){Int_t f = m_TrackFilterFlag; m_TrackFilterFlag=flag; return f;} // *MENU*
   virtual Int_t        SetHitFilterFlag(Int_t flag=1)  {Int_t f = m_HitFilterFlag; m_HitFilterFlag=flag; return f;}  // *MENU*

   virtual Int_t        HitsFilter(const StVecPtrTpcHit &hitPoints);
   virtual Int_t        GlobalTrackFilter(StGlobalTrack *globTrack);
   virtual Int_t        TrackFilter(StGlobalTrack *globTrack);

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StEventDisplayMaker.h,v 1.1 1999/07/14 01:46:16 fine Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StEventDisplayMaker, 0)   //
};

#endif

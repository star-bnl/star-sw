// $Id: StFtpcTrackMaker.h,v 1.2 2000/07/03 12:45:23 jcs Exp $
// $Log: StFtpcTrackMaker.h,v $
// Revision 1.2  2000/07/03 12:45:23  jcs
// get (pre)Vertex coordinates directly from (pre)Vertex table instead of from
// fptpars
//
// Revision 1.1  2000/05/10 13:39:30  oldi
// Initial version of StFtpcTrackMaker
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcTrackMaker virtual base class for Maker                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcTrackMaker
#define STAR_StFtpcTrackMaker

#include "StMaker.h"

class TH1F;
class TH2F;
class TProfile;
class St_fde_fdepar;
 
class StFtpcTrackMaker : public StMaker {

 private:
  virtual const char *GetCVS()
  {static const char cvs[]="Tag $Name:  $ $Id: StFtpcTrackMaker.h,v 1.2 2000/07/03 12:45:23 jcs Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  protected:
       TH1F          *m_q;          //! charge
       TH1F          *m_theta;      //! theta
       TH1F          *m_ndedx;      //! # points used in de/dx calulation
       TH1F          *m_found;      //! # points found per track
       TH1F          *m_track;      //! # tracks found   
       TH2F          *m_nrec_track; //! # points found per track vs. # tracks found

   St_fde_fdepar   *m_fdepar;   //!

 public: 
                  StFtpcTrackMaker(const char *name="ftpc_tracks"); // constructor
   virtual       ~StFtpcTrackMaker();                               // destructor
   virtual Int_t  Init();                                           // Initialisation 
   virtual Int_t  Make();                                           // actual program
   virtual void   PrintInfo();                                      // prints information
           void   MakeHistograms();                                 // makes histograms

   ClassDef(StFtpcTrackMaker, 1)   //StAF chain virtual base class for Makers
};

#endif

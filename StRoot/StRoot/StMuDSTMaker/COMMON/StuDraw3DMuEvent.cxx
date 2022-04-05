// $Id: StuDraw3DMuEvent.cxx,v 1.17 2010/04/02 22:34:09 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DMuEvent.h"
#include "Gtypes.h"
#include "StHelixHelper.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StThreeVector.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

      
//! StuDraw3DMuEvent( const char *detectorName,TVirtualPad *pad) ctor
/*!
         \param detectorName (default = "TPC") - the names of the STAR detectors 
                                                 to be used as the "event primitives" background.
                               The detectorName is a comma separated list of the OpenInventor files 
                               with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                                     \code   <name>.iv \endcode   
         \param detectorName = 0  - no detector geometry is to be rendered
         \param pad (default = 0) - The ROOT TPad to be used to render the event wireframe view
\htmlonly
<table>
<tr>
<th>Event over detector geometry
<th>Event with no detector geometry
</tr>
<tr>
<td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWGeom.png">
<td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWOGeom.png">
</tr></table>
\endhtmlonly
\note 
   If this is the first instance of the class then the global pointer to the 
         current "display" is to be set too \sa Display()
         \sa StRoot/macros/mudst/draw3DTracks.C
*/
//___________________________________________________
StuDraw3DMuEvent::StuDraw3DMuEvent( const char *detectorName,TVirtualPad *pad): 
StDraw3D(detectorName,pad),fEndcapGeom(0)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (!gMuEventDisplay) gMuEventDisplay = this;
}

//! ~StuDraw3DMuEvent( ) dtor
/*! 
    Reset the global  \c gMuEventDisplay pointer to the current display if the current display is \c this 
*/
//___________________________________________________
StuDraw3DMuEvent::~StuDraw3DMuEvent()
{
   if (gMuEventDisplay == this) gMuEventDisplay = 0;
   delete fEndcapGeom; fEndcapGeom=0;
}

//! Add all tracks of the given \a type from the  current event to the display list.
/*! the \c style and \c size vizual attributes are defined by 
    the StTrackType \c global  / \c primary styles. \n
    The \c color is defined by the track \c pt 
    \code double pt =  double pt =track.pt(); \endcode
*/
//___________________________________________________
void StuDraw3DMuEvent::Tracks(StTrackType type)
{
   Int_t n_prim=0;
   Int_t n_glob=0;
   TObjArray *globTracks = 0;
   TObjArray *primTracks = 0;
   if (type == global && ( globTracks= StMuDst::globalTracks() ) ){
      n_glob=StMuDst::GetNGlobalTrack();
   } else if ( (primTracks = StMuDst::primaryTracks()) ) {
      n_prim=StMuDst::GetNPrimaryTrack();
   }

   Int_t i_track=0;
   while (i_track < n_prim) {
       StMuTrack &track = *(StMuTrack *)primTracks->UncheckedAt(i_track++);
       double pt =track.pt();
       Style_t sty = Style(kPrimaryTrack).Sty();
       Size_t  siz = Style(kPrimaryTrack).Siz();
       Track(track,StDraw3DStyle::Pt2Color(pt),sty,siz);
       cout << ".";
   }
   i_track=0;
   while (i_track < n_glob) {
       StMuTrack &track = *(StMuTrack *)globTracks->UncheckedAt(i_track++);
       double pt =track.pt();
       Style_t sty = Style(kGlobalTrack).Sty();
       Size_t  siz = Style(kGlobalTrack).Siz();
       Track(track,StDraw3DStyle::Pt2Color(pt),sty,siz);
       cout << "+";
   }
   cout << endl << n_prim << " primary and " << n_glob << " global tracks have been rendered" << endl;

}


//! Add \a track to the display list with the \a col color, \a sty style, and \a siz size if provided
/*! 
   \param track - reference to the StMuTrack object from StMuDst data structure
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TObject of \a track model
*/
//___________________________________________________
TObject *StuDraw3DMuEvent::Track(const StMuTrack &track, Color_t col,Style_t sty,Size_t siz)
{

   StHelixHelper trPnt(track.helix(),track.outerHelix(),track.length());
   Int_t size;
   Float_t *xyz = trPnt.GetPoints(size);
   TObject *l = Line(size,xyz,col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a track to the display list with the \a sty pre-defined style if provided 
 */
//___________________________________________________
TObject *StuDraw3DMuEvent::Track(const StMuTrack &track, EDraw3DStyle sty)
{
   TObject *view = 0;
   const StDraw3DStyle &style =  Style(sty);
   const StMuTrack *thisTrack = 0;
   if (sty == kPrimaryTrack) 
       thisTrack = track.primaryTrack();
   else
      thisTrack = track.globalTrack();
   if (thisTrack) 
       view = Track(*thisTrack, style.Col(),style.Sty(),style.Siz() );
   return view;

}

//! Add  the \a in  point of the given \a track to the display list with the \a col color, \a sty style, and \a siz size if provided
/*! 
   \param track - reference to the StMuTrack object from StMuDst data structure
   \param in      flag \c true (default) is to be set to add the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \return - a pointer to the ROOT "view" TObject of star/end point of \a track model
*/
//___________________________________________________
TObject *StuDraw3DMuEvent::TrackInOut(const StMuTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // to be completed yet
   const StThreeVectorF  &pnt = in ? track.firstPoint() : track.lastPoint();

   return Point(pnt.x(),pnt.y(),pnt.z(), col,sty,siz);
}

//! This is an overloaded member function, provided for convenience.
/*! 
   \param track - reference to the StMuTrack object from StMuDst data structure
   \param in    - flag \c true (default) is to be set to add the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   sty - EDraw3DStyle pre-defined visual style of this object.
   \return - a pointer to the ROOT "view" TObject of star/end point of \a track model
*/
//___________________________________________________
TObject *StuDraw3DMuEvent::TrackInOut(const StMuTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
} 

//___________________________________________________
EEmcGeomSimple *StuDraw3DMuEvent::EndcapGeom()
{
   // endcap tower geometry:
   // see http://drupal.star.bnl.gov/STAR/subsys/eemc/endcap-calorimeter/db-usage
   if  (!fEndcapGeom) fEndcapGeom = new EEmcGeomSimple();
   return fEndcapGeom; 
}
//___________________________________________________
//! Add the endcap towers to the list to display 
/*!
  \param   sty - is the ROOT TAttFill style: \n
   =0 - solid view,\n 
   =4001 - "wireframe" view,\n 
   =4002-4100 - solid semitransparent view
  \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.C.Endcap.tracks.png "The first event  from Run 9046031 pProduction2008 produced by EdMu.C macros"
*/
//___________________________________________________
void   StuDraw3DMuEvent::Endcaps(Style_t sty)
{
   StMuEmcCollection *emc= StMuDst::muEmcCollection();
   if (emc)  Endcaps(*emc,sty);
}


//! This is an overloaded member function, provided for convenience.
/*!
  \param emc - the reference to the collection (singletone) of the emc properties.
  \param   sty - is the ROOT TAttFill style: \n
   =0 - solid view,\n 
   =4001 - "wireframe" view,\n 
   =4002-4100 - solid semitransparent view\n
 \htmlonly
 <table>
 <tr>
 <th colspan=2>Endcap towers and tpc tracks rendering example
</tr>
 <tr>
 <th>XY plane view
 <th>ZX plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.XY.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.ZX.png" width=340px></center>
 </tr>
 <tr>
 <th>YZ plane view
 <th>3D plane view
 </tr>
 <tr>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.YZ.png" width=340px></center>
 <td><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.XYZ.png" width=340px></center>
 </tr> 
 <tr><td colspan=2><center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.C.Endcap.tracks.in.mag.png"  width=100%></center>
 </tr></table>
 \endhtmlonly 
*/
//___________________________________________________
void   StuDraw3DMuEvent::Endcaps(const StMuEmcCollection &emc,Style_t sty)
{
  Color_t colorResponce = 0;
  int nTowers = emc.getNEndcapTowerADC();
  for (int i=0; i< nTowers; i++) { 
    int adc,  isec, isub, ieta;
    emc.getEndcapTowerADC(i,adc,isec,isub,ieta);
    isec--;    ieta--;    isub--;
    if (adc<=0) continue; // print only non-zero values
    // access geometry info
    float etaCenter     =EndcapGeom()->getEtaMean(ieta);
    if (etaCenter <= 0) continue;
    float phiCenter     =EndcapGeom()->getPhiMean(isec,isub);
    if (phiCenter <= 0) continue;
    static const float dPhi = 2*EndcapGeom()->getPhiHalfWidth(isec,isub);
    static const float deta = 2*EndcapGeom()->getEtaHalfWidth(ieta);
    static const float radius = 270.; // no idea where I should pick it from.
    float energy = adc*60./4096-0.15;
    if ( energy > 0.15 ) {
    
       // If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
       if (  energy  < 0.3) {         colorResponce = kBlue; 
          // If edep large then MIP but less then 1 GeV 
       } else if (  energy  < 1.0 ) { colorResponce = kGreen;
           // If between 1 GeV and lowest HT threshold (4 GeV for Run7)
       } else if (  energy  < 4.0 ) { colorResponce = kYellow;
           // If above lowest HT thershold
       } else                         colorResponce = kRed;
	
       static const double maxSize =  400.; // (cm)
       static const double scale   =   15; // 120.; // (cm/Gev)
       double size =(energy > 0.3 ? scale : scale/30.)*energy;
       if (size > maxSize)  size = maxSize ;
       Tower( radius
                , StarRoot::StEta(etaCenter,deta)
                , phiCenter, dPhi
                , colorResponce
                , sty
                , size);
        SetComment(Form("Endcap eta=%f, phi=%f, energy=%f",etaCenter,phiCenter,energy));
     }
  }
}
//____________________________________________________________________________________
//! Add EMC hit defined \a emcHitsSoftId to the display list with the \a col color \a sty and \a size if provided
/*! 
   \param   emcHitsSoftId - StEmcRawHit soft ID that one wants to be present as the ROOT TTRAP 
                    object with ROOT visual attributes \a col \a sty \a siz
   \param   col - Tower color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   sty - Tower style ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   siz - Tower size (cm) ( \sa StDraw3D::Tower( float radius, const StarRoot::StEta &eta,float phi,float dphi, Color_t col,Style_t sty, Size_t siz) )
   \return - a pointer to the ROOT "view" TObject of \a emcHit model
*/
//____________________________________________________________________________________
TObject *StuDraw3DMuEvent::EmcHit(Int_t emcHitsSoftId, Color_t col,Style_t sty,Size_t siz, const char *detId)
{  
   TObject *model = 0;
   StEmcGeom *emcGeom =StEmcGeom::getEmcGeom(detId);
   if (emcGeom) {
      Int_t softId=emcHitsSoftId;
      Float_t eta;
      Float_t phi;
      emcGeom->getEtaPhi(softId,eta,phi);
      Float_t etaStep = 1.0/emcGeom->NEta();
      Float_t phiStep = TMath::Pi()/60; 
      static int entries = 0;
        //if (entries) return 0;
        // printf(" m=%d, e=%d, s=%d; eta=%e deta=%e phi=%e dphi=%e id %d\n",m, e, s,eta,etaStep ,phi, phiStep, softId);
      entries++;
      model  = Tower(emcGeom->Radius(), StarRoot::StEta(eta,etaStep)
                         , phi, phiStep
                         , col,sty+(strcmp(detId,"bemc")?0:kBarrelStyle),siz);
   } else {
      LOG_ERROR <<  __FILE__ << ":  there is no geometry information for \"" << detId << "\"" << endm;
   }
   return model;
}

//____________________________________________________________________________________
TObject *StuDraw3DMuEvent::EmcHit(Int_t emcHitsSoftId, float energy, const char *detIdt)
{
   return EmcHit<StEmcTowerColor>(emcHitsSoftId, energy, detIdt);
}


/*! \return The pointer to the current instance of the StuDraw3DMuEvent  class 
   to visualize StMuDst components
*/
//___________________________________________________
StuDraw3DMuEvent *StuDraw3DMuEvent::Display(){ return gMuEventDisplay;}

//___________________________________________________
StEmcTowerColor::StEmcTowerColor(float energy)
{
   Color_t colorResponce=0;
   Size_t  size=0;
   if (energy > 0  && energy < 30) {
       // If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
      if (  energy  < 0.3)   {   
           colorResponce = kBlue; 
                // style = 4001;                 //wireframe 
               // If edep large then MIP but less then 1 GeV 
      } else if (  energy  < 1.0 ) colorResponce = kGreen;
               // If between 1 GeV and lowest HT threshold (4 GeV for Run7)
      else if (  energy  < 4.0 )   colorResponce = kYellow;
               // If above lowest HT thershold
      else                         colorResponce = kRed;

      static const double maxSize =  400.; // (cm)
      static const double scale   =  200.; // (cm/Gev)
      size =(energy > 0.3 ? scale : scale/30.)*energy;
      if (size > maxSize)  size = maxSize ;
      fStyle.SetCol(colorResponce);
      fStyle.SetSiz(size);
  }
}

StuDraw3DMuEvent *StuDraw3DMuEvent::gMuEventDisplay = new StuDraw3DMuEvent();

ClassImp(StuDraw3DMuEvent)
      

      

// $Id: EdMu.C,v 1.18 2010/04/02 22:52:31 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009

//  Additions to use BEMC towers (for display of upsilon events) by Manuel Calderon de la Barca. Feb 2010.

class StMuEmcHit;
class StMuDst;
StMuDst* muDst = 0; //< The pointer to the current instanse fo the StMuDst object
class StMuEvent;
StMuEvent* muEvent = 0; //< The pointer to the \a StMuEvent object 
class StMuDstMaker;
StMuDstMaker* muDstMaker = 0;
class StChain;
StChain *chain = 0;
class St_db_Maker;
class StuDraw3DMuEvent;
StuDraw3DMuEvent *gEd = 0; //< The pointer to the current instance of the Event Display. You may want it to change the the drawing option
class StBemcTables;
StBemcTables* mBemcTables = 0;
bool gRotationIsOn = false; //< Flag whether the animated rotation was initialized to do it at once

//! \file EdMu.C 
/*! \file $STAR/macros/mudst/EdMu.C 
  \brief EdMu.C macro is the simple ROOT macro to draw the StMuDst from ROOT file
  
   Additions to use BEMC towers (for display of upsilon events) by Manuel Calderon de la Barca. Feb 2010.

  EdMu.C macro demonstrates how the StuDraw3DMuEvent class object can be used
    to loop over StMuEvent's from the ROOT file and draw the event components
    like "tracks" in 3D space over the the detector geometry.
    
  \author Valery Fine ( fine@bnl.gov )   25/02/2009
    
    Macro defines three functions:
    
     - void EdMu() - is a main entry point
     - void mae() - advance event function
     - void mrd() - redraw  event function
     .
     Macro creates two global pointers 
     
    - \c StMuDst \c *event is a pointer to the StMuDst object read by \a mae() function
    - \c gEd is a pointer to the instance of StuDraw3DMuEvent
    
    These two pointers are to allow the user to play with the image 
    from ROOT command prompt directly:
    
      - \c gEd->Clear(); - to clear the display \sa StDraw3D::Clear(Option_t *)
      - \c gEd->Tracks(); - to add the all tracks of the current
      \a event to display
      .
      To start the display invoke:

          \code 
                root4star EdMu.C
          \endcode
          
       - To draw the next event invoke from the ROOT command prompt:
         \code
               root4star[] mae()
         \endcode
       - To re-draw the current event invoke from the ROOT command prompt:
         \code
               root4star[] mrd()
         \endcode
         
         \note to make this macro useful one is supposed to provide his /her own 
         version of the \code "void mrd(bool hits=false, bool clear=false)"  \endcode function
*/

//____________________________________________________________________________________
//! Add emc hits to the list of the rendered objects 
/*! Look up all BEMC  tower adc values and render it  if (adc > ped + 3 * rms)
 This function loops over all the \a softIds, gets the \a ADC values, \a pedestals
 and \a gain calibrations and calculates the energy. \br
 \br The towers with dc > ped + 3 * rms will be drawn:
- green for E<0.3 GeV, 
- blue for 0.3 < E < 1 GeV, 
- yellow for 1 < E < 4 GeV (which is approximately the HT threshold for the upsilon trigger at Level-0)
- red for towers above 4 GeV. MCBS.
 \param cuts -  render the towers above 1 GeV only
   \htmlonly
    <table align=center>
    <tr><th>cuts=true</th><th>cuts=false</th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsCutYZ.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsYZ.png"> </td>
    </tr>
    <tr><th colspan=2> Courtesy of  <a href="http://nuclear.ucdavis.edu/~calderon">Manuel Calderon de la Barca Sanchez</a> </th></tr>
    </table>
   \endhtmlonly
*/
//____________________________________________________________________________________
void addEmcHits(bool cuts=false) { 
     if (!cuts) gEd->Endcaps();
    //TClonesArray *emcs = StMuDst::emcArray(1);
    //TIter next(emcs);
    //StMuEmcHit *hit = 0;
    //while (hit = (StMuEmcHit *)next()  ) {
    for (int softId = 1; softId<=4800; ++softId) {
      int adc = StMuDst::muEmcCollection()->getTowerADC(softId);
      Color_t colorResponce=kWhite;
      int status;
      mBemcTables->getStatus(1, softId, status);
      if (status != 1) continue;
      float ped, rms;
      mBemcTables->getPedestal(1, softId, 0, ped, rms);
      if (adc < ped + 3 * rms) continue;
      float calib;
      mBemcTables->getCalib(1, softId, 1, calib);
      float energy = calib * (adc - ped);
	//energy = (adc/285.0*3.7);
      if (softId==1) {
         cout << "id =" << softId << "  energy =" << energy << endl;
         cout << "    adc    = " << adc << endl;
         cout << "    status = " << status << endl;
         cout << "    ped    = " << ped << endl;
         cout << "    rms    = " << rms << endl;
         cout << "    calib  = " << calib << endl;
      }
      if (energy > 0  && energy < 30) {
         // If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
         if (  energy  < 0.3 )   {                colorResponce = kBlue; 
             // style = 4001;                 //wireframe 
             // If edep larger than MIP but less then 1 GeV 
         } else if (energy  < 1.0 )               colorResponce = kGreen;

         // If between 1 GeV and lowest HT threshold (4 GeV for Run7)
         else if (  energy  < 4.0 && energy>1.0)  colorResponce = kYellow;
         // If above lowest HT thershold
         else                                     colorResponce = kRed;

         const double maxSize =  400.; // (cm)
         const double scale   =  10.; // (cm/Gev)
         float size =(scale)*energy;
         if (size > maxSize)  size = maxSize ;
         if (!cuts || (cuts && energy>1.0)) {
           gEd->EmcHit(softId,colorResponce, 0, size);
           gEd->AddComment(Form("energy =  %f size=%f ",energy,size));
        }
    //  gEd->EmcHit(softId, adc*0.1);	
     }
  }
}

//____________________________________________________________________________________
//! Add tracks to the list of the rendered objects from current MuDst event
/*! Option \a cuts is to add the High Pt tracks only, to focus on high pt electron candidates.
   \param cuts - add High Pt tracks only (tracks with pt > 2 GeV and nSigmaElectron >-0.5 ) MCBS
   \htmlonly
    <table align=center>
    <tr><th>cuts=true</th><th>cuts=false</th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsCutYZ.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksHitsCutYZ.png"> </td>
    </tr>
    <tr><th colspan=2> Courtesy of  <a href="http://nuclear.ucdavis.edu/~calderon">Manuel Calderon de la Barca Sanchez</a> </th></tr>
    </table>
   \endhtmlonly
*/
void addTracks(bool cuts=false) {
    cout << "Adding tracks  ----------------------------- " << endl;
    cout << "Run / Event Number: " << muDstMaker->muDst()->event()->runNumber() << " / " << muDstMaker->muDst()->event()->eventNumber() << endl;

    size_t n_prim=StMuDst::GetNPrimaryTrack();
    
    int counter = 0;
    for (size_t i_track=0; i_track < n_prim; ++i_track) {
      StMuTrack &track = *(StMuDst::primaryTracks(i_track));
      double pt =track.pt();
      short charge= track.charge();
      double nSigmaE = track.nSigmaElectron();
      if (!cuts || (cuts && (pt>2 && nSigmaE>-0.5))) {
        Style_t sty = gEd->Style(kPrimaryTrack).Sty();
        Size_t  siz = gEd->Style(kPrimaryTrack).Siz();
        gEd->Track(track,StDraw3DStyle::Pt2Color(pt),sty,siz);
        gEd->AddComment(Form("pT =  %f charge=%d ",pt,charge));
        ++counter;
      }
   }
   cout << counter << " primary tracks have been rendered" << endl;
}
//____________________________________________________________________________________
//! This mrd (RedrawEvent) function \b redraws all hits and/or tracks from the \c current event
/*! 
   \param doTowerCuts - apply the tower cut (see: mae(bool rotation, bool doTowerCuts, bool doTrackCuts, int skipEvent )  )
   \param doTrackCuts - apply the track cut (see: mae(bool rotation, bool doTowerCuts, bool doTrackCuts, int skipEvent )  )
   \param clear - flag to mark whether the screen has to be cleaned 
                   first (before any new component is added)
   \htmlonly
    <table align=center>
    <tr><th>doTowerCuts=true<br>doTrackCuts=true</th><th>doTowerCuts=false<br>doTrackCuts=true </th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsCutXY.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsXY.png"> </td>
    </tr>
    <tr><th>doTowerCuts=true<br>doTrackCuts=false</th><th>doTowerCuts=false<br>doTrackCuts=false </th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksHitsCutXY.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksHitsXY.png"> </td>
    </tr>
    <tr><th colspan=2> Courtesy of  <a href="http://nuclear.ucdavis.edu/~calderon">Manuel Calderon de la Barca Sanchez</a> </th></tr>
    </table>
   \endhtmlonly
   \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu.C.Endcap.tracks.png "The event  42752 from Run 8112020 2007Production2 (upsilon) produced by EdMu.C macros"   
*/
//____________________________________________________________________________________
void mrd(bool doTowerCuts=false, bool doTrackCuts=false, bool clear=true) 
{  
   // redraw the event
   if (muEvent) {
      if (clear) gEd->Clear();
      addEmcHits(doTowerCuts);
      addTracks(doTrackCuts);
   
   }
 }
 
//____________________________________________________________________________________
//! This \a mae (AdvanceEvent) function is to search for the next non-empty event and draw it by looping over STAR muDST file (reading the next events from the file)
/*! 
   \param rotation - add the animated rotation defined by "rotation.iv" file  
      \htmlonly ( <b>See also:  <a href="http://techpubs.sgi.com/library/manuals/2000/007-2469-001/pdf/007-2469-001.pdf"> 
       Inventor Nodes/File Format Quick Reference</a> </b> ) \endhtmlonly 
       
   \param doTowerCuts - apply the tower cut  (see: addEmcHits(bool cuts )
   \param doTrackCuts - apply the track cut (see: addTracks(bool cuts )
   \param skipEvent - the number of the events to be skipped on the input file
   \htmlonly
    <table align=center>
    <tr><th>doTowerCuts=true<br>doTrackCuts=true</th><th>doTowerCuts=false<br>doTrackCuts=true </th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsCutYZ.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksCutHitsYZ.png"> </td>
    </tr>
    <tr><th>doTowerCuts=true<br>doTrackCuts=false</th><th>doTowerCuts=false<br>doTrackCuts=false </th><tr>
    <tr>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksHitsCutYZ.png"> </td>
    <td align=center><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EdMu/EdMuTracksHitsYZ.png"> </td>
    </tr>
    <tr><th colspan=2> Courtesy of  <a href="http://nuclear.ucdavis.edu/~calderon">Manuel Calderon de la Barca Sanchez</a> </th></tr>
    </table>
   \endhtmlonly
*/
//____________________________________________________________________________________
void mae(bool rotation=false, bool doTowerCuts=false, bool doTrackCuts=false,  int skipEvent = 205 ) 
{
 // Advance till next "good" event
 // One may want to replace the "plain"  "if" clause below
 // with the full-flegded filter
    gEd->Clear();
    
    chain->Skip(skipEvent); // want to get to event 42752, skip 205 events in file
 newevent:
     chain->Make();
     mBemcTables->loadTables(chain);

     if (muEvent = muDstMaker->muDst()->event()) {
     cout << "Run / Event Number: " << muDstMaker->muDst()->event()->runNumber() << " / " << muDstMaker->muDst()->event()->eventNumber() << endl;
         mrd(doTowerCuts,doTrackCuts);     // Draw the tracks and towers
      // The file rotation.iv should be in the StRoot/macros/graphics directory 
      // It can be used to set up a rotation of the coordinates.  The orientation,
      // rotation angles and the angular velocity are set in that file. MCBS.
         if (rotation && !gRotationIsOn ) {
            gEd->SetDrawOption("{file:rotation.iv}"); 
            gRotationIsOn = true;
         }
      } else {
        printf(" muEvent is empty\n");
        goto newevent;
      }
 }
//____________________________________________________________________________________
//____________________________________________________________________________________
//! Main entry point to initialize the primitive "Event Display" and the STAR bfc chain 
/*! 
   \param file  - the ROOT file with StEvent
   \param detectorNames  - the list of the detector names or "0" to draw none 
   \note To start "Event Display" \c EdMu just invoke:
   \code 
     ln -s $STAR/StRoot/macros/.rootrc
     root4star EdMu.C 
   \endcode
   \image html "http://nuclear.ucdavis.edu/~calderon/Images/upsEvent.gif" "The tracks in the TPC and also the signal seen in the BEMC ( Courtesy of Manuel Calderon de la Barca Sanchez )"
   <htmlonly>
    For 3D viewer GUI interface see <a href="http://doc.coin3d.org/SoQt/classSoQtExaminerViewer.html#_details">
    SoQtExaminerViewer Class Reference</a> also.
   \endhtmlonly
*/
//____________________________________________________________________________________
 void EdMu(const char* file =
 "/star/institutions/bnl/fine/testfiles/st_upsilon_8112020_raw_1130030.MuDst.root"
 , const char * detectorNames="TPC,StarBeam")
 {
   // Start application open the file provided.
   if ( gSystem->AccessPathName(file)) {
      cout << endl << endl 
           << "** Error ** : The input file: <"<< file << "> does not exist !!!" 
           << endl << endl
           <<  " Please select the existing one and re-call the function: " 
           << endl
           << endl << " root [0] EdMu(\"new file ROOT file name\")" 
           << endl 
           << endl << "To draw the StEvent components with no detector geometry, use: "
           << endl
           << endl << " root [0] EdMu(\"new file ROOT file name\",0)" 
           << endl
           << endl;
      return;
   }
   TString muDstFile=file;
   gROOT->Macro("loadMuDst.C");
   gSystem->Load("St_Tables.so");
   gSystem->Load("St_db_Maker.so");
   gSystem->Load("StTpcDb");
   gSystem->Load("StDetectorDbMaker");
   gSystem->Load("StDbUtilities");
   gSystem->Load("StDbLib");
   gSystem->Load("StDbBroker");

   gSystem->Load("StEEmcUtil");
   chain = new StChain();
   St_db_Maker* dbMaker = new St_db_Maker("dbName","$STAR/StarDb","MySQL:StarDb");
   muDstMaker = new StMuDstMaker(0,0,muDstFile.Data());
   chain->Init();
   //chain->InitRun(8112020);//hardwired...
   mBemcTables = new StBemcTables;  
   delete gEd; // destroy the built-in display
   gEd = new StuDraw3DMuEvent(detectorNames); // create our own one (with no detector geometry)
   gEd->SetBkColor(kBlack);
   printf("\n The display is ready!\n");
   printf(" calling:\n");   
   printf("\t---\n");
   printf("\tmae()\n");
   printf("\t---\n");
   printf("method to read and show the next event\n");
   mae();
   printf(" call:\n");   
   printf("\t---\n");
   printf("\tmae() - to  advance the event \n");
   printf("\tmrd() - to redraw the event\n");
   printf("\t---\n");
   gEd->SetDrawOption("{view:all}");
 }

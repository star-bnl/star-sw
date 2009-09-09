// $Id: EdMu.C,v 1.1 2009/09/09 20:46:44 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009

//! \file EdMu.C 

/*!
  \brief EdMu.C macro is the simple ROOT macro to draw the StEvent from ROOT file

  EdMu.C macro demonstrates how the StuDraw3DEvent class object can be used
    to loop over StEvent from the ROOT file and to draw the event components
    like "tracks" and hits" in 3D space over the the detector geometry.
    
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

class StMuDst;
StMuDst* muDst = 0;
class StMuEvent;
StMuEvent* muEvent = 0;
class StMuDstMaker;
StMuDstMaker *chain = 0;
class StuDraw3DMuEvent;
StuDraw3DMuEvent *gEd = 0;

//! This function \b redraws all hits and/or tracks from the \c current event
/*! 
   \param hits - flag to mark whether the hits from the event should be rendered 
   if the \a hits = \c true the hits is to be drawn otherwise it is to render the tracks
   \param clear - flag to mark whether the screen has to be cleaned 
                   first (before any new component is added)
   \note No MuHits can be rendered directly for the time being.
*/
//__________________________________________
void mrd(bool hits=false, bool clear=false) 
{  
   // redraw the event
   if (muEvent) {
      if (clear) gEd->Clear();
      if (hits) { // gEd->Hits();         
      } else gEd->Tracks();
   }
 }
//! This function is to search for the next non-empty event and draw it by looping over StBFChain (reading the next events from the file)
/*! 
   \param hits - flag to mark whether the hits from the event should be rendered 
   if the \a hist = \c true the hits is to be drawn otherwise it is to render the tracks
*/
//__________________________________________
void mae(bool hits=false) 
{
 // Advance till next "good" event
 // One may want to replace the "plain"  "if" clause below
 // with the full-flegded filter
    gEd->Clear();
 newevent:
     chain->Make();
     if (muEvent = chain->muDst()->event()) {
         mrd();     // Draw the tracks
    } else {
        printf(" muEvent is empty\n");
        goto newevent;
     }
 }
//! Main entry point to initialize the primitive "Event Display" and the STAR bfc chain 
/*! 
   \param file  - the ROOT file with StEvent
   \param detectorNames  - the list of the detector names or "0" to draw none 
   \note To start "Event Display" \c EdMu just invoke:
   \code root4star EdMu.C \endcode
*/
//__________________________________________
 void EdMu(const char* file =
 "/star/data15/reco/ppProduction2008/ReversedFullField/P08ie/2008/046/9046031/st_physics_adc_9046031_raw_2070002.MuDst.root"
 , const char * detectorNames="TPC")
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
   chain=new StMuDstMaker(0,0,muDstFile.Data());
   chain->Init();
//   gROOT->Macro(Form("bfc.C(0,\"doevents\",\"%s\")",file));
   delete gEd; // destroy the built-in display
   gEd = new StuDraw3DMuEvent(detectorNames); // create our own one (with no detector geometry)
//   new StuDraw3DEvent("TPC"); // create our own one (with TPC detector)
   gEd->SetBkColor(kBlack);
   printf("\n The display is ready!\n");
   printf(" call:\n");   
   printf("\t---\n");
   printf("\tmae()\n");
   printf("\t---\n");
   printf("method to see the next event\n");
   mae();
   printf(" call:\n");   
   printf("\t---\n");
   printf("\tmae() - to draw the tracks\n");
   printf("\tmae(1) - to draw the tracksand its hits\n");
   printf("\t---\n");
   printf("method to see the next event\n");
 }

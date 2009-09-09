// $Id: Ed.C,v 1.7 2009/09/09 20:47:03 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009

//! \file Ed.C 

/*!
  \brief Ed.C macro is the simple ROOT macro to draw the StEvent from ROOT file

  Ed.C macro demonstrates how the StuDraw3DEvent class object can be used
    to loop over StEvent from the ROOT file and to draw the event components
    like "tracks" and hits" in 3D space over the the detector geometry.
    
 \author Valery Fine ( fine@bnl.gov )   25/02/2009
    
    Macro defines three functions:
    
     - void Ed() - is a main entry point
     - void ae() - advance event function
     - void rd() - redraw  event function
     .
     Macro creates two global pointers 
     
    - \c StEvent \c *event is a pointer to the StEvent object read by \a ae() function
    - \c gEventDisplay is a pointer to the instance of StuDraw3DEvent
    
    These two pointers are to allow the user to play with the image 
    from ROOT command prompt directly:
    
      - \c gEventDisplay->Clear(); - to clear the display \sa StDraw3D::Clear(Option_t *)
      - \c gEventDisplay->Hits(event); - to add the "used" hits to display
      - \c gEventDisplay->Tracks(event); - to add the all tracks of the current
      \a event to display
      - \c gEventDisplay->Hits(event,kUnusedHitsOnly); -  to add the "unused" hits to display
      .
      To start the display invoke:

          \code 
                root4star Ed.C
          \endcode
          
       - To draw the next event invoke from the ROOT command prompt:
         \code
               root4star[] ae()
         \endcode
       - To re-draw the current event invoke from the ROOT command prompt:
         \code
               root4star[] rd()
         \endcode
         
         \note to make this macro useful one is supposed to provide his /her own 
         version of the \code "void rd(bool hits=false, bool clear=false)"  \endcode function
*/

class StEvent;
StEvent* event = 0;

//! This function \b redraws all hits and/or tracks from the \c current event
/*! 
   \param hits - flag to mark whether the hits from the event should be rendered 
   if the \a hits = \c true the hits is to be drawn otherwise it is to render the tracks
   \param clear - flag to mark whether the screen has to be cleaned 
                   first (before any new component is added)
*/
//__________________________________________
void rd(bool hits=false, bool clear=false) 
{  
   // redraw the event
   if (event) {
      if (clear) gEventDisplay->Clear();
      if (hits) gEventDisplay->Hits(event);
      else gEventDisplay->Tracks(event);
   }
 }
//! This function is to search for the next non-empty event and draw it by looping over StBFChain (reading the next events from the file)
/*! 
   \param hits - flag to mark whether the hits from the event should be rendered 
   if the \a hist = \c true the hits is to be drawn otherwise it is to render the tracks
*/
//__________________________________________
void ae(bool hits=false) 
{
 // Advance till next "good" event
 // One may want to replace the "plain"  "if" clause below
 // with the full-flegded filter
    gEventDisplay->Clear();
 newevent:
     chain->MakeEvent();
     event = (StEvent*)chain->GetDataSet("StEvent");
     if (event && !event->trackNodes().empty())) {
         rd();     // Draw the tracks
         rd(hits); // Add the hits to the image
    } else {
        printf(" event is empty\n");
        goto newevent;
     }
 }
//! Main entry point to initialize the primitive "Event Display" and the STAR bfc chain 
/*! 
   \param file  - the ROOT file with StEvent
   \param detectorNames  - the list of the detector names or "0" to draw none 
   \note To start "Event Display" \c Ed just invoke:
   \code root4star Ed.C \endcode
*/
//__________________________________________
 void Ed(const char* file =
 "/star/data15/reco/production_dAu2008/ReversedFullField/P08ie/2008/025/9025036/st_physics_9025036_raw_1010030.event.root"
 , const char * detectorNames="TPC")
 {
   // Start application open the file provided.
   if ( gSystem->AccessPathName(file)) {
      cout << endl << endl 
           << "** Error ** : The input file: <"<< file << "> does not exist !!!" 
           << endl << endl
           <<  " Please select the existing one and re-call the function: " 
           << endl
           << endl << " root [0] Ed(\"new file ROOT file name\")" 
           << endl 
           << endl << "To draw the StEvent components with no detector geometry, use: "
           << endl
           << endl << " root [0] Ed(\"new file ROOT file name\",0)" 
           << endl
           << endl;
      return;
   }
   gROOT->Macro("Load.C");
   gROOT->Macro(Form("bfc.C(0,\"doevents\",\"%s\")",file));
   delete gEventDisplay; // destroy the built-in display
   new StuDraw3DEvent(detectorNames); // create our own one (with no detector geometry)
//   new StuDraw3DEvent("TPC"); // create our own one (with TPC detector)
   gEventDisplay->SetBkColor(kBlack);
   printf("\n The display is ready!\n");
   printf(" call:\n");   
   printf("\t---\n");
   printf("\tae()\n");
   printf("\t---\n");
   printf("method to see the next event\n");
   ae();
   printf(" call:\n");   
   printf("\t---\n");
   printf("\tae() - to draw the tracks\n");
   printf("\tae(1) - to draw the tracksand its hits\n");
   printf("\t---\n");
   printf("method to see the next event\n");
 }

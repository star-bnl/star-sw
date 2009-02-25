// $Id: Ed.C,v 1.6 2009/02/25 21:37:43 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009

//! Ed.C macro is the simple script to draw the StEvent from ROOT file

/*! Ed.C macro demonstrates how the StuDraw3DEvent class can be use
    to loop over event read from the ROOT file and draw the event comonents
    like "tracks" and hits" in 3D space over the the detector geometry
    
    Macro defines two functions:
    
     void Ed() - is an entry points
     void ae() - advance event function
     Macro creates two global pointers 
    /a StEvent *event is a pointer to the StEvent object read by /a ae() function
    /a gEventDisplay is a pointer to the instance of StuDraw3DEvent
    
    These two pouner are to allow the user to play with the image 
    from ROOT commnad prompt:
      gEventDisplay->Clear(); - to clear the displya
      gEventDisplay->Hits(event): to add the "used" hits to display
      gEventDisplay->Tacks(event): to add the all tracks of the cuurent
      /a event to display
      gEventDisplay->Hits(event,kUnusedHitsOnly);  to add the "unused" hits to display
      
      To start the display invoke:
      
          root.exe Ed.C
          
       to draw the next event invoke from the ROOT command promot:
          root[] ae()
          
*/
class StEvent;
StEvent* event = 0;

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
         gEventDisplay->Tracks(event);
         if (hits) gEventDisplay->Hits(event);
    } else {
        printf(" event is empty\n");
        goto newevent;
     }
 }
//__________________________________________
 void Ed(const char* file =
 "/star/data07/calib/fisyak/TpcRS/daq_2008_AuAu9_7/st_physics_9071073_raw_1010001.event.root")
 {
   // Start application open the file provided. 
   gROOT->Macro("Load.C");
   gROOT->Macro(Form("bfc.C(0,\"doevents\",\"%s\")",file));
   delete gEventDisplay; // destroy the built-in display
   new StuDraw3DEvent(0); // create our own one (with no detector geometry)
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

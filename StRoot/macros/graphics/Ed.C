// $Id: Ed.C,v 1.3 2009/02/25 21:13:06 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008

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
 }

//*-- Author :    Valery Fine(fine@bnl.gov)   02/12/99
// $Id: TurnDisplay.C,v 1.2 1999/12/02 19:56:22 fine Exp $
  StEventDisplayMaker *dsMaker = 0;
  StVirtualEventFilter *trackFilter;
//___________________________________________________________________
void TurnDisplay(const Char_t *filterName="StTrackFilter") {
  // TurnDisplay.C macro:
  //
  //  - Load StEventDisplayMaker
  //  - Load user-defined filert class if provided
  //  - defines the tables one wants to be drawn

    if (!chain->GetOption("DISPLAY")) {
       gSystem->Load("StEventDisplayMaker");
       dsMaker = new StEventDisplayMaker();
       dsMaker->Init();
    }
    dsMaker = (StEventDisplayMaker *) chain->GetMaker("EventDisplay");
    if (dsMaker) {
       if (filterName && filterName[0]) {
           gSystem->Load(filterName);  // Load the user-defined filter
           TClass *filterClass = GetClass(filterName);
           if (filterClass) {
             trackFilter = (StVirtualEventFilter *)filterClass->New();

             // Activate some filter channels
             dsMaker->SetFilter(trackFilter,StEventDisplayMaker::kTable);    
             dsMaker->SetFilter(trackFilter,StEventDisplayMaker::kTptTrack); 
           } 
           else 
             cout << " *** ERROR ***: There is no class filter <" << filterName << ">. The defualt one will be used instead" << endl;           
       }

       // define "Event" geometry (the objects to be drawn out)

       // "Reference tables" have no (x,y,z) information and can not be drawn
       // They should be sorted by the foreign key provided in brackets to be effecient
       // No filter channel used. No default action 

       dsMaker->AddName("g2t_track(ge_pid)");   //Add the "reference" tables to the Event Display list
       dsMaker->AddName("g2t_vertex(id)");      //Add the "reference" tables to the Event Display list

       // "Regular" tables:
       //      contain 3 columns associated with x,y,z and 
       //      foreing key to link all points together

       dsMaker->AddName("g2t_tpc_hit(track_p,x[0]:x[1]:x[2])");   //Add the tables to the Event Display list
       dsMaker->AddName("g2t_svt_hit(track_p,x[0]:x[1]:x[2])");   //Add the tables to the Event Display list
       dsMaker->AddName("tphit(id_globtrk,x:y:z)");               //Add the tables to the Event Display list

       // "Irregular" tables: has no column associated directly with (x,y,z) coordinates

       dsMaker->AddName("tptrack");                // the table has no column with (x,y,z) coordinates,
                                                   // a special method has to be invoked to draw this table 

       if (trackFilter) trackFilter->TurnOn(); 
       dsMaker->SetDebug();
    }
  }
//__________________________________________________________________________
// $Log: TurnDisplay.C,v $
// Revision 1.2  1999/12/02 19:56:22  fine
// Clean up
//
// Revision 1.1  1999/12/02 18:49:35  fine
// two new macro to play with Event Display have been introduced
//

//*-- Author :    Valery Fine(fine@bnl.gov)   02/12/99
// $Id: TurnDisplay.C,v 1.8 2000/08/31 21:28:01 fine Exp $
  StEventDisplayMaker *dsMaker = 0;
  StVirtualEventFilter *trackFilter;
//___________________________________________________________________
void TurnDisplay(const Char_t *filterName=0) {
  //
  // TurnDisplay.C macro:
  //
  //  - Load StEventDisplayMaker
  //  - Load user-defined filter class if provided
  //  - defines the tables one wants to be drawn

    if (chain->IsA() == StChain::Class() || !chain->GetOption("DISPLAY") ) {
       gSystem->Load("St_geom_Maker");
       gSystem->Load("StEventDisplayMaker");
       StMaker *mini = new St_geom_Maker();
       mini->Init();
       dsMaker = new StEventDisplayMaker();
       dsMaker->Init();
    }
    dsMaker = (StEventDisplayMaker *) chain->GetMaker("EventDisplay");
    if (dsMaker) {
       if (filterName && filterName[0]) {
           gSystem->Load(filterName);  // Load the user-defined filter
           TClass *filterClass = gROOT->GetClass(filterName);
           if (filterClass) {
             trackFilter = (StVirtualEventFilter *)filterClass->New();

             // Activate some filter channels
             dsMaker->SetFilter(trackFilter,StEventDisplayMaker::kTable);    
             trackFilter->TurnOn();

             trackFilter = (StVirtualEventFilter *)filterClass->New();
             dsMaker->SetFilter(trackFilter,StEventDisplayMaker::kTptTrack); 
           } 
           else {
             cout << " *** ERROR ***: There is no class filter <" << filterName << ">. The defualt one will be used instead" << endl;           
             dsMaker->SetTableFlag();
           }
       }
       else {
          dsMaker->SetTableFlag();
          dsMaker->SetTptTrackFlag();
       }

        // define "Event" geometry (the objects to be drawn out)
        //  This is under construction !!!!
     //___________________________________________________________________
     //
     //                  User defined area follow:
     //___________________________________________________________________

       // "Reference tables" have no (x,y,z) information and can not be drawn
       // They should be sorted by the foreign key provided in brackets to be effecient
       // No filter channel used. No default action 

       dsMaker->AddName("dst/dst_dedx(id_track)");   //Add the "reference" tables to the Event Display list
       // dsMaker->AddName("g2t_track(ge_pid)");   //Add the "reference" tables to the Event Display list
       // dsMaker->AddName("g2t_vertex(id)");      //Add the "reference" tables to the Event Display list

       // "Regular" tables:
       //      contain 3 columns associated with x,y,z and 
       //      foreing key to link all points together

       // dsMaker->AddName("g2t_tpc_hit(track_p,x[0]:x[1]:x[2])");   //Add the tables to the Event Display list
       // dsMaker->AddName("g2t_svt_hit(track_p,x[0]:x[1]:x[2])");   //Add the tables to the Event Display list
       dsMaker->AddName("tphit(id_globtrk,x:y:z)");               //Add the tables to the Event Display list
       // dsMaker->AddName("scs_spt(id_globtrk,x[0]:x[1]:x[2])");    //Add the tables to the Event Display list

       // extra table to check packed options
       // http://www.star.bnl.gov/STAR/html/comp_l/root/html/egr_globtrk_st.html

       // dsMaker->AddName("egr_globtrk(id,position[0]:position[1]:charge)");    //Add the tables to the Event Display list

       // "Packed" tables:
       //      contain 1 columns with 2 long numbers associated with packed x,y,z and 
       //      foreing key to link all points together
       //      http://www.star.bnl.gov/STAR/html/comp_l/root/html/dst_point_st.html
       dsMaker->AddName("dst/point(id_track,position[0]:position[1]:charge)");       //Add the tables to the Event Display list

       // "Irregular" tables: has no column associated directly with (x,y,z) coordinates
       dsMaker->AddName("dst/primtrk");
       dsMaker->AddName("tptrack");                // the table has no column with (x,y,z) coordinates,
                                                   // a special method has to be invoked to draw this table 
     //___________________________________________________________________
     //
     //               End of the user defined area follow:
     //___________________________________________________________________
       if (trackFilter) trackFilter->TurnOn(); 
       dsMaker->SetDebug();     
       dsMaker->TurnOn();
    }
  }
//__________________________________________________________________________
// $Log: TurnDisplay.C,v $
// Revision 1.8  2000/08/31 21:28:01  fine
// new default to draw dst objects
//
// Revision 1.7  2000/04/18 21:43:14  fine
// make TurnDisplay macro available for doEvents
//
// Revision 1.6  1999/12/27 21:47:06  fine
// St_geom_Maker has been added
//
// Revision 1.5  1999/12/27 20:54:39  fine
// it shows how to active the ttrack points
//
// Revision 1.4  1999/12/19 00:12:34  fine
// some corrections for the packed tables
//
// Revision 1.3  1999/12/06 04:46:56  fine
// Bug fixes
//
// Revision 1.2  1999/12/02 19:56:22  fine
// Clean up
//
// Revision 1.1  1999/12/02 18:49:35  fine
// two new macro to play with Event Display have been introduced
//

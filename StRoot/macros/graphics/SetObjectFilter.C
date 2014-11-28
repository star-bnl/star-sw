//*-- Author :    Valery Fine(fine@bnl.gov)   02/12/99  
// $Id: SetObjectFilter.C,v 1.4 2000/08/10 17:09:30 fine Exp $
//___________________________________________________________________
void SetObjectFilter()
{
  // This is a pattern to be used by user to create his /her
  // own custom filter parameters.

  // - Make your own copy of this macro
  // - Keep this file open with your favorite editor
  // - Change it when it is appropriated and 
  // - RELOAD in to the running ROOT session with 
  //          .x SetObjectFilter.C 
  //   as many time as you want
   
  //  To get an access to your custom filter one has to cast
  //  the global virtual pointer to his /her custom type
  //  This means the type "StVirtualFilter" must be replaced with the
  //  custom one.

   //________________________________________________
   //
   // You should adjust the type of filter below
   //________________________________________________
   //
      StTrackFilter *localFilter = (StVirtualEventFilter *)trackFilter;
   //________________________________________________
   Int_t tr_p[]={39250,39344};
   Int_t ltr_p = sizeof(tr_p)/sizeof(int);
   localFilter->SetTrack_P(tr_p,ltr_p);

   Int_t id_globtrk[]={3975,2485};
   Int_t lid_globtrk = sizeof(id_globtrk)/sizeof(int);
   localFilter->SetId_globtrk(id_globtrk,lid_globtrk);
        
   Int_t tr_id[]={5489,5491};
   Int_t ltr_id = sizeof(tr_id)/sizeof(int);
   localFilter->SetTptID(tr_id,ltr_id);
   //______________________________________________//
   //                                              //
   // Redraw picture with a new filter parameters. //
   // You should NOT change the line below !!!     //
   //______________________________________________//
   //                                              //
      if (dsMaker) dsMaker->ReDraw();              //
   //______________________________________________//
}

// $Log: SetObjectFilter.C,v $
// Revision 1.4  2000/08/10 17:09:30  fine
//  Wensheng example has been fixed
//
// Revision 1.3  1999/12/06 04:46:56  fine
// Bug fixes
//
// Revision 1.2  1999/12/02 20:03:31  fine
// Clean up
//
// Revision 1.1  1999/12/02 18:49:35  fine
// two new macro to play with Event Display have been introduced
//

//*-- Author :    Valery Fine(fine@bnl.gov)   02/12/99  
// $Id: SetObjectFilter.C,v 1.1 1999/12/02 18:49:35 fine Exp $
//___________________________________________________________________
void SetObjectFilter()
{
  // This is a pattern to be used by user to create his /her
  // own custom filter parameters.

  // - Make your own copy of this macro
  // - Keep this file open with your favorite editor
  // - Change it when it is approriated and 
  // - RELOAD in to the running ROOT session with 
  //          .x SetObjectFilter.C 
  //   as many time as you want
   
  //  To get an access to your custom filter one has to cast
  //  the global virtual pointer to his /her custom type
  //  This means the type "StVirtualFilter" must be replaced with the
  //  custom one.

   StVirtualFilter *localFilter = (StVirtualFilter *)trackFilter;
   Int_t tr_p[]={39250,39344};
   Int_t ltr_p = sizeof(tr_p)/sizeof(int);
//   localFilter->SetTrack_P(tr_p,ltr_p);

   Int_t id_globtrk[]={3975,2485};
   Int_t lid_globtrk = sizeof(id_globtrk)/sizeof(int);
   //       localFilter->SetId_globtrk(id_globtrk,lid_globtrk);
        
   Int_t tr_id[]={5489,5491};
   Int_t ltr_id = sizeof(tr_id)/sizeof(int);
//   localFilter->SetTptID(tr_id,ltr_id);
}

// $Log: SetObjectFilter.C,v $
// Revision 1.1  1999/12/02 18:49:35  fine
// two new macro to play with Event Display have been introduced
//

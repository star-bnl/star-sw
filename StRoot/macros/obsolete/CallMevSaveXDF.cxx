//*-- Author :    Valery Fine   26/06/98  (E-mail:fine@bnl.gov)
// $Id: CallMevSaveXDF.cxx,v 1.4 1998/07/25 00:31:49 fisyak Exp $
// $Log: CallMevSaveXDF.cxx,v $
// Revision 1.4  1998/07/25 00:31:49  fisyak
// Working versions of tutorials
//
// Revision 1.3  1998/07/23 11:32:41  fisyak
// Small fixes
//
// Revision 1.2  1998/07/22 01:14:29  fisyak
// Release SL98e
//
{
  // Load the ROOT dictionaries for STAR
  Bool_t NT = kFALSE;
  if (strcmp(gSystem.GetName(),"WinNT")==0){
    NT = kTRUE;
    if (gSystem.Load("St_base.dll"))      printf(" Loading DLL \"St_base.dll\" failed \n");
    if (gSystem.Load("St_Tables.dll"))    printf(" Loading DLL \"St_Tables.dll\" failed \n");
    // Load dictionary for begin_html <a href="http://www.rhic.bnl.gov/afs/rhic/star/doc/www/packages_l/dev/pams/doc/module.html#mevsim_mod">mevsim</a> end_html STAF module
    if (gSystem.Load("libSt_mevsim.dll")) printf(" Loading DLL \"libSt_mevsim.dll\" failed \n");
  }
  else {
    if (gSystem.Load("libasu.so"))      printf(" Loading DLL \"libasu.so\" failed \n");
    if (gSystem.Load("libdsl.so"))      printf(" Loading DLL \"libdsl.so\" failed \n");
    if (gSystem.Load("St_base.so"))      printf(" Loading DLL \"St_base.so\" failed \n");
    if (gSystem.Load("St_Tables.so"))    printf(" Loading DLL \"St_Tables.so\" failed \n");
    // Load dictionary for begin_html <a href="http://www.rhic.bnl.gov/afs/rhic/star/doc/www/packages_l/dev/pams/doc/module.html#mevsim_mod">mevsim</a> end_html STAF module
    if (gSystem.Load("mev.sl")) printf(" Loading DLL \"mev.sl\" failed \n");
    if (gSystem.Load("St_mev.so")) printf(" Loading DLL \"St_mev.so\" failed \n");
  }

  //  Delete the "old" log file
  if (NT) gSystem.Exec("del *.log");   // This file is created by mevsim module and must be deleted if present
  else    gSystem.Exec("rm *.log");
  // Check whether the input data file does exist
  Char_t *infile = gSystem->Which("./","mult_gen.in");
  if (infile) delete [] infile;
  else {
  // Copy the input file from the server
     Char_t *cmd = new Char_t[256];


     if (!(infile = gSystem->Which("$STAR/StRoot/macros","mult_gen.in"))){
            printf(" There is no data file! \n Exit !!!\n"); 
            return;
      }
      // Prepare the shell command to copy file into the current directory
      if (NT)  strcpy( cmd, "copy " );
      else     strcpy( cmd, "/bin/cp " );

      strcat(cmd, infile);// delete [] infile;
      strcat(cmd, " .");
      if (NT)  {   // change the path separators from "slash" to "backslash" for Windows NT
        Char_t *bptr = 0;
        while( bptr = strchr( cmd, '/') )
               *bptr = '\\';
      }
      printf (" %s \n",cmd);
      Int_t ret = !gSystem->Exec( cmd );   // Launch the shell command
   }
   gBenchmark->Start("mevsim");

//   Read XDF file
   const Char_t *filename;
   if (NT) filename = "$(temp)/test.xdf";
   else  filename = "test.xdf";
   St_XDFFile  xdf;                       // Create xdf object of St_XDFFile class
   if (xdf.OpenXDF(filename,"w")) return; // Try to open the inout XDF file

// File has been opened

   Int_t nevent = 10;
   //   printf(" Writting %d events to XDF file %s \n", nevent);
   Int_t i;

 // Entering the event loop

   for (i=0;i < nevent;i++) {
      St_DataSet event("evgen");                // Create an empty dataset;
      St_particle *pa = new St_particle(15000); // Create the St_particle object for the STAF "particle" table
      event->Add(pa);                   // Add St_partcile object "pa" into the "event" St_DataSet object
      printf(" Calling the 'mevsim' module for %d event \n",i+1);
      if( mevsim(pa) ) {
        pa->ls("*");
        xdf.NextEventPut(&event);               // Write the current event into XDF file
      }
      else
        printf(" Module failed \n");
   }
   //   xdf.CloseXDF();                             // Close the XDF file
   gBenchmark->Stop("mevsim");                 // Stop timer

  // Print timing
      printf("    Real Time Cpu Time  Os    CPU     RAM   output     size\n");
      printf("      sec       sec                    Mb    file \n");
      printf(" 1.  17.75     15.45    NT  Intel 266 128   local     7'116'900\n");
      printf("\n ======== Your system for %d events is: \n",nevent);
    gBenchmark->Show("mevsim");
    printf(" File %s in XDF format has been created\n",gSystem->ExpandPathName(filename));
    printf(" You may pick up this example from /afs/rhic/star/packages/dev/StRoot/macros/CallMevSaveXDF.cxx\n");
}

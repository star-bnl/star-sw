//*-- Author :    Valery Fine   26/06/98  (E-mail:fine@bnl.gov)
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

  //  Delete the "old" log file
  gSystem.Exec("del *.log");   // This file is created by mevsim module and must be deleted if present

  // Check whether the input data file does exist
  Char_t *infile = gSystem->Which(".","mult_gen.in");
  if (infile) delete [] infile;
  else {
  // Copy the input file from the server
     Char_t *cmd = new Char_t[256];


     if (!(infile = gSystem->Which("$(rootsys)/star/macros","mult_gen.in"))){
            printf(" There is no data file! \n Exit !!!\n"); 
            return;
      }
      // Prepare the shell command to copy file into the current directory
      if (NT)  strcpy( cmd, "copy " );
      else     strcpy( cmd, "/bin/cp " );

      strcat(cmd, infile); delete [] infile;
      strcat(cmd, " ");
      strcat(cmd, ".");
      if (NT)  {   // change the path separators from "slash" to "backslash" for Windows NT
        Char_t *bptr = 0;
        while( bptr = strchr( cmd, '/') )
               *bptr = '\\';
      }
      Int_t ret = !gSystem->Exec( cmd );   // Launch the shell command
   }
   gBenchmark->Start("mevsim");

//   Read XDF file
   const Char_t *filename =  "$(temp)/test.xdf";
   St_XDFFile  xdf;                       // Create xdf object of St_XDFFile class
   if (xdf.OpenXDF(filename,"w")) return; // Try to open the inout XDF file

// File has been opened

   Int_t nevent = 10;
   printf(" Writting %d events to XDF file %s \n", nevent);
   Int_t i;

 // Entering the event loop

   for (i=0;i < nevent;i++) {
      St_DataSet event("evgen");                // Create an empty dataset;
      St_particle *pa = new St_particle(15000); // Create the St_particle object for the STAF "particle" table
      event->InsertTable(pa);                   // Add St_partcile object "pa" into the "event" St_DataSet object
      printf(" Calling the 'mevsim' module for %d event \n",i+1);
      if( mevsim(pa) ) {
        pa->ls("*");
        xdf.NextEventPut(&event);               // Write the current event into XDF file
      }
      else
        printf(" Module failed \n");
   }
   xdf.CloseXDF();                             // Close the XDF file
   gBenchmark->Stop("mevsim");                 // Stop timer

  // Print timing
   printf("    Real Time Cpu Time  Os    CPU     RAM   output     size\n");
   printf("      sec       sec                    Mb    file \n");
   printf(" 1.  17.75     15.45    NT  Intel 266   128    local     7'116'900\n");
   printf("\n ======== Your system for %d events is: \n",nevent);
   gBenchmark->Show("mevsim");
   printf(" File %s in XDF format has been created\n",gSystem->ExpandPathName(filename));
}

//
// $Id: getBranchSizes.C,v 3.1 2003/02/05 23:06:19 magestro Exp $
// $Log: getBranchSizes.C,v $
// Revision 3.1  2003/02/05 23:06:19  magestro
// Calculate sizes of branches in a TTree
//
//
//======================================================================
//
// getBranchSizes(TTree*)
//
// This function takes a TTree as argument and returns a formatted list of 
// data containers with the file size of each container.  For a container
// to print, it must have a minimum size of 64 kB otherwise it doesn't get
// compressed and GetZipBytes() returns zero.
//
//    - Dan Magestro, Feb. 5, 2003 

void getBranchSizes(TTree *tree) {

   printf("\n");
   printf("File: %s  \n",tree->GetCurrentFile()->GetName());
   printf("      (%i entries) \n",(int) tree->GetEntries());
   printf("\n");
   printf("         Container          File Size         Size/evt\n");
   printf("      ---------------  ------------------   ------------\n");

   Int_t nBytes = 0, nBytesBranch2 = 0;
   TObjArray *array2 = 0, *array3 = 0, *array4 = 0;
   TBranch *branch1 = 0, *branch2 = 0, *branch3 = 0, *branch4 = 0;

   TObjArray *fBranches = tree->GetListOfBranches();

   for(Int_t i=0;i<fBranches->GetEntries();i++) {

      branch1 = (TBranch*) fBranches->At(i);
      array2 = branch1->GetListOfBranches();

      nBytesBranch2 = 0;
      for(Int_t j=0;j<array2->GetEntries();j++) {

         branch2 = (TBranch*) array2->At(j);
         nBytesBranch2 += (int) branch2->GetZipBytes();
         array3 = branch2->GetListOfBranches();
         Int_t nBranch2 = array3->GetEntries();

         for(Int_t k=0;k<nBranch2;k++) {

            branch3 = (TBranch*) array3->At(k);
            nBytesBranch2 += (int) branch3->GetZipBytes();
         }
      }

      if(nBytesBranch2) printf(" %20s %11i (%4.1f%%)   %6.1f bytes\n", 
               branch1->GetName(),nBytesBranch2,
               nBytesBranch2*100./tree->GetZipBytes(), 
               nBytesBranch2/branch1->GetEntries() );

      nBytes += nBytesBranch2;
   }

   printf("                       ------------------   ------------\n");
   printf("                      %11i (%4.1f%%)   %6.3f kB/event\n",
         nBytes,nBytes*100./tree->GetZipBytes(), nBytes/branch1->GetEntries()/1024. );
   printf("\n");

}

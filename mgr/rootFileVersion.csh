#
   starver .eta2
   set TFILE = $1

   root.exe -b -q << EOD  |& grep  '==='
   
   TFile ff("$TFILE");
   int ver = gFile->GetVersion();
   int fmt = gFile->GetFormat();
   printf("=== \n");
   printf("\n\n\n=== File: %s\n",gFile->GetName());
   printf("=== Was made by ROOT = %d format = %d\n",ver,fmt);
   if (fmt >=2                ) {printf("=== It is STAR  I/O format. You can read it by STAR ROOT2 ONLY\n");}
   if (fmt >=2                ) {printf("=== Can be converted by script doCopy.csh\n");}
   if (fmt <=1 && ver < 30000 ) {printf("=== It is STAR  I/O format. You can read it by ROOT2 or ROOT3 \n");}
   if (fmt <=1 && ver >=30000 ) {printf("=== It is ROOT3 I/O format. You can read it ONLY by ROOT3     \n");}
   printf("=== \n");
   .q
EOD

#

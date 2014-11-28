int rdEzt2Print(
 int nEve=5,
 Int_t nFiles  = 10,
 char* file="R6020035.lis", 
 char* inDir   = "./"
 ){ 
  inDir="/star/data05/scratch/eemcdb/muDst/2005/034/";
  file="st_physics_6034014_raw_1020001.MuDst.root";
   gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
   loadSharedLibraries();
   cout << " loading done " << endl;
   
   // create chain    
   chain = new StChain("StChain"); 
   
   printf("adding muDst from '%s' ....\n",file);
   // Now we add Makers to the chain...   
   muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
   TChain* tree=muMk->chain(); assert(tree); 
   int nEntries=tree->GetEntries();
   printf("total eve in chain =%d\n",nEntries);
   printf("in=%s%s=\n",inDir,file);
   //return;
   
   muMk->SetStatus("*",0);
   muMk->SetStatus("MuEvent",1);
   muMk->SetStatus("EztAll",1);
   chain->Init();
   chain->ls(3);
   muMk->printArrays();
   
   printf("All Ezt-branches set\n");
   int eventCounter=0;
   int stat=0;
   
   //---------------------------------------------------
   while ( 1) {// loop over events
     if(eventCounter>=nEve) break;
     eventCounter++;
     chain->Clear();
     stat = chain->Make();
     if(stat) break;
     
     EztEventHeader *eHead= muMk->muDst()->eztHeader();
     EztEmcRawData  *eETow= muMk->muDst()->eztETow();
     EztEmcRawData  *eESmd= muMk->muDst()->eztESmd();
     EztTrigBlob    *eTrig= muMk->muDst()->eztTrig(); 
     printf("\n\n ====================%d  processing  ==============\n", eventCounter);

     eHead->print();
     // eETow->print(0); // 0 -less , 1 -more printing
     // eESmd->print(0); // 0 -less , 1 -more printing
     // eTrig->print(0); // 0 -less , 1 -more printing
     int tMinB=muMk->muDst()->event()->triggerIdCollection().nominal().isTrigger(66007);
     int tEHT10=muMk->muDst()->event()->triggerIdCollection().nominal().isTrigger(66210);
     printf("trig: minB=%d EHT10=%d\n",tMinB,tEHT10);
     // break;
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);

}

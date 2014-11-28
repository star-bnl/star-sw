 {
   // Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine  (fine@bnl.gov). All right reserved",
   // TGenericTable and TIndexTable test macro
    int problemCounter = 0;
    gSystem->Load("libTable");
    struct hit {
     float  energy;     /* energy */
     int    detectorId; /* geometry id */
    };
 
    TGenericTable *allHits = new TGenericTable("hit","hits",1000);
    allHits->Print();
    hit  a;
    memset(&a,0,sizeof(a));
    int i = 0;
    for (i=0; i<100; i++) {
	   a.energy = sin(i*0.1);
	   a.detectorId = i;
	   allHits->AddAt(&a);
    }
    allHits->Print();
    // Draw the histogram for the selected column
    allHits->Draw("energy");
    // Print the table descriptor
    printf("\n\n Print out the hit table descriptor: \n");
    allHits->GetTableDescriptors()->Print(0,10);
    TFile ff("generic.root","RECREATE");
    allHits->Write();
    ff.Write();
    ff.Close();
    allHits->Print(0,10);
    // Create an index table
    TRandom rndIndex;
    TIndexTable indxRandom(allHits);
    TIndexTable indxSeq(allHits);
    if ( indxRandom.GetRowSize() != sizeof(int) ) {
      printf(" The empty index table has been created. The row size = %d\n",indxRandom.GetRowSize());
      problemCounter++;
    }
    for (i=0; i<100; i++) {
          indxRandom.push_back(rndIndex.Integer(100));
          indxSeq.push_back(i);
    }
    TIndexTable::iterator first = indxRandom.begin();
    TIndexTable::iterator last = indxRandom.end();
    i = 0;
    hit  *nextRow = 0;
    for (;first != last; ++first,i++) {
      if ( (int)first != ((hit *)*first)->detectorId )  
      {
         printf(" *** Error ***. %d-th index does point to right row of %s table. It points to %d:%d row instead\n",
         i,indxRandom.GetName(),((hit *)*first)->detectorId,(int)first);
         problemCounter++;
         break;
      }
    }
    first = indxSeq.begin();
    last  = indxSeq.end();
    i = 0;
    hit  *nextRow = 0;
    for (;first != last; ++first,i++) {
      if ( i != ((hit *)*first)->detectorId )  
      {
        printf(" *** Error ***. %d-th index does point to right row of %s table. It points to energy= %f; id=%d:%d row instead\n",
        i,indxSeq.GetName(),((hit *)*first)->energy, ((hit *)*first)->detectorId,(int)first);
        problemCounter++;
        break;
      }
    }
    if (problemCounter) printf("There were several problem detected !!!\n");
    else printf(" no problem have been found\n");
 }
 

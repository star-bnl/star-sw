
/// Code is not complete

#if 0 // test
  {
   StEvent *stEvent= (StEvent *) GetInputDS("StEvent");
   printf("StEvent time=%d, ID=%d, runID=%d\n",(int)stEvent->time(),(int)stEvent->id(),(int)stEvent->runId());
   
   StEvtHddr* fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
   printf("EvtHddr actual event time stamp= %d, yyyy/mm/dd=%d hh/mm/ss=%d\n",
             (int)fEvtHddr->GetUTime(),fEvtHddr->GetDate(),fEvtHddr->GetTime());
   StL0Trigger* L0=(StL0Trigger*)stEvent->l0Trigger();
   printf("L0 token=%d  \n",L0->triggerToken());
   // access EEMC data
   St_DataSet *daq = GetDataSet("StDAQReader");                 assert(daq);
   StDAQReader *fromVictor = (StDAQReader*) (daq->GetObject()); assert(fromVictor);
   StEEMCReader *steemcreader  = fromVictor->getEEMCReader();   assert(steemcreader);
   
   for(int crate=3;crate<=5;crate++) {
     printf("Data for crate=%d\n",crate);
     for(int chan=0;chan<128;chan++) {
       int adc=steemcreader->getTowerAdc(crate,chan);
       //  printf("JJJ EEMC crate=%3d chan=%3d ADC=%5d\n",crate,chan,adc);
       printf("0x%4.4x ",adc);
       if(chan%8==7)printf("\n");
     } 
   }
  }
#endif

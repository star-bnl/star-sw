#include "logging/TxEventLogFactory.h"
#include "logging/TxEventLog.h"
#include "logging/StUcmTasks.h"
#include "logging/StRecordIterator.h"
#include "logging/RecordList.h"
#include "logging/StDbFieldI.h"
#include "logging/StUcmJobs.h"
#include "logging/StUcmEvents.h"
#include <iostream>

using namespace TxLogging;
using namespace std;

int main(int argc , char *argv[]) 
{
   TxEventLog &ucm = *TxEventLogFactory::create("c");
          
   cout << "Total tasks recorded by \"Tasks\" table :=" <<ucm.queryTableSize("Tasks") << endl;
   const char *username = "starreco";
   ucm.setRequesterName(username);
   cout << "Total tasks recorded by \"Tasks\" table for  <" 
        << username  << ">:= "
        << ucm.queryTableSize("Tasks") << endl;
   StUcmTasks &tasks =  *ucm.getTaskList(10);
   Iterator task = tasks.taskIterator();
   StRecord *r = 0;
   if (task.hasNext()) {
       r = task.next();
       cout << "*** Total " << ucm.queryTableSize("Jobs",r)
            <<" jobs were found for the current task" << endl;
       r->printHeader();
       r->print();
       StUcmJobs &jobs = *ucm.getJobList(r,20);
       Iterator  job = jobs.jobIterator();
       ucm.setDbJobID(1);
       {
         StUcmEvents &events=*ucm.getEventList(20);
         Iterator event = events.eventIterator();
         while(event.hasNext() ){
            r = event.next();
            r->print();
         }
       }
       bool doHeader = true;
       while (job.hasNext() ) {
          r = job.next();
          cout << "*** Total " << ucm.queryTableSize("Events",r)
               <<" events were found for the current job" << endl;
          if (doHeader) { 
              r->printHeader(); 
              r->print();
              doHeader = false;
              StUcmEvents &events=*ucm.getEventList(r,20);
              Iterator event = events.eventIterator();
              while(event.hasNext() ){
                 r = event.next();
                 r->print();
              }
          } else {
             r->print();
          }
       }
       ucm.setDbJobID(1);
       cout << " --------------------------------------" << endl;
       ucm.logJobAttribute ("queue","Very Long");
       cout << " --------------------------------------" << endl;
       StUcmEvents &events=*ucm.getEventList(20);
       Iterator event = events.eventIterator();
       while(event.hasNext() ){
          r = event.next();
          r->print();
       }
   }
}

import java.lang.System;
import com.txLogging.*; 
class testUcm {
 public static void main(String [] argv)
 {
   System.out.println("Java / C++ ucm test:");       
   TxEventLog ucm = TxEventLogFactory.create("ucm");
   System.out.println("Total tasks recorded by \"Tasks\" table:");       
   System.out.println(ucm.queryTableSize("Tasks"));
   String username = "starreco";
   ucm.setRequesterName(username);
   System.out.println("Total tasks recorded by \"Tasks\" table for the " + username + " user:" );     
   System.out.println(ucm.queryTableSize("Tasks"));
   StUcmTasks tasks =  ucm.getTaskList(10);
   Iterator task = tasks.taskIterator();
   StRecord r;
   if (task.hasNext()) {
       r = task.next();
       System.out.println("*** Total " + ucm.queryTableSize("Jobs",r)
           + " jobs were found for the current task");
       r.printHeader();
       r.print();
       StUcmJobs jobs = ucm.getJobList(r,20);
       Iterator  job = jobs.jobIterator();
       int doHeader = 1;
       while (job.hasNext() ) {
          r = job.next();
          System.out.println("*** Total " + ucm.queryTableSize("Events",r)
           + " events were found for the current task");
          if (doHeader==1) { 
              r.printHeader(); 
              doHeader = 0;
              StUcmEvents events=ucm.getEventList(r,20);
              Iterator event = events.eventIterator();
              while(event.hasNext() ){
                 r = event.next();
                 r.print();
              }
          } else {
              r.print();
          }
       }
    }
     System.out.println(" Test GetJobID");
     System.out.println(" Test GetJobID " +  ucm.getJobId("starreco","6d451854-408d-4629-aa25-1bf609001ab5",2));
  }
 }
 

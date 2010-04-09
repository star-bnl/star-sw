#include "TxEventLog.h"
#include "TxEventLogFactory.h"
#include <unistd.h>


int main () {
  TxLogging::TxEventLog* el1 = TxLogging::TxEventLogFactory::create();
  el1->setBrokerTaskID ("RoopaBrokerTaskID1234");
  el1->setBrokerJobID (1234);
  el1->setRequesterName ("Roopa");

  el1->logEvent ("com.txcorp.ucm.newtask", 
		 "brokerID='1', taskName='Roopa's task', taskDescription='Test task from TxEventLogMain', taskSize='1'");
  el1->logEvent ("com.txcorp.ucm.updatetask",
		 "taskRemainSize='1'");

  el1->logEvent ("com.txcorp.ucm.addjob",
		 "queue='some queue', queuePosition='2'");
  el1->logEvent ("com.txcorp.ucm.updatejob",
		 "stateID='4'"); 

  el1->setJobSubmitLocation ("some location");
  el1->setJobSubmitID ("some job id");
  el1->setJobSubmitState (TxLogging::TxEventLog::STAGEIN);

  el1->logStart ();
  sleep (1);
  el1->logEvent ("test log message");
  sleep (1);
  el1->logEvent ("test key", "test value");
  sleep (1);
  el1->setJobSubmitLocation ("GRAM");
  sleep (1);
  el1->setJobSubmitState (TxLogging::TxEventLog::DONE);
  sleep (1);
  el1->setJobSubmitID ("Job ID asdfasdf");
  el1->logJobAttribute ("queue","Very Long");
  sleep (1);
  el1->logEnd ();
  delete (el1);

  return 0;
}

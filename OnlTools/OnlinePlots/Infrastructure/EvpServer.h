#ifndef EVENTLOOP_H
#define EVENTLOOP_H

#include <iostream>
#include <stdlib.h>
#include <string.h>
using namespace std;

#include <TSystem.h>
#include <TDatime.h>
#include <TThread.h>
#include <TMapFile.h>
#include "ServerGui.h"



#define NO_RTS_LOG

#include <time.h>


#include "RunStatus.h"
#include "ServerStatus.h"

#include "GroupCollection.h"


//*********************************************************************

#ifndef NEW_DAQ_READER
  class evpReader;
#endif
class HistoHandler;
class ServerGui;    

class EvpServer {

private:

    int  mGoFlag;        // flag = 0 - "pause"
    char mTarget[1024];  // File or directory name to pass to event reader
    int  mDebugLevel;
    int mHistoSaved;
    bool mDoEndOfRun;
    bool mDoEndOfRunQuit;  // quit after end of run
    bool mQuit;
    int mMaxEventsPerRun;
    // Event reader part
    evpReader *evp; //!

    GroupCollection mGroups;
public:
    HistoHandler* mHistoHandler;
    TMapFile* mFile;
    RunStatus* mRS;
    ServerStatus* mSS;

    ServerGui* mGui;

    EvpServer(bool gui=true);
    virtual ~EvpServer();

    void run();
    int NextEvent(void);
    void MainLoop(void);
    void UpdateInfo();

    void SetEnabled(bool b=true);
    void SetNewTarget(char *);
    void SetLive();
    void SetStopFlag(void);
    void SetGoFlag(void);
    void SetDefaults(void);
    int  GetGoFlag(void){return mGoFlag;}
    char *GetTarget(void){return mTarget;}

    void SetMaxEventsPerRun(int n) { mMaxEventsPerRun = n; }
    void SetDoEndOfRun(bool b) { mDoEndOfRun=b; }
    void SetDoEndOfRunQuit(bool b) { mDoEndOfRunQuit=b; }
    void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
    int  GetDebugLevel(void){return mDebugLevel;}
    void SetPhiAngleMap(void);
    void Print();
    void Reset();
    void Save(const char* filename);
    void MakePS();
    void Update();
    void LaunchEndOfRunAction(long run);

    ClassDef(EvpServer,0) ;
};
//*********************************************************************

#endif


/***************************************************************************
 *
 * $Id: EvpServer.h,v 1.1 2009/01/23 16:10:54 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpServer.h,v $
 * Revision 1.1  2009/01/23 16:10:54  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.4  2008/12/19 15:51:16  dkettler
 * Added new daqReader
 *
 * Revision 1.3  2007/04/25 17:52:29  laue
 * Minor updates
 *
 * Revision 1.2  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/


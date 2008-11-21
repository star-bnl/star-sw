//////////////////////////////////////////////////////////////////////
//                                                                  //
// author: Alexander Stolpovsky                                     //
// modifications: Oleksandr Grebenyuk                               //
//                                                                  //
//////////////////////////////////////////////////////////////////////

#include "StPi0AnalysisUtilDB.h"

#include <TSQLServer.h>
#include <TSQLRow.h>
#include <TSQLResult.h>

#include <TNtuple.h>
#include <TTree.h>
#include <TH2S.h>
#include <TH2F.h>
#include <TRandom.h>
#include <TDatime.h>

#include <vector>
#include <algorithm>
#include <math.h>
#include <iostream>
using namespace std;

/* Trigger IDs
---------------------
RUN III
dAu @ 200 GeV
    MinBias:     2001, 2003
    HighTower-1: 2201
    HighTower-2: 2202
pp @ 200 GeV
    MinBias:     1000
    HighTower-1: 1101
    HighTower-2: 1102
---------------------
RUN IV
AuAu @ 200 GeV
    MinBias:     15003, 15007
    HighTower-1: 15203
    HighTower-2: none
pp @ 200 GeV
    pp-minbias: 45010, 10
    bht-1-slow: 45201
    bht-2-slow: 45202
---------------------
2005
pp @ 200 GeV
    ppMinBias: 96011
    bemc-ht1-mb: 96201
    bemc-ht2-mb: 96211
---------------------
*/
// Trigger IDs:              dAu2003           pp2003         AuAu2004         pp2004        pp2005
Int_t triggerIDsMB_tmp[] =  {2001, 2003,       1000,          15003, 15007,    45010, 10,    96011,     0};
Int_t triggerIDsHT1_tmp[] = {2201,             1101,          15203,           45201,        96201,     0};
Int_t triggerIDsHT2_tmp[] = {2202,             1102,                           45202,        96211,     0};

Int_t *triggerIDsMB = &triggerIDsMB_tmp[0];
Int_t *triggerIDsHT1 = &triggerIDsHT1_tmp[0];
Int_t *triggerIDsHT2 = &triggerIDsHT2_tmp[0];

// DB on RCF:
//const Char_t *DBConnectString = "mysql://dbx.star.bnl.gov:3316/RunLog_onl"; // good for pp2005 prescales
const Char_t *DBConnectString = "mysql://onlsun1.star.bnl.gov:3401/RunLog"; // see http://www.star.bnl.gov/STAR/comp/db/onlsun1.html
/*
From http://www.star.bnl.gov/STAR/html/daq_l/DBscripts/thresh.pl
#
#  Server and Port depend on run year:
#
#  Year : 2006  onldb:3501
#         2005  onlsun1:3404
#         2004  onlsun1:3403
#         2003  onlsun1:3402
#         2002  onlsun1:3401
#
*/
// See also http://drupal.star.bnl.gov/STAR/comp/db/online-sever-port-map/

// DB at PDSF:
//const Char_t *DBConnectString = "mysql://stardb.nersc.gov:3306/RunLog_onl";

Int_t getPrescalesFromDB(Int_t runNumber, Int_t &ps_minBias, Int_t &ps_ht1, Int_t &ps_ht2, Bool_t print, const Int_t *ourMinBiasTriggers, const Int_t *ourHt1Triggers, const Int_t *ourHt2Triggers) {
    if (!ourMinBiasTriggers) ourMinBiasTriggers = &triggerIDsMB[0];
    if (!ourHt1Triggers) ourHt1Triggers = &triggerIDsHT1[0];
    if (!ourHt2Triggers) ourHt2Triggers = &triggerIDsHT2[0];
    TSQLServer *db = 0;
    int nTry = 0;
    int maxTries = 10;
    while(!db && (nTry < maxTries)) {
	db = TSQLServer::Connect(DBConnectString, "", "");
	// time delay for db connection
	int delayInSeconds = 10; // sometimes 5 is not enough
	time_t startTime, curTime;
	time(&startTime);
	if (!db) {
	    if (print) cout << "Connection to db failed. Re-trying after " << delayInSeconds << " seconds..." << endl;
	    do {
		time(&curTime);
	    } while((curTime - startTime) < delayInSeconds);
	}
	nTry+=1;
    }
    if (!db) {
	if (print) cout << "Could not connect to db server after " << nTry << " tries!" << endl;
	return 1;
    } else {
	if (print && (nTry > 1)) cout << "Connected to db successfully after " << nTry << " tries" << endl;
    }
    TString query("select psL0 from L0TriggerInfo where runNumber = ");
    query+=runNumber;
    TString queryMinBias(query);
    queryMinBias.Append(" and (");
    Int_t trigInd = 0;
    Bool_t needSeparator = false;
    while (ourMinBiasTriggers && (ourMinBiasTriggers[trigInd] != -1) && (ourMinBiasTriggers[trigInd] != 0)) {
	if (needSeparator) queryMinBias.Append(" or ");
	queryMinBias.Append("offlineTriggerId = ");
	queryMinBias += ourMinBiasTriggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryMinBias.Append(")");
    TString queryHt1(query);
    queryHt1.Append(" and (");
    trigInd = 0;
    needSeparator = false;
    while (ourHt1Triggers && (ourHt1Triggers[trigInd] != -1) && (ourHt1Triggers[trigInd] != 0)) {
	if (needSeparator) queryHt1.Append(" or ");
	queryHt1.Append("offlineTriggerId = ");
	queryHt1 += ourHt1Triggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryHt1.Append(")");
    TString queryHt2(query);
    queryHt2.Append(" and (");
    trigInd = 0;
    needSeparator = false;
    while (ourHt2Triggers && (ourHt2Triggers[trigInd] != -1) && (ourHt2Triggers[trigInd] != 0)) {
	if (needSeparator) queryHt2.Append(" or ");
	queryHt2.Append("offlineTriggerId = ");
	queryHt2 += ourHt2Triggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryHt2.Append(")");
    if (print) cout << "MB query: " << queryMinBias << endl;
    if (print) cout << "HT1 query: " << queryHt1 << endl;
    if (print) cout << "HT2 query: " << queryHt2 << endl;
    TSQLRow* row = 0;
    TSQLResult* result = db->Query(queryMinBias);
    if(!result) {
	if(print) cout << "SQL query for MinBias failed... setting prescale to zero" << endl;
	ps_minBias = 0;
    } else {
	row = 0;
	while((row = result->Next())) {
	    ps_minBias = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Prescale for MinBias = " << row->GetField(0) << " (" << ps_minBias << ")" << endl;
	}
    }
    result = db->Query(queryHt1);
    if(!result) {
	if(print) cout << "SQL query for HT1 failed... setting prescale to zero" << endl;
	ps_ht1 = 0;
    } else {
	row = 0;
        while((row = result->Next())) {
	    ps_ht1 = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Prescale for HT1 = " << row->GetField(0) << " (" << ps_ht1 << ")" << endl;
	}
    }
    result = db->Query(queryHt2);
    if(!result) {
	if(print) cout << "SQL query for HT2 failed... setting prescale to zero" << endl;
	ps_ht2 = 0;
    } else {
	row = 0;
	while((row = result->Next())) {
	    ps_ht2 = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Prescale for HT2 = " << row->GetField(0) << " (" << ps_ht2 << ")" << endl;
	}
    }
    db->Close();
    delete db;
    return 0;
}

Int_t getEventNumbersFromDB(Int_t runNumber, Int_t &evNum_minBias, Int_t &evNum_ht1, Int_t &evNum_ht2, bool print, const Int_t *ourMinBiasTriggers, const Int_t *ourHt1Triggers, const Int_t *ourHt2Triggers) {
    if (!ourMinBiasTriggers) ourMinBiasTriggers = &triggerIDsMB[0];
    if (!ourHt1Triggers) ourHt1Triggers = &triggerIDsHT1[0];
    if (!ourHt2Triggers) ourHt2Triggers = &triggerIDsHT2[0];
    TSQLServer *db = 0;
    int nTry = 0;
    int maxTries = 10;
    while(!db && (nTry < maxTries)) {
	db = TSQLServer::Connect(DBConnectString, "", "");
	// time delay for db connection
	int delayInSeconds = 10; // sometimes 5 is not enough
	time_t startTime, curTime;
	time(&startTime);
	if (!db) {
	    if (print) cout << "Connection to db failed. Re-trying after " << delayInSeconds << " seconds..." << endl;
	    do {
		time(&curTime);
	    } while((curTime - startTime) < delayInSeconds);
	}
	nTry+=1;
    }
    if (!db) {
	if (print) cout << "Could not connect to db server after " << nTry << " tries!" << endl;
	return 1;
    } else {
	if (print && (nTry > 1)) cout << "Connected to db successfully after " << nTry << " tries" << endl;
    }
    TString query("select numberOfEvents from L0TriggerInfo where runNumber = ");
    query+=runNumber;
    TString queryMinBias(query);
    queryMinBias.Append(" and (");
    Int_t trigInd = 0;
    Bool_t needSeparator = false;
    while (ourMinBiasTriggers && (ourMinBiasTriggers[trigInd] != -1)) {
	if (needSeparator) queryMinBias.Append(" or ");
	queryMinBias.Append("offlineTriggerId = ");
	queryMinBias += ourMinBiasTriggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryMinBias.Append(")");
    TString queryHt1(query);
    queryHt1.Append(" and (");
    trigInd = 0;
    needSeparator = false;
    while (ourHt1Triggers && (ourHt1Triggers[trigInd] != -1)) {
	if (needSeparator) queryHt1.Append(" or ");
	queryHt1.Append("offlineTriggerId = ");
	queryHt1 += ourHt1Triggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryHt1.Append(")");
    TString queryHt2(query);
    queryHt2.Append(" and (");
    trigInd = 0;
    needSeparator = false;
    while (ourHt2Triggers && (ourHt2Triggers[trigInd] != -1)) {
	if (needSeparator) queryHt2.Append(" or ");
	queryHt2.Append("offlineTriggerId = ");
	queryHt2 += ourHt2Triggers[trigInd];
	needSeparator = true;
	trigInd++;
    }
    queryHt2.Append(")");
    TSQLRow* row = 0;
    TSQLResult* result = db->Query(queryMinBias);
    if(!result) {
	if(print) cout << "SQL query for MinBias failed... setting events number to -1" << endl;
	evNum_minBias = -1;
    } else {
	row = 0;
	while((row = result->Next())) {
	    evNum_minBias = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Events number for MinBias = " << row->GetField(0) << " (" << evNum_minBias << ")" << endl;
	}
    }
    result = db->Query(queryHt1);
    if(!result) {
	if(print) cout << "SQL query for HT1 failed... setting events number to -1" << endl;
	evNum_ht1 = -1;
    } else {
	row = 0;
        while((row = result->Next())) {
	    evNum_ht1 = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Events number for HT1 = " << row->GetField(0) << " (" << evNum_ht1 << ")" << endl;
	}
    }
    result = db->Query(queryHt2);
    if(!result) {
	if(print) cout << "SQL query for HT2 failed... setting events number to -1" << endl;
	evNum_ht2 = -1;
    } else {
	row = 0;
	while((row = result->Next())) {
	    evNum_ht2 = (Int_t)atof(row->GetField(0));
	    if(print) cout << "Events number for HT2 = " << row->GetField(0) << " (" << evNum_ht2 << ")" << endl;
	}
    }
    db->Close();
    delete db;
    return 0;
}

Int_t getRunTimesFromDB(Int_t runNumber, Int_t &date, Int_t &time, bool print) {date=1;time=2;return 0;
    TSQLServer *db = 0;
    int nTry = 0;
    int maxTries = 10;
    while(!db && (nTry < maxTries)) {
	db = TSQLServer::Connect(DBConnectString, "", "");
	// time delay for db connection
	int delayInSeconds = 10; // sometimes 5 is not enough
	time_t startTime, curTime;
	::time(&startTime);
	if (!db) {
	    if (print) cout << "Connection to db failed. Re-trying after " << delayInSeconds << " seconds..." << endl;
	    do {
		::time(&curTime);
	    } while((curTime - startTime) < delayInSeconds);
	}
	nTry+=1;
    }
    if (!db) {
	if (print) cout << "Could not connect to db server after " << nTry << " tries!" << endl;
	return 1;
    } else {
	if (print && (nTry > 1)) cout << "Connected to db successfully after " << nTry << " tries" << endl;
    }
    TString query("select beginTime from L0TriggerInfo where runNumber = ");
    query+=runNumber;
    query+= ";";
    if (print) cout << "Query: "<< query << endl;
    TSQLRow* row = 0;
    TSQLResult* result = db->Query(query);
    if(!result) {
	if(print) cout << "SQL query failed... setting date and time to -1" << endl;
	date = -1;
	time = -1;
    } else {
	row = 0;
	if((row = result->Next())) {
	    TDatime d(row->GetField(0));
	    if(print) cout << "Timestamp = " << row->GetField(0) << " (" << d.AsString() << ")" << endl;
	    date = d.GetDate();
	    time = d.GetTime();
	}
    }
    db->Close();
    delete db;
    return 0;
}

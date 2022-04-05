#include <string>
#include <iostream>
#include <sstream>

#include <TSQLServer.h> 
#include <TSQLResult.h> 
#include <TSQLRow.h> 

template <class T>
inline Int_t to_int(const T& t)
{
    Int_t res;
    std::stringstream ss(t);
    ss >> res;
    return res;
}

template <class T>
inline Double_t to_double(const T& t)
{
    Double_t res;
    std::stringstream ss(t);
    ss >> res;
    return res;
}


void ftpc_sqldraw(std::string dbpath = "Conditions_ftpc/ftpcTemps/extra1East", std::string minTime = "2009-06-16 00:00:00", 
	std::string maxTime = "2009-06-29 23:59:59") {

	std::string::size_type pos = dbpath.find_last_not_of("/");
	if ( pos == (dbpath.length()-1) ) {
		dbpath.append("/");
	}

	std::stringstream ss(dbpath); 
	std::string s;

	std::string mDatabase, mTable, mParam;
	std::getline(ss, mDatabase, '/');
	std::getline(ss, mTable, '/');
	std::getline(ss, mParam, '/');

	std::string dsn = "mysql://onldb.starp.bnl.gov:3502/";
	dsn.append(mDatabase);
	TSQLServer *db = 0;
	db = TSQLServer::Connect(dsn.c_str(),"staruser", "");
	if (!db) {
		std::cerr << "Error: cannot connect to database!" << std::endl;
	}

    TSQLRow *row;
    TSQLResult *res;

    std::string sql = "SELECT MIN(UNIX_TIMESTAMP(beginTime)), MAX(UNIX_TIMESTAMP(beginTime)), COUNT(beginTime), MIN(";
	sql.append(mParam);
	sql.append("), MAX(");
	sql.append(mParam);
	sql.append(") FROM ");
	sql.append(mTable);
    sql.append(" WHERE beginTime > '");
	sql.append(minTime);
	sql.append("' AND beginTime < '");
	sql.append(maxTime);
	sql.append("'");

    res = db->Query(sql.c_str());
	int nrows = res->GetRowCount();
	if (nrows <= 0) {
		std::err << "Error: no entries for selected period!" << std::endl;
		return;
	}

    row = res->Next();
	
	if (row->GetField(0) == NULL || row->GetField(1) == NULL || row->GetField(2) == NULL || row->GetField(3) == NULL || row->GetField(4) == NULL) {
		std::cerr << "Error: count query returned null, probably no entries for selected period" << std::endl;
		return;
	}

    Int_t timeMin = to_int(row->GetField(0));
    Int_t timeMax = to_int(row->GetField(1));
    Int_t timeDiff = timeMax - timeMin;
    Int_t timeNbins = to_int(row->GetField(2));

//    Double_t valMin = to_double(row->GetField(3));
//    Double_t valMax = to_double(row->GetField(4));
    Double_t valMin = 20.0;
    Double_t valMax = 30.0;
	
    TDatime dh(timeMin);

    delete row;
    delete res;

	std::string h1title;
	h1title.append(mParam);
	h1title.append("(");
	h1title.append(mDatabase);
	h1title.append("/");
	h1title.append(mTable);
	h1title.append(")");
	h1title.append(" vs. beginTime");

	gStyle->SetOptStat(0);

	TH2D* h1 = new TH2D("th2d", h1title.c_str(), timeNbins, 0, timeDiff+1, 100, valMin - fabs(valMin*0.01), valMax + fabs(valMax*0.01));
	h1->SetStats(0);
	h1->SetTitle(h1title.c_str());
    h1->GetXaxis()->SetTimeOffset(dh.Convert());
    h1->GetXaxis()->SetTimeDisplay(1);
    h1->GetXaxis()->SetTimeFormat("#splitline{%Y-%m-%d}{%H:%M:%S}");
    h1->GetXaxis()->SetLabelSize(0.02);
    h1->GetXaxis()->SetLabelOffset(0.02);
    h1->SetMarkerSize(0.8);
    h1->SetMarkerStyle(20);
    h1->SetMarkerColor(4);

	std::string sql = "SELECT UNIX_TIMESTAMP(beginTime), ";
	sql.append(mParam);
	sql.append(" FROM ");
	sql.append(mTable);
	sql.append(" WHERE beginTime > '");
	sql.append(minTime);
	sql.append("' AND beginTime < '");
	sql.append(maxTime);
	sql.append("' ORDER BY beginTime ASC");

    res = db->Query(sql.c_str());
	
	nrows = res->GetRowCount();
    std::cout << "Got " << nrows << " rows from database, please wait...\n";
	if (nrows > 500) {
		std::cout << "[found many rows, it may take a few minutes to create histogram]" << std::endl;
	}

    int i = 0;
    while ((row = res->Next())) {
        i++;
        if (i%100 == 0) {
            std::cout << "  working on " << i << "th row \n";
        }
        h1->Fill(to_int(row->GetField(0)) - timeMin, to_double(row->GetField(1)));
    }
	
   delete res;
   delete db;

   h1->Draw("L");	
	
}


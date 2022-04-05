//
// macro to transfer data from old geometry tables to new data base structures
//

#define NUMBER_OF_WAFERS 216
#include "/afs/rhic/star/packages/DEV/StRoot/StDbLib/StDbDefs.hh"

void insertAlignedGeom(char* unixTime = 0, Bool_t write = kFALSE, Bool_t writeHtml = kTRUE)
{
  int numberOfBarrels = 3;
  int numberOfLadders[3] = {8,12,16};
  int numberOfWafers[3] = {4,6,7};
  int barrelRadius[6] = {6.125,7.185,10.185,11.075,13.995,14.935};
  double theta_xy =  0.0;  
  double theta_xz = 0.0;
  double theta_yz = 0.0;
  double delta_x = 0.2332;
  double delta_y = 0.0;
  double delta_z = 0;

  if (writeHtml) {
    ofstream file("svt_geometry_comp.html");
    //ofstream file("svt_geometry_after3.html");
    //ofstream file2("svt_geometry_before2.html");
  }

  ntuple = new TNtuple("ntuple","ntuple","x:y:z:xa:ya:za:nx:ny:nz:nxa:nya:nza:dx:dy:dz:dxa:dya:dza:tx:ty:tz:txa:tya:tza:barrel:ladder:wafer");
  float nt[27];

  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  
  // DB-specific libs
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("geometry");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StSvtDbMaker");
  gSystem->Load("StSvtClassLibrary");
      
  chain  = new StChain("StChain");
  
  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
  StSvtDbMaker *svtDbMk  = new StSvtDbMaker("svtDb");
  
  // Make reaquests for data
  // choose timestamp 
  // dbMk->SetDateTime(20010617,000000); // pp 2001
  //  dbMk->SetDateTime(20011023,000000); // pp 2001 - simu
  dbMk->SetDateTime(20030102,000000);

  chain->Init();
  chain->InitRun(0);
  svtDbMk->InitRun(0);
  
  chain->Make();

  St_DataSet* dataSet = chain->GetDataSet("StSvtGeometry");
  StSvtGeometry* mSvtGeom;
  if (dataSet)
    mSvtGeom = (StSvtGeometry*)dataSet->GetObject();
  int* rowIDs=new int[NUMBER_OF_WAFERS];
  svtWafersPosition_st *wafersPosition;

  if (mSvtGeom) { // if geometry object exists
    
    if (writeHtml) {
      file << "<p>" << endl;
      file << "<b> Time stamp = " << "2003-10-31 00:00:00" << endl;
      file << "<p>" << endl;
      file << "<table BORDER=1 WIDTH=\"100%\" >" << endl;
      
      file << "<tr>" ;
      file << "<td><center><b><font color=\"#FF0000\">barrel</font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\">ladder</font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\">wafer</font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> x </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> y </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> z </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> nx </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> ny </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> nz </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> dx </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> dy </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> dz </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> tx </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> ty </font></b></center></td>"<< endl;
      file << "<td><center><b><font color=\"#FF0000\"> tz </font></b></center></td>"<< endl;
      file << "</tr>" << endl;
    }

    // define new table to be stored
    wafersPosition = new svtWafersPosition_st[NUMBER_OF_WAFERS];

    // define IDs
    int layer, HardWarePos, index = 0;
    StSvtWaferGeometry* waferGeom;

    for (int barrel =1;barrel <= numberOfBarrels;barrel++) {
      for (int ladder =1;ladder <= numberOfLadders[barrel-1];ladder++) {
	for (int wafer =1;wafer <= numberOfWafers[barrel-1];wafer++) {	  
	  
	  index = mSvtGeom->getWaferIndex(barrel,ladder,wafer);
	  waferGeom = (StSvtWaferGeometry*)mSvtGeom->at(index);

	  if (!waferGeom) {
	    cout << "No wafer geometry information! Fatal error!!!" << endl;
	    return;
	  }
	      

	  switch (barrel) {
	    
	  case 1:
	    if (ladder%2)
	      layer = 2;
	    else
	      layer = 1;
	    
	    break;
	    
	  case 2:    
	    if (ladder%2)
	      //layer = 3; // MDC4
	      layer = 4; // real installation
	    else
	      //layer = 4; // MDC4
	      layer = 3; // real installation
	    
	    break;
	    
	  case 3:
	    if (ladder%2)
	      layer = 6;
	    else
	      layer = 5;
	    
	    break;
	  }
	  	  
	  wafersPosition[index].ID = 1000*layer+100*wafer+ladder;
	  rowIDs[index]=index;

	  float dx, dy, dz, nx, ny, nz, tx, ty, tz;

	  // xy rotation
	  nx = waferGeom->n(0)*cos(theta_xy) - waferGeom->n(1)*sin(theta_xy);
	  ny = waferGeom->n(0)*sin(theta_xy) + waferGeom->n(1)*cos(theta_xy);
	  nz = waferGeom->n(2);
	  dx = waferGeom->d(0)*cos(theta_xy) - waferGeom->d(1)*sin(theta_xy);
	  dy = waferGeom->d(0)*sin(theta_xy) + waferGeom->d(1)*cos(theta_xy);
	  dz = waferGeom->d(2);
	  tx = waferGeom->t(0)*cos(theta_xy) - waferGeom->t(1)*sin(theta_xy);
	  ty = waferGeom->t(0)*sin(theta_xy) + waferGeom->t(1)*cos(theta_xy);
	  tz = waferGeom->t(2);
	  
	  // xz rotation
	  nx = nx*cos(theta_xz) - nz*sin(theta_xz);
	  ny = ny;
	  nz = nx*sin(theta_xz) + nz*cos(theta_xz);
	  dx = dx*cos(theta_xz) - dz*sin(theta_xz);
	  dy = dy;
	  dz = dx*sin(theta_xz) + dz*cos(theta_xz);
	  tx = tx*cos(theta_xz) - tz*sin(theta_xz);
	  ty = ty;
	  tz = tx*sin(theta_xz) + tz*cos(theta_xz);
	  
	  // yz rotation
	  nx = nx;
	  ny = ny*cos(theta_yz) - nz*sin(theta_yz);
	  nz = ny*sin(theta_yz) + nz*cos(theta_yz);
	  dx = dx;
	  dy = dy*cos(theta_yz) - dz*sin(theta_yz);
	  dz = dy*sin(theta_yz) + dz*cos(theta_yz);
	  tx = tx;
	  ty = ty*cos(theta_yz) - tz*sin(theta_yz);
	  tz = ty*sin(theta_yz) + tz*cos(theta_yz);
	  
	  float rx, ry, rz, d;
	  
	  switch (barrel) { 
	    
	  case 1:
	    d = 3.1525*(2*(2.5-wafer));
	    break;
	    
	  case 2:
	    d = 3.1525*(2*(3.5-wafer));
	    break;
	    
	  case 3:
	    d = 3.1525*(2*(4-wafer));
	    break;
	  }
	  
	  //xy rotation
	  rx = waferGeom->x(0)*cos(theta_xy) - waferGeom->x(1)*sin(theta_xy);
	  ry = waferGeom->x(0)*sin(theta_xy) + waferGeom->x(1)*cos(theta_xy);
	  rz = waferGeom->x(2);
	  
	  // xz rotation
	  rx = rx + d*sin(theta_xz);
	  ry = ry;
	  rz = rz + d*(1-cos(theta_xz));
	  
	  // yz rotation
	  rx = rx;
	  ry = ry + d*sin(theta_yz);
	  rz = rz + d*(1-cos(theta_yz));
	  
	  // shifts
	  rx += delta_x;
	  ry += delta_y;
	  rz += delta_z;
	  
	  wafersPosition[index].driftDirection[0] = dx;
	  wafersPosition[index].driftDirection[1] = dy;
	  wafersPosition[index].driftDirection[2] = dz;
	  wafersPosition[index].normalDirection[0] = nx;
	  wafersPosition[index].normalDirection[1] = ny;
	  wafersPosition[index].normalDirection[2] = nz;
	  wafersPosition[index].transverseDirection[0] = tx;
	  wafersPosition[index].transverseDirection[1] = ty;
	  wafersPosition[index].transverseDirection[2] = tz;
	  
	  wafersPosition[index].centerPosition[0] = rx;
	  wafersPosition[index].centerPosition[1] = ry;
	  wafersPosition[index].centerPosition[2] = rz;
	  
	  nt[0] = waferGeom->x(0);
	  nt[1] = waferGeom->x(1);
	  nt[2] = waferGeom->x(2);
	  nt[3] = rx;
	  nt[4] = ry;
	  nt[5] = rz;
	  nt[6] = waferGeom->n(0);
	  nt[7] = waferGeom->n(1);
	  nt[8] = waferGeom->n(2);
	  nt[9] = nx;
	  nt[10] = ny;
	  nt[11] = nz;
	  nt[12] = waferGeom->d(0);
	  nt[13] = waferGeom->d(1);
	  nt[14] = waferGeom->d(2);
	  nt[15] = dx;
	  nt[16] = dy;
	  nt[17] = dz;
	  nt[18] = waferGeom->t(0);
	  nt[19] = waferGeom->t(1);
	  nt[20] = waferGeom->t(2);
	  nt[21] = tx;
	  nt[22] = ty;
	  nt[23] = tz;
	  nt[24] = barrel;
	  nt[25] = ladder;
	  nt[26] = wafer;
	  ntuple->Fill(nt);
	  
	  if (writeHtml) {
	    file << "<tr>" 
		 << "<td><center><b>" << barrel << "</b></center></td>" 
		 << "<td><center><b>" << ladder << "</b></center></td>" 
		 << "<td><center><b>" << wafer << "</b></center></td>" 
		 << "<td><center><b>" << rx << "</b></center></td>" 
		 << "<td><center><b>" << ry << "</b></center></td>" 
		 << "<td><center><b>" << rz << "</b></center></td>" 
		 << "<td><center><b>" << nx << "</b></center></td>" 
		 << "<td><center><b>" << ny << "</b></center></td>" 
		 << "<td><center><b>" << nz << "</b></center></td>" 
		 << "<td><center><b>" << dx << "</b></center></td>" 
		 << "<td><center><b>" << dy << "</b></center></td>" 
		 << "<td><center><b>" << dz << "</b></center></td>" 
		 << "<td><center><b>" << tx << "</b></center></td>" 
		 << "<td><center><b>" << ty << "</b></center></td>" 
		 << "<td><center><b>" << tz << "</b></center></td>" 
		 << "</tr></b>" << endl;
	    
	    //file2 << "<tr>" 
	    file << "<tr>" 
		  << "<td><center>" << barrel << "</center></td>" 
		  << "<td><center>" << ladder << "</center></td>" 
		  << "<td><center>" << wafer << "</center></td>" 
		  << "<td><center>" << waferGeom->x(0) << "</center></td>" 
		  << "<td><center>" << waferGeom->x(1) << "</center></td>" 
		  << "<td><center>" << waferGeom->x(2) << "</center></td>" 
		  << "<td><center>" << waferGeom->n(0) << "</center></td>" 
		  << "<td><center>" << waferGeom->n(1) << "</center></td>" 
		  << "<td><center>" << waferGeom->n(2) << "</center></td>" 
		  << "<td><center>" << waferGeom->d(0) << "</center></td>" 
		  << "<td><center>" << waferGeom->d(1) << "</center></td>" 
		  << "<td><center>" << waferGeom->d(2) << "</center></td>" 
		  << "<td><center>" << waferGeom->t(0) << "</center></td>" 
		  << "<td><center>" << waferGeom->t(1) << "</center></td>" 
		  << "<td><center>" << waferGeom->t(2) << "</center></td>" 
		  << "</tr>" << endl;
	  }

	  /*
	    cout << "barrel = " << barrel << ", ladder = " << ladder << ", wafer = " << wafer << endl;
	    cout << "x = " << rx << ", y = " << ry << ", z = " << rz << endl;
	    cout << "nx = " << nx << ", ny = " << ny << ", nz = " << nz << endl;
	    cout << "dx = " << dx << ", dy = " << dy << ", dz = " << dz << endl;
	    cout << "tx = " << tx << ", ty = " << ty << ", tz = " << tz << endl;
	  */	            
	  
	  cout << "index = " << index << ", ID = " << wafersPosition[index].ID << endl; 
	}
      }
    }

    if (writeHtml) 
      file << "</table>" << endl; 
  }
    
  if (write) {
    StDbManager* mgr=StDbManager::Instance();
    
    // connect to Geometry_svt database
    //StDbConfigNode* svtGeomNode = mgr->initConfig("Geometry_svt","reconV1"); // reconV1 might change in the future
    StDbConfigNode* svtGeomNode = mgr->initConfig(dbGeometry,dbSvt);       // use this line then
    
    // print out some info
    //mgr->setVerbose(true);
    //svtGeomNode->printTree(0);
    
    // request db-table by name
    StDbTable* svtGeomTable = svtGeomNode->addDbTable("svtWafersPosition");
    svtGeomTable->SetTable((char*)wafersPosition,NUMBER_OF_WAFERS,rowIDs);
    
    mgr->setStoreTime(unixTime);
    cout<<" Will attempt store with timestamp="<<mgr->getDateStoreTime()<<endl;  
    mgr->storeDbTable(svtGeomTable);
    
    delete [] wafersPosition;
  }     
}

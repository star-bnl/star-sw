// SusyLesHouches.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Peter Skands, Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

#include "SusyLesHouches.h"


//*********

// Main routine to read in SLHA and LHEF+SLHA files

int SusyLesHouches::readFile(string slhaFile) {

  // Check that input file is OK.
  int iFailFile=0;
  spectrumFile=slhaFile;
  const char* cstring = slhaFile.c_str();
  ifstream file(cstring);  
  if (!file) {
    message(2,"readFile","SLHA file "+slhaFile+" not found",0);
    return -1;
    slhaRead=false;
  }  

  //Print header if not already done
  if (! headerPrinted) printHeader();

  if (verbose >= 2) message(0,"readFile","parsing "+slhaFile,0);

  //Initial values for read-in variables.  
  slhaRead=true;
  lhefRead=false;
  lhefSlha=false;
  string line="";
  string blockIn="";
  string decay="";
  string comment="";
  string blockName="";
  string pdgName="";
  int pdgCode=0;
  double width=0.0;

  //Initialize line counter
  int iLine=0;

  // Read in one line at a time.
  while ( getline(file, line) ) {
    iLine++;

    //Rewrite string in lowercase
    for (unsigned int i=0;i<line.length();i++) line[i]=tolower(line[i]);

    //Detect whether read-in is from a Les Houches Event File (LHEF).
    if (line.find("<leshouches") != string::npos 
	|| line.find("<slha") != string::npos) {
      lhefRead=true;
    }
    if (lhefRead) {
      //If LHEF, detect when <slha> tag reached.
      if (line.find("<slha")) lhefSlha=true;
      //If LHEF but <slha> tag not yet reached, skip
      if (lhefRead && ! lhefSlha) continue;
    }

    //Ignore comment lines with # as first character
    if (line.find("#") == 0) continue;

    //Move comment to separate string
    if (line.find("#") != string::npos) {
      comment=line.substr(line.find("#")+1,line.length()-line.find("#")-1);
      line.erase(line.find("#"),line.length()-line.find("#"));
    }

    // Remove extra blanks, also remove before and after an = sign.
    while (line.find("  ") != string::npos) line.erase( line.find("  "), 1);
    while (line.find(" =") != string::npos) line.erase( line.find(" ="), 1);
    while (line.find("= ") != string::npos) line.erase( line.find("= ")+1, 1);

    //New block. 
    if (line.find("block") <= 1) { 
      blockIn=line ; 
      decay="";
      int nameBegin=6 ;
      int nameEnd=blockIn.find(" ",7);
      blockName=line.substr(nameBegin,nameEnd-nameBegin);
      
      //Find Q=... for DRbar running blocks
      if (blockIn.find("q=") != string::npos) {
	int qbegin=blockIn.find("q=")+2;
	istringstream qstream(blockIn.substr(qbegin,blockIn.length()));
	double q=0.0;
	qstream >> q;
	if (qstream) {
	  // SLHA1 running blocks
	  if (blockName=="hmix") hmix.setq(q);
	  if (blockName=="yu") yu.setq(q);
	  if (blockName=="yd") yd.setq(q);
	  if (blockName=="ye") ye.setq(q);
	  if (blockName=="au") au.setq(q);
	  if (blockName=="ad") ad.setq(q);
	  if (blockName=="ae") ae.setq(q);
	  if (blockName=="msoft") msoft.setq(q);
	  if (blockName=="gauge") gauge.setq(q);
	  // SLHA2 running blocks
	  if (blockName=="vckm") vckm.setq(q);
	  if (blockName=="upmns") upmns.setq(q);
	  if (blockName=="msq2") msq2.setq(q);
	  if (blockName=="msu2") msu2.setq(q);
	  if (blockName=="msd2") msd2.setq(q);
	  if (blockName=="msl2") msl2.setq(q);
	  if (blockName=="mse2") mse2.setq(q);
	  if (blockName=="tu") tu.setq(q);
	  if (blockName=="td") td.setq(q);
	  if (blockName=="te") te.setq(q);
	  if (blockName=="rvlamlle") rvlamlle.setq(q);
	  if (blockName=="rvlamlqd") rvlamlqd.setq(q);
	  if (blockName=="rvlamudd") rvlamudd.setq(q);
	  if (blockName=="rvtlle") rvtlle.setq(q);
	  if (blockName=="rvtlqd") rvtlqd.setq(q);
	  if (blockName=="rvtudd") rvtudd.setq(q);
	  if (blockName=="rvkappa") rvkappa.setq(q);
	  if (blockName=="rvd") rvd.setq(q);
	  if (blockName=="rvm2lh1") rvm2lh1.setq(q);
	  if (blockName=="rvsnvev") rvsnvev.setq(q);	  
	  if (blockName=="imau") imau.setq(q);
	  if (blockName=="imad") imad.setq(q);
	  if (blockName=="imae") imae.setq(q);
	  if (blockName=="imtu") imtu.setq(q);
	  if (blockName=="imtd") imtd.setq(q);
	  if (blockName=="imte") imte.setq(q);
	  if (blockName=="imvckm") imvckm.setq(q);
	  if (blockName=="imupmns") imupmns.setq(q);
	  if (blockName=="immsq2") immsq2.setq(q);
	  if (blockName=="immsu2") immsu2.setq(q);
	  if (blockName=="immsd2") immsd2.setq(q);
	  if (blockName=="immsl2") immsl2.setq(q);
	  if (blockName=="immse2") immse2.setq(q);
	};
      };
      
      //Skip to next line.
      continue ; 
      
    } 

    //New decay table
    else if (line.find("decay") <= 1) {

      //Set decay block name
      decay=line;
      blockIn="";
      int nameBegin=6 ;
      int nameEnd=decay.find(" ",7);
      pdgName=decay.substr(nameBegin,nameEnd-nameBegin);
      
      //Extract PDG code and width
      istringstream dstream(pdgName);
      dstream >> pdgCode;
      if (dstream) {
	string widthName=decay.substr(nameEnd+1,decay.length());
	istringstream wstream(widthName);
	wstream >> width;
	if (wstream) {
	  //Set PDG code and width
	  if (width <= 0.0) {
	    message(0,"readFile","skipping stable particle "+pdgName,0);
	    width=0.0;
	    decay="";
	  } else {
	    message(0,"readFile","skipping DECAY table for "+pdgName,0);
	  }
	} else {
	  message(2,"readFile","WIDTH unreadable for "+pdgName,iLine);
	  message(0,"readFile","skipping stable particle "+pdgName,0);
	  width=0.0;
	  decay="";
	  continue;
	}
      }
      else {
	message(2,"readFile","PDG Code unreadable. Ignoring this DECAY block",iLine);
	decay="";
	continue;
      }

      //Read in PDG code and width
      //...
      //Set stable if width = 0d0
      //...
      //don't read in decay channels if width=0.0
      //if (width <= 0.0) decay="";

      //Skip to next line
      continue ;
    }

    //Switch off SLHA read-in via LHEF if outside <slha> tag.
    else if (line.find("</slha>") != string::npos) {
      lhefSlha=false;
      blockIn="";
      decay="";
      continue;
    }

    //End of SLHA read-in (via LHEF)
    else if (line.find("</header>") != string::npos ||
	     line.find("<init") != string::npos) {
      break;
    }

    //Skip not currently reading block data lines.
    if (blockIn != "") {

      // Replace an equal sign by a blank to make parsing simpler.
      while (line.find("=") != string::npos) {
	int firstEqual = line.find_first_of("=");
	line.replace(firstEqual, 1, " ");   
      };
    
      //Parse data lines within given block
      //Constructed explicitly so that each block can have its own types and
      //own rules defined. For extra user blocks, just add more recognized 
      //blockNames at the end and implement user defined rules accordingly.
      //string comment = line.substr(line.find("#"),line.length());    
      int ifail=-2;
      istringstream linestream(line);
      //MODEL
      if (blockName == "modsel") {
	int i;
	linestream >> i; 
	if (linestream) {
	  if (i == 12) {ifail=modsel12.set(0,linestream);} 
	  else if (i == 21) {ifail=modsel21.set(0,linestream);}
	  else {ifail=modsel.set(i,linestream);};}
	else {
	  ifail = -1;}
      };
      if (blockName == "minpar") ifail=minpar.set(linestream); 
      if (blockName == "sminputs") ifail=sminputs.set(linestream);
      if (blockName == "extpar") ifail=extpar.set(linestream);
      if (blockName == "qextpar") ifail=qextpar.set(linestream);
      //FLV
      if (blockName == "vckmin") ifail=vckmin.set(linestream);
      if (blockName == "upmnsin") ifail=upmnsin.set(linestream);
      if (blockName == "msq2in") ifail=msq2in.set(linestream);
      if (blockName == "msu2in") ifail=msu2in.set(linestream);
      if (blockName == "msd2in") ifail=msd2in.set(linestream);
      if (blockName == "msl2in") ifail=msl2in.set(linestream);
      if (blockName == "mse2in") ifail=mse2in.set(linestream);
      if (blockName == "tuin") ifail=tuin.set(linestream);
      if (blockName == "tdin") ifail=tdin.set(linestream);
      if (blockName == "tein") ifail=tein.set(linestream);
      //RPV
      if (blockName == "rvlamllein") ifail=rvlamllein.set(linestream);
      if (blockName == "rvlamlqdin") ifail=rvlamlqdin.set(linestream);
      if (blockName == "rvlamuddin") ifail=rvlamuddin.set(linestream);
      if (blockName == "rvtllein") ifail=rvtllein.set(linestream);
      if (blockName == "rvtlqdin") ifail=rvtlqdin.set(linestream);
      if (blockName == "rvtuddin") ifail=rvtuddin.set(linestream);
      if (blockName == "rvkappain") ifail=rvkappain.set(linestream);
      if (blockName == "rvdin") ifail=rvdin.set(linestream);
      if (blockName == "rvm2lh1in") ifail=rvm2lh1in.set(linestream);
      if (blockName == "rvsnvevin") ifail=rvsnvevin.set(linestream);
      //CPV 
      if (blockName == "imminpar") ifail=imminpar.set(linestream);
      if (blockName == "imextpar") ifail=imextpar.set(linestream);
      //CPV +FLV
      if (blockName == "immsq2in") ifail=immsq2in.set(linestream);
      if (blockName == "immsu2in") ifail=immsu2in.set(linestream);
      if (blockName == "immsd2in") ifail=immsd2in.set(linestream);
      if (blockName == "immsl2in") ifail=immsl2in.set(linestream);
      if (blockName == "immse2in") ifail=immse2in.set(linestream);
      if (blockName == "imtuin") ifail=imtuin.set(linestream);
      if (blockName == "imtdin") ifail=imtdin.set(linestream);
      if (blockName == "imtein") ifail=imtein.set(linestream);
      //Info:
      if (blockName == "spinfo" || blockName=="dcinfo") {
	int i;
	string entry;
	linestream >> i >> entry;
	string blockStr="RGE";
	if (blockName=="dcinfo") blockStr="DCY";

	if (linestream) {
	  if ( i == 3 ) {
	    string warning=line.substr(line.find("3")+1,line.length());
	    message(1,"readFile","(from "+blockStr+" program): "+warning,0);
	    if (blockName == "spinfo") spinfo3.set(warning);
	    else dcinfo3.set(warning);
	  } else if ( i == 4 ) {
	    string error=line.substr(line.find("4")+1,line.length());
	    message(2,"readFile","(from "+blockStr+" program): "+error,0);
	    if (blockName == "spinfo") spinfo4.set(error);
	    else dcinfo4.set(error);
	  } else {
	    //Rewrite string in uppercase
	    for (unsigned int j=0;j<entry.length();j++) 
	      entry[j]=toupper(entry[j]);
	    ifail=(blockName=="spinfo") ? spinfo.set(i,entry)
	      : dcinfo.set(i,entry);
	  };
	} else {
	  ifail=-1;
	};
      };
      //SPECTRUM
      //Pole masses
      if (blockName == "mass") ifail=mass.set(linestream);

      //Mixing
      if (blockName == "alpha") ifail=alpha.set(linestream);
      if (blockName == "stopmix") ifail=stopmix.set(linestream);
      if (blockName == "sbotmix") ifail=sbotmix.set(linestream);
      if (blockName == "staumix") ifail=staumix.set(linestream);
      if (blockName == "nmix") ifail=nmix.set(linestream);
      if (blockName == "umix") ifail=umix.set(linestream);
      if (blockName == "vmix") ifail=vmix.set(linestream);
      //FLV
      if (blockName == "usqmix") ifail=usqmix.set(linestream);
      if (blockName == "dsqmix") ifail=dsqmix.set(linestream);
      if (blockName == "selmix") ifail=selmix.set(linestream);
      if (blockName == "snumix") ifail=snumix.set(linestream);
      if (blockName == "snsmix") ifail=snsmix.set(linestream);
      if (blockName == "snamix") ifail=snamix.set(linestream);
      //RPV
      if (blockName == "rvnmix") ifail=rvnmix.set(linestream);
      if (blockName == "rvumix") ifail=rvumix.set(linestream);
      if (blockName == "rvvmix") ifail=rvvmix.set(linestream);
      if (blockName == "rvhmix") ifail=rvhmix.set(linestream);
      if (blockName == "rvamix") ifail=rvamix.set(linestream);
      if (blockName == "rvlmix") ifail=rvlmix.set(linestream);
      //CPV
      if (blockName == "cvhmix") ifail=cvhmix.set(linestream);
      if (blockName == "imcvhmix") ifail=imcvhmix.set(linestream);
      //CPV + FLV
      if (blockName == "imusqmix") ifail=imusqmix.set(linestream);
      if (blockName == "imdsqmix") ifail=imdsqmix.set(linestream);
      if (blockName == "imselmix") ifail=imselmix.set(linestream);
      if (blockName == "imsnumix") ifail=imsnumix.set(linestream);
      if (blockName == "imnmix") ifail=imnmix.set(linestream);
      if (blockName == "imumix") ifail=imumix.set(linestream);
      if (blockName == "imvmix") ifail=imvmix.set(linestream);
      //NMSSM
      if (blockName == "nmhmix") ifail=nmhmix.set(linestream);
      if (blockName == "nmamix") ifail=nmamix.set(linestream);
      if (blockName == "nmnmix") ifail=nmnmix.set(linestream);
      
      //DRbar Lagrangian parameters
      if (blockName == "gauge") ifail=gauge.set(linestream);      
      if (blockName == "yu") ifail=yu.set(linestream);
      if (blockName == "yd") ifail=yd.set(linestream);
      if (blockName == "ye") ifail=ye.set(linestream);
      if (blockName == "au") ifail=au.set(linestream);
      if (blockName == "ad") ifail=ad.set(linestream);
      if (blockName == "ae") ifail=ae.set(linestream);
      if (blockName == "hmix") ifail=hmix.set(linestream);
      if (blockName == "msoft") ifail=msoft.set(linestream);
      //FLV
      if (blockName == "vckm") ifail=vckm.set(linestream);
      if (blockName == "upmns") ifail=upmns.set(linestream);
      if (blockName == "msq2") ifail=msq2.set(linestream);
      if (blockName == "msu2") ifail=msu2.set(linestream);
      if (blockName == "msd2") ifail=msd2.set(linestream);
      if (blockName == "msl2") ifail=msl2.set(linestream);
      if (blockName == "mse2") ifail=mse2.set(linestream);
      if (blockName == "tu") ifail=tu.set(linestream);
      if (blockName == "td") ifail=td.set(linestream);
      if (blockName == "te") ifail=te.set(linestream);
      //RPV
      if (blockName == "rvlamlle") ifail=rvlamlle.set(linestream);
      if (blockName == "rvlamlqd") ifail=rvlamlqd.set(linestream);
      if (blockName == "rvlamudd") ifail=rvlamudd.set(linestream);
      if (blockName == "rvtlle") ifail=rvtlle.set(linestream);
      if (blockName == "rvtlqd") ifail=rvtlqd.set(linestream);
      if (blockName == "rvtudd") ifail=rvtudd.set(linestream);
      if (blockName == "rvkappa") ifail=rvkappa.set(linestream);
      if (blockName == "rvd") ifail=rvd.set(linestream);
      if (blockName == "rvm2lh1") ifail=rvm2lh1.set(linestream);
      if (blockName == "rvsnvev") ifail=rvsnvev.set(linestream);
      //CPV
      if (blockName == "imau") ifail=imau.set(linestream);
      if (blockName == "imad") ifail=imad.set(linestream);
      if (blockName == "imae") ifail=imae.set(linestream);
      //CPV+FLV
      if (blockName == "imvckm") ifail=imvckm.set(linestream);
      if (blockName == "imupmns") ifail=imupmns.set(linestream);
      if (blockName == "immsq2") ifail=immsq2.set(linestream);
      if (blockName == "immsu2") ifail=immsu2.set(linestream);
      if (blockName == "immsd2") ifail=immsd2.set(linestream);
      if (blockName == "immsl2") ifail=immsl2.set(linestream);
      if (blockName == "immse2") ifail=immse2.set(linestream);
      if (blockName == "imtu") ifail=imtu.set(linestream);
      if (blockName == "imtd") ifail=imtd.set(linestream);
      if (blockName == "imte") ifail=imte.set(linestream);
      //NMSSM
      if (blockName == "nmssmrun") ifail=nmssmrun.set(linestream);      

      //Diagnostics
      if (ifail != 0) { 
	if (ifail == -2) {
	  message(1,"readFile","Ignoring unknown block: "+blockName,iLine);
	  blockIn="";
	};
	if (ifail == -1) {
	  message(2,"readFile","Error on line.",iLine);	
	};
	if (ifail == 1) {
	  message(0,"readFile",blockName+" existing entry overwritten.",iLine);
	};
      };
      
    } 

    // Decay table read-in
    // Not yet implemented
    else if (decay != "") {
      // cout << " found decay mode "<<line<<endl;
    }
  };

  //Print footer 
  printFooter();

  //Return 0 if read-in successful 
  return iFailFile;
    
}

//*********

// Print a header with information on version, last date of change, etc.

void SusyLesHouches::printHeader() {
  setprecision(3);
  if (! headerPrinted) {
    cout <<" *--------------------  SusyLesHouches v0.03 SUSY/BSM Interface  ---------------------*\n";
    message(0,"","Last Change 06 Mar 2008 - P.Z. Skands",0);
    headerPrinted=true;
  }
}

//*********

// Print a footer

void SusyLesHouches::printFooter() {
  if (! footerPrinted) {
    //    cout << " *"<<endl;
    cout <<" *------------------------------------------------------------------------------------*\n";
    footerPrinted=true;
    //    headerPrinted=false; 
  }
}

//*********

// Print the current spectrum on stdout.
// Not yet fully implemented.

void SusyLesHouches::printSpectrum() {
  // Print header if not already done
  if (! headerPrinted) printHeader();
  message(0,"","");

  // Print Calculator and File name
  if (slhaRead) {
    message(0,"","  Spectrum Calculator was:   "+spinfo(1)+"   version "+spinfo(2));
    if (lhefRead) message(0,"","  Read <slha> spectrum from: "+spectrumFile);
    else message(0,"","  Read SLHA spectrum from: "+spectrumFile);
  }

  // gluino
  message(0,"","");
  cout<<" |  ~g                  m"<<endl; 
  cout<<setprecision(3)<<" |     1000021 "<<setw(10)<<
      ( (mass(2000003) > 1e7) ? scientific : fixed)<<mass(1000021)<<endl;

  // d squarks
  message(0,"","");
  cout<<" |  ~d                  m     ~dL     ~sL     ~bL     ~dR     ~sR     ~bR"<<endl;

  cout<<setprecision(3) <<" |     1000001 "<<setw(10)<<
    ( (mass(1000001) > 1e7) ? scientific : fixed)<<mass(1000001)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(1,icur)<<"  ";

  cout<<endl<<" |     1000003 "<<setw(10)<<
    ( (mass(1000003) > 1e7) ? scientific : fixed)<<mass(1000003)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(2,icur)<<"  ";

  cout<<endl<<" |     1000005 "<<setw(10)<<
    ( (mass(1000005) > 1e7) ? scientific : fixed)<<mass(1000005)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(3,icur)<<"  ";

  cout<<endl<<" |     2000001 "<<setw(10)<<
    ( (mass(2000001) > 1e7) ? scientific : fixed)<<mass(2000001)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(4,icur)<<"  ";

  cout<<endl<<" |     2000003 "<<setw(10)<<
    ( (mass(2000003) > 1e7) ? scientific : fixed)<<mass(2000003)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(5,icur)<<"  ";

  cout<<endl<<" |     2000005 "<<setw(10)<<
    ( (mass(2000005) > 1e7) ? scientific : fixed)<<mass(2000005)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<dsqmix(6,icur)<<"  ";

  cout<<endl;
  
  // u squarks
  message(0,"","");
  cout<<" |  ~u                  m     ~uL     ~cL     ~tL     ~uR     ~cR     ~tR"<<endl; 

  cout<<setprecision(3)<<" |     1000002 "<<setw(10)<<
    ( (mass(1000002) > 1e7) ? scientific : fixed)<<mass(1000002)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(1,icur)<<"  ";

  cout<<endl<<" |     1000004 "<<setw(10)<<
    ( (mass(1000004) > 1e7) ? scientific : fixed)<<mass(1000004)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(2,icur)<<"  ";

  cout<<endl<<" |     1000006 "<<setw(10)<<
    ( (mass(1000006) > 1e7) ? scientific : fixed)<<mass(1000006)<<fixed<<"  "; 
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(3,icur)<<"  ";

  cout<<endl<<" |     2000002 "<<setw(10)<<
    ( (mass(2000002) > 1e7) ? scientific : fixed)<<mass(2000002)<<fixed<<"  "; 
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(4,icur)<<"  ";

  cout<<endl<<" |     2000004 "<<setw(10)<<
    ( (mass(2000004) > 1e7) ? scientific : fixed)<<mass(2000004)<<fixed<<"  " ;
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(5,icur)<<"  ";

  cout<<endl<<" |     2000006 "<<setw(10)<<
    ( (mass(2000006) > 1e7) ? scientific : fixed)<<mass(2000006)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<usqmix(6,icur)<<"  ";

  cout<<endl;

  // sleptons  
  // (NB: should be an if/then/else for RPV case here)
  message(0,"","");
  cout<<" |  ~e                  m     ~eL    ~muL   ~tauL     ~eR    ~muR   ~tauR"<<endl; 

  cout<<setprecision(3)<<" |     1000011 "<<setw(10)<<
    ( (mass(1000011) > 1e7) ? scientific : fixed)<<mass(1000011)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(1,icur)<<"  ";

  cout<<endl<<" |     1000013 "<<setw(10)<<
    ( (mass(1000013) > 1e7) ? scientific : fixed)<<mass(1000013)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(2,icur)<<"  ";

  cout<<endl<<" |     1000015 "<<setw(10)<<
    ( (mass(1000015) > 1e7) ? scientific : fixed)<<mass(1000015)<<fixed<<"  "; 
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(3,icur)<<"  ";

  cout<<endl<<" |     2000011 "<<setw(10)<<
    ( (mass(2000011) > 1e7) ? scientific : fixed)<<mass(2000011)<<fixed<<"  "; 
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(4,icur)<<"  ";

  cout<<endl<<" |     2000013 "<<setw(10)<<
    ( (mass(2000013) > 1e7) ? scientific : fixed)<<mass(2000013)<<fixed<<"  " ;
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(5,icur)<<"  ";

  cout<<endl<<" |     2000015 "<<setw(10)<<
    ( (mass(2000015) > 1e7) ? scientific : fixed)<<mass(2000015)<<fixed<<"  ";
  for (int icur=1;icur<=6;icur++) cout<<setw(6)<<selmix(6,icur)<<"  ";

  cout<<endl;

  // sneutrinos
  // (NB: should be an if/then/else for RPV case here)
  message(0,"","");
  cout<<" |  ~nu                 m";
  if (snumix.exists()) cout<<"   ~nu_e  ~nu_mu ~nu_tau";
  cout<<endl; 

  cout<<setprecision(3)<<" |     1000012 "<<setw(10)<<
    ( (mass(1000012) > 1e7) ? scientific : fixed)<<mass(1000012)<<fixed<<"  ";
  if (snumix.exists()) for (int icur=1;icur<=3;icur++) 
    cout<<setw(6)<<snumix(1,icur)<<"  ";

  cout<<endl<<" |     1000014 "<<setw(10)<<
    ( (mass(1000014) > 1e7) ? scientific : fixed)<<mass(1000014)<<fixed<<"  ";
  if (snumix.exists()) for (int icur=1;icur<=3;icur++) 
    cout<<setw(6)<<snumix(2,icur)<<"  ";

  cout<<endl<<" |     1000016 "<<setw(10)<<
    ( (mass(1000016) > 1e7) ? scientific : fixed)<<mass(1000016)<<fixed<<"  "; 
  if (snumix.exists()) for (int icur=1;icur<=3;icur++) 
    cout<<setw(6)<<snumix(3,icur)<<"  ";

  cout<<endl;

  // neutralinos
  // (NB: should be an if/then/else for RPV case here)
  message(0,"","");
  cout<<" |  ~chi0               m      ~B    ~W_3    ~H_1    ~H_2"<<endl; 

  cout<<setprecision(3)<<" |     1000022 "<<setw(10)<<
    ( (mass(1000022) > 1e7) ? scientific : fixed)<<mass(1000022)<<fixed<<"  ";
  for (int icur=1;icur<=4;icur++) cout<<setw(6)<<nmix(1,icur)<<"  ";

  cout<<endl<<" |     1000023 "<<setw(10)<<
    ( (mass(1000023) > 1e7) ? scientific : fixed)<<mass(1000023)<<fixed<<"  ";
  for (int icur=1;icur<=4;icur++) cout<<setw(6)<<nmix(2,icur)<<"  ";

  cout<<endl<<" |     1000025 "<<setw(10)<<
    ( (mass(1000025) > 1e7) ? scientific : fixed)<<mass(1000025)<<fixed<<"  "; 
  for (int icur=1;icur<=4;icur++) cout<<setw(6)<<nmix(3,icur)<<"  ";

  cout<<endl<<" |     1000035 "<<setw(10)<<
    ( (mass(1000035) > 1e7) ? scientific : fixed)<<mass(1000035)<<fixed<<"  "; 
  for (int icur=1;icur<=4;icur++) cout<<setw(6)<<nmix(4,icur)<<"  ";

  cout<<endl;

  // charginos
  // (NB: should be an if/then/else for RPV case here)
  message(0,"","");
  cout<<" |  ~chi+               m   U:   ~W      ~H  |  V:   ~W      ~H"
      <<endl; 

  cout<<setprecision(3)<<" |     1000024 "<<setw(10)<<
    ((mass(1000024) > 1e7) ? scientific : fixed)<<mass(1000024)<<fixed<<"    ";
  for (int icur=1;icur<=2;icur++) cout<<setw(6)<<umix(1,icur)<<"  ";
  cout<<"|   ";
  for (int icur=1;icur<=2;icur++) cout<<setw(6)<<vmix(1,icur)<<"  ";

  cout<<endl<<" |     1000037 "<<setw(10)<<
    ((mass(1000037) > 1e7) ? scientific : fixed)<<mass(1000037)<<fixed<<"    ";
  for (int icur=1;icur<=2;icur++) cout<<setw(6)<<umix(2,icur)<<"  ";
  cout<<"|   " ;
  for (int icur=1;icur<=2;icur++) cout<<setw(6)<<vmix(2,icur)<<"  ";

  cout<<endl;

  // Higgs bosons 
  // (NB: should be an if/then/else for RPV case here)
  // ...

  // Print footer  
  footerPrinted=false;
  message(0,"","");
  printFooter();
}

//*********

// Check consistency of spectrum, unitarity of matrices, etc.

int SusyLesHouches::checkSpectrum() {

  if (! headerPrinted) printHeader();
  int ifail=0;

  //-----------------------------------------------------------------
  //1) Check MODSEL. Assign default values where applicable.
  if (!modsel.exists(1)) {
    message(1,"checkSpectrum","MODSEL(1) undefined. Assuming =0.",0);
    modsel.set(1,0);
    ifail=max(ifail,1);
  }
  if (!modsel.exists(3)) modsel.set(3,0);
  if (!modsel.exists(4)) modsel.set(4,0);
  if (!modsel.exists(5)) modsel.set(5,0);
  if (!modsel.exists(6)) modsel.set(6,0);
  if (!modsel.exists(11)) modsel.set(11,1);
  
  //-----------------------------------------------------------------
  //2) Check for existence / duplication of blocks

  //Global
  if (!modsel.exists()) {
      message(1,"checkSpectrum","MODSEL not found",0);
      ifail=max(ifail,1);    
  }
  if (!minpar.exists()) {
      message(1,"checkSpectrum","MINPAR not found",0);
      ifail=max(ifail,1);    
  }
  if (!mass.exists()) {
      message(1,"checkSpectrum","MASS not found",0);
      ifail=max(ifail,1);    
  }
  if (!sminputs.exists()) {
      message(1,"checkSpectrum","SMINPUTS not found",0);
      ifail=max(ifail,1);    
  }
  if (!gauge.exists()) {
      message(1,"checkSpectrum","GAUGE not found",0);
      ifail=max(ifail,1);    
  }

  //SLHA1
  if (modsel(3) == 0 && modsel(4) == 0 && modsel(5) == 0 && modsel(6) == 0) {
    // Check for required SLHA1 blocks
    if (!staumix.exists()) {
      message(1,"checkSpectrum","STAUMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!sbotmix.exists()) {
      message(1,"checkSpectrum","SBOTMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!stopmix.exists()) {
      message(1,"checkSpectrum","STOPMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!nmix.exists()) {
      message(1,"checkSpectrum","NMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!umix.exists()) {
      message(1,"checkSpectrum","UMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!vmix.exists()) {
      message(1,"checkSpectrum","VMIX not found",0);
      ifail=max(ifail,1);
    };  
    if (!alpha.exists()) {
      message(1,"checkSpectrum","ALPHA not found",0);
      ifail=max(ifail,1);    
    }
    if (!hmix.exists()) {
      message(1,"checkSpectrum","HMIX not found",0);
      ifail=max(ifail,1);    
    }
    if (!msoft.exists()) {
      message(1,"checkSpectrum","MSOFT not found",0);
      ifail=max(ifail,1);    
    }
  } 

  //RPV (+ FLV)
  else if (modsel(4) != 0) {
    // Check for required SLHA2 blocks
    if (!rvnmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVNMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!rvumix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVUMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!rvvmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVVMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!rvhmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVHMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!rvamix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVAMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!rvlmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but RVLMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!usqmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but USQMIX not found",0);
      ifail=max(ifail,1);
    }
    if (!dsqmix.exists()) {
      message(1,"checkSpectrum","MODSEL 4 != 0 but DSQMIX not found",0);
      ifail=max(ifail,1);
    }
  }

  // FLV but not RPV (see above for FLV+RPV, below for FLV regardless of RPV)
  else if (modsel(6) != 0) {
    // Quark FLV
    if (modsel(6) != 2) {
      if (!usqmix.exists()) {
	message(1,"checkSpectrum","quark FLV on but USQMIX not found",0);
	ifail=max(ifail,1);
      }
      if (!dsqmix.exists()) {
	message(1,"checkSpectrum","quark FLV on but DSQMIX not found",0);
	ifail=max(ifail,1);
      }
    }
    // Lepton FLV
    if (modsel(6) != 1) {
      if (!upmns.exists()) {
	message(1,"checkSpectrum","lepton FLV on but UPMNSIN not found",0);
	ifail=max(ifail,1);
      }
      if (!selmix.exists()) {
	message(1,"checkSpectrum","lepton FLV on but SELMIX not found",0);
	ifail=max(ifail,1);
      }
      if (!snumix.exists() && !snsmix.exists()) {
	message(1,"checkSpectrum","lepton FLV on but SNUMIX not found",0);
	ifail=max(ifail,1);
      }
    }
  }

  // CPV
  if (modsel(5) != 0) {
    if (!cvhmix.exists()) {
      message(1,"checkSpectrum","MODSEL 5 != 0 but CVHMIX not found",0);
      ifail=max(ifail,1);
    }
  }

  // FLV (regardless of whether RPV or not)
  if (modsel(6) != 0) {
    // Quark FLV
    if (modsel(6) != 2) {
      if (!vckmin.exists()) {
	message(1,"checkSpectrum","quark FLV on but VCKMIN not found",0);
	ifail=max(ifail,1);
      }
      if (!msq2in.exists()) {
	message(0,"checkSpectrum","note: quark FLV on but MSQ2IN not found",0);
	ifail=max(ifail,0);
      }
      if (!msu2in.exists()) {
	message(0,"checkSpectrum","note: quark FLV on but MSU2IN not found",0);
	ifail=max(ifail,0);
      }
      if (!msd2in.exists()) {
	message(0,"checkSpectrum","note: quark FLV on but MSD2IN not found",0);
	ifail=max(ifail,0);
      }
      if (!tuin.exists()) {
	message(0,"checkSpectrum","note: quark FLV on but TUIN not found",0);
	ifail=max(ifail,0);
      }
      if (!tdin.exists()) {
	message(0,"checkSpectrum","note: quark FLV on but TDIN not found",0);
	ifail=max(ifail,0);
      }
    }
    // Lepton FLV
    if (modsel(6) != 1) {
      if (!msl2in.exists()) {
	message(0,"checkSpectrum","note: lepton FLV on but MSL2IN not found",0);
	ifail=max(ifail,0);
      }
      if (!mse2in.exists()) {
	message(0,"checkSpectrum","note: lepton FLV on but MSE2IN not found",0);
	ifail=max(ifail,0);
      }
      if (!tein.exists()) {
	message(0,"checkSpectrum","note: lepton FLV on but TEIN not found",0);
	ifail=max(ifail,0);
      }
    }
  }
  
  //-----------------------------------------------------------------
  //3) SLHA1 --> SLHA2 interoperability
  //Note: the mass basis is NOT mass-ordered in SLHA1, so be careful!
  //Here, the mass basis is hence by PDG code, not by mass-ordered value.

  if (stopmix.exists() && ! usqmix.exists() ) {
    //1000002 = ~uL, 1000004 = ~cL, 2000002 = ~uR, 2000004 = ~cR 
    usqmix.set(1,1, 1.0);
    usqmix.set(2,2, 1.0); 
    usqmix.set(4,4, 1.0);
    usqmix.set(5,5, 1.0);
    //Fill (1000006,2000006) sector from stopmix
    usqmix.set(3,3, stopmix(1,1));
    usqmix.set(3,6, stopmix(1,2));
    usqmix.set(6,3, stopmix(2,1));
    usqmix.set(6,6, stopmix(2,2));    
  };
  if (sbotmix.exists() && ! dsqmix.exists() ) {
    //1000001 = ~dL, 1000003 = ~sL, 2000001 = ~dR, 2000003 = ~sR 
    dsqmix.set(1,1, 1.0);
    dsqmix.set(2,2, 1.0); 
    dsqmix.set(4,4, 1.0);
    dsqmix.set(5,5, 1.0);
    //Fill (1000005,2000005) sector from sbotmix
    dsqmix.set(3,3, sbotmix(1,1));
    dsqmix.set(3,6, sbotmix(1,2));
    dsqmix.set(6,3, sbotmix(2,1));
    dsqmix.set(6,6, sbotmix(2,2));
  };
  if (staumix.exists() && ! selmix.exists() ) {
    //1000011 = ~eL, 1000013 = ~muL, 2000011 = ~eR, 2000013 = ~muR 
    selmix.set(1,1, 1.0);
    selmix.set(2,2, 1.0); 
    selmix.set(4,4, 1.0);
    selmix.set(5,5, 1.0);
    //Fill (1000015,2000015) sector from staumix
    selmix.set(3,3, staumix(1,1));
    selmix.set(3,6, staumix(1,2));
    selmix.set(6,3, staumix(2,1));
    selmix.set(6,6, staumix(2,2));
  };
  if (! snumix.exists() && ! snsmix.exists()) {
    //1000012 = ~nu_e, 1000014 = ~nu_mu, 1000016 = ~nu_tau
    snumix.set(1,1, 1.0);
    snumix.set(2,2, 1.0); 
    snumix.set(3,3, 1.0);
  };

  //-----------------------------------------------------------------
  //4) Check unitarity/orthogonality of mixing matrices

  //NMIX
  if (nmix.exists()) {
    for (int i=1;i<=4;i++) {
      double cn1=0.0;
      double cn2=0.0;
      for (int j=1;j<=4;j++) {
	cn1 += pow(nmix(i,j),2);
	cn2 += pow(nmix(j,i),2);
      }
      if (abs(1.0-cn1) > 1e-3 || abs(1.0-cn2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of NMIX",0);
      }
    }
  }

  //VMIX, UMIX
  if (vmix.exists() && umix.exists()) {
    for (int i=1;i<=2;i++) {
      double cu1=0.0;
      double cu2=0.0;
      double cv1=0.0;
      double cv2=0.0;
      for (int j=1;j<=2;j++) {
	cu1 += pow(umix(i,j),2);
	cu2 += pow(umix(j,i),2);
	cv1 += pow(vmix(i,j),2);
	cv2 += pow(vmix(j,i),2);
      }
      if (abs(1.0-cu1) > 1e-3 || abs(1.0-cu2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of UMIX",0);
      }
      if (abs(1.0-cv1) > 1e-3 || abs(1.0-cv2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of VMIX",0);
      }
    }
    
  }

  //STOPMIX, SBOTMIX
  if (stopmix.exists() && sbotmix.exists()) {
    for (int i=1;i<=2;i++) {
      double ct1=0.0;
      double ct2=0.0;
      double cb1=0.0;
      double cb2=0.0;
      for (int j=1;j<=2;j++) {
	ct1 += pow(stopmix(i,j),2);
	ct2 += pow(stopmix(j,i),2);
	cb1 += pow(sbotmix(i,j),2);
	cb2 += pow(sbotmix(j,i),2);
      }
      if (abs(1.0-ct1) > 1e-3 || abs(1.0-ct2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of STOPMIX",0);
      }
      if (abs(1.0-cb1) > 1e-3 || abs(1.0-cb2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of SBOTMIX",0);
      }
    }    
  }

  //STAUMIX
  if (staumix.exists()) {
    for (int i=1;i<=2;i++) {
      double ct1=0.0;
      double ct2=0.0;
      for (int j=1;j<=2;j++) {
	ct1 += pow(staumix(i,j),2);
	ct2 += pow(staumix(j,i),2);
      }
      if (abs(1.0-ct1) > 1e-3 || abs(1.0-ct2) > 1e-3) { 
	ifail=2; 
	message(2,"checkSpectrum","inconsistent normalization of STAUMIX",0);
      }
    }    
  }

  //NMSSM:
  if (modsel(3) == 1) {
    //NMNMIX
    if ( nmnmix.exists() ) {
      for (int i=1;i<=5;i++) {
	double cn1=0.0;
	double cn2=0.0;
	for (int j=1;j<=4;j++) {
	  cn1 += pow(nmnmix(i,j),2);
	  cn2 += pow(nmnmix(j,i),2);
	}
	if (abs(1.0-cn1) > 1e-3 || abs(1.0-cn2) > 1e-3) { 
	  ifail=max(ifail,2); 
	  message(2,"checkSpectrum","inconsistent normalization of NMNMIX",0);
	}
      }
    }
    else {
      ifail=max(ifail,1);
      message(1,"checkSpectrum","MODSEL 3 = 1 (NMSSM) but no NMNMIX found",0);
    }
    //NMAMIX
    if ( nmamix.exists() ) {
      for (int i=1;i<=2;i++) {
	double cn1=0.0;
	for (int j=1;j<=3;j++) {
	  cn1 += pow(nmamix(i,j),2);
	}
	if (abs(1.0-cn1) > 1e-3) { 
	  ifail=max(ifail,2); 
	  message(2,"checkSpectrum","inconsistent normalization of NMAMIX",0);
	}
      }
    }
    else {
      ifail=max(ifail,1);
      message(1,"checkSpectrum","MODSEL 3 = 1 (NMSSM) but no NMAMIX found",0);
    }
    //NMHMIX
    if ( nmhmix.exists() ) {
      for (int i=1;i<=3;i++) {
	double cn1=0.0;
	double cn2=0.0;
	for (int j=1;j<=3;j++) {
	  cn1 += pow(nmhmix(i,j),2);
	  cn2 += pow(nmhmix(j,i),2);
	}
	if (abs(1.0-cn1) > 1e-3 || abs(1.0-cn2) > 1e-3) { 
	  ifail=max(ifail,2); 
	  message(2,"checkSpectrum","inconsistent normalization of NMHMIX",0);
	}
      }
    }
    else {
      ifail=max(ifail,1);
      message(1,"checkSpectrum","MODSEL 3 = 1 (NMSSM) but no NMHMIX found",0);
    }
    //NMSSMRUN
    if (! nmssmrun.exists() ) {
      ifail=max(ifail,1);
      message(2,"checkSpectrum","MODSEL 3 = 1 (NMSSM) but no NMSSMRUN found",
	      0);
    }
  }
  
  //Check for documentation
  if (slhaRead && ! spinfo.exists(1)) spinfo.set(1,"unknown");
  if (slhaRead && ! spinfo.exists(2)) spinfo.set(2,"unknown");
  if (! slhaRead && ! spinfo.exists(1)) {
    spinfo.set(1,"DEFAULT");
    spinfo.set(2,"n/a");
  }

  //Give status 
  if (ifail >= 2) 
    message(0,"checkSpectrum","one or more serious problems were found");

  //Print Footer
  printFooter();

  //Return
  return ifail;
}

//*********

// Simple utility to print messages, warnings, and errors

void SusyLesHouches::message(int level, string place,string themessage,int line) {
  //Send normal messages and warnings to stdout, errors to stderr.
  ostream* outstream = &cerr;
  if (level <= 1) outstream = &cout;
  // if (level == 2) { *outstream<<endl; }
  if (place != "") *outstream << " | (SLHA::"+place+") ";
  else *outstream << " | ";
  if (level == 1) *outstream<< "warning: "; 
  if (level == 2) { *outstream <<"ERROR: "; } 
  if (line != 0) *outstream<< "line "<<line<<" - ";
  *outstream << themessage << endl;
  //  if (level == 2) *outstream <<endl;
  footerPrinted=false;
  return;
}





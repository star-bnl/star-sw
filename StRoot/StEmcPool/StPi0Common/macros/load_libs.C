#include <TSystem.h>
#include <TH1.h>
#include <TString.h>
#include <TROOT.h>
#ifndef __CINT__
#include <TError.h>
#endif

Bool_t load_lib(const Char_t *libName = "") {
    Bool_t result = false;
    if (!libName) return result;
    TString hostSysStr = gSystem->Getenv("STAR_HOST_SYS");
//Info(__FILE__, "1. System is %s", hostSysStr.Data());
    if (hostSysStr == "") hostSysStr = gSystem->GetBuildArch();
//Info(__FILE__, "2. System is %s", hostSysStr.Data());
    TString libPath = "." + hostSysStr + "/lib";
    TString verStr = gROOT->GetVersion();
    verStr.ReplaceAll("/", ".");
    verStr.Prepend(".root");
    TString libNameStr = libName;
    TString libNameStrVer = libPath + "/" + libName + verStr;
//Info(__FILE__, "1. Looking in %s", libNameStrVer.Data());
    const Char_t *temp = gSystem->DynamicPathName(libNameStrVer.Data(), true);
    if (temp && !gSystem->AccessPathName(temp)) {
	libNameStr = libNameStrVer;
	Info(__FILE__, "Loading %s...", libNameStrVer.Data());
	result = (gSystem->Load(libNameStrVer.Data()) >= 0);
    } else {
	TString libNameStrLib = libPath + "/" + libName;
//Info(__FILE__, "2. Looking in %s", libNameStrLib.Data());
	temp = gSystem->DynamicPathName(libNameStrLib.Data(), true);
	if (temp && !gSystem->AccessPathName(temp)) {
		Info(__FILE__, "Loading %s...", libNameStrLib.Data());
		result = (gSystem->Load(libNameStrLib.Data()) >= 0);
	} else {
//Info(__FILE__, "3. Looking in %s", libNameStr.Data());
		temp = gSystem->DynamicPathName(libNameStr.Data(), true);
		if (temp && !gSystem->AccessPathName(temp)) {
			Info(__FILE__, "Loading %s...", libNameStr.Data());
			result = (gSystem->Load(libNameStr.Data()) >= 0);
		} else {
			Info(__FILE__, "Cannod load %s", libName);
		}
	}
    }
    return result;
}

Bool_t load_libs(Bool_t useUroot = false
		, const Char_t *commonLib = "StPi0Common"
		, const Char_t *analysisLib = "StPi0Analysis"
		, const Char_t *resultsLib = "StPi0Results"
		) {
    Bool_t result = true;
    //gPrintViaErrorHandler = true;
    TString dynPath = TString(".") + gSystem->GetBuildArch() + TString("/lib");
    TString thisPath = __FILE__; thisPath = thisPath.Resize(thisPath.Last('/'));
    dynPath = TString(gSystem->GetDynamicPath()) + "" + thisPath;// + "/" + dynPath;
    gSystem->SetDynamicPath(dynPath);
    result &= (gSystem->Load("libPhysics") >= 0);
    result &= (gSystem->Load("libThread") >= 0);
    result &= load_lib(commonLib);
    if (load_lib(analysisLib)) {
	initCuts();
    } else {
	result = false;
    }
    result &= load_lib(resultsLib);
    //result &= load_lib(dataLib);
    //result &= load_lib(timeRandomizerLib);
    //result &= load_lib(triggerSimulatorLib);
    TH1::AddDirectory(0);
    gStyle->SetPalette(1);
    if (result) {
	if (useUroot) {
    	    Info(__FILE__, "Starting uroot...");
    	    Info(__FILE__, "Setting signal handlers...");
	    mySignalHandler = new TMySignalHandler(kSigUser1, "/var/tmp/uroot.in", "/var/tmp/uroot.out");
	    if (mySignalHandler) gSystem->AddSignalHandler(mySignalHandler);
	    gSystem->AddSignalHandler(new TMySignalHandler(kSigTermination, "/var/tmp/uroot.pid", ""));
    	    Info(__FILE__, "Writing PID...");
	    ofstream ofstr("/var/tmp/uroot.pid");
	    if (ofstr.good()) {
		ofstr << gSystem->GetPid();
		if (!ofstr.good()) Error(__FILE__, "Cannot write PID!");
	    }
    	    Info(__FILE__, "Setting kill timer...");
	    TString timerStr = "gSystem->Exec(\"kill "; timerStr += gSystem->GetPid(); timerStr += "\")";
	    TTimer *timer = new TTimer(timerStr.Data(), 60000 * 10, false);
	    mySignalHandler->timer = timer;
	    if (timer) timer->Start(-1, false);
    	    Info(__FILE__, "...uroot done");
	}
    }
    return result || true;
}

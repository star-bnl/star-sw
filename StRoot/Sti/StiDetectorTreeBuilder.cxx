//StiDetectorTreeBuilder.cxx
//M.L. Miller (Yale Software)
//07/01

//Std
#include <iostream>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>

//StiGui
//#include "StiGui/StiRootDrawableDetector.h"

//Sti
#include "StiDetector.h"
#include "StiCompositeTreeNode.h"
#include "StiDetectorTreeBuilder.h"
#include "StlUtilities.h"

ostream& operator<<(ostream&, const StiDetector&);

StiDetectorTreeBuilder::StiDetectorTreeBuilder()
    : mroot(0), mnodefactory(0), mdetfactory(0), mregion(0)
{
    cout <<"StiDetectorTreeBuilder::StiDetectorTreeBuilder()"<<endl;
}

StiDetectorTreeBuilder::~StiDetectorTreeBuilder()
{
    cout <<"StiDetectorTreeBuilder::~StiDetectorTreeBuilder()"<<endl;
}

data_node* StiDetectorTreeBuilder::build(const char* path,
								 data_node_factory* nodefactory,
								 detector_factory* detfactory)
{
    if (mroot) {
	cout <<"StiDetectorTreeBuilder::build()\tError!\troot tree already built"<<endl;
	return 0;
    }
    
    mnodefactory = nodefactory;
    mdetfactory = detfactory;
    buildRoot();
    loopOnDetectors(path);
    SortDaughters<data_t> mysorter;
    mysorter(mregion);
    
    return mroot;
}

void StiDetectorTreeBuilder::buildRoot()
{
    mroot = mnodefactory->getObject();
    mroot->setName("star");

    //make 3 daughters
    data_node* mid = mnodefactory->getObject();
    mid->setName("midrapidity");

    mroot->add(mid);
    mregion = mid;
}


void StiDetectorTreeBuilder::addToTree(StiDetector* layer)
{
    //Where do we hang in radius?
    StiOrderKey_t radius = layer->getCenterRadius();
    string radstring = "_radius";
    data_node* radialnode = hangWhere(mregion, radius, radstring);

    //Where do we hang in phi?
    StiOrderKey_t refAngle = layer->getCenterRefAngle();
    string phistring = "_refAngle";
    data_node* phinode = hangWhere(radialnode, refAngle, phistring);
    phinode->setData(layer);
}

data_node* StiDetectorTreeBuilder::hangWhere(data_node* parent,
								     StiOrderKey_t order, string& keystring)
{
    SameOrderKey<data_t> mySameOrderKey;
    mySameOrderKey.morderKey = order;

    data_node_vec::iterator where = find_if(parent->begin(), parent->end(), mySameOrderKey);

    if (where == parent->end()) {
	data_node* temp = mnodefactory->getObject();
	char* tempname = new char[100];
	sprintf(tempname,"_%f", order);
	keystring.append(tempname);
	string newname = parent->getName();
	newname.append(keystring);
	
	temp->setName(newname);
	temp->setOrderKey(order);
	parent->add(temp);
	delete tempname;
	return temp;
    }
    else {
	return (*where);
    }
}

void StiDetectorTreeBuilder::loopOnDetectors(const char* buildDirectory)
{
    char* buildfile = new char[200];
    
    DIR *pDir = opendir(buildDirectory);
    struct dirent *pDirEnt;
    struct stat fileStat;
    
    while( (pDirEnt = readdir(pDir)) != 0){
	sprintf(buildfile, "%s/%s", buildDirectory, pDirEnt->d_name);
	
	// get file attributes
	stat(buildfile, &fileStat);
	
	// is this a directory?  if so, recursively build directory
	if((S_ISDIR(fileStat.st_mode)) && pDirEnt->d_name[0] != '.'){
	    loopOnDetectors(buildfile);
	} // if is directory
	
	// if regular file, process as detector
	if(S_ISREG(fileStat.st_mode)){
	    //StiDetector* layer = makeDetectorObject();
	    StiDetector* layer = mdetfactory->getObject();

	    layer->build(buildfile);
	    addToTree(layer);
	    
	} // if is regular file
    }
    closedir(pDir);
    return;
}

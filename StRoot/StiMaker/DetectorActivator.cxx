#include <stdexcept>
#include <iostream.h>
#include <algorithm>
using std::find_if;

//root
#include "TRootEmbeddedCanvas.h"
#include "TShape.h"
#include "TBRIK.h"
#include "TVolume.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TPaveLabel.h"
#include "DetectorActivator.h"
#include "Sti/StiToolkit.h"

DetectorActivator::DetectorActivator(const TGWindow *p, const TGWindow *main,
				     UInt_t w, UInt_t h, UInt_t options) :
    TGTransientFrame(p, main, w, h, options),
    fRedTextGC(TGButton::GetDefaultGC())
{

    ULong_t red;
    fClient->GetColorByName("red", red);
    fRedTextGC.SetForeground(red);
    
    int ax, ay;
    
    ChangeOptions((GetOptions() & ~kVerticalFrame) | kHorizontalFrame);
    
    f1 = new TGCompositeFrame(this, 60, 20, kVerticalFrame | kFixedWidth);
    f2 = new TGCompositeFrame(this, 60, 20, kVerticalFrame);
    f3 = new TGCompositeFrame(f2, 60, 20, kHorizontalFrame);
    
    fTestButton = new TGTextButton(f1, "&Apply", 1, fRedTextGC());
    
    fCloseButton = new TGTextButton(f1, "&Close", 2);

    f1->Resize(fTestButton->GetDefaultWidth()+40, GetDefaultHeight());
    
    fTestButton->Associate(this);
    fCloseButton->Associate(this);
    
    fL1 = new TGLayoutHints(kLHintsTop | kLHintsExpandX,
			    2, 2, 3, 0);
    fL2 = new TGLayoutHints(kLHintsTop | kLHintsRight | kLHintsExpandX,
			    2, 5, 0, 2);
    fL21 = new TGLayoutHints(kLHintsTop | kLHintsRight,
			     2, 5, 10, 0);
    
    f1->AddFrame(fTestButton, fL1);
    f1->AddFrame(fCloseButton, fL1);    
    
    AddFrame(f1, fL21);
    
    //--------- create check and radio buttons groups

    //cout <<"Make fG1-fG4"<<endl;
    fG1 = new TGGroupFrame(f3, new TGString("ITTF Detector Layers"));
    //cout <<"done"<<endl;
    
    fL3 = new TGLayoutHints(kLHintsTop | kLHintsLeft |
			    kLHintsExpandX | kLHintsExpandY,
			    2, 2, 2, 2);
    fL4 = new TGLayoutHints(kLHintsTop | kLHintsLeft,
			    0, 0, 5, 0);
    const  StiCompositeTreeNode<StiDetector>   foo;
    //Build Dynamically from Detector Tree:
    StiDetectorContainer* detStore = StiToolkit::instance()->getDetectorContainer();
    const  StiCompositeTreeNode<StiDetector>  * root = detStore->root();
    const  StiCompositeTreeNode<StiDetector>  * midRapidity = *(root->begin());
    if (midRapidity->getName()!="midrapidity") {
	cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
	     <<"Did not find midrapidity region.  Undefined behavior"<<endl;
    }
    
    //cout <<"Loop on radii"<<endl;
    //Now loop on radii:
    typedef  StiCompositeTreeNode<StiDetector>  ::vec_type vecType;
    for (vecType::const_iterator it=midRapidity->begin(); it!=midRapidity->end(); ++it) {
	string tempName = (*it)->getName();
	
	//Check first node in phi for active key
	StiDetector* tempDetector = (*(*it)->begin())->getData();
	if (!tempDetector) {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Null StiDetector on phi-node.  Undefined behavior"<<endl;
	}
	
	//check if it's from the tpc.  if so, skip it
	string::size_type where = tempDetector->getName().find("Tpc");
	if ( where != tempDetector->getName().npos ) { //it's from tpc
	    //skip it
	}
	else {
	    TGCheckButton* tempButton = new TGCheckButton(fG1, new TGHotString(tempName.c_str()), -1);
	    bool active = tempDetector->isOn();
	    if (active) {
		tempButton->SetState(kButtonDown);
	    }
	    
	    DetectorActivatePair tempPair(tempName, tempButton);
	    fC.push_back(tempPair);
	}
    }
    
    //cout <<"\tdone"<<endl;
    
    //put 20 buttons/vertical frame
    //cout <<"Hang buttons on frame"<<endl;
    for (unsigned int i=0; i<fC.size(); ++i) {
	fG1->AddFrame(fC[i].second, fL4);
    }
    //cout <<"\tdone"<<endl;
    
    //cout <<"Add Frames fG1-fG4 to f3"<<endl;
    f3->AddFrame(fG1, fL3);
    //cout <<"\tdone"<<endl;
    
    f2->AddFrame(f3, fL1);
    AddFrame(f2, fL2);
    MapSubwindows();
    Resize(GetDefaultSize());
    
    // position relative to the parent's window
    Window_t wdum;
    gVirtualX->TranslateCoordinates(main->GetId(), GetParent()->GetId(),
				    (((TGFrame *) main)->GetWidth() - fWidth) >> 1,
				    (((TGFrame *) main)->GetHeight() - fHeight) >> 1,
				    ax, ay, wdum);
    Move(ax, ay);
    
    SetWindowName("ITTF Inner Detectors");
    
    MapWindow();
    //fClient->WaitFor(this);
}

// Order is important when deleting frames. Delete children first,
// parents last.

DetectorActivator::~DetectorActivator()
{
    // Delete widgets created by dialog.
    
    delete fTestButton; delete fCloseButton;
    for (unsigned int i=0; i<fC.size(); ++i) {
	delete fC[i].second;
    }

    delete f3; delete f2; delete f1;
    delete fL1; delete fL2; delete fL3; delete fL4;
    delete fL21;
}

void DetectorActivator::CloseWindow()
{
    // Close dialog in response to window manager close.
    
    delete this;
}

Bool_t DetectorActivator::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
    // Process messages sent to this dialog.
    
    switch(GET_MSG(msg)) {
    case kC_COMMAND:
	
	switch(GET_SUBMSG(msg)) {
	case kCM_BUTTON:
	    switch(parm1) {
	    case 1:
		updateDetectors();		
		break;
		
	    case 2:
		CloseWindow();
		break;
		
	    }
	    break;
	    
	case kCM_RADIOBUTTON:
	    
	case kCM_CHECKBUTTON:
	    break;
	    
	default:
	    break;
	}
	break;
	
    default:
	break;
    }
    
    return kTRUE;
}

void DetectorActivator::updateDetectors()
{
    for (unsigned int j=0; j<fC.size(); ++j) {
	if (fC[j].second->GetState() == kButtonDown) {
	    activateLayer(fC[j].first, true);
	}
	else {
	    activateLayer(fC[j].first, false);
	}
    }
    StiRootDisplayManager::instance()->draw();
    StiRootDisplayManager::instance()->update();
}

void DetectorActivator::activateLayer(const string& name, bool on)
{
    StiDetectorContainer* detStore = StiToolkit::instance()->getDetectorContainer();
    const  StiCompositeTreeNode<StiDetector>  * root = detStore->root();
    const  StiCompositeTreeNode<StiDetector>  * midRapidity = *(root->begin());
    if (midRapidity->getName()!="midrapidity") {
	cout <<"DetectorActivator::activateNode() ERROR:\t"
	     <<"Did not find midrapidity region.  Undefined behavior"<<endl;
    }
    
    typedef  StiCompositeTreeNode<StiDetector>::vec_type vecType;

    //find this radial layer
    //SameNodeName<StiDetector> mySameName(name);
    
    vecType::const_iterator where = find_if(midRapidity->begin(), midRapidity->end(),
					    //mySameName);
					    SameNodeName<StiDetector>(name));
    if (where==midRapidity->end()) {
	cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
	     <<"Node "<<name<<" not found in tree.  Abort"<<endl;
	return;
    }

    //we'll write the loop by hand so that it's easy to see what's going on
    //This will not be used in real running, so efficiency not so important
    for (vecType::iterator it=(*where)->begin(); it!=(*where)->end(); ++it) {
	StiDetector* tempDetector = (*it)->getData();
	if (!tempDetector) {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Null StiDetector on phi-node.  Abort"<<endl;
	    return;
	}
	//now we have the detector, do what we like:
	tempDetector->setIsOn(on);
	//cout <<"Setting detector  "<<tempDetector->getName()<<" to state isOn="<<on<<endl;
	StiDrawable* tempDrawable = dynamic_cast<StiDrawable*>(tempDetector);
	if (tempDrawable) {
	    tempDrawable->setVisible(on);
	    //cout <<"Setting detector  "<<tempDetector->getName()
	    //<<" to state visible="<<on<<endl;	    
	}
	else {
	    cout <<"DetectorActivator::DetectorActivator() ERROR:\t"
		 <<"Cast to drawable failed."<<endl;
	}
	
    }
}


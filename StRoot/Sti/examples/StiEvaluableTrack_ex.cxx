//StiEvaluableTrack_ex.cxx

//This function is an example of how to use StiEvaluableTrack
//This is a code fragment (it is not meant to actually compile!)

int main()
{
    StAssociationMaker* mAssociationMaker=0;
    // ... code to make mAssociationMaker point to a valid instance

    //The track factory
    StiObjectFactoryInterface<StiKalmanTrack>* trackfactory =
	new StiRDEvaluableTrackFactory("StiRDEvaluableTrackFactory");

    StiEvaluableTrackSeedFinder* sf =
	new StiEvaluableTrackSeedFinder(mAssociationMaker);

    sf->setFactory(mtrackfactory);
    sf->setBuildPath("EvaluableSeedFinder.txt");
    sf->build();

    //now withing event loop
    for (int i=0; i<nEvents; ++i) {

	StMcEvent* mcEvent;
	
	//... code to point mcEvent to a valid StMcEvent ...

	sf->setEvent(mcEvent);

	while (sf->hasMore) {

	    StiEvaluableTrack* track = sf->next();
	    
	    StTrackPairInfo* associatedPair = track->stTrackPairInfo();
	    if (!associatedPair) {
		cout <<"StiEvaluator::evaluateForEvent(). ERROR:\t";
		cout <<"Associated Pair==0.  Abort"<<endl;
		return;
	    }
	    
	    //Get the monte-carlo track
	    StMcTrack = associatedPair->partnerMcTrack();
	    
	    //Get the StTrack
	    StGlobalTrack* = associatedPair->partnerTrack();
	}
    }
	
    
    return 1;
}

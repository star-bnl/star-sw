//Trigger Cuts

    Int_t mTriggerId[nTrigger] =
    {
        // st_physics stream
        520001,    // VPDMB-5-p-sst  0
        520011,    // VPDMB-5-p-sst  1
        520021,    // VPDMB-5-p-sst  2
        520031,    // VPDMB-5-p-sst  3
        520041,    // VPDMB-5-p-sst  4
        520051,    // VPDMB-5-p-sst  5

        54,        // vpd-zdc-novtx  6
        520005,    // vpd-zdc-novtx  7
        520015,    // vpd-zdc-novtx  8
        570005     // vpd-zdc-novtx  9
    };

//D0 reco cuts

    int   const nPtBins = 5;
    float const PtBinsEdge[nPtBins + 1] = {0., 1., 2., 3., 5., 10.};
    float const dcaV0ToPv[nPtBins] = {0.0061, 0.0049, 0.0038, 0.0038, 0.0040};
    float const decayLength[nPtBins] = {0.0145, 0.0181, 0.0212, 0.0247, 0.0259};
    float const cosTheta[nPtBins] = {0.95, 0.95, 0.95, 0.95, 0.95};
    float const dcaDaughters[nPtBins] = {0.0084, 0.0066, 0.0057, 0.0050, 0.0060}; //0.0050;
    float const kDca[nPtBins] = {0.0103, 0.0091, 0.0095, 0.0079, 0.0058};//0.008, // minimum
    float const pDca[nPtBins] = {0.0110, 0.0111, 0.0086, 0.0081, 0.0062};//0.008

//Event cuts code

mEvent = (StPicoEvent*) mPicoDst->event();
if(!mEvent) return -1;

B = mEvent->bField();
int runId = mEvent->runId();
hnEvents->Fill(0);
nEvents++;

bool isVpdMB5=kFALSE;
for(int i=0;i<6;i++) { if(mEvent->isTrigger(TriggerConst::mTriggerId[i])) isVpdMB5=kTRUE ;}
if(runId < 17062047) isVpdMB5 = false;
if(!isVpdMB5) return -1;

hnEvents->Fill(1);

if(mRefMultCorr->isBadRun((Int_t)mEvent->runId())){
    if(DEBUG) cout<<"Not my event rejected by RefMultCorr"<<endl;
        return -1;
        }
        if(DEBUG) cout<<"Find my event"<<endl;
        mRefMultCorr->init((Int_t)mEvent->runId());
        Vertex = mEvent->primaryVertex();
        float VertexXPos = Vertex.x();
        float VertexYPos = Vertex.y();
        float VertexZPos = Vertex.z();
        float vpdVz = mEvent->vzVpd();
        float Vr = sqrt(pow(VertexXPos,2.)+pow(VertexYPos,2.));
        
        if(mUseHist) {
            h2VpdVzTpcVz->Fill(VertexZPos,vpdVz);
            h2VxVy->Fill(VertexYPos,VertexXPos);
            hVz->Fill(VertexZPos);
            hdiffVz->Fill(VertexZPos-vpdVz);
        }
        
        if(fabs(VertexXPos)<1e-5 && fabs(VertexYPos)<1e-5 && fabs(VertexZPos)<1e-5) return -1;
        hnEvents->Fill(2);
        if(fabs(VertexZPos)>6)   return -1;
        hnEvents->Fill(3);
        if(Vr>2) return -1;
        hnEvents->Fill(4);
        if(fabs(VertexZPos-vpdVz)>3) return -1;
        hnEvents->Fill(5);
        
// Tracks Cuts Code
        nPar1 = 0;
        nPar2 = 0;
    int nTr = mPicoDst->numberOfTracks();
    for(int i=0;i<nTr;i++ ){
        mTrack= (StPicoTrack*) mPicoDst->track(i);
        if(!mTrack) continue;
        int q = mTrack->charge();
        if(abs(q)!=1)continue;
        bool isHFT = mTrack->isHFTTrack();
        if(!isHFT) continue;
        
        StPhysicalHelixD helix = mTrack->helix(B);
        StThreeVectorF gmom = helix.momentumAt(helix.pathLength(Vertex), B * kilogauss);  //primary vertex.
        float gdca = helix.geometricSignedDistance(Vertex); // helix to vtx DCA
        if(gdca<0) gdca = fabs(gdca);
        StThreeVectorF dcaPoint = helix.at(helix.pathLength(Vertex.x(),Vertex.y()));
        StThreeVectorF dcaPoint3D = helix.at(helix.pathLength(Vertex));
        
        float p = gmom.mag();
        float pt = gmom.perp();
        float eta = gmom.pseudoRapidity();
        double phi = gmom.phi();
        double phi0 = phi; // range 0~2PI
        if(phi0 < 0) phi0 += twoPI;
        int nHitsFit = (int) fabs(mTrack->nHitsFit());
        int nHitsMax = (int) fabs(mTrack->nHitsMax());
        if(nHitsMax==0) continue;
        float nsigmapi = mTrack->nSigmaPion();
        float nsigmak = mTrack->nSigmaKaon();

        Int_t   btofMatchFlag =  0;
        Float_t btofYLocal    =  -999;
        int index2tof = mTrack->bTofPidTraitsIndex();
        float beta = -1.;
        if(index2tof>=0) {
            StPicoBTofPidTraits *tofPid = mPicoDst->btofPidTraits(index2tof);
            btofMatchFlag = tofPid->btofMatchFlag();
            btofYLocal    = tofPid->btofYLocal();
            if(tofPid)
            {
                beta = tofPid->btofBeta();
                if(beta<1e-4)
                {
                    StThreeVectorF btofHitPos = tofPid->btofHitPos();
                    
                    float L = tofPathLength(&Vertex, &btofHitPos, helix.curvature());
                    float tof = tofPid->btof();
                    if(tof>0) beta = L/(tof*(C_C_LIGHT/1.e9));
                    else beta = -1;
                }
            }
        }
        bool isGoodTof = btofMatchFlag>0 && beta>0 && fabs(btofYLocal)<1.8;
        
        float diffBetaPi = -1;
        float diffBetaK = -1;
        float IvaBetaTpcpi = -999;
        float IvaBetaTpck = -999;
        float IvaBeta  = -999;
        if(isGoodTof) {
            IvaBetaTpcpi = sqrt(massPi*massPi/p/p + 1);
            IvaBetaTpck = sqrt(massK*massK/p/p + 1);
            IvaBeta = 1./beta;
            diffBetaPi = IvaBeta - IvaBetaTpcpi;
            diffBetaK = IvaBeta - IvaBetaTpck;
        }
        
        bool isTpcPion = fabs(nsigmapi)<3.0;
        bool isTpcKaon = fabs(nsigmak)<2.0;
        bool isTofPion = isGoodTof && fabs(diffBetaPi)<0.03;
        bool isTofKaon = isGoodTof && fabs(diffBetaK)<0.03;
        
        bool isGoodTrack = (pt>0.6//pt>0.15
                            && fabs(eta)<1
                            && nHitsFit>=20
                            && gdca>0.002
                            );
        if(!isGoodTrack) continue;
        
        bool isPionCandidate = false;
        if(isTpcPion) {
            if(!isGoodTof) {
                isPionCandidate = true;
            }
            else {
                if(isTofPion) isPionCandidate = true;
            }
        }
        
        bool isKaonCandidate = false;
        if(isTpcKaon) {
            if(!isGoodTof) {
                isKaonCandidate = true;
            }
            else {
                if(isTofKaon) isKaonCandidate = true;
            }
        }
        
        if(isPionCandidate) {
            mcharge1[nPar1] = q;
            gmomx1[nPar1] = gmom.x();
            gmomy1[nPar1] = gmom.y();
            gmomz1[nPar1] = gmom.z();
            moriginx1[nPar1] = dcaPoint3D.x();
            moriginy1[nPar1] = dcaPoint3D.y();
            moriginz1[nPar1] = dcaPoint3D.z();
            mDiffInvBeta1[nPar1] = diffBetaPi;
            mNsigmaX1[nPar1] = nsigmapi;
            mTofMatch1[nPar1] = isGoodTof;
            mtrackID1[nPar1] = i;
            mdca1[nPar1] = gdca;
            mClean1[nPar1] = isClean;
            mEta1[nPar1] = eta;
            mqx1[nPar1] = qx;
            mqy1[nPar1] = qy;
            
            nPar1++;
        }
        //partner particle
        if(isKaonCandidate) {
            mcharge2[nPar2] = q;
            gmomx2[nPar2] = gmom.x();
            gmomy2[nPar2] = gmom.y();
            gmomz2[nPar2] = gmom.z();
            moriginx2[nPar2] = dcaPoint3D.x();
            moriginy2[nPar2] = dcaPoint3D.y();
            moriginz2[nPar2] = dcaPoint3D.z();
            mDiffInvBeta2[nPar2] = diffBetaK;
            mNsigmaX2[nPar2] = nsigmak;
            mTofMatch2[nPar2] = isGoodTof;
            mtrackID2[nPar2] = i;
            mdca2[nPar2] = gdca;
            mClean2[nPar2] = isClean;
            mEta2[nPar2] = eta;
            mqx2[nPar2] = qx;
            mqy2[nPar2] = qy;
            nPar2++;
        }

    }

        
// D0 Cuts
        
for(int i=0; i<nPar1; i++) {
    int q1 = mcharge1[i];
    StThreeVectorD pos1_ori(moriginx1[i],moriginy1[i],moriginz1[i]);
    StThreeVectorD mom1_ori(gmomx1[i],gmomy1[i],gmomz1[i]);
    StPhysicalHelix helix1(mom1_ori*GeV,pos1_ori*centimeter,B*kilogauss,q1);
    float pt1 = mom1_ori.perp();
    float p1 = mom1_ori.mag();
    for(int j=0; j<nPar2; j++) {
        
        int q2 = mcharge2[j];
        bool mUL = kFALSE;
        if(q1+q2==0) mUL = kTRUE;
        else if(q1==q2) mUL = kFALSE;
        else continue;
        if(mtrackID1[i] == mtrackID2[j]) continue;
        StThreeVectorD pos2_ori(moriginx2[j],moriginy2[j],moriginz2[j]);
        StThreeVectorD mom2_ori(gmomx2[j],gmomy2[j],gmomz2[j]);
        StPhysicalHelix helix2(mom2_ori*GeV,pos2_ori*centimeter,B*kilogauss,q2);
        float pt2 = mom2_ori.perp();
        float p2 = mom2_ori.mag();
        
        StPhysicalHelix line1(mom1_ori*GeV,pos1_ori*centimeter,0,q1); //B=0, the momtum of this helix equal to 0
        StPhysicalHelix line2(mom2_ori*GeV,pos2_ori*centimeter,0,q2);  //if momemtum is not at the dca, can't use straight line to aproach the dca betweent two helix
        
        pairD pair = line1.pathLengths(line2);
        StThreeVectorF pos1 = line1.at(pair.first);
        StThreeVectorF pos2 = line2.at(pair.second);
        float pairDCA = (pos1 - pos2).mag();  /// pair DCA
        StThreeVectorF V0 = (pos1+pos2)*0.5;   // secendary vertex
        StThreeVectorF V0ToPv  = V0 - Vertex;
        float decayLength = V0ToPv.mag();
        
        StThreeVectorF mom1 = helix1.momentumAt(pair.first, B*kilogauss);
        StThreeVectorF mom2 = helix2.momentumAt(pair.second, B*kilogauss);
        float E1 = TMath::Sqrt(mom1.mag2() + massPi*massPi);
        float E2 = TMath::Sqrt(mom2.mag2() + massK*massK);
        StLorentzVector<Float_t> FourMom1(mom1,E1);
        StLorentzVector<Float_t> FourMom2(mom2,E2);
        StLorentzVector<Float_t> FourMom = FourMom1 +FourMom2;
        StThreeVectorF Mom = mom1 + mom2;
        float mass = FourMom.m();
        float pt = FourMom.perp();
        float y = FourMom.rapidity();
        float eta = FourMom.pseudoRapidity();
        float phi = FourMom.phi();
        if(phi<0) phi += twoPI;
        float openAngle = V0ToPv.angle(Mom);
        float cosTheta = TMath::Cos(openAngle);   //cosTheta
        float DCA = V0ToPv.mag()*TMath::Sin(openAngle); //Dca
        
        bool isD0 = ( mdca2[j] > anaCuts::kDca[ptIndex] &&    //1--pi--i, 2--k--j
                     mdca1[i] > anaCuts::pDca[ptIndex] &&
                     pairDCA < anaCuts::dcaDaughters[ptIndex] &&
                     decayLength > anaCuts::decayLength[ptIndex] &&
                     cosTheta > anaCuts::cosTheta[ptIndex] &&
                     DCA < anaCuts::dcaV0ToPv[ptIndex]
                     );
        bool daughterPtCut = pt1>0.6 && pt2>0.6;
      
        bool isReco = mass>1.6 && mass<2.1 && fabs(cosTheta)>0.95 && fabs(y)<1.0;
        if(isReco && isD0 && daughterPtCut)
    {
        if(mUL)
            h3MassUn->Fill(pt,centrality,mass);
        else
            h3MassLs->Fill(pt,centrality,mass);
    }
        




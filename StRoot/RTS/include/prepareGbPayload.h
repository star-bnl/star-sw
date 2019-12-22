#ifndef __LXGBX_H__
#define __LXGBX_H__

// Emulate LXGBX in a single class
//
// To use
//
// 1.  SEND_CONFIG time, call --> config()
// 2.  for each event call    --> lxgbx()
// 3.  place an SFS file called "EventSummary"
//     containing gbPayload first in the message to EVBX
//     (immediately following the iccp2k header)

#include <iccp2k.h>
#include <rtsSystems.h>
#include <RC_Config.h>
#include <rtsLog.h>
#include <time.h>
#include <daqFormats.h>
#include <daq100Decision.h>
#include <XMLCFG/SimpleXmlDoc.h>

class Lxgbx {
 public:
    int run_type;
    int cl_run;     // clusters for tpx
    int raw_write;  // raw write for tpx
    UINT64 dets_in_run_mask;

    UINT64 tokenZeroTriggers;
 

    struct evpCfg {
	UINT32 groupdef[EVP_GROUP_MAX][2];  // [trigger group] [0 low mask, 1 highmask]
	float rate[EVP_GROUP_MAX];   // -1 -> take every one, 0 -> take none
	int policy;
    } evpCfg;

    struct evpCtrs {
	float runStartTime;
	int cnt[EVP_GROUP_MAX];
    } evpCtrs;

    Lxgbx() {};
  
    int configEvp(SimpleXmlDoc *xml, int divisor=1) {
	// zero out counters...
	evpCtrs.runStartTime = 0;   // untill the first event!
	memset(evpCtrs.cnt, 0, sizeof(evpCtrs.cnt));
      
	// do configuration
	evpCfg.policy = xml->getParamI("trg_setup.extra.evp_policy");

	for(int i=0;i<EVP_GROUP_MAX;i++) {
 
	    evpCfg.groupdef[i][0] = xml->getParamMask(0, "trg_setup.evpGroup[%d].definition", i);
	    
	    evpCfg.groupdef[i][1] = xml->getParamMask(1, "trg_setup.evpGroup[%d].definition", i);
      
	    evpCfg.rate[i] = xml->getParamI("trg_setup.evpGroup[%d].rate", i);

	    LOG(NOTE , "Groups[%d].rate = %f : def[0] = 0x%x  def[1] = 0x%x",i,evpCfg.rate[i],evpCfg.groupdef[i][0],evpCfg.groupdef[i][1],0);

	    if((evpCfg.rate[i] > 0.0) && (divisor > 1)) {
		evpCfg.rate[i] /= (float)divisor;
	    }
      
	    if((int)(evpCfg.rate[i]*1000) > 0) {
		LOG(NOTE, "configEvp: rate[%d]*1000 = %d (0x%x-0x%x): divisor=%d",i,(int)(evpCfg.rate[i]*1000),evpCfg.groupdef[i][0],evpCfg.groupdef[i][1],divisor);
	    }
	} 

	return 0;  
    }

    /*
    // divisor is for use in EVB where rate gets divided by number of evbs...
    int configEvp(STAR_CFG *cfg, int divisor=1)
    {
	// zero out counters...
	evpCtrs.runStartTime = 0;   // untill the first event!
	memset(evpCtrs.cnt, 0, sizeof(evpCtrs.cnt));

	// do configuration
	EvpGroup *groups = cfg->trg_setup.evpGroup;
	evpCfg.policy = cfg->trg_run.EvpPolicy;
	for(int i=0;i<EVP_GROUP_MAX;i++) {
 
	    evpCfg.groupdef[i][0] = groups[i].definition[0];
	    evpCfg.groupdef[i][1] = groups[i].definition[1];
      
	    LOG(NOTE , "Groups[%d].rate = %f : def[0] = 0x%x  def[1] = 0x%x",i,evpCfg.rate[i],evpCfg.groupdef[i][0],evpCfg.groupdef[i][1],0);

	    evpCfg.rate[i] = groups[i].rate;
	    if((evpCfg.rate[i] > 0.0) && (divisor > 1)) {
		evpCfg.rate[i] /= (float)divisor;
	    }
      
	    if((int)(evpCfg.rate[i]*1000) > 0) {
		LOG(NOTE, "configEvp: rate[%d]*1000 = %d (0x%x-0x%x): divisor=%d",i,(int)(evpCfg.rate[i]*1000),evpCfg.groupdef[i][0],evpCfg.groupdef[i][1],divisor);
	    }
	} 

	return 0;
    }
    */

    int config(SimpleXmlDoc *xml) {
	int ret;
    
	ret = configEvp(xml);

	run_type = xml->getParamI("daq_setup.run_type");
	cl_run = xml->getParamI("daq_setup.detectors[%d].cl_done",TPX_ID);
	raw_write = xml->getParamI("daq_setup.detectors[%d].raw_write",TPX_ID);

	dets_in_run_mask = 0;

	for(int i=0;i<MAX_NODES;i++) {
	    int inrun = xml->getParamI("subsys_tasks.nodes[%d].inrun",i);

	    if(!inrun) continue;
	    
	    UINT32 node = xml->getParamI("subsys_tasks.nodes[%d].node",i);

	    LOG(DBG, "node = 0x%x", node,0,0,0,0);

	    int sys = GET_SYSTEM(node);
				 
	    if(sys == DAQ_SYSTEM) continue;
	    if(sys == TRG_SYSTEM) continue;
	    
	    dets_in_run_mask |= (1ll<<sys);
	}
	
	tokenZeroTriggers = 0;
	
	for(int i=0;i<TRIGGERS_MAX;i++) {
	    int inrun = xml->getParamI("trg_setup.triggers[%d].used", i);
	    if(!inrun) continue;

	    if(xml->getParamI("trg_setup.triggers[%d].userdata.tokenZero", i)) {
		tokenZeroTriggers |= (1ll<<i);
	    }
	}
	
	LOG(NOTE, "dets_in_run_mask = 0x%llx",dets_in_run_mask,0,0,0,0);
	return ret;
    }

    /*
    // Returns -1 if invalid configuration
    int config(STAR_CFG *cfg)
    {
	int ret;
    
	ret = configEvp(cfg);

	run_type = cfg->daq_setup.run_type;
	cl_run = cfg->daq_setup.detectors[TPX_ID].cl_done;
	raw_write = cfg->daq_setup.detectors[TPX_ID].raw_write;

	dets_in_run_mask = 0;

	for(int i=0;i<MAX_NODES;i++) {
	    TASK *n = &cfg->subsys_tasks.nodes[i];
	    if(!n->inrun) continue;

	    int sys = GET_SYSTEM(n->node);
      
	    if(sys == DAQ_SYSTEM) continue;
	    if(sys == TRG_SYSTEM) continue;

	    dets_in_run_mask |= (1<<sys);
	}

	tokenZeroTriggers = 0;

	for(int i=0;i<TRIGGERS_MAX;i++) {
	    Trigger *t = &cfg->trg_setup.triggers[i];
	    if(t->userdata.tokenZero) {
		tokenZeroTriggers |= (1<<i);
	    }
	}

	return ret;
    }
    */

    double mygettime() {
#ifdef __vxworks
	UINT32 sec = time(NULL);
	float currtime = (float)sec;
#else
	struct timeval tm;
	gettimeofday(&tm, NULL);
	double sec = tm.tv_sec;
	double usec = tm.tv_usec;

	double currtime = sec + usec / 1000000.0;

	LOG(DBG, "currtime = %lf",currtime);
#endif


	return currtime;
    }

    UINT32 evpAssign(UINT32 trg_lo, UINT32 trg_hi)
    {
	static double resettime=0;
	static long int seed = 0;
	
	double currtime = mygettime();

	if(seed == 0) {
	    seed = (long int)(currtime * 1e6);
	    srand48(seed);
	    LOG("JEFF", "seed48=%llx %d", seed, sizeof(seed));
	}

	if(currtime > 15 + resettime) {
	    resettime = currtime + drand48();
	    
	    for(int i=0;i<EVP_GROUP_MAX;i++) {
		evpCtrs.cnt[i] = 0;
	    }
	}

	double et = currtime - resettime;
	if(et < .01) et = .01;
	LOG(NOTE, "currtime=%d resettime=%d et*1000=%d",(int)currtime,(int)resettime,(int)(et*1000),0,0);

	// get event group mask
	UINT32 grpmask = 0;
	for(int i=0;i<EVP_GROUP_MAX;i++) {
	    if((trg_lo & evpCfg.groupdef[i][0]) ||
	       (trg_hi & evpCfg.groupdef[i][1])) {
		grpmask |= (1<<i);
	    }
	}

	LOG(NOTE, "Event: et*1000=%d trg_lo=0x%x trg_hi=0x%x grpmask=0x%x",(int)(et*1000),trg_lo,trg_hi,grpmask,0);

	// get firemask (after rates)
	UINT32 firemask = 0;
	for(int i=0;i<EVP_GROUP_MAX;i++) {
	    if(!(grpmask & (1<<i))) continue;
      
	    double r = ((double)evpCtrs.cnt[i]/et);
	    LOG(NOTE, "group[%d] rate*1000=%d",i, (int)(r * 1000),0,0,0);

	    if(r < evpCfg.rate[i]) {
		firemask |= (1<<i);
	
		LOG(NOTE, "EVP[%d]: r*1000=%d rate*1000=%d, cnt=%d et=%d",i,(int)(r*1000),(int)(evpCfg.rate[i]*1000),evpCtrs.cnt[i],(int)et);
	    }
	    else {
		LOG(NOTE, "NOEVP[%d]: r*1000=%d rate*1000=%d, cnt=%d et=%d",i,(int)(r*1000),(int)(evpCfg.rate[i]*1000),evpCtrs.cnt[i],(int)et);
	    }
      
	    if(evpCfg.rate[i] < 0) {
		LOG(NOTE, "Set fire mask because of neg rate[%d]*1000 %d?",i,(int)(evpCfg.rate[i]*1000),0,0,0);
		firemask |= (1<<i);
	    }
	}
    

	if(evpCfg.policy == 1) { // all events
	    LOG(NOTE, "Set fire mask because of take all",0,0,0,0,0);
	    firemask = 1;
	}
    
	if(evpCfg.policy == 2) { // 10 hz
	    double r = ((double)evpCtrs.cnt[0]/et);
	    if(r < 10.0) {
		LOG(NOTE, "Set fire mask because of 10hz",0,0,0,0,0);
		firemask = 1;
	    }
	    else {
		firemask = 0;
	    }
	}
    
	for(int i=0;i<EVP_GROUP_MAX;i++) {
	    if(firemask & (1<<i)) evpCtrs.cnt[i]++;
	}

	return firemask;
    }

    // assume evtDescData comes in as big endian...
    // assume other args are host endian
    //
    // l25abort --> 1 = l2 abort
    //              2 = l2 timeout
    int doEvent(gbPayload *pay, EvtDescData *evt, UINT32 l1trg_lo, UINT32 l2trg_lo, UINT32 l25abort, UINT32 token, UINT32 eventNumber, UINT32 l1trg_hi=0, UINT32 l2trg_hi=0)
    {
       	pay->gbPayloadVersion = l2h32(GB_PAYLOAD_VERSION);
	pay->eventNumber = l2h32(eventNumber);
	pay->token = l2h32(token);


	if(&pay->EventDescriptor == evt) {
	    LOG(NOTE, "Event descriptor is already in payload!   Fine.",0,0,0,0,0);
	}
	else {
	    LOG(NOTE, "Event descriptor not in payload, copy it! %p %p", &pay->EventDescriptor, evt,0,0,0);
	    memcpy(&pay->EventDescriptor, evt, sizeof(EvtDescData));
	}
	
	// The rest should be little endian 
	pay->L1summary[0] = l2h32(l1trg_lo);
	pay->L1summary[1] = l2h32(l1trg_hi);
	pay->L2summary[0] = l2h32(l2trg_lo);
	pay->L3summary[0] = l2h32(l2trg_lo);
	pay->L2summary[1] = l2h32(l2trg_hi);
	pay->L3summary[1] = l2h32(l2trg_hi);

	LOG(NOTE, "l1: 0x%x/0x%x,   l2: 0x%x/0x%x   evtdesc->dets: 0x%x",
	    pay->L1summary[0],
	    pay->L1summary[1],
	    pay->L2summary[0],
	    pay->L2summary[1],
	    evt->actionWdDetectorBitMask);


	pay->evp = l2h32(evpAssign(l2trg_lo, l2trg_hi));
	pay->L3summary[3] = pay->evp;

#ifdef __vxworks
	struct timespec tm;
	clock_gettime(CLOCK_REALTIME, &tm);
	pay->sec = l2h32(tm.tv_sec);
	pay->usec = l2h32(tm.tv_nsec * 1000);
#else
	struct timeval tm;
	gettimeofday(&tm, NULL);
	pay->sec = tm.tv_sec;
	pay->usec = tm.tv_usec;
#endif

	LOG(DBG, "Payload: ver=0x%x token=%d trgcmd=%d daqcmd=0x%x",
	    pay->gbPayloadVersion,
	    pay->EventDescriptor.TrgToken,
	    pay->EventDescriptor.actionWdTrgCommand,
	    pay->EventDescriptor.actionWdDaqCommand,0);

	pay->flags = daq100Decision(l2h32(pay->token), evt->actionWdDaqCommand, run_type, cl_run, raw_write);
    
	if(l25abort & 0x1) {
	    pay->flags |= EVBFLAG_L25ABORT;
	}
	if(l25abort & 0x2) {
	    pay->flags |= EVBFLAG_L25TIMEOUT;
	}

	pay->flags = l2h32(pay->flags);
    
	UINT32 awdetmask = b2h16(evt->actionWdDetectorBitMask);
      	LOG(NOTE, "grp_mask = 0x%x",awdetmask,0,0,0,0);
	
	UINT64 detmask = grp2rts_mask(awdetmask);

	
	LOG(NOTE, "potential det_mask  = 0x%llx dets_in_run_mask 0x%llx",detmask,dets_in_run_mask,0,0,0);

	detmask &= dets_in_run_mask;
	detmask |= (1ll<<TRG_SYSTEM);
	
	LOG(NOTE, "final det_mask = 0x%llx",detmask,0,0,0,0);

	pay->rtsDetMask = l2h64(detmask);

	LOG(NOTE, "super final det_mask = 0x%llx",pay->rtsDetMask,0,0,0,0);

	if(pay->flags & EVBFLAG_L25ABORT) {
	    LOG(DBG, "Sending L25Abort: token=%d event=%d 1l=0x%x l2=0x%x l2abort=%d",
		token, eventNumber, l1trg_lo, l2trg_lo, l25abort);
	}


	LOG(DBG, "l1: 0x%x/0x%x,   l2: 0x%x/0x%x",
	    pay->L1summary[0],
	    pay->L1summary[1],
	    pay->L2summary[0],
	    pay->L2summary[1],0);

	return 0;
    }

    int prepareTokenZeroPayload(gbPayload *pay, int eventNumber)
    {
	memset(pay, 0, sizeof(gbPayload));

	// Set event descriptor...  (in big endian)
	EvtDescData *des = (EvtDescData *)pay->eventDesc;
	des->TrgToken = b2h16(0);


	// WRONG!
	des->actionWdDetectorBitMask = b2h16(0);

	des->TrgDataFmtVer = FORMAT_VERSION & 0x000000ff;
	// 

	pay->gbPayloadVersion = l2h32(GB_PAYLOAD_VERSION);
	pay->eventNumber = l2h32(eventNumber);
	pay->token = 0;

	// The rest should be little endian 
	UINT32 tzt_lo = tokenZeroTriggers & 0xffffffff;
	UINT32 tzt_hi = (tokenZeroTriggers >> 32) & 0xffffffff;
	
	pay->L1summary[0] = l2h32(tzt_lo);
	pay->L1summary[1] = l2h32(tzt_hi);
	pay->L2summary[0] = l2h32(tzt_lo);
	pay->L2summary[1] = l2h32(tzt_hi);
	pay->L3summary[0] = l2h32(tzt_lo);
	pay->L3summary[1] = l2h32(tzt_hi);

	pay->evp = l2h32(1);
	pay->L3summary[3] = pay->evp;
    
#ifdef __vxworks
	struct timespec tm;
	clock_gettime(CLOCK_REALTIME, &tm);
	pay->sec = l2h32(tm.tv_sec);
	pay->usec = l2h32(tm.tv_nsec * 1000);
#else
	struct timeval tm;
	gettimeofday(&tm, NULL);
	pay->sec = tm.tv_sec;
	pay->usec = tm.tv_usec;
#endif

	//	int detmask = grp2rts_mask(dets_in_run_mask);
	UINT64 detmask = dets_in_run_mask | (1ll <<TRG_ID);
	pay->rtsDetMask = l2h64(detmask);    

	return 0;
    }
};

#endif

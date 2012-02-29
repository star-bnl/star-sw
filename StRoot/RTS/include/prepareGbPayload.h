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

class Lxgbx {
public:
  int run_type;
  int cl_run;     // clusters for tpx
  int raw_write;  // raw write for tpx
  UINT32 dets_in_run_mask;

  UINT32 tokenZeroTriggers;
 

  struct evpCfg {
    UINT32 groupdef[TRIGGERS_MAX][2];  // [trigger group] [0 low mask, 1 highmask]
    float rate[TRIGGERS_MAX];   // -1 -> take every one, 0 -> take none
    int policy;
  } evpCfg;

  struct evpCtrs {
    UINT32 runStartTime;
    int cnt[TRIGGERS_MAX];
  } evpCtrs;

  Lxgbx() {};
  
  // divisor is for use in EVB where rate gets divided by number of evbs...
  int configEvp(STAR_CFG *cfg, int divisor=1)
  {
    // zero out counters...
    evpCtrs.runStartTime = (UINT32)-1;   // untill the first event!
    memset(evpCtrs.cnt, 0, sizeof(evpCtrs.cnt));

    // do configuration
    EvpGroup *groups = cfg->trg_setup.evpGroup;
    evpCfg.policy = cfg->trg_run.EvpPolicy;
    for(int i=0;i<32;i++) {
      

      evpCfg.groupdef[i][0] = groups[i].definition[0];
      evpCfg.groupdef[i][1] = groups[i].definition[1];
      
      evpCfg.rate[i] = groups[i].rate;
      if((evpCfg.rate[i] > 0.0) && (divisor > 1)) {
	evpCfg.rate[i] /= (float)divisor;
      }
      
      if((int)(evpCfg.rate[i]*1000) > 0) {
	LOG(DBG, "configEvp: rate[%d]*1000 = %d",i,(int)(evpCfg.rate[i]*1000),0,0,0);
      }
    } 

    return 0;
  }

  
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

  UINT32 evpAssign(UINT32 trg_lo, UINT32 trg_hi)
  {
    if(evpCtrs.runStartTime == (UINT32)-1) {
      evpCtrs.runStartTime = time(NULL);
    }

#ifdef __vxworks
    UINT32 sec = time(NULL);
    UINT32 iet = sec - evpCtrs.runStartTime;
    float et = (float)iet;
    if(et < 1.0) et = 1;

    LOG(DBG, "et=%d",(int)et,0,0,0,0);
#else
    struct timeval tm;
    gettimeofday(&tm, NULL);
    float sec = tm.tv_sec;
    float usec = tm.tv_usec;

    float currtime = sec + usec / 1000000.0;

    float et = currtime - evpCtrs.runStartTime;
    if(et < 1.0) et = 1;
#endif
   
    // get event group mask
    UINT32 grpmask = 0;
    for(int i=0;i<32;i++) {
      if((trg_lo & evpCfg.groupdef[i][0]) ||
	 (trg_hi & evpCfg.groupdef[i][1])) {
	grpmask |= (1<<i);
      }
    }

    // get firemask (after rates)
    UINT32 firemask = 0;
    for(int i=0;i<32;i++) {
      if(!(grpmask & (1<<i))) continue;

      float r = ((float)evpCtrs.cnt[i]/et);
      
      if(r < evpCfg.rate[i]) {
	firemask |= (1<<i);

	LOG(DBG, "Set fire mask because of rate[%d]: r*1000=%d rate*1000=%d, cnt=%d et=%d",i,(int)r*1000,(int)evpCfg.rate[i]*1000,evpCtrs.cnt[i],(int)et);
      }

      if(evpCfg.rate[i] < 0) {
	LOG(DBG, "Set fire mask because of neg rate[%d]*1000 %d?",i,evpCfg.rate[i]*1000,0,0,0);
	firemask |= (1<<i);
      }
    }

    if(evpCfg.policy == 1) { // all events
      LOG(DBG, "Set fire mask because of take all",0,0,0,0,0);
      firemask |= 1;
    }
    
    if(evpCfg.policy == 2) { // 10 hz
      float r = ((float)evpCtrs.cnt[0]/et);
      if(r < 10.0) {
	LOG(DBG, "Set fire mask because of 10hz",0,0,0,0,0);
	firemask |= 1;
      }
      else 
	firemask = 0;
    }

    if(firemask) firemask |= 1;  // all triggers satisfy "any"
    
    for(int i=0;i<32;i++) {
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
    // Stays big endian
    EvtDescData *paydesc = (EvtDescData *)pay->eventDesc;
    memcpy(paydesc, evt, sizeof(EvtDescData));

    pay->gbPayloadVersion = l2h32(GB_PAYLOAD_VERSION);
    pay->eventNumber = l2h32(eventNumber);
    pay->token = l2h32(token);

    // The rest should be little endian 
    pay->L1summary[0] = l2h32(l1trg_lo);
    pay->L1summary[1] = l2h32(l1trg_hi);
    pay->L2summary[0] = l2h32(l2trg_lo);
    pay->L3summary[0] = l2h32(l2trg_lo);
    pay->L2summary[1] = l2h32(l2trg_hi);
    pay->L3summary[1] = l2h32(l2trg_hi);

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
    
    UINT32 detmask = b2h16(evt->actionWdDetectorBitMask);

    LOG(NOTE, "grp_mask = 0x%x",detmask,0,0,0,0);

    detmask = grp2rts_mask(detmask);

    LOG(NOTE, "potential det_mask = 0x%x",detmask,0,0,0,0);

    detmask &= dets_in_run_mask;
    detmask |= (1<<TRG_SYSTEM);

    LOG(NOTE, "final det_mask = 0x%x",detmask,0,0,0,0);

    pay->rtsDetMask = l2h32(detmask);

    if(pay->flags & EVBFLAG_L25ABORT) {
      LOG(DBG, "Sending L25Abort: token=%d event=%d 1l=0x%x l2=0x%x l2abort=%d",
	  token, eventNumber, l1trg_lo, l2trg_hi, l25abort);
    }

    return 0;
  }

  int prepareTokenZeroPayload(gbPayload *pay, int eventNumber)
  {
    memset(pay, 0, sizeof(gbPayload));

    // Set event descriptor...  (in big endian)
    EvtDescData *des = (EvtDescData *)pay->eventDesc;
    des->TrgToken = b2h16(0);
    des->actionWdDetectorBitMask = b2h16(dets_in_run_mask);
    des->TrgDataFmtVer = FORMAT_VERSION & 0x000000ff;
    // 

    pay->gbPayloadVersion = GB_PAYLOAD_VERSION;
    pay->eventNumber = l2h32(eventNumber);
    pay->token = 0;

    // The rest should be little endian 
    pay->L1summary[0] = l2h32(tokenZeroTriggers);
    pay->L2summary[0] = l2h32(tokenZeroTriggers);
    pay->L3summary[0] = l2h32(tokenZeroTriggers);
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

    int detmask = grp2rts_mask(dets_in_run_mask);
    pay->rtsDetMask = l2h32(detmask);    

    return 0;
  }
};

#endif

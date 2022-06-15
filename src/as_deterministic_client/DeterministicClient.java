package as_deterministic_client;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

public final class DeterministicClient {

	private Integer workersCount = 10;
	private Random rand = new Random();
	private Long lastSetRandomSeed = null;
	private ActivationReturnType activationState = null;
	private Integer kRunLoopCount = -1;
	private boolean _initialized = false; 
	private TimeStamp lastActivation = null;
	private TimeStamp cycleTime = null;
	private Integer poolRun = 0;
	
	public DeterministicClient () {
		// each worker should be configurable with the data set number it services???
		// worker needs to know the activation cycle and data set number, (and which call to random)???
	}

	
	void setParameters(Integer kRunLoopCount, TimeStamp cycleTime, Integer numberofWorkers) {
		if(_initialized)
			throw new IllegalStateException(this.getClass().getSimpleName()+" can only be initialized once!");
		assert numberofWorkers != null;
		this.workersCount = numberofWorkers;
		if(kRunLoopCount != null) {
			this.kRunLoopCount = kRunLoopCount;
		}
		if(cycleTime != null) {
			this.cycleTime = cycleTime;
		}
	
	}
	
	public Result<ActivationReturnType> waitForActivation() {
		Long timeRemain = null;
		poolRun = 0;
		if (activationState == null) {
			activationState = ActivationReturnType.kRegisterServices; 
		}else if (activationState == ActivationReturnType.kRegisterServices) {
			activationState = ActivationReturnType.kServiceDiscovery;
		}else if (activationState == ActivationReturnType.kServiceDiscovery) {
			activationState = ActivationReturnType.kInit;
		}else if (activationState == ActivationReturnType.kInit) { 
			activationState = ActivationReturnType.kRun;
		}else if (activationState == ActivationReturnType.kRun) {
			if(cycleTime != null) {
				timeRemain = lastActivation.shift(cycleTime).elapsed(TimeStamp.now()).getTime();
				if(timeRemain < 0) {
					return new Result<ActivationReturnType>(ErrorType.kCycleOverrun);					
				}
			}
		}else if (activationState == ActivationReturnType.kTerminate) {
			return new Result<ActivationReturnType>(ErrorType.kFailed);
		}
		if(activationState == ActivationReturnType.kRun) {
			if (kRunLoopCount > 0) { 
				kRunLoopCount--;
			}else if(kRunLoopCount == 0) {
				activationState = ActivationReturnType.kTerminate;
			}else {
				assert kRunLoopCount == -1;
			}
			if(timeRemain != null) {
				try {
					synchronized(this) {
						long millis = timeRemain / 1000000;
						this.wait(millis, (int)(timeRemain - millis*1000000));
					}
				}catch(InterruptedException ie) {
					
				}
			}else{
				// TODO event driven? do wait() and trigger notify() externally?
			}
			lastActivation = TimeStamp.now();
		}
		return new Result<ActivationReturnType>(activationState);
	}
	
	public <ValueType> Result<Void> runWorkerPool(WorkerRunnable<ValueType> runnableObj, Iterator<ValueType> container) {
		assert workersCount > 0;
		List<WorkerThreadImpl> workers = new ArrayList<WorkerThreadImpl>();
		for(int i=0;i<workersCount;i++) {
			workers.add(new WorkerThreadImpl());
		}
		int count = 0;
		// TODO run #workers.size() parallel threads at the time
		// And this should be configurable through the setup 
		while(container.hasNext()) {
			WorkerThreadImpl wt = workers.get(count % workers.size());
			if(lastSetRandomSeed != null) {
				wt.setRandomSeed(lastSetRandomSeed*(poolRun+1)*(count+1));
			}
			runnableObj.run(container.next(), wt);
			count++; 
		}
		poolRun++;
		return new Result<Void>();
	}
	

	/**
	 * It is the responsibility of the lock step manager to set the seed to
	 * the same value for all clients working redundantly.
	 */ 
	public void setRandomSeed(Long seed) {
		lastSetRandomSeed = seed;
		rand.setSeed(seed);
		// TODO
		// forward the same seed to the workers (seed + data set), or set something like last seed
	}
	
	public Result<TimeStamp> getActivationTime() {
		// Only in kRun state
		if(lastActivation == null)
			return new Result<TimeStamp>(ErrorType.kNoTimeStamp);
		return new Result<TimeStamp>(lastActivation);
	}

	public Result<TimeStamp> getNextActivationTime() {
		// Only in kRun state and only for cyclic operation,
		// so no time stamp if this activation is event driven, not time driven
		if(lastActivation == null || cycleTime == null)
			return new Result<TimeStamp>(ErrorType.kNoTimeStamp);
		return new Result<TimeStamp>(lastActivation.shift(cycleTime));
	}
	
	public Long getRandom() {
		return rand.nextLong();
	}

	private class WorkerThreadImpl implements WorkerThread {
		private Random r = new Random();
		
		private void setRandomSeed(Long seed) {
			r.setSeed(seed);
		}
		
		@Override
		public Long getRandom() {
			return r.nextLong();
		}
		
	}
}

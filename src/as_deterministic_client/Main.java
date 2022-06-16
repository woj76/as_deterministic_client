package as_deterministic_client;

import java.util.Arrays;

public class Main {

	public static void main(String[] args) {
		DeterministicClient dc = new DeterministicClient();
		int cycles = 10;
		dc.setParameters(cycles, TimeStamp.period(1000000000L) /* duration of one cycle, 1 sec. */, 4 /* worker threads */, true /* run workers in parallel */);
		// When this is set, all random numbers in a complete run should be repeatable
		dc.setRandomSeed(1234567890L);
		System.out.println("waitForActivation: "+dc.waitForActivation());
		System.out.println("getActivationTime: "+dc.getActivationTime());
		System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
		System.out.println("getRandom: "+dc.getRandom());
		System.out.println("waitForActivation: "+dc.waitForActivation());
		System.out.println("getActivationTime: "+dc.getActivationTime());
		System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
		System.out.println("getRandom: "+dc.getRandom());
		System.out.println("waitForActivation: "+dc.waitForActivation());
		System.out.println("getActivationTime: "+dc.getActivationTime());
		System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
		System.out.println("getRandom: "+dc.getRandom());
		for(int i=0; i<cycles;i++) {
			System.out.println("waitForActivation: "+dc.waitForActivation());
			System.out.println("getActivationTime: "+dc.getActivationTime());
			System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
			System.out.println("getRandom: "+dc.getRandom());
			dc.runWorkerPool(new WorkerRunnable<Integer>() {
				@Override
				public void run(Integer element, WorkerThread t) {
					System.out.println(t.getRandom());
					try {
						Thread.sleep(element*100);
					} catch (InterruptedException ie) {
					}
				}
				
			}, Arrays.asList(new Integer[] {2, 3, 4, 5}));
			
		}
		System.out.println("waitForActivation: "+dc.waitForActivation());
		System.out.println("getActivationTime: "+dc.getActivationTime());
		System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
		System.out.println("getRandom: "+dc.getRandom());
		
		
	}

}

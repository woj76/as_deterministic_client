package as_deterministic_client;

import java.util.Random;

public class Main {

	public static void main(String[] args) {
		DeterministicClient dc = new DeterministicClient();
		int cycles = 10;
		dc.setParameters(cycles, TimeStamp.period(1000000000L) /* 1 sec. */, 4);
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
		}
		System.out.println("waitForActivation: "+dc.waitForActivation());
		System.out.println("getActivationTime: "+dc.getActivationTime());
		System.out.println("getNextActivationTime: "+dc.getNextActivationTime());
		System.out.println("getRandom: "+dc.getRandom());
		
		
	}

}

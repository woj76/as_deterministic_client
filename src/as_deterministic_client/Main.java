package as_deterministic_client;

import java.util.Random;

public class Main {

	public static void main(String[] args) {
		System.out.println(ActivationReturnType.kInit);

		Random r = new Random(100);
		System.out.println(r.nextLong());
		System.out.println(r.nextLong());
		
		r = new Random();
		r.setSeed(100);
		System.out.println(r.nextLong());
		System.out.println(r.nextLong());
		
		System.out.print("a".repeat(Math.max(0, 0)));

	}

}

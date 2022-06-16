package as_deterministic_client;

public class TorXakisInterface {
	private int id;
	private DeterministicClient dc;
	private boolean debug = true;
	
	public TorXakisInterface(int portNum) {
		id = portNum;
		dc = new DeterministicClient();
		dc.setRandomSeed(12345678L*id);
	}
	
	public String processMessage(String inMessage) {
		assert inMessage != null;
	    if(debug)
	    	System.out.printf("[%d][%s][%s]\n", id, dc.toString(), inMessage);
		if(inMessage.equals("stop")) 
			return "EXIT";
		String[] tokens = inMessage.split(" ");
		if(tokens[0].equals("start")) {
			assert tokens.length == 5;
			try {
				dc.setParameters(
					Integer.parseInt(tokens[1]),
					TimeStamp.period(Long.parseLong(tokens[2])),
					Integer.parseInt(tokens[3]),
					Boolean.valueOf(tokens[4]));
			}catch(IllegalStateException ise) {
				return "ERR";
			}
			return "OK";
		}
		if(tokens[0].equals("set_random_seed")) {
			assert tokens.length == 2;
			dc.setRandomSeed(Long.parseLong(tokens[1]));
			return "OK";
		}
		if(tokens[0].equals("wait_for_activation")) {
			assert tokens.length == 1;
			Result<ActivationReturnType> r = dc.waitForActivation();
			if(r.hasValue()) {
				return "OK "+r.getValue().ordinal();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_activation_time")) {
			assert tokens.length == 1;
			Result<TimeStamp> r = dc.getActivationTime();
			if(r.hasValue()) {
				return "OK "+r.getValue().getTime();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_next_activation_time")) {
			assert tokens.length == 1;
			Result<TimeStamp> r = dc.getNextActivationTime();
			if(r.hasValue()) {
				return "OK "+r.getValue().getTime();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_random")) {
			assert tokens.length == 1;
			Long r = dc.getRandom();
			return "OK "+r.toString();
		}
		// TODO run worker pool
		return "UNK";
	}
}

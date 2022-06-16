package as_deterministic_client;

import java.util.ArrayList;
import java.util.List;

public class TorXakisInterface {
	private int id;
	private List<DeterministicClient> objects = new ArrayList<DeterministicClient>();
	private boolean debug = true;
	
	public TorXakisInterface(int portNum) {
		id = portNum;
	}
	
	public String processMessage(String inMessage) {
		assert inMessage != null;
	    if(debug)
	    	System.out.printf("[%d][%s]\n", id, inMessage);
		if(inMessage.equals("stop")) 
			return "EXIT";
		String[] tokens = inMessage.split(" ");
		if(tokens[0].equals("create")) {
			assert tokens.length == 1;
			int objRef = objects.size();
			objects.add(new DeterministicClient());
			return "OK "+objRef;
		}
		assert tokens.length >= 2;
		int objId = Integer.parseInt(tokens[1]);
		if(tokens[0].equals("delete")) {
			objects.set(objId, null);
			return "OK";
		}
		if(objId >= objects.size() || objects.get(objId) == null) {
			return "ERR";
		}
		if(tokens[0].equals("start")) {
			assert tokens.length == 6;
			try {
				objects.get(objId).setParameters(
					Integer.parseInt(tokens[2]),
					TimeStamp.period(Long.parseLong(tokens[3])),
					Integer.parseInt(tokens[4]),
					Boolean.valueOf(tokens[5]));
			}catch(IllegalStateException ise) {
				return "ERR";
			}
			return "OK";
		}
		if(tokens[0].equals("set_random_seed")) {
			assert tokens.length == 3;
			objects.get(objId).setRandomSeed(Long.parseLong(tokens[2]));
			return "OK";
		}
		if(tokens[0].equals("wait_for_activation")) {
			assert tokens.length == 2;
			Result<ActivationReturnType> r = objects.get(objId).waitForActivation();
			if(r.hasValue()) {
				return "OK "+r.getValue().ordinal();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_activation_time")) {
			assert tokens.length == 2;
			Result<TimeStamp> r = objects.get(objId).getActivationTime();
			if(r.hasValue()) {
				return "OK "+r.getValue().getTime();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_next_activation_time")) {
			assert tokens.length == 2;
			Result<TimeStamp> r = objects.get(objId).getNextActivationTime();
			if(r.hasValue()) {
				return "OK "+r.getValue().getTime();
			}
			return "ERR "+r.getError().ordinal();
		}
		if(tokens[0].equals("get_random")) {
			assert tokens.length == 2;
			Long r = objects.get(objId).getRandom();
			return "OK "+r.toString();
		}
		if(tokens[0].equals("run_worker_pool")) {
			// TODO run worker pool
			return "TODO";
		}
		return "UNK";
	}
}
